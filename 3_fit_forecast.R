## =====================================================================
## 3_fit_forecast.R  --  Final fits under publication rules
##
## Cross-validation ranks the candidates; publication then filters them.
## A candidate may only be published if ALL of these hold:
##   R1  DRIFT      it carries a drift/trend term, so the path is not
##                  constant by construction (a no-drift d=1 model always
##                  forecasts a horizontal line)
##   R2  FLOOR      the whole 20-quarter path stays above a positive floor,
##                  so no cell is forecast to zero out
##   R3  MOVEMENT   the 5-year point differs from today by at least
##                  MIN_MOVE_PCT, catching drift estimates of ~0
## The best-ranked candidate satisfying all three is published. If none
## does, a log-scale drift model is forced as a last resort.
##
## Every departure from the raw CV winner is recorded per cell.
## =====================================================================

library(dplyr)
library(tidyr)
library(forecast)

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)
## cvr  <- readRDS("cu_count_cv.rds");   list2env(cvr,  .GlobalEnv)

FC_START <- c(2026, 2)

## ---------------------------------------------------------------------
## [3.1] Publication rules
## ---------------------------------------------------------------------
REQUIRE_DRIFT <- TRUE    # R1: no-drift d=1 models cannot be published
FLOOR_FRAC    <- 0.15    # R2: path must stay above 15% of today's count
FLOOR_ABS     <- 1       #     and never below 1 credit union
MIN_MOVE_PCT  <- 0.02    # R3: 5-year point must move at least 2%
MIN_MOVE_ABS  <- 1

## ---------------------------------------------------------------------
## [3.2] Selection and forecasting
## ---------------------------------------------------------------------
fits <- list(); fc_tables <- list(); diag_rows <- list()

for (i in seq_len(nrow(cells))) {

  tab    <- cells$tab_name[i]
  y      <- ts_list[[tab]]
  yv     <- as.numeric(y)
  last_v <- yv[N_Q]
  status <- cell_diag$status[cell_diag$tab_name == tab]
  trend  <- trend_tbl$has_trend[trend_tbl$tab_name == tab]

  floor_v <- max(FLOOR_FRAC * last_v, FLOOR_ABS)
  min_mv  <- max(MIN_MOVE_PCT * last_v, MIN_MOVE_ABS)

  rank_used <- NA_integer_; cv_winner <- NA_character_
  rmse_cost <- NA_real_;    rule_note <- ""; forced <- FALSE

  if (status == "MODEL") {

    sc      <- cv_summary[[tab]] %>% filter(eligible) %>% arrange(rank)
    cnd_all <- cand_store[[tab]]
    cv_winner <- sc$spec[1]
    can_log <- min(yv) > 0

    chosen <- NULL; rejected <- character(0)

    for (rk in seq_len(nrow(sc))) {
      cn  <- cnd_all[sc$cand_id[rk], ]
      lam <- if (is.na(cn$lambda)) NULL else cn$lambda

      ## R1: drift required
      if (REQUIRE_DRIFT && !cn$drift) {
        rejected <- c(rejected, paste0("rank ", rk, ": no drift (constant path)"))
        next
      }

      f <- tryCatch(
        Arima(y, order = c(cn$p, cn$d, cn$q),
              seasonal = list(order = c(cn$P, cn$D, cn$Q), period = 4),
              include.drift = cn$drift, lambda = lam, biasadj = TRUE, method = "ML"),
        error = function(e) NULL, warning = function(w) NULL)
      if (is.null(f)) next

      fcx <- tryCatch(forecast(f, h = H_MAX, level = c(80, 95), biasadj = TRUE),
                      error = function(e) NULL)
      if (is.null(fcx) || any(!is.finite(as.numeric(fcx$mean)))) next

      pth  <- as.numeric(fcx$mean)
      move <- abs(pth[H_MAX] - last_v)

      ## R2: positive floor over the whole path
      if (min(pth) < floor_v) {
        rejected <- c(rejected, sprintf("rank %d: path falls to %.1f (floor %.1f)",
                                        rk, min(pth), floor_v))
        next
      }
      ## R3: must actually move
      if (move < min_mv) {
        rejected <- c(rejected, sprintf("rank %d: 5-yr move only %.1f (min %.1f)",
                                        rk, move, min_mv))
        next
      }

      chosen <- list(f = f, fc = fcx, rk = rk, spec = sc$spec[rk]); break
    }

    ## Last resort: force a log-scale drift model (guaranteed positive,
    ## monotone, and non-constant whenever the historical drift is non-zero)
    if (is.null(chosen)) {
      forced <- TRUE
      lam_f  <- if (can_log) 0 else NULL
      f <- NULL
      for (ord in list(c(0, 1, 1), c(1, 1, 0), c(0, 1, 0))) {
        f <- tryCatch(
          Arima(y, order = ord, include.drift = TRUE,
                lambda = lam_f, biasadj = TRUE, method = "ML"),
          error = function(e) NULL, warning = function(w) NULL)
        if (!is.null(f)) break
      }
      if (is.null(f)) f <- Arima(y, order = c(0, 1, 0), include.drift = TRUE)
      fcx <- forecast(f, h = H_MAX, level = c(80, 95), biasadj = TRUE)
      chosen <- list(f = f, fc = fcx, rk = NA_integer_,
                     spec = paste0("ARIMA(", paste(arimaorder(f)[1:3], collapse = ","),
                                   ") w/ drift", if (can_log) " on log scale" else ""))
      rule_note <- "FORCED: no CV candidate satisfied the publication rules"
    }

    fit <- chosen$f; fc <- chosen$fc
    rank_used <- chosen$rk; spec_used <- chosen$spec
    rmse_cost <- if (!is.na(chosen$rk)) round(sc$rmse_all[chosen$rk] - sc$rmse_all[1], 3) else NA_real_

    if (!nzchar(rule_note)) {
      rule_note <- if (!is.na(rank_used) && rank_used == 1)
        "CV winner satisfied all publication rules"
      else paste0("Published CV rank ", rank_used, "; skipped -> ",
                  paste(head(rejected, 3), collapse = "; "))
    }
    method <- rule_note

  } else if (status == "SPARSE - naive fallback") {
    fit <- naive(y, h = H_MAX, level = c(80, 95)); fc <- fit
    spec_used <- "Naive (last value carried forward)"
    method <- "Series too sparse for ARIMA; naive fallback is constant by construction"
  } else {
    fits[[tab]] <- NULL
    fc_tables[[tab]] <- tibble::tibble(
      q_label = character(), horizon_q = integer(), point = numeric(),
      lo80 = numeric(), hi80 = numeric(), lo95 = numeric(), hi95 = numeric())
    diag_rows[[tab]] <- tibble::tibble(
      tab_name = tab, status = status, spec = "None - cell is empty",
      method = "No credit unions ever observed in this cell",
      aicc = NA_real_, sigma2 = NA_real_, lb_pvalue = NA_real_,
      has_trend = FALSE, forced = FALSE, cv_rank_used = NA_integer_,
      cv_winner_spec = NA_character_, rule_rmse_cost = NA_real_,
      min_path = NA_real_, chg_5yr_pct = NA_real_,
      fc_h4 = 0, fc_h12 = 0, fc_h20 = 0)
    next
  }

  fits[[tab]] <- fit

  ## Hard safety net: nothing is published at or below the floor
  pt <- pmax(as.numeric(fc$mean), floor_v)

  fq <- tibble::tibble(
    horizon_q = 1:H_MAX,
    year      = FC_START[1] + (FC_START[2] - 1 + 0:(H_MAX - 1)) %/% 4,
    quarter   = (FC_START[2] - 1 + 0:(H_MAX - 1)) %% 4 + 1) %>%
    mutate(q_label = paste0(year, "Q", quarter),
           point = round(pt),
           lo80  = round(pmax(as.numeric(fc$lower[, 1]), 0)),
           hi80  = round(pmax(as.numeric(fc$upper[, 1]), 0)),
           lo95  = round(pmax(as.numeric(fc$lower[, 2]), 0)),
           hi95  = round(pmax(as.numeric(fc$upper[, 2]), 0)),
           horizon_label = case_when(horizon_q == 4  ~ "1-year ahead",
                                     horizon_q == 12 ~ "3-year ahead",
                                     horizon_q == 20 ~ "5-year ahead",
                                     TRUE ~ ""))
  fc_tables[[tab]] <- fq

  res  <- residuals(fit)
  npar <- if (status == "MODEL") length(coef(fit)) else 0
  lb   <- tryCatch(Box.test(res, lag = 8, type = "Ljung-Box", fitdf = npar)$p.value,
                   error = function(e) NA_real_)

  diag_rows[[tab]] <- tibble::tibble(
    tab_name = tab, status = status, spec = spec_used, method = method,
    aicc   = if (status == "MODEL") round(fit$aicc, 2) else NA_real_,
    sigma2 = if (status == "MODEL") round(fit$sigma2, 3) else NA_real_,
    lb_pvalue = round(lb, 4), has_trend = trend, forced = forced,
    cv_rank_used = rank_used, cv_winner_spec = cv_winner,
    rule_rmse_cost = rmse_cost, min_path = min(fq$point),
    chg_5yr_pct = round(100 * (fq$point[20] - last_v) / max(last_v, 1), 1),
    fc_h4 = fq$point[4], fc_h12 = fq$point[12], fc_h20 = fq$point[20])

  cat(sprintf("[%2d/%2d] %-16s %-44s -> %4d / %4d / %4d  (%+.1f%%)%s\n",
              i, nrow(cells), tab, spec_used, fq$point[4], fq$point[12],
              fq$point[20], diag_rows[[tab]]$chg_5yr_pct,
              ifelse(forced, "  <-- FORCED", "")))
}

diagnostics <- bind_rows(diag_rows)

## ---------------------------------------------------------------------
## [3.3] Audit: nothing constant, nothing at zero
## ---------------------------------------------------------------------
audit <- diagnostics %>%
  filter(status != "EMPTY - no model") %>%
  mutate(is_constant = abs(chg_5yr_pct) < 100 * MIN_MOVE_PCT,
         hits_floor  = min_path <= 1) %>%
  select(tab_name, status, spec, cv_rank_used, forced, chg_5yr_pct,
         min_path, is_constant, hits_floor, method)

print(as.data.frame(audit), row.names = FALSE)

cat("\n--- PUBLICATION AUDIT ---\n")
cat("Cells published at CV rank 1 :", sum(audit$cv_rank_used == 1, na.rm = TRUE), "\n")
cat("Cells published at lower rank:", sum(audit$cv_rank_used > 1, na.rm = TRUE), "\n")
cat("Cells forced                 :", sum(audit$forced, na.rm = TRUE), "\n")
cat("Still constant               :", sum(audit$is_constant, na.rm = TRUE),
    "(should be sparse/naive cells only)\n")
cat("Still hitting the floor      :", sum(audit$hits_floor, na.rm = TRUE), "\n\n")

## Cells where the rules cost the most CV accuracy - review these by hand
diagnostics %>% filter(!is.na(rule_rmse_cost), rule_rmse_cost > 0) %>%
  arrange(desc(rule_rmse_cost)) %>%
  select(tab_name, spec, cv_winner_spec, cv_rank_used, rule_rmse_cost) %>%
  as.data.frame() %>% print(row.names = FALSE)

## ---------------------------------------------------------------------
## [3.4] Summary table
## ---------------------------------------------------------------------
summary_tbl <- cells %>%
  left_join(cell_diag %>% select(tab_name, n_nonzero, first_count, last_count),
            by = "tab_name") %>%
  left_join(diagnostics, by = "tab_name") %>%
  left_join(winners %>% select(tab_name, cv_rmse_all = rmse_all,
                               cv_rmse_h4 = rmse_h4, cv_rmse_h12 = rmse_h12,
                               cv_rmse_h20 = rmse_h20), by = "tab_name") %>%
  mutate(chg_1yr = fc_h4 - last_count, chg_3yr = fc_h12 - last_count,
         chg_5yr = fc_h20 - last_count) %>%
  select(cell_id, tab_name, region, cu_type, asset_cat, label,
         actual_2026Q1 = last_count, fc_1yr = fc_h4, fc_3yr = fc_h12,
         fc_5yr = fc_h20, chg_1yr, chg_3yr, chg_5yr, chg_5yr_pct, min_path,
         spec, status, has_trend, forced, cv_rank_used, cv_winner_spec,
         rule_rmse_cost, cv_rmse_all, cv_rmse_h4, cv_rmse_h12, cv_rmse_h20,
         aicc, lb_pvalue)

print(as.data.frame(summary_tbl), row.names = FALSE)

## ---------------------------------------------------------------------
## [3.5] Coherence check
## ---------------------------------------------------------------------
fit_total <- auto.arima(ts_total, seasonal = TRUE, stepwise = FALSE,
                        approximation = FALSE)
fc_total  <- forecast(fit_total, h = H_MAX, level = c(80, 95))

bottom_up <- fc_tables[sapply(fc_tables, nrow) > 0] %>%
  bind_rows(.id = "tab_name") %>%
  group_by(horizon_q, q_label) %>%
  summarise(bottom_up = sum(point), .groups = "drop")

coherence <- bottom_up %>%
  mutate(direct_total = round(as.numeric(fc_total$mean)),
         gap = bottom_up - direct_total,
         gap_pct = round(100 * gap / direct_total, 2),
         horizon_label = case_when(horizon_q == 4  ~ "1-year ahead",
                                   horizon_q == 12 ~ "3-year ahead",
                                   horizon_q == 20 ~ "5-year ahead",
                                   TRUE ~ ""))
print(as.data.frame(coherence), row.names = FALSE)

saveRDS(list(fits = fits, fc_tables = fc_tables, diagnostics = diagnostics,
             summary_tbl = summary_tbl, coherence = coherence, audit = audit,
             fit_total = fit_total, fc_total = fc_total, FC_START = FC_START,
             REQUIRE_DRIFT = REQUIRE_DRIFT, FLOOR_FRAC = FLOOR_FRAC,
             MIN_MOVE_PCT = MIN_MOVE_PCT),
        file = "cu_count_fits.rds")
