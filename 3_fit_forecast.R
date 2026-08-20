## =====================================================================
## 3_fit_forecast.R  --  Final fits, flat-forecast override, diagnostics
##
## Selection now has two stages:
##   (a) the CV winner from 2_cv.R
##   (b) if that model's forecast path is essentially constant AND the cell
##       has a statistically detectable trend, walk down the CV ranking to
##       the best non-flat candidate and publish that instead.
## Both models are recorded, along with the CV RMSE cost of the override.
## =====================================================================

library(dplyr)
library(tidyr)
library(forecast)

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)
## cvr  <- readRDS("cu_count_cv.rds");   list2env(cvr,  .GlobalEnv)

FC_START <- c(2026, 2)     # first forecast quarter

## ---------------------------------------------------------------------
## [3.1] Flatness rule
## A path is "flat" if the 5-year point forecast moves less than FLAT_TOL
## of the current level, or less than FLAT_MIN credit unions -- whichever
## is the looser test, so tiny cells are not forced to move.
## ---------------------------------------------------------------------
FLAT_TOL <- 0.02      # 2% of the current level over 5 years
FLAT_MIN <- 2         # or fewer than 2 credit unions of movement

fits       <- list()
fc_tables  <- list()
diag_rows  <- list()

for (i in seq_len(nrow(cells))) {

  tab    <- cells$tab_name[i]
  y      <- ts_list[[tab]]
  last_v <- as.numeric(y)[N_Q]
  status <- cell_diag$status[cell_diag$tab_name == tab]
  trend  <- trend_tbl$has_trend[trend_tbl$tab_name == tab]

  override    <- FALSE
  rank_used   <- NA_integer_
  cv_winner   <- NA_character_
  rmse_cost   <- NA_real_
  flat_note   <- ""

  if (status == "MODEL") {

    sc  <- cv_summary[[tab]] %>% filter(eligible) %>% arrange(rank)
    cnd_all <- cand_store[[tab]]
    cv_winner <- sc$spec[1]

    ## Walk the ranking until we get a fit that is acceptable
    chosen <- NULL
    for (rk in seq_len(nrow(sc))) {
      cn  <- cnd_all[sc$cand_id[rk], ]
      lam <- if (is.na(cn$lambda)) NULL else cn$lambda

      f <- tryCatch(
        Arima(y, order = c(cn$p, cn$d, cn$q),
              seasonal = list(order = c(cn$P, cn$D, cn$Q), period = 4),
              include.drift = cn$drift, lambda = lam, biasadj = TRUE, method = "ML"),
        error = function(e) NULL, warning = function(w) NULL)
      if (is.null(f)) next

      fcx <- tryCatch(forecast(f, h = H_MAX, level = c(80, 95), biasadj = TRUE),
                      error = function(e) NULL)
      if (is.null(fcx) || any(!is.finite(as.numeric(fcx$mean)))) next

      move    <- abs(as.numeric(fcx$mean)[H_MAX] - last_v)
      is_flat <- move < max(FLAT_TOL * last_v, FLAT_MIN)

      ## Accept the top-ranked model unless it is flat on a trending series
      if (rk == 1 && !(is_flat && trend)) { chosen <- list(f = f, fc = fcx, rk = rk, spec = sc$spec[rk]); break }
      if (rk == 1 &&  (is_flat && trend)) { flat_note <- "CV winner was flat on a trending series"; next }
      if (rk  > 1 && !is_flat)            { chosen <- list(f = f, fc = fcx, rk = rk, spec = sc$spec[rk]); override <- TRUE; break }
    }

    ## If every candidate is flat, keep the CV winner and say so
    if (is.null(chosen)) {
      cn  <- cnd_all[sc$cand_id[1], ]
      lam <- if (is.na(cn$lambda)) NULL else cn$lambda
      f   <- Arima(y, order = c(cn$p, cn$d, cn$q),
                   seasonal = list(order = c(cn$P, cn$D, cn$Q), period = 4),
                   include.drift = cn$drift, lambda = lam, biasadj = TRUE, method = "ML")
      chosen <- list(f = f, fc = forecast(f, h = H_MAX, level = c(80, 95), biasadj = TRUE),
                     rk = 1, spec = sc$spec[1])
      flat_note <- "No non-flat candidate available; CV winner retained"
    }

    fit <- chosen$f; fc <- chosen$fc
    rank_used <- chosen$rk
    spec_used <- chosen$spec
    if (override) {
      rmse_cost <- round(sc$rmse_all[chosen$rk] - sc$rmse_all[1], 3)
      method    <- paste0("Flat-forecast override: published CV rank ", chosen$rk,
                          " (CV preferred a constant path)")
    } else {
      rmse_cost <- 0
      method    <- "ARIMA selected by expanding-window CV"
      if (nzchar(flat_note)) method <- paste0(method, "; ", flat_note)
    }

  } else if (status == "SPARSE - naive fallback") {
    fit <- naive(y, h = H_MAX, level = c(80, 95)); fc <- fit
    spec_used <- "Naive (last value carried forward)"
    method    <- "Series too sparse for ARIMA; naive fallback (constant by construction)"
  } else {
    fits[[tab]] <- NULL
    fc_tables[[tab]] <- tibble::tibble(
      q_label = character(), horizon_q = integer(), point = numeric(),
      lo80 = numeric(), hi80 = numeric(), lo95 = numeric(), hi95 = numeric())
    diag_rows[[tab]] <- tibble::tibble(
      tab_name = tab, status = status, spec = "None - cell is empty",
      method = "No credit unions ever observed in this cell", aicc = NA_real_,
      sigma2 = NA_real_, lb_pvalue = NA_real_, has_trend = FALSE,
      override = FALSE, cv_rank_used = NA_integer_, cv_winner_spec = NA_character_,
      override_rmse_cost = NA_real_, chg_5yr_pct = NA_real_,
      fc_h4 = 0, fc_h12 = 0, fc_h20 = 0)
    next
  }

  fits[[tab]] <- fit

  fq <- tibble::tibble(
    horizon_q = 1:H_MAX,
    year      = FC_START[1] + (FC_START[2] - 1 + 0:(H_MAX - 1)) %/% 4,
    quarter   = (FC_START[2] - 1 + 0:(H_MAX - 1)) %% 4 + 1) %>%
    mutate(q_label = paste0(year, "Q", quarter),
           point = round(pmax(as.numeric(fc$mean), 0)),
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
    lb_pvalue = round(lb, 4),
    has_trend = trend, override = override, cv_rank_used = rank_used,
    cv_winner_spec = cv_winner, override_rmse_cost = rmse_cost,
    chg_5yr_pct = round(100 * (fq$point[20] - last_v) / max(last_v, 1), 1),
    fc_h4 = fq$point[4], fc_h12 = fq$point[12], fc_h20 = fq$point[20])

  cat(sprintf("[%2d/%2d] %-16s %-46s -> %4d / %4d / %4d %s\n",
              i, nrow(cells), tab, spec_used,
              fq$point[4], fq$point[12], fq$point[20],
              ifelse(override, "  <-- OVERRIDE", "")))
}

diagnostics <- bind_rows(diag_rows)
print(as.data.frame(diagnostics), row.names = FALSE)

## ---------------------------------------------------------------------
## [3.2] Did anything still come out flat?
## ---------------------------------------------------------------------
flat_check <- diagnostics %>%
  filter(status != "EMPTY - no model") %>%
  mutate(still_flat = abs(chg_5yr_pct) < 100 * FLAT_TOL) %>%
  select(tab_name, status, spec, has_trend, override, chg_5yr_pct, still_flat)

print(as.data.frame(flat_check), row.names = FALSE)
cat("\nCells overridden:", sum(diagnostics$override, na.rm = TRUE),
    "\nStill flat after override:", sum(flat_check$still_flat, na.rm = TRUE),
    "(check these individually)\n")

## ---------------------------------------------------------------------
## [3.3] Summary table -- one row per cell
## ---------------------------------------------------------------------
summary_tbl <- cells %>%
  left_join(cell_diag %>% select(tab_name, n_nonzero, first_count, last_count),
            by = "tab_name") %>%
  left_join(diagnostics, by = "tab_name") %>%
  left_join(winners %>% select(tab_name, cv_rmse_all = rmse_all,
                               cv_rmse_h4 = rmse_h4, cv_rmse_h12 = rmse_h12,
                               cv_rmse_h20 = rmse_h20),
            by = "tab_name") %>%
  mutate(chg_1yr = fc_h4  - last_count,
         chg_3yr = fc_h12 - last_count,
         chg_5yr = fc_h20 - last_count) %>%
  select(cell_id, tab_name, region, cu_type, asset_cat, label,
         actual_2026Q1 = last_count, fc_1yr = fc_h4, fc_3yr = fc_h12,
         fc_5yr = fc_h20, chg_1yr, chg_3yr, chg_5yr, chg_5yr_pct,
         spec, status, has_trend, override, cv_rank_used, cv_winner_spec,
         override_rmse_cost, cv_rmse_all, cv_rmse_h4, cv_rmse_h12,
         cv_rmse_h20, aicc, lb_pvalue)

print(as.data.frame(summary_tbl), row.names = FALSE)

## ---------------------------------------------------------------------
## [3.4] Coherence check
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
         gap          = bottom_up - direct_total,
         gap_pct      = round(100 * gap / direct_total, 2),
         horizon_label = case_when(horizon_q == 4  ~ "1-year ahead",
                                   horizon_q == 12 ~ "3-year ahead",
                                   horizon_q == 20 ~ "5-year ahead",
                                   TRUE ~ ""))
print(as.data.frame(coherence), row.names = FALSE)

saveRDS(list(fits = fits, fc_tables = fc_tables, diagnostics = diagnostics,
             summary_tbl = summary_tbl, coherence = coherence,
             flat_check = flat_check, fit_total = fit_total,
             fc_total = fc_total, FC_START = FC_START,
             FLAT_TOL = FLAT_TOL, FLAT_MIN = FLAT_MIN),
        file = "cu_count_fits.rds")
