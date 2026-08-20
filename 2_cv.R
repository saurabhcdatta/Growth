## =====================================================================
## 2_cv.R  --  Expanding-window time series cross-validation
##
## Sample 2005Q1-2026Q1 (85 quarters). First origin 2005Q1-2012Q4.
## Candidate set now includes log-scale (lambda = 0) specifications, which
## produce geometrically declining rather than linear or flat forecasts.
## Also computes a per-cell trend test used by 3_fit_forecast.R to decide
## whether a flat CV winner should be overridden.
##
## Expect roughly 10-20 minutes for all cells with ORIGIN_STEP = 1.
## =====================================================================

library(dplyr)
library(tidyr)
library(forecast)

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)

## ---------------------------------------------------------------------
## [2.1] CV settings
## ---------------------------------------------------------------------
MIN_TRAIN   <- 32        # first origin uses 2005Q1-2012Q4 (8 years)
H_MAX       <- 20        # 20 quarters = 5 years
ORIGIN_STEP <- 1
H_REPORT    <- c(4, 12, 20)

origins <- seq(MIN_TRAIN, N_Q - 1, by = ORIGIN_STEP)
length(origins)          # ~53 origins

## ---------------------------------------------------------------------
## [2.2] Trend test per cell
## Slope of a linear fit over the last 10 years, plus the change over the
## last 20 quarters relative to the current level. 3_fit_forecast.R uses
## these to decide whether a constant forecast is defensible.
## ---------------------------------------------------------------------
TREND_WIN <- 40          # quarters used for the trend test

trend_tbl <- cells %>%
  mutate(slope = NA_real_, slope_p = NA_real_, rel_chg_20q = NA_real_,
         has_trend = FALSE)

for (i in seq_len(nrow(cells))) {
  yv <- as.numeric(ts_list[[cells$tab_name[i]]])
  w  <- tail(yv, TREND_WIN)
  if (length(unique(w)) < 3) next
  m  <- summary(lm(w ~ seq_along(w)))
  trend_tbl$slope[i]       <- coef(m)[2, 1]
  trend_tbl$slope_p[i]     <- coef(m)[2, 4]
  last_v                   <- tail(yv, 1)
  trend_tbl$rel_chg_20q[i] <- (last_v - yv[max(1, length(yv) - 20)]) / max(last_v, 1)
}

trend_tbl <- trend_tbl %>%
  mutate(has_trend = !is.na(slope_p) & slope_p < 0.10 & abs(rel_chg_20q) > 0.05)

print(as.data.frame(trend_tbl[, c("tab_name", "slope", "slope_p",
                                  "rel_chg_20q", "has_trend")]), row.names = FALSE)
table(trend_tbl$has_trend)

## ---------------------------------------------------------------------
## [2.3] Candidate specifications
##
## lambda = NA  -> fit on raw counts (linear drift)
## lambda = 0   -> fit on logs: drift becomes a constant PERCENTAGE change,
##                 so the forecast decays geometrically, stays positive, and
##                 cannot come out flat once drift is estimated away from
##                 zero. Only usable when the series has no zeros.
## Drift is only legal when d == 1 and D == 0.
## ---------------------------------------------------------------------
cand_base <- tibble::tribble(
  ~p, ~d, ~q, ~P, ~D, ~Q, ~drift, ~lambda, ~source,
   0,  1,  0,  0,  0,  0,  FALSE,  NA,      "grid",          # random walk (FLAT)
   0,  1,  0,  0,  0,  0,  TRUE,   NA,      "grid",          # RW + drift
   0,  1,  1,  0,  0,  0,  FALSE,  NA,      "grid",
   0,  1,  1,  0,  0,  0,  TRUE,   NA,      "grid",
   1,  1,  0,  0,  0,  0,  TRUE,   NA,      "grid",
   1,  1,  1,  0,  0,  0,  TRUE,   NA,      "grid",
   2,  1,  0,  0,  0,  0,  TRUE,   NA,      "grid",
   1,  1,  1,  0,  0,  0,  FALSE,  NA,      "grid",
   0,  1,  1,  0,  1,  1,  FALSE,  NA,      "grid",          # seasonal
   1,  1,  0,  1,  0,  0,  TRUE,   NA,      "grid",
   0,  1,  0,  0,  0,  0,  TRUE,   0,       "grid (log)",    # geometric decay
   0,  1,  1,  0,  0,  0,  TRUE,   0,       "grid (log)",
   1,  1,  0,  0,  0,  0,  TRUE,   0,       "grid (log)",
   1,  1,  1,  0,  0,  0,  TRUE,   0,       "grid (log)",
   2,  1,  1,  0,  0,  0,  TRUE,   0,       "grid (log)"
)

## ---------------------------------------------------------------------
## [2.4] Per-cell auto.arima orders, appended to the grid
## ---------------------------------------------------------------------
model_cells <- cell_diag %>% filter(status == "MODEL")
nrow(model_cells)

auto_orders <- vector("list", nrow(model_cells))

for (i in seq_len(nrow(model_cells))) {
  y  <- ts_list[[model_cells$tab_name[i]]]
  a1 <- auto.arima(y, seasonal = TRUE,  stepwise = FALSE, approximation = FALSE)
  a2 <- auto.arima(y, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  o1 <- arimaorder(a1); o2 <- arimaorder(a2)
  g1 <- if (length(o1) == 3) c(o1, 0, 0, 0) else o1
  g2 <- c(o2, 0, 0, 0)
  auto_orders[[i]] <- tibble::tibble(
    p = c(g1[1], g2[1]), d = c(g1[2], g2[2]), q = c(g1[3], g2[3]),
    P = c(g1[4], g2[4]), D = c(g1[5], g2[5]), Q = c(g1[6], g2[6]),
    drift  = c("drift" %in% names(coef(a1)), "drift" %in% names(coef(a2))),
    lambda = c(NA_real_, NA_real_),
    source = c("auto.arima (seasonal allowed)", "auto.arima (non-seasonal)"))
  cat(sprintf("[%2d/%2d] %-16s auto: %s\n", i, nrow(model_cells),
              model_cells$tab_name[i], paste(o1, collapse = ",")))
}
names(auto_orders) <- model_cells$tab_name

## ---------------------------------------------------------------------
## [2.5] The CV loop
## ---------------------------------------------------------------------
cv_summary <- list()
err_store  <- list()
cand_store <- list()

t0 <- Sys.time()

for (i in seq_len(nrow(model_cells))) {

  tab <- model_cells$tab_name[i]
  y   <- ts_list[[tab]]
  yv  <- as.numeric(y)
  can_log <- min(yv) > 0

  cand <- bind_rows(cand_base, auto_orders[[tab]]) %>%
    mutate(drift = drift & d == 1 & D == 0) %>%
    filter(is.na(lambda) | can_log) %>%
    distinct(p, d, q, P, D, Q, drift, lambda, .keep_all = TRUE) %>%
    mutate(spec = sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[4]%s%s",
                          p, d, q, P, D, Q,
                          ifelse(drift, " w/ drift", ""),
                          ifelse(is.na(lambda), "", " on log scale")))
  cand_store[[tab]] <- cand

  err <- array(NA_real_, dim = c(length(origins), H_MAX, nrow(cand)))

  for (oi in seq_along(origins)) {
    n_tr  <- origins[oi]
    train <- subset(y, end = n_tr)
    h_o   <- min(H_MAX, N_Q - n_tr)

    for (k in seq_len(nrow(cand))) {
      lam <- if (is.na(cand$lambda[k])) NULL else cand$lambda[k]
      if (!is.null(lam) && min(as.numeric(train)) <= 0) next

      fit <- tryCatch(
        Arima(train,
              order         = c(cand$p[k], cand$d[k], cand$q[k]),
              seasonal      = list(order = c(cand$P[k], cand$D[k], cand$Q[k]), period = 4),
              include.drift = cand$drift[k],
              lambda = lam, biasadj = TRUE, method = "ML"),
        error = function(e) NULL, warning = function(w) NULL)
      if (is.null(fit)) next

      fc <- tryCatch(as.numeric(forecast(fit, h = h_o, biasadj = TRUE)$mean),
                     error = function(e) NULL)
      if (is.null(fc) || any(!is.finite(fc))) next

      fc <- pmax(fc, 0)
      err[oi, seq_len(h_o), k] <- yv[(n_tr + 1):(n_tr + h_o)] - fc
    }
  }

  err_store[[tab]] <- err

  scores <- tibble::tibble(cand_id = seq_len(nrow(cand)), spec = cand$spec,
                           source = cand$source)
  scores$n_fits   <- apply(err, 3, function(m) sum(!is.na(m[, 1])))
  scores$rmse_all <- apply(err, 3, function(m) sqrt(mean(m^2, na.rm = TRUE)))
  scores$mae_all  <- apply(err, 3, function(m) mean(abs(m), na.rm = TRUE))
  for (h in H_REPORT) {
    scores[[paste0("rmse_h", h)]] <- apply(err, 3, function(m) sqrt(mean(m[, h]^2, na.rm = TRUE)))
    scores[[paste0("mae_h",  h)]] <- apply(err, 3, function(m) mean(abs(m[, h]), na.rm = TRUE))
  }

  scores <- scores %>%
    mutate(eligible = n_fits >= 0.8 * length(origins) & is.finite(rmse_all)) %>%
    arrange(desc(eligible), rmse_all) %>%
    mutate(rank = row_number(), winner = rank == 1 & eligible)

  cv_summary[[tab]] <- scores

  cat(sprintf("[%2d/%2d] %-16s winner: %-46s CV RMSE %.2f  (%s)\n",
              i, nrow(model_cells), tab, scores$spec[1], scores$rmse_all[1],
              format(round(difftime(Sys.time(), t0, units = "mins"), 1))))
}

difftime(Sys.time(), t0, units = "mins")

## ---------------------------------------------------------------------
## [2.6] What CV picked, before any flat-forecast override
## ---------------------------------------------------------------------
winners <- bind_rows(lapply(names(cv_summary), function(tab)
  cv_summary[[tab]] %>% filter(winner) %>% mutate(tab_name = tab)))

print(as.data.frame(winners[, c("tab_name", "spec", "rmse_all",
                                "rmse_h4", "rmse_h12", "rmse_h20")]),
      row.names = FALSE)
table(winners$spec)

saveRDS(list(cv_summary = cv_summary, cand_store = cand_store,
             winners = winners, origins = origins, trend_tbl = trend_tbl,
             MIN_TRAIN = MIN_TRAIN, H_MAX = H_MAX, H_REPORT = H_REPORT),
        file = "cu_count_cv.rds")
