## =====================================================================
## 3_fit_forecast.R  --  Final fits, forecasts, diagnostics, coherence
## =====================================================================

library(dplyr)
library(tidyr)
library(forecast)

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)
## cvr  <- readRDS("cu_count_cv.rds");   list2env(cvr,  .GlobalEnv)

FC_START <- c(2026, 2)     # first forecast quarter

fits      <- list()
fc_tables <- list()
diag_rows <- list()

for (i in seq_len(nrow(cells))) {

  tab    <- cells$tab_name[i]
  y      <- ts_list[[tab]]
  status <- cell_diag$status[cell_diag$tab_name == tab]

  if (status == "MODEL") {
    w   <- cv_summary[[tab]] %>% filter(winner)
    cnd <- cand_store[[tab]][w$cand_id, ]
    fit <- Arima(y,
                 order         = c(cnd$p, cnd$d, cnd$q),
                 seasonal      = list(order = c(cnd$P, cnd$D, cnd$Q), period = 4),
                 include.drift = cnd$drift, method = "ML")
    fc        <- forecast(fit, h = H_MAX, level = c(80, 95))
    spec_used <- w$spec
    method    <- "ARIMA selected by expanding-window CV"
  } else if (status == "SPARSE - naive fallback") {
    fit       <- naive(y, h = H_MAX, level = c(80, 95))
    fc        <- fit
    spec_used <- "Naive (last value carried forward)"
    method    <- "Series too sparse for ARIMA; naive fallback"
  } else {
    fits[[tab]] <- NULL
    fc_tables[[tab]] <- tibble::tibble(
      q_label = character(), horizon_q = integer(), point = numeric(),
      lo80 = numeric(), hi80 = numeric(), lo95 = numeric(), hi95 = numeric())
    diag_rows[[tab]] <- tibble::tibble(
      tab_name = tab, status = status, spec = "None - cell is empty",
      method = "No credit unions ever observed in this cell", aicc = NA_real_,
      sigma2 = NA_real_, lb_pvalue = NA_real_,
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
    tab_name  = tab,
    status    = status,
    spec      = spec_used,
    method    = method,
    aicc      = if (status == "MODEL") round(fit$aicc, 2) else NA_real_,
    sigma2    = if (status == "MODEL") round(fit$sigma2, 3) else NA_real_,
    lb_pvalue = round(lb, 4),
    fc_h4     = fq$point[4], fc_h12 = fq$point[12], fc_h20 = fq$point[20])

  cat(sprintf("[%2d/56] %-16s %-40s -> %4d / %4d / %4d\n",
              i, tab, spec_used, fq$point[4], fq$point[12], fq$point[20]))
}

diagnostics <- bind_rows(diag_rows)
print(as.data.frame(diagnostics), row.names = FALSE)

## ---------------------------------------------------------------------
## [3.1] Summary table -- one row per cell
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
         fc_5yr = fc_h20, chg_1yr, chg_3yr, chg_5yr,
         spec, status, cv_rmse_all, cv_rmse_h4, cv_rmse_h12, cv_rmse_h20,
         aicc, lb_pvalue)

print(as.data.frame(summary_tbl), row.names = FALSE)

## ---------------------------------------------------------------------
## [3.2] Coherence check
## 56 independent models are not constrained to sum to the total CU count.
## Fit the total directly and compare.
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
             fit_total = fit_total, fc_total = fc_total, FC_START = FC_START),
        file = "cu_count_fits.rds")
