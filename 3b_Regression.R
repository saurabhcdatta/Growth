############################################################
# PART 3b — FICU & FISCU TOTAL ASSETS FORECASTING
#
# Targets  : yoy_ficu_assets_pct  (YoY % change in FICU total assets)
#             yoy_fiscu_assets_pct (YoY % change in FISCU total assets)
#             7 asset-size categories each → 14 models total
#
# Method   : 1. auto.arima() on training series → fixes ARIMA order
#            2. TSCV with expanding window to select best xreg subset:
#               - LASSO pre-screens candidates → top MAX_XREG_VARS
#               - Backward elimination using TSCV RMSE
#            3. Final model fit on full training window
#            4. Multi-step ARIMAX forecast to 2030Q4
#               Exit vars set to 0; macro from FRB forward panel
#
# Outputs  : results_3b/  (forecasts, metrics, coefficients CSVs)
#            plots_3b/     (PDF plots)
#
# Reads    : qtrly_enriched_v3.rds  (from macro_v4_frb.R)
############################################################

# ════════════════════════════════════════════════════════════
# 0. PACKAGES
# ════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(glmnet)
  library(forecast)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(tictoc)
  library(stringr)
})

set.seed(42)
options(scipen = 999)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 &&
                             !is.na(a[1])) a else b

# ════════════════════════════════════════════════════════════
# 1. CONFIG
# ════════════════════════════════════════════════════════════
DATA_DIR   <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR   <- "plots_3b"
RESULT_DIR <- "results_3b"

DEBUG_MODE   <- TRUE
DEBUG_ROLL_Q <- 6

# Store as numeric to avoid yearqtr/closure collision in data.table
TRAIN_END <- as.numeric(zoo::as.yearqtr("2021 Q1"))  # 2021.00
FC_END    <- as.numeric(zoo::as.yearqtr("2030 Q4"))  # 2030.75

TSCV_MIN_TRAIN        <- 12L
TSCV_MIN_TRAIN_SPARSE <- 8L
TSCV_H                <- 1L

SPARSE_CATS <- c("7_10B_Plus", "1_Less_10M")

MAX_XREG_VARS <- 10L
SIG_LEVEL     <- 0.10

YOY_CAP <- 25.0   # assets swing wider than counts

setwd(DATA_DIR)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)

# ════════════════════════════════════════════════════════════
# 2. LOAD DATA
# ════════════════════════════════════════════════════════════
message("\n[1] Loading qtrly_enriched_v3.rds...")
if (!file.exists("qtrly_enriched_v3.rds"))
  stop("qtrly_enriched_v3.rds not found. Run Part 1 v4 + macro_v4_frb.R first.")

qtrly <- readRDS("qtrly_enriched_v3.rds")
setDT(qtrly)

# Ensure yearqtr column is numeric for safe data.table comparisons
qtrly[, yearqtr := as.numeric(yearqtr)]

message(sprintf("    %s rows x %s cols", nrow(qtrly), ncol(qtrly)))

CAT_LABELS <- c(
  "1_Less_10M"  = "< $10M",
  "2_10M_50M"   = "$10M-$50M",
  "3_50M_100M"  = "$50M-$100M",
  "4_100M_500M" = "$100M-$500M",
  "5_500M_1B"   = "$500M-$1B",
  "6_1B_10B"    = "$1B-$10B",
  "7_10B_Plus"  = "> $10B"
)

# ════════════════════════════════════════════════════════════
# 3. TARGETS & CANDIDATE REGRESSORS
# ════════════════════════════════════════════════════════════

# Expected columns: yoy_ficu_assets_pct_{cat}, yoy_fiscu_assets_pct_{cat}
TARGET_ROOTS <- c(
  FICU  = "yoy_ficu_assets_pct",
  FISCU = "yoy_fiscu_assets_pct"
)

MACRO_VARS <- c(
  "fedfunds", "t10y", "t10y_t3m_spread", "t10y_t2y_spread",
  "mortgage30", "prime_rate", "libor_3m",
  "gdp_growth", "rgdp_growth", "unemployment", "cpi_yoy",
  "pce_yoy", "core_pce_yoy", "cci",
  "bbb_spread", "hy_spread", "ted_spread", "vix",
  "hpi_yoy", "sp500_yoy",
  "fedfunds_lag1", "fedfunds_lag2",
  "t10y_lag1", "t10y_lag2",
  "mortgage30_lag1", "mortgage30_lag2",
  "gdp_growth_lag1", "gdp_growth_lag2",
  "unemployment_lag1", "unemployment_lag2",
  "bbb_spread_lag1", "hy_spread_lag1",
  "hpi_yoy_lag1", "sp500_yoy_lag1",
  "cpi_yoy_lag1", "core_pce_yoy_lag1"
)

EXIT_VARS     <- c("merger_rate", "liquid_rate", "acquisition_rate")
EXIT_VARS_LAG <- paste0(EXIT_VARS, "_lag1")
SEAS_VARS     <- c("q1", "q2", "q3")
ALL_CANDIDATES <- c(MACRO_VARS, EXIT_VARS, EXIT_VARS_LAG, SEAS_VARS)

# ════════════════════════════════════════════════════════════
# 4. HELPER FUNCTIONS
# ════════════════════════════════════════════════════════════

build_xreg <- function(dt, vars) {
  avail <- intersect(vars, names(dt))
  if (length(avail) == 0L) return(NULL)
  mat <- as.matrix(dt[, ..avail])
  if (any(!is.finite(mat))) {
    bad <- which(colSums(!is.finite(mat)) > 0)
    mat <- mat[, -bad, drop = FALSE]
  }
  if (ncol(mat) == 0L) return(NULL)
  mat
}

lasso_screen <- function(y, xmat, top_n = MAX_XREG_VARS) {
  if (is.null(xmat) || ncol(xmat) == 0L) return(character(0))
  ok <- is.finite(y) & apply(xmat, 1, function(r) all(is.finite(r)))
  y2 <- y[ok]; x2 <- xmat[ok, , drop = FALSE]
  if (nrow(x2) < 10L) return(character(0))
  cv <- tryCatch(
    cv.glmnet(x2, y2, alpha = 1, nfolds = 5, standardize = TRUE),
    error = function(e) NULL
  )
  if (is.null(cv))
    return(colnames(xmat)[seq_len(min(top_n, ncol(xmat)))])
  coefs <- coef(cv, s = "lambda.1se")[-1, , drop = FALSE]
  nz    <- rownames(coefs)[coefs[, 1] != 0]
  if (length(nz) == 0L)
    nz <- colnames(xmat)[order(abs(coefs[, 1]),
                               decreasing = TRUE)[seq_len(min(top_n, ncol(xmat)))]]
  nz[seq_len(min(top_n, length(nz)))]
}

tscv_rmse <- function(y_ts, xreg_full, arima_order, arima_seasonal,
                      min_train = TSCV_MIN_TRAIN, h = TSCV_H) {
  n   <- length(y_ts)
  err <- numeric(0)
  for (i in seq(min_train, n - h)) {
    y_tr  <- window(y_ts, end   = time(y_ts)[i])
    y_te  <- window(y_ts, start = time(y_ts)[i + 1],
                    end   = time(y_ts)[min(i + h, n)])
    xr_tr <- if (!is.null(xreg_full))
      xreg_full[seq_len(i), , drop = FALSE] else NULL
    xr_te <- if (!is.null(xreg_full))
      xreg_full[seq(i + 1, min(i + h, n)), , drop = FALSE] else NULL
    fit <- tryCatch(
      Arima(y_tr, order = arima_order, seasonal = arima_seasonal,
            xreg = xr_tr, method = "ML"),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    fc <- tryCatch(
      forecast(fit, h = length(y_te), xreg = xr_te),
      error = function(e) NULL
    )
    if (is.null(fc)) next
    err <- c(err, as.numeric(y_te) - as.numeric(fc$mean))
  }
  if (length(err) == 0L) return(Inf)
  sqrt(mean(err^2, na.rm = TRUE))
}

backward_tscv <- function(y_ts, candidate_vars, xreg_full,
                          arima_order, arima_seasonal,
                          min_train = TSCV_MIN_TRAIN) {
  current_vars <- candidate_vars
  if (length(current_vars) == 0L || is.null(xreg_full)) {
    return(list(vars = character(0),
                rmse = tscv_rmse(y_ts, NULL, arima_order,
                                 arima_seasonal, min_train)))
  }
  best_rmse <- tscv_rmse(
    y_ts, xreg_full[, current_vars, drop = FALSE],
    arima_order, arima_seasonal, min_train)
  message(sprintf("      [BKWD] Start: vars=%d  RMSE=%.5f",
                  length(current_vars), best_rmse))
  repeat {
    if (length(current_vars) == 0L) break
    improved <- FALSE
    for (drop_v in current_vars) {
      test_vars <- setdiff(current_vars, drop_v)
      xr_test   <- if (length(test_vars) > 0)
        xreg_full[, test_vars, drop = FALSE] else NULL
      rmse_test <- tscv_rmse(y_ts, xr_test, arima_order,
                             arima_seasonal, min_train)
      if (rmse_test < best_rmse - 1e-6) {
        best_rmse    <- rmse_test
        current_vars <- test_vars
        improved     <- TRUE
        message(sprintf("      [BKWD] Drop '%s' -> RMSE=%.5f",
                        drop_v, best_rmse))
        break
      }
    }
    if (!improved) break
  }
  list(vars = current_vars, rmse = best_rmse)
}

build_coef_dt <- function(fit, sig_level = SIG_LEVEL) {
  cf <- tryCatch(coef(fit), error = function(e) NULL)
  if (is.null(cf) || length(cf) == 0L) {
    return(data.table(
      term = "sigma2", estimate = fit$sigma2 %||% NA_real_,
      std_error = NA_real_, statistic = NA_real_,
      p_value = NA_real_, significance = "", tag = ""
    ))
  }
  vmat  <- tryCatch(diag(fit$var.coef), error = function(e) NULL)
  se    <- if (!is.null(vmat)) sqrt(pmax(vmat, 0)) else rep(NA_real_, length(cf))
  tstat <- if (!is.null(vmat)) cf / se            else rep(NA_real_, length(cf))
  pval  <- if (!is.null(vmat))
    2 * pt(-abs(tstat), df = fit$nobs - length(cf)) else rep(NA_real_, length(cf))
  
  sig_stars <- function(p)
    ifelse(is.na(p), "",
           ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.10,  ".", "")))))
  
  tag_term <- function(nm) {
    if (grepl("^(ar|ma|sar|sma)\\d*$", nm, ignore.case = TRUE)) return("[ARIMA]")
    if (nm %in% c(EXIT_VARS, EXIT_VARS_LAG)) return("[EXIT]")
    if (nm %in% SEAS_VARS)  return("[SEAS]")
    if (nm %in% MACRO_VARS) return("[MACRO]")
    ""
  }
  
  dt <- data.table(
    term         = names(cf),
    estimate     = as.numeric(cf),
    std_error    = se,
    statistic    = tstat,
    p_value      = pval,
    significance = sig_stars(pval),
    tag          = sapply(names(cf), tag_term)
  )
  rbind(dt, data.table(
    term = "sigma2", estimate = fit$sigma2 %||% NA_real_,
    std_error = NA_real_, statistic = NA_real_,
    p_value = NA_real_, significance = "", tag = ""
  ))
}

print_model_summary <- function(label, fit, chosen_vars,
                                tscv_rmse_val, tscv_rmse_base,
                                n_train, oos_rmse = NA,
                                oos_rmse_base = NA) {
  message(sprintf(
    "\n  +-- %s\n  |  ARIMA(%d,%d,%d)(%d,%d,%d)[4]   nobs=%d",
    label,
    fit$arma[1], fit$arma[6], fit$arma[2],
    fit$arma[3], fit$arma[7], fit$arma[4],
    n_train))
  message(sprintf(
    "  |  TSCV-RMSE: %.5f  (base %.5f, Delta=%.4f%%)",
    tscv_rmse_val, tscv_rmse_base,
    100 * (tscv_rmse_val - tscv_rmse_base) / (tscv_rmse_base + 1e-9)))
  if (!is.na(oos_rmse))
    message(sprintf("  |  OOS-RMSE : %.5f  (base %.5f)",
                    oos_rmse, oos_rmse_base))
  message(sprintf("  |  AIC=%.1f  BIC=%.1f  sigma2=%.4f",
                  fit$aic, fit$bic, fit$sigma2 %||% NA))
  
  cd       <- build_coef_dt(fit)
  arma_t   <- cd[tag == "[ARIMA]"]
  xreg_t   <- cd[tag != "[ARIMA]" & term != "sigma2"]
  
  if (nrow(arma_t) > 0) {
    message("  |  -- ARIMA terms --------------------------------")
    for (i in seq_len(nrow(arma_t))) {
      r <- arma_t[i]
      if (!is.na(r$p_value))
        message(sprintf(
          "  |    %-14s %+.4f  SE=%.4f  t=%.2f  p=%.3f %s",
          r$term, r$estimate, r$std_error %||% NA,
          r$statistic %||% NA, r$p_value, r$significance))
    }
  }
  if (nrow(xreg_t) > 0) {
    message("  |  -- Regressors ---------------------------------")
    for (i in seq_len(nrow(xreg_t))) {
      r <- xreg_t[i]
      if (!is.na(r$p_value))
        message(sprintf(
          "  |    %-22s %+.4f  SE=%.4f  t=%.2f  p=%.3f %s  %s",
          r$term, r$estimate, r$std_error %||% NA,
          r$statistic %||% NA, r$p_value, r$significance, r$tag))
      else
        message(sprintf("  |    %-22s %+.4f  %s",
                        r$term, r$estimate, r$tag))
    }
  }
  message(sprintf("  |  sigma2 = %.4f", fit$sigma2 %||% NA))
  message("  +------------------------------------------------")
}

# ════════════════════════════════════════════════════════════
# 5. MODEL LOOP — 14 MODELS (7 cats x 2 CU types)
# ════════════════════════════════════════════════════════════
message("\n[2] Building 14 ARIMAX asset models...")
tic("Total Part 3b")

cats   <- names(CAT_LABELS)
types  <- c("FICU", "FISCU")
models <- CJ(cu_type = types, cat = cats, sorted = FALSE)
setorder(models, cu_type, cat)

all_forecasts <- list()
all_metrics   <- list()
all_coefs     <- list()

qtrly_train <- qtrly[yearqtr <= TRAIN_END]
qtrly_oos   <- qtrly[yearqtr >  TRAIN_END]

available_macro <- intersect(MACRO_VARS,                  names(qtrly))
available_exit  <- intersect(c(EXIT_VARS, EXIT_VARS_LAG), names(qtrly))
available_seas  <- intersect(SEAS_VARS,                   names(qtrly))
available_cands <- c(available_macro, available_exit, available_seas)

if (DEBUG_MODE)
  message(sprintf("  [DEBUG] Rolling windows limited to %d quarters",
                  DEBUG_ROLL_Q))

for (m in seq_len(nrow(models))) {
  
  cu_type   <- models$cu_type[m]
  cat       <- models$cat[m]
  cat_lbl   <- CAT_LABELS[cat]
  is_sparse <- cat %in% SPARSE_CATS
  min_train_use <- if (is_sparse) TSCV_MIN_TRAIN_SPARSE else TSCV_MIN_TRAIN
  
  target_col <- paste0(TARGET_ROOTS[cu_type], "_", cat)
  label      <- sprintf("Model %02d/14 | %s | %s", m, cu_type, cat)
  
  message(sprintf("\n======================================================"))
  message(sprintf("  %s  [%s]", label, cat_lbl))
  message(sprintf("======================================================"))
  
  if (!target_col %in% names(qtrly)) {
    message(sprintf("  [SKIP] Column '%s' not found.", target_col))
    next
  }
  
  train_dt <- qtrly_train[is.finite(get(target_col))]
  if (nrow(train_dt) < min_train_use) {
    message(sprintf("  [SKIP] Only %d valid training obs (need %d).",
                    nrow(train_dt), min_train_use))
    next
  }
  
  y_vec  <- train_dt[[target_col]]
  yq_vec <- train_dt$yearqtr   # already numeric
  y_ts   <- ts(y_vec, frequency = 4,
               start = c(as.integer(floor(min(yq_vec))),
                         as.integer(round((min(yq_vec) %% 1) * 4 + 1))))
  
  # ── Step 1: auto.arima ──────────────────────────────────
  message("  [Step 1] auto.arima to determine order...")
  fit_auto <- tryCatch(
    auto.arima(y_ts, seasonal = TRUE, stepwise = FALSE,
               approximation = FALSE, ic = "aicc"),
    error = function(e) NULL
  )
  if (is.null(fit_auto)) {
    message("  [WARN] auto.arima failed -- using ARIMA(1,1,0).")
    arima_order    <- c(1L, 1L, 0L)
    arima_seasonal <- list(order = c(0L, 0L, 0L), period = 4L)
  } else {
    arima_order    <- fit_auto$arma[c(1, 6, 2)]
    arima_seasonal <- list(order = fit_auto$arma[c(3, 7, 4)], period = 4L)
    message(sprintf("  -> ARIMA(%d,%d,%d)(%d,%d,%d)[4]",
                    arima_order[1], arima_order[2], arima_order[3],
                    arima_seasonal$order[1], arima_seasonal$order[2],
                    arima_seasonal$order[3]))
  }
  
  # ── Step 2: LASSO pre-screen ────────────────────────────
  xreg_all <- build_xreg(train_dt, available_cands)
  
  if (is_sparse || is.null(xreg_all) || ncol(xreg_all) == 0) {
    message("  [Step 2] Sparse / no regressors -- skipping LASSO.")
    lasso_vars <- character(0)
  } else {
    message(sprintf("  [Step 2] LASSO pre-screen (%d candidates)...",
                    ncol(xreg_all)))
    lasso_vars <- lasso_screen(y_vec, xreg_all, top_n = MAX_XREG_VARS)
    message(sprintf("  -> %d vars selected: %s",
                    length(lasso_vars),
                    paste(lasso_vars, collapse = ", ")))
  }
  
  # ── Step 3: Backward elimination via TSCV ───────────────
  if (length(lasso_vars) > 0) {
    message("  [Step 3] Backward elimination via TSCV...")
    bkwd <- backward_tscv(y_ts, lasso_vars, xreg_all,
                          arima_order, arima_seasonal, min_train_use)
    chosen_vars      <- bkwd$vars
    tscv_rmse_chosen <- bkwd$rmse
  } else {
    chosen_vars      <- character(0)
    tscv_rmse_chosen <- tscv_rmse(y_ts, NULL, arima_order,
                                  arima_seasonal, min_train_use)
  }
  
  tscv_rmse_base <- tscv_rmse(y_ts, NULL, arima_order,
                              arima_seasonal, min_train_use)
  
  message(sprintf("  -> Chosen vars (%d): %s",
                  length(chosen_vars),
                  if (length(chosen_vars) == 0) "none (pure ARIMA)"
                  else paste(chosen_vars, collapse = ", ")))
  
  # ── Step 4: Exit gate ───────────────────────────────────
  if (!is_sparse && !is.null(xreg_all)) {
    exit_candidates <- intersect(c(EXIT_VARS, EXIT_VARS_LAG),
                                 colnames(xreg_all))
    for (ev in setdiff(exit_candidates, chosen_vars)) {
      test_vars <- c(chosen_vars, ev)
      xr_test   <- xreg_all[, test_vars, drop = FALSE]
      rmse_test <- tscv_rmse(y_ts, xr_test, arima_order,
                             arima_seasonal, min_train_use)
      if (rmse_test < tscv_rmse_chosen - 1e-6) {
        tscv_rmse_chosen <- rmse_test
        chosen_vars      <- test_vars
        message(sprintf("  [EXIT GATE] Added '%s' -> RMSE=%.5f",
                        ev, rmse_test))
      }
    }
  }
  
  # ── Step 5: Fit final model ─────────────────────────────
  xreg_train_final <- if (length(chosen_vars) > 0)
    xreg_all[, chosen_vars, drop = FALSE] else NULL
  
  fit_final <- tryCatch(
    Arima(y_ts, order = arima_order, seasonal = arima_seasonal,
          xreg = xreg_train_final, method = "ML"),
    error = function(e) {
      message(sprintf("  [WARN] Final Arima() failed: %s", e$message))
      NULL
    }
  )
  if (is.null(fit_final)) {
    message("  [FALLBACK] Fitting pure ARIMA without xreg...")
    chosen_vars      <- character(0)
    xreg_train_final <- NULL
    fit_final <- tryCatch(
      Arima(y_ts, order = arima_order, seasonal = arima_seasonal,
            method = "ML"),
      error = function(e) NULL
    )
  }
  if (is.null(fit_final)) {
    message("  [SKIP] Could not fit any ARIMA model.")
    next
  }
  
  # ── OOS evaluation ──────────────────────────────────────
  oos_rmse_val  <- NA_real_
  oos_rmse_base <- NA_real_
  if (nrow(qtrly_oos) > 0 && target_col %in% names(qtrly_oos)) {
    oos_dt <- qtrly_oos[is.finite(get(target_col))]
    if (nrow(oos_dt) >= 1) {
      xreg_oos <- if (length(chosen_vars) > 0)
        build_xreg(oos_dt, chosen_vars) else NULL
      fc_oos <- tryCatch(
        forecast(fit_final, h = nrow(oos_dt), xreg = xreg_oos),
        error = function(e) NULL
      )
      if (!is.null(fc_oos)) {
        actual_oos   <- oos_dt[[target_col]]
        oos_rmse_val <- sqrt(mean(
          (actual_oos - as.numeric(fc_oos$mean))^2, na.rm = TRUE))
      }
      fit_base_oos <- tryCatch(
        Arima(y_ts, order = arima_order, seasonal = arima_seasonal,
              method = "ML"),
        error = function(e) NULL
      )
      if (!is.null(fit_base_oos)) {
        fc_base_oos <- tryCatch(
          forecast(fit_base_oos, h = nrow(oos_dt)),
          error = function(e) NULL
        )
        if (!is.null(fc_base_oos)) {
          actual_oos    <- oos_dt[[target_col]]
          oos_rmse_base <- sqrt(mean(
            (actual_oos - as.numeric(fc_base_oos$mean))^2,
            na.rm = TRUE))
        }
      }
    }
  }
  
  print_model_summary(label, fit_final, chosen_vars,
                      tscv_rmse_chosen, tscv_rmse_base,
                      length(y_vec), oos_rmse_val, oos_rmse_base)
  
  # ── Step 6: Forecast to 2030Q4 ──────────────────────────
  message("  [Step 6] Forecasting to 2030Q4...")
  last_train_q <- max(yq_vec)
  fc_qtrs      <- seq(last_train_q + 0.25, FC_END, by = 0.25)
  h_fc         <- length(fc_qtrs)
  
  if (length(chosen_vars) > 0) {
    fwd_dt <- qtrly[yearqtr %in% fc_qtrs]
    if (nrow(fwd_dt) < h_fc) {
      miss_q <- setdiff(fc_qtrs, fwd_dt$yearqtr)
      fwd_dt <- rbindlist(list(fwd_dt,
                               data.table(yearqtr = miss_q)),
                          fill = TRUE)
      setorder(fwd_dt, yearqtr)
    }
    for (ev in intersect(c(EXIT_VARS, EXIT_VARS_LAG), chosen_vars))
      set(fwd_dt, j = ev, value = 0)
    xreg_fc <- build_xreg(fwd_dt, chosen_vars)
    if (!is.null(xreg_fc) && nrow(xreg_fc) < h_fc) {
      pad_rows  <- h_fc - nrow(xreg_fc)
      col_means <- colMeans(xreg_train_final, na.rm = TRUE)
      xreg_fc   <- rbind(xreg_fc,
                         matrix(rep(col_means, pad_rows), nrow = pad_rows,
                                byrow = TRUE,
                                dimnames = list(NULL, names(col_means))))
    }
    if (!is.null(xreg_fc))
      xreg_fc <- xreg_fc[, chosen_vars, drop = FALSE]
  } else {
    xreg_fc <- NULL
  }
  
  fc_out <- tryCatch(
    forecast(fit_final, h = h_fc, xreg = xreg_fc),
    error = function(e) {
      message(sprintf("  [WARN] forecast() failed: %s", e$message))
      NULL
    }
  )
  if (is.null(fc_out)) next
  
  fc_mean <- pmin(pmax(as.numeric(fc_out$mean), -YOY_CAP), YOY_CAP)
  
  all_forecasts[[m]] <- data.table(
    cu_type   = cu_type,  cat = cat,  cat_label = cat_lbl,
    yearqtr   = fc_qtrs,
    fc_mean   = fc_mean,
    fc_lo80   = as.numeric(fc_out$lower[, 1]),
    fc_hi80   = as.numeric(fc_out$upper[, 1]),
    fc_lo95   = as.numeric(fc_out$lower[, 2]),
    fc_hi95   = as.numeric(fc_out$upper[, 2])
  )
  
  all_metrics[[m]] <- data.table(
    model_id       = m,
    cu_type        = cu_type,
    cat            = cat,
    cat_label      = cat_lbl,
    target         = target_col,
    arima_order    = sprintf("(%d,%d,%d)(%d,%d,%d)[4]",
                             arima_order[1], arima_order[2], arima_order[3],
                             arima_seasonal$order[1], arima_seasonal$order[2],
                             arima_seasonal$order[3]),
    n_train        = length(y_vec),
    n_xreg         = length(chosen_vars),
    xreg_chosen    = paste(chosen_vars, collapse = "|"),
    aic            = fit_final$aic,
    bic            = fit_final$bic,
    sigma2         = fit_final$sigma2 %||% NA_real_,
    tscv_rmse      = tscv_rmse_chosen,
    tscv_rmse_base = tscv_rmse_base,
    tscv_delta_pct = 100 * (tscv_rmse_chosen - tscv_rmse_base) /
      (tscv_rmse_base + 1e-9),
    oos_rmse       = oos_rmse_val,
    oos_rmse_base  = oos_rmse_base
  )
  
  cd_dt <- build_coef_dt(fit_final)
  cd_dt[, `:=`(model_id  = m,        cu_type   = cu_type,
               cat       = cat,      cat_label = cat_lbl)]
  all_coefs[[m]] <- cd_dt
  
  message(sprintf("  Done: %d forecast quarters saved.", h_fc))
}

toc()

# ════════════════════════════════════════════════════════════
# 6. SAVE RESULTS
# ════════════════════════════════════════════════════════════
message("\n[3] Saving results to results_3b/ ...")

fc_all   <- rbindlist(all_forecasts, fill = TRUE)
met_all  <- rbindlist(all_metrics,   fill = TRUE)
coef_all <- rbindlist(all_coefs,     fill = TRUE)

fwrite(fc_all,   file.path(RESULT_DIR, "forecasts_3b.csv"))
fwrite(met_all,  file.path(RESULT_DIR, "metrics_3b.csv"))
fwrite(coef_all, file.path(RESULT_DIR, "coefficients_3b.csv"))

message(sprintf("  Forecasts   : %d rows -> forecasts_3b.csv",    nrow(fc_all)))
message(sprintf("  Metrics     : %d rows -> metrics_3b.csv",      nrow(met_all)))
message(sprintf("  Coefficients: %d rows -> coefficients_3b.csv", nrow(coef_all)))

# ════════════════════════════════════════════════════════════
# 7. PLOTS
# ════════════════════════════════════════════════════════════
message("\n[4] Generating plots_3b/ ...")

theme_cu <- function() {
  theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#2c3e50", color = NA),
      strip.text       = element_text(color = "white", face = "bold", size = 8),
      plot.title       = element_text(face = "bold", size = 10),
      plot.subtitle    = element_text(size = 8, color = "grey40"),
      legend.position  = "bottom",
      legend.key.size  = unit(0.4, "cm")
    )
}

make_ribbon_plot <- function(cu, title_str) {
  fc_sub   <- fc_all[cu_type == cu]
  hist_pfx <- paste0("yoy_", tolower(cu), "_assets_pct_")
  hist_cols <- intersect(paste0(hist_pfx, names(CAT_LABELS)), names(qtrly))
  fc_sub[, cat_label := factor(cat_label, levels = unname(CAT_LABELS))]
  
  p <- ggplot()
  if (length(hist_cols) > 0) {
    hist_dt <- melt(qtrly[, c("yearqtr", hist_cols), with = FALSE],
                    id.vars      = "yearqtr",
                    variable.name = "cat_col",
                    value.name   = "actual")
    hist_dt[, cat      := sub(hist_pfx, "", as.character(cat_col))]
    hist_dt[, cat_label := factor(CAT_LABELS[cat],
                                  levels = unname(CAT_LABELS))]
    p <- p + geom_line(data = hist_dt[!is.na(actual)],
                       aes(x = yearqtr, y = actual, color = "Actual"),
                       linewidth = 0.6)
  }
  p +
    geom_ribbon(data = fc_sub,
                aes(x = yearqtr, ymin = fc_lo95, ymax = fc_hi95, fill = "95% CI"),
                alpha = 0.18) +
    geom_ribbon(data = fc_sub,
                aes(x = yearqtr, ymin = fc_lo80, ymax = fc_hi80, fill = "80% CI"),
                alpha = 0.28) +
    geom_line(data = fc_sub,
              aes(x = yearqtr, y = fc_mean, color = "Forecast"),
              linewidth = 0.7, linetype = "dashed") +
    facet_wrap(~ cat_label, ncol = 2, scales = "free_y") +
    scale_color_manual("",
                       values = c("Actual" = "#2c3e50", "Forecast" = "#e74c3c")) +
    scale_fill_manual("",
                      values = c("80% CI" = "#3498db", "95% CI" = "#85c1e9")) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    labs(title = title_str,
         subtitle = "ARIMAX with FRB Baseline 2026 Macro | Part 3b",
         x = NULL, y = "YoY % Change") +
    theme_cu()
}

# P01: FICU ribbons
if (nrow(fc_all[cu_type == "FICU"]) > 0) {
  p01 <- make_ribbon_plot("FICU",
                          "FICU Total Assets -- YoY % Change Forecasts")
  ggsave(file.path(PLOT_DIR, "P01_ficu_asset_forecast_ribbons.pdf"),
         p01, width = 11, height = 10, device = cairo_pdf)
  message("    Saved P01.")
}

# P02: FISCU ribbons
if (nrow(fc_all[cu_type == "FISCU"]) > 0) {
  p02 <- make_ribbon_plot("FISCU",
                          "FISCU Total Assets -- YoY % Change Forecasts")
  ggsave(file.path(PLOT_DIR, "P02_fiscu_asset_forecast_ribbons.pdf"),
         p02, width = 11, height = 10, device = cairo_pdf)
  message("    Saved P02.")
}

# P03: TSCV RMSE comparison
if (nrow(met_all) > 0) {
  met_p3 <- copy(met_all)
  met_p3[, model_label := sprintf("%s | %s", cu_type, cat)]
  met_p3[, model_label := factor(model_label, levels = rev(model_label))]
  m3_melt <- melt(met_p3, id.vars = "model_label",
                  measure.vars = c("tscv_rmse", "tscv_rmse_base"),
                  variable.name = "rmse_type", value.name = "rmse")
  m3_melt[, rmse_type := ifelse(rmse_type == "tscv_rmse",
                                "ARIMAX", "Pure ARIMA")]
  p03 <- ggplot(m3_melt,
                aes(x = model_label, y = rmse, fill = rmse_type)) +
    geom_col(position = position_dodge(0.7), width = 0.6) +
    coord_flip() +
    scale_fill_manual("",
                      values = c("ARIMAX" = "#2ecc71", "Pure ARIMA" = "#e74c3c")) +
    labs(title    = "TSCV RMSE: ARIMAX vs Pure ARIMA -- All 14 Asset Models",
         subtitle = "Part 3b | Lower is better",
         x = NULL, y = "TSCV RMSE (YoY % pts)") +
    theme_cu() + theme(axis.text.y = element_text(size = 7))
  ggsave(file.path(PLOT_DIR, "P03_tscv_rmse_comparison.pdf"),
         p03, width = 10, height = 7, device = cairo_pdf)
  message("    Saved P03.")
}

# P04: Metrics heatmap
if (nrow(met_all) > 0) {
  met_h <- met_all[, .(cu_type, cat, tscv_delta_pct, sigma2)]
  met_h[, cat_label := factor(CAT_LABELS[cat], levels = unname(CAT_LABELS))]
  
  p04a <- ggplot(met_h,
                 aes(x = cu_type, y = cat_label, fill = tscv_delta_pct)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f%%", tscv_delta_pct)), size = 3) +
    scale_fill_gradient2("TSCV Delta%",
                         low = "#27ae60", mid = "white", high = "#e74c3c", midpoint = 0) +
    labs(title    = "TSCV RMSE Improvement Over Pure ARIMA",
         subtitle = "Negative = ARIMAX better",
         x = NULL, y = NULL) + theme_cu()
  
  p04b <- ggplot(met_h,
                 aes(x = cu_type, y = cat_label, fill = sigma2)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", sigma2)), size = 3) +
    scale_fill_gradient("sigma2",
                        low = "#d5f5e3", high = "#e74c3c") +
    labs(title = "Residual Variance (sigma2)", x = NULL, y = NULL) +
    theme_cu()
  
  p04 <- p04a + p04b +
    plot_annotation(
      title = "Part 3b -- Asset Model Diagnostics",
      theme = theme(plot.title = element_text(face = "bold")))
  ggsave(file.path(PLOT_DIR, "P04_metrics_heatmap.pdf"),
         p04, width = 11, height = 6, device = cairo_pdf)
  message("    Saved P04.")
}

# P05: Coefficient heatmap
if (nrow(coef_all) > 0) {
  coef_x <- coef_all[tag %in% c("[MACRO]", "[EXIT]", "[SEAS]") &
                       !is.na(p_value)]
  if (nrow(coef_x) > 0) {
    coef_x[, model_label := sprintf("%s|%s", cu_type, cat)]
    p05 <- ggplot(coef_x,
                  aes(x = model_label, y = term, fill = estimate)) +
      geom_tile(color = "grey80") +
      geom_text(aes(label = significance), size = 2.5) +
      scale_fill_gradient2("Coef",
                           low = "#3498db", mid = "white", high = "#e74c3c", midpoint = 0) +
      labs(title    = "Regressor Coefficients -- All 14 Asset Models",
           subtitle = "Stars: ***p<0.001  **p<0.01  *p<0.05  .p<0.10",
           x = NULL, y = NULL) +
      theme_cu() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
            axis.text.y = element_text(size = 6))
    ggsave(file.path(PLOT_DIR, "P05_coef_heatmap.pdf"),
           p05, width = 14, height = 10, device = cairo_pdf)
    message("    Saved P05.")
  }
}

# P06: OOS RMSE comparison
if (nrow(met_all) > 0 && any(!is.na(met_all$oos_rmse))) {
  oos_sub <- met_all[!is.na(oos_rmse)]
  oos_sub[, model_label := sprintf("%s | %s", cu_type, cat)]
  oos_m <- melt(oos_sub, id.vars = "model_label",
                measure.vars = c("oos_rmse", "oos_rmse_base"),
                variable.name = "type", value.name = "rmse")
  oos_m[, type := ifelse(type == "oos_rmse", "ARIMAX", "Pure ARIMA")]
  p06 <- ggplot(oos_m,
                aes(x = model_label, y = rmse, fill = type)) +
    geom_col(position = position_dodge(0.7), width = 0.6) +
    coord_flip() +
    scale_fill_manual("",
                      values = c("ARIMAX" = "#2ecc71", "Pure ARIMA" = "#e74c3c")) +
    labs(title    = "OOS RMSE: ARIMAX vs Pure ARIMA -- Asset Models",
         subtitle = "Part 3b | Post-2021Q1 holdout",
         x = NULL, y = "OOS RMSE (YoY % pts)") +
    theme_cu()
  ggsave(file.path(PLOT_DIR, "P06_oos_rmse_comparison.pdf"),
         p06, width = 10, height = 6, device = cairo_pdf)
  message("    Saved P06.")
}

# P07: xreg selection frequency
if (nrow(met_all) > 0) {
  xr_rows <- met_all[!is.na(xreg_chosen) & nchar(xreg_chosen) > 0,
                     .(ml = sprintf("%s|%s", cu_type, cat), xreg_chosen)]
  if (nrow(xr_rows) > 0) {
    xr_long  <- xr_rows[,
                        .(var = unlist(strsplit(xreg_chosen, "\\|"))), by = ml]
    freq_dt  <- xr_long[, .N, by = var][order(-N)]
    p07 <- ggplot(freq_dt[seq_len(min(20, nrow(freq_dt)))],
                  aes(x = reorder(var, N), y = N)) +
      geom_col(fill = "#3498db") +
      coord_flip() +
      labs(title    = "Most Frequently Selected Regressors -- Part 3b",
           subtitle = "Count of models (out of 14) where variable chosen by TSCV",
           x = NULL, y = "# Models Selected") +
      theme_cu()
    ggsave(file.path(PLOT_DIR, "P07_xreg_selection_freq.pdf"),
           p07, width = 9, height = 6, device = cairo_pdf)
    message("    Saved P07.")
  }
}

# P08: Per-model 2x2 regression PDF (all 14 models)
message("  P08: Per-model regression PDF (all 14 models)...")
pdf_pages <- list()
model_idx <- 0L

for (m in seq_len(nrow(models))) {
  
  cu_type_m <- models$cu_type[m]
  cat_m     <- models$cat[m]
  cat_lbl_m <- CAT_LABELS[cat_m]
  target_m  <- paste0(TARGET_ROOTS[cu_type_m], "_", cat_m)
  
  met_row <- met_all[cu_type == cu_type_m & cat == cat_m]
  if (nrow(met_row) == 0 || !target_m %in% names(qtrly)) next
  
  train_m <- qtrly_train[is.finite(get(target_m))]
  if (nrow(train_m) < 8) next
  
  y_m    <- train_m[[target_m]]
  yq_m   <- train_m$yearqtr
  y_ts_m <- ts(y_m, frequency = 4,
               start = c(as.integer(floor(min(yq_m))),
                         as.integer(round((min(yq_m) %% 1) * 4 + 1))))
  
  # Parse stored ARIMA order string  e.g. "(1,1,0)(0,1,1)[4]"
  astr <- met_row$arima_order[1]
  om   <- regmatches(astr,
                     regexpr("\\((\\d+),(\\d+),(\\d+)\\)\\((\\d+),(\\d+),(\\d+)\\)", astr))
  if (length(om) == 0) next
  nums_m <- as.integer(regmatches(om[[1]],
                                  gregexpr("\\d+", om[[1]])[[1]]))
  ao_m   <- nums_m[1:3]
  as_m   <- list(order = nums_m[4:6], period = 4L)
  
  cv_str <- met_row$xreg_chosen[1]
  cv_m   <- if (!is.na(cv_str) && nchar(cv_str) > 0)
    strsplit(cv_str, "\\|")[[1]] else character(0)
  
  xr_m <- if (length(cv_m) > 0) build_xreg(train_m, cv_m) else NULL
  
  fit_r <- tryCatch(
    Arima(y_ts_m, order = ao_m, seasonal = as_m,
          xreg = xr_m, method = "ML"),
    error = function(e) NULL
  )
  if (is.null(fit_r)) next
  
  fv     <- as.numeric(fitted(fit_r))
  rv     <- as.numeric(residuals(fit_r))
  dt_fit <- data.table(yq = yq_m, actual = y_m, fitted = fv, resid = rv)
  
  pan1 <- ggplot(dt_fit, aes(x = yq)) +
    geom_line(aes(y = actual, color = "Actual"),  linewidth = 0.7) +
    geom_line(aes(y = fitted, color = "Fitted"),  linewidth = 0.6,
              linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    scale_color_manual("",
                       values = c("Actual" = "#2c3e50", "Fitted" = "#e74c3c")) +
    labs(title = "Actual vs Fitted",
         x = NULL, y = "YoY % Change (Assets)") + theme_cu()
  
  cd_row <- coef_all[cu_type == cu_type_m & cat == cat_m]
  if (nrow(cd_row) > 0) {
    cd_row[, disp := sprintf("%-20s %+.4f  %s %s",
                             term, estimate,
                             ifelse(is.na(significance), "", significance), tag)]
    coef_txt <- paste(c(
      sprintf("ARIMA%s", astr),
      sprintf("AIC=%.1f  BIC=%.1f  sigma2=%.4f",
              fit_r$aic, fit_r$bic, fit_r$sigma2 %||% NA),
      "----------------------------------------",
      cd_row$disp
    ), collapse = "\n")
  } else {
    coef_txt <- "No coefficients available."
  }
  
  pan2 <- ggplot() +
    annotate("text", x = 0, y = 1, label = coef_txt,
             hjust = 0, vjust = 1, family = "mono", size = 2.5) +
    xlim(0, 1) + ylim(0, 1) + theme_void() +
    labs(title = "Model Coefficients")
  
  pan3 <- ggplot(dt_fit, aes(x = yq, y = resid)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    geom_line(color = "#7f8c8d", linewidth = 0.5) +
    geom_smooth(method = "loess", se = FALSE, color = "#e74c3c",
                linewidth = 0.7, span = 0.5) +
    labs(title = "Residuals", x = NULL, y = "Residual") + theme_cu()
  
  r2_v      <- 1 - sum(rv^2, na.rm = TRUE) /
    sum((y_m - mean(y_m, na.rm = TRUE))^2, na.rm = TRUE)
  stats_txt <- sprintf(
    "n = %d obs\nARIMA%s\nxreg vars: %d\nTSCV RMSE: %.4f\nBase RMSE: %.4f\nDelta RMSE: %.2f%%\nAdj-R2 ~ %.4f\nAIC: %.1f\nBIC: %.1f\nsigma2 = %.4f",
    length(y_m), astr, length(cv_m),
    met_row$tscv_rmse[1], met_row$tscv_rmse_base[1],
    met_row$tscv_delta_pct[1], r2_v,
    fit_r$aic, fit_r$bic, fit_r$sigma2 %||% NA)
  
  pan4 <- ggplot() +
    annotate("text", x = 0, y = 1, label = stats_txt,
             hjust = 0, vjust = 1, family = "mono", size = 3) +
    xlim(0, 1) + ylim(0, 1) + theme_void() +
    labs(title = "Model Statistics")
  
  page <- (pan1 + pan2) / (pan3 + pan4) +
    plot_annotation(
      title    = sprintf("Part 3b | Model %02d/14 | %s | %s [%s]",
                         m, cu_type_m, cat_m, cat_lbl_m),
      subtitle = sprintf("Target: %s", target_m),
      theme    = theme(
        plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 8, color = "grey40")))
  
  model_idx             <- model_idx + 1L
  pdf_pages[[model_idx]] <- page
}

if (length(pdf_pages) > 0) {
  out_pdf <- file.path(PLOT_DIR, "P08_all_models_regression.pdf")
  pdf(out_pdf, width = 14, height = 9)
  for (pg in pdf_pages) print(pg)
  dev.off()
  message(sprintf("  Saved P08_all_models_regression.pdf (%d pages).",
                  length(pdf_pages)))
}

message("\n======================================================")
message("  Part 3b complete.")
message(sprintf("  Plots   -> %s/", PLOT_DIR))
message(sprintf("  Results -> %s/", RESULT_DIR))
message("======================================================\n")