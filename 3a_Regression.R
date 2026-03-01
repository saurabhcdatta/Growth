############################################################
# PART 3a — FICU & FISCU COUNT FORECASTING
#
# Targets  : yoy_ficu_pct  (YoY % change in FICU count)
#             yoy_fiscu_pct (YoY % change in FISCU count)
#             7 asset-size categories each → 14 models total
#
# Theory   : CU counts are driven by macroeconomic conditions
#             (interest rates, credit spreads, unemployment,
#             GDP growth, etc.) and by CU-industry exit dynamics
#             (mergers, liquidations, acquisitions). Balance-sheet
#             variables (deposits, loans, expenses) are endogenous
#             to macro and do not independently drive count changes.
#
# Features : FRB Baseline 2026 macro variables only +
#             merger_rate, liquid_rate, acquisition_rate (CU exits)
#             Seasonal dummies q1/q2/q3
#             Future values of exit vars set to ZERO (unknown)
#
# Method   : 1. auto.arima() on training series → fixes ARIMA order
#            2. Time-series cross-validation (TSCV) with expanding
#               window to select the best xreg subset:
#               - LASSO pre-screens candidates → top MAX_XREG_VARS
#               - Backward elimination using TSCV RMSE (not in-sample
#                 p-values) → prevents overfitting
#            3. Final model fit on full training window
#            4. Multi-step ARIMAX forecast to 2030Q4
#               Exit vars set to 0; macro from FRB forward panel
#
# Outputs  : results_3a/  (forecasts, metrics, coefficients CSVs)
#            plots_3a/     (PDF plots — same set as Part 3)
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
PLOT_DIR   <- "plots_3a"
RESULT_DIR <- "results_3a"

DEBUG_MODE    <- TRUE    # FALSE for full production run
DEBUG_ROLL_Q  <- 6       # quarters to use in debug mode

# Rolling window: first test quarter
TRAIN_END  <- zoo::as.yearqtr("2021 Q1")

# TSCV settings
TSCV_MIN_TRAIN <- 20L    # minimum obs in each TSCV fold
TSCV_H         <- 1L     # forecast horizon per fold (1-step ahead)

# Feature selection
MAX_XREG_VARS  <- 10L    # top N by LASSO magnitude passed to ARIMAX
SIG_LEVEL      <- 0.10   # p-value threshold for final model print

# Forecast horizon
FC_END <- zoo::as.yearqtr("2030 Q4")

# YoY% clamp — hard ceiling on per-quarter forecast
YOY_CAP <- 15.0          # ±15% per quarter

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
message(sprintf("    %s rows × %s cols",
                format(nrow(qtrly), big.mark=","),
                format(ncol(qtrly), big.mark=",")))

CAT_MAP <- c("1"="1_Less_10M","2"="2_10M_50M","3"="3_50M_100M",
             "4"="4_100M_500M","5"="5_500M_1B","6"="6_1B_10B",
             "7"="7_10B_Plus")
if (!"cat_label" %in% names(qtrly))
  qtrly[, cat_label := CAT_MAP[as.character(categories)]]

required <- c("date","categories","cat_label","yoy_ficu_pct","yoy_fiscu_pct")
miss <- setdiff(required, names(qtrly))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse=", "))

setorderv(qtrly, c("categories","date"))
all_quarters <- sort(unique(qtrly$date))
cats         <- sort(unique(qtrly$cat_label))
message(sprintf("    Categories : %s", paste(cats, collapse=" | ")))
message(sprintf("    Date range : %s → %s  (%d quarters)",
                as.character(min(all_quarters)),
                as.character(max(all_quarters)),
                length(all_quarters)))

# ════════════════════════════════════════════════════════════
# 3. DEFINE TARGETS AND FEATURES
# ════════════════════════════════════════════════════════════
message("\n[2] Defining targets and features...")

DEP_VARS <- list(
  yoy_ficu_pct  = list(label="FICU YoY % Change",  short="ficu"),
  yoy_fiscu_pct = list(label="FISCU YoY % Change", short="fiscu")
)

# ── CU exit variables ────────────────────────────────────────
# These directly reduce count (mergers absorb CUs, liquidations
# close them, acquisition_rate = fraction acquired by other CUs).
# Future values will be set to ZERO in the forecast horizon.
EXIT_VARS <- c("merger_rate", "liquid_rate", "acquisition_rate",
               "exit_rate", "exit_roll4")

# ── Curated macro variable list ───────────────────────────────
#
# Rationale for each group:
#
# INTEREST RATES & MONETARY POLICY
#   fedfunds / gs3m / gs10  — level and slope of the yield curve
#   directly affect CU borrowing costs and member demand for
#   deposits and loans. Rate hike cycles historically precede
#   consolidation waves as smaller CUs face margin pressure.
#   yield_curve / spread_2s10s — inversion signals recession
#   and reduced new-charter viability.
#   mortgage30 — mortgage rate drives member refinancing volume
#   and indirectly CU growth appetite.
#
# CREDIT CONDITIONS
#   baa_spread / credit_tightness — widening spreads signal
#   financial stress that accelerates mergers and liquidations.
#   real_rate — negative real rates favour CU growth; sharply
#   positive real rates compress net interest margins.
#
# LABOUR MARKET & INCOME
#   unrate — unemployment drives member financial distress,
#   delinquencies, and ultimately exit pressure for weaker CUs.
#   disp_income / savings_rate — household financial health
#   determines deposit inflows and new membership growth.
#
# ECONOMIC ACTIVITY & CONFIDENCE
#   gdp_real — overall economic cycle; recessions historically
#   trigger consolidation via regulatory pressure and losses.
#   cons_confidence — forward-looking member sentiment.
#
# INFLATION
#   cpi_yoy (derived from cpi) / core_cpi — inflation erodes
#   real deposit values and triggers rate responses that
#   reshape CU competitive landscape.
#
# HOUSING & CONSUMER CREDIT
#   housing_permits / hpi_fed — CU mortgage portfolios drive
#   asset growth; housing downturns stress balance sheets and
#   can trigger NCUA supervisory actions → exits.
#   consumer_bankrupt / cons_loan_delinq — member credit
#   stress directly impairs CU financial health.
#
# FRB FORWARD RATES (leading indicators)
#   fwd_1y1y / fwd_1y5y — market expectations of future rate
#   path embedded in current term structure; forward-looking
#   signal of monetary policy trajectory.
#
# All raw level series are STATIONARY (rates, spreads, YoY%,
# or QoQ%). Only their lag/rmean/rsd transforms are included.
# ─────────────────────────────────────────────────────────────
CURATED_MACRO <- c(
  # Interest rates & monetary policy
  "fedfunds", "fedfunds_chg", "fedfunds_cycle",
  "gs3m", "gs10", "gs30",
  "yield_curve", "yield_curve_inv", "spread_2s10s",
  "mortgage30", "real_rate",
  # Credit conditions
  "baa_spread", "credit_tightness",
  # Labour & income
  "unrate", "disp_income", "savings_rate",
  # Economic activity & confidence
  "gdp_real", "cons_confidence",
  # Inflation
  "cpi_yoy", "core_cpi",
  # Housing & consumer credit
  "housing_permits", "hpi_fed",
  "consumer_bankrupt", "cons_loan_delinq",
  # FRB forward rates (leading indicators)
  "fwd_1y1y", "fwd_1y5y",
  # FOMC regime
  "fomc_regime", "hike_run"
)

# Stationary transforms to include for each curated macro var
# yoy_ / qoq_ changes, lags (1,4,8), rolling means (4,8), rolling SDs
STATIONARY_TRANSFORMS <- paste(
  paste(paste0("^", CURATED_MACRO, "$"),   collapse="|"),  # raw level (if stationary)
  paste(paste0("^yoy_(",  paste(CURATED_MACRO, collapse="|"), ")$"), collapse="|"),
  paste(paste0("^qoq_(",  paste(CURATED_MACRO, collapse="|"), ")$"), collapse="|"),
  paste(paste0("^(",      paste(CURATED_MACRO, collapse="|"), ")_lag[0-9]"), collapse="|"),
  paste(paste0("^(",      paste(CURATED_MACRO, collapse="|"), ")_rmean[0-9]"), collapse="|"),
  paste(paste0("^(",      paste(CURATED_MACRO, collapse="|"), ")_rsd[0-9]"), collapse="|"),
  paste(paste0("^(",      paste(CURATED_MACRO, collapse="|"), ")_chg$"), collapse="|"),
  paste(paste0("^(",      paste(CURATED_MACRO, collapse="|"), ")_cyc$"), collapse="|"),
  sep="|"
)

# Collect all numeric column names
all_num_cols <- names(qtrly)[vapply(qtrly, is.numeric, logical(1))]

# Macro features: only curated list and their stationary transforms
macro_feats <- grep(STATIONARY_TRANSFORMS, all_num_cols,
                    value=TRUE, perl=TRUE)

# Drop any column whose variance is near zero (constant/near-constant)
# — these survive the pattern match but add no signal
macro_feats_avail <- macro_feats[macro_feats %in% names(qtrly)]
macro_feats_avail <- macro_feats_avail[vapply(macro_feats_avail, function(cn) {
  v <- qtrly[[cn]]
  length(unique(v[!is.na(v)])) > 3L
}, logical(1))]
macro_feats <- macro_feats_avail

# Exit variable features (raw rates + their transforms)
exit_feats_pat <- paste(EXIT_VARS, collapse="|")
exit_feats <- grep(exit_feats_pat, all_num_cols, value=TRUE, perl=TRUE)
# Keep raw rates (already stationary) + lag/rmean transforms
stationary_exit <- "^(merger_rate|liquid_rate|acquisition_rate|exit_rate|exit_roll4)$|_lag[0-9]|_rmean[0-9]"
exit_feats <- grep(stationary_exit, exit_feats, value=TRUE, perl=TRUE)
exit_feats <- unique(c(exit_feats, intersect(EXIT_VARS, all_num_cols)))

# Seasonal dummies
seas_feats <- intersect(c("q1","q2","q3"), all_num_cols)

# Combined feature set
FEATS_ALL <- unique(c(macro_feats, exit_feats, seas_feats))

# Hard exclusions — targets themselves and any ficu/fiscu leakage
HARD_EXCL <- c("yoy_ficu_pct","yoy_fiscu_pct","yoy_assets_pct",
               "qoq_ficu_pct","qoq_fiscu_pct",
               "yoy_ficu_count","yoy_fiscu_count",
               "qoq_ficu_count","qoq_fiscu_count",
               "ficu_count","fiscu_count",
               "ficu_count_lag4","fiscu_count_lag4",
               "ld_ficu","ld_fiscu",
               "net_entry_rate","net_entry_rate_fiscu",
               "categories","n_active","n_total","q4")
FEATS_ALL <- setdiff(FEATS_ALL, HARD_EXCL)

message(sprintf("    Curated macro vars: %d base series", length(CURATED_MACRO)))
message(sprintf("    Macro features    : %d (incl. transforms)", length(macro_feats)))
message(sprintf("    Exit rate features: %d", length(exit_feats)))
message(sprintf("    Seasonal dummies  : %d", length(seas_feats)))
message(sprintf("    Total features    : %d", length(FEATS_ALL)))
message(sprintf("    (was ~500+ with all FRB transforms; now focused on %d causal drivers)",
                length(FEATS_ALL)))

# ════════════════════════════════════════════════════════════
# 4. HELPER FUNCTIONS
# ════════════════════════════════════════════════════════════
message("\n[3] Building helpers...")

# Winsorise at 1st/99th percentile
winsorise <- function(x, p=0.01) {
  lo <- quantile(x, p,   na.rm=TRUE)
  hi <- quantile(x, 1-p, na.rm=TRUE)
  pmax(pmin(x, hi), lo)
}

# Back-transform YoY% → level
yoy_to_level <- function(yoy_pct, anchor) {
  if (is.na(yoy_pct) || is.na(anchor) || anchor <= 0) return(NA_real_)
  anchor * (1 + yoy_pct / 100)
}

# OOS metrics
reg_metrics <- function(actual, pred) {
  ok  <- !is.na(actual) & !is.na(pred)
  a   <- actual[ok]; p <- pred[ok]
  n   <- sum(ok)
  if (n < 2) return(list(rmse=NA,mae=NA,mape=NA,r2_oos=NA,n=n))
  rmse  <- sqrt(mean((a-p)^2))
  mae   <- mean(abs(a-p))
  mape  <- mean(abs((a-p)/a)*100, na.rm=TRUE)
  ss_r  <- sum((a-p)^2)
  ss_t  <- sum((a-mean(a))^2)
  r2    <- if (ss_t>0) 1-ss_r/ss_t else NA_real_
  list(rmse=rmse, mae=mae, mape=mape, r2_oos=r2, n=n)
}

# Extract xreg column names from a fitted Arima object
# (strips AR/MA/intercept/drift parameter names)
get_xreg_names <- function(fit) {
  if (is.null(fit)) return(character(0))
  nms <- names(fit$coef)
  nms[!grepl("^(ar|ma|sar|sma)[0-9]+$|^intercept$|^drift$|^mean$",
              nms, perl=TRUE)]
}

# Build a clean feature matrix from a data.table
prep_X <- function(dt, feats, corr_cut=0.92, min_nonmiss=0.70) {
  cols <- intersect(feats, names(dt))
  if (length(cols) == 0) return(NULL)
  mat <- as.matrix(dt[, cols, with=FALSE])
  # Drop near-constant or high-missing columns
  ok_miss <- apply(mat, 2, function(x) mean(!is.na(x)) >= min_nonmiss)
  ok_var  <- apply(mat, 2, function(x) var(x, na.rm=TRUE) > 1e-10)
  mat <- mat[, ok_miss & ok_var, drop=FALSE]
  if (ncol(mat) == 0) return(NULL)
  # Impute remaining NAs with column median
  for (j in seq_len(ncol(mat))) {
    na_j <- is.na(mat[,j])
    if (any(na_j)) mat[na_j, j] <- median(mat[,j], na.rm=TRUE)
  }
  # Drop highly correlated columns (keep first of each pair)
  if (ncol(mat) > 2) {
    cr  <- cor(mat, use="pairwise.complete.obs")
    cr[is.na(cr)] <- 0
    keep <- rep(TRUE, ncol(mat))
    for (i in seq_len(ncol(mat)-1)) {
      if (!keep[i]) next
      for (j in seq(i+1, ncol(mat))) {
        if (keep[j] && abs(cr[i,j]) > corr_cut) keep[j] <- FALSE
      }
    }
    mat <- mat[, keep, drop=FALSE]
  }
  if (ncol(mat) == 0) return(NULL)
  mat
}

# Compute p-values for xreg coefficients in a fitted Arima
xreg_pvals <- function(fit, n_obs) {
  xv <- get_xreg_names(fit)
  if (length(xv) == 0) return(setNames(numeric(0), character(0)))
  cf  <- fit$coef[xv]
  se  <- sqrt(diag(fit$var.coef))[match(xv, rownames(fit$var.coef))]
  tv  <- cf / se
  df  <- max(n_obs - length(fit$coef), 1L)
  pv  <- 2 * pt(-abs(tv), df=df)
  setNames(pv, xv)
}

# Save a ggplot to PDF
save_plot <- function(p, filename, w=12, h=7) {
  path <- file.path(PLOT_DIR, filename)
  tryCatch({
    pdf(path, width=w, height=h)
    print(p)
    dev.off()
    message(sprintf("    Saved: %s", filename))
  }, error=function(e) {
    try(dev.off(), silent=TRUE)
    message(sprintf("    [WARN] Plot save failed: %s — %s", filename, e$message))
  })
}

# ════════════════════════════════════════════════════════════
# 5. CORE FITTING FUNCTION: TSCV-SELECTED ARIMAX
# ════════════════════════════════════════════════════════════
#
# Architecture for one rolling window:
#   Step 1: LASSO pre-screens features → top MAX_XREG_VARS candidates
#   Step 2: auto.arima() on y_ts alone → fixes ARIMA(p,d,q) order
#   Step 3: TSCV backward elimination: start with all candidates,
#           iteratively drop the variable whose removal most
#           IMPROVES TSCV RMSE (i.e. reduces overfitting).
#           Stop when no removal improves RMSE.
#   Step 4: Fit final ARIMAX on full training window
#   Step 5: 1-step ahead forecast for rolling evaluation
# ════════════════════════════════════════════════════════════

# TSCV RMSE for an xreg variable set on a training series
# Uses expanding window: each fold adds one more observation
tscv_rmse <- function(y_ts, xreg_mat, arima_ord, has_seas,
                      P_ord, D_ord, Q_ord, min_train=TSCV_MIN_TRAIN) {
  n     <- length(y_ts)
  if (n < min_train + 2L) return(Inf)
  errs  <- numeric(0)

  # For efficiency: if n is large, evaluate only from (n - 20) onward
  # This keeps TSCV honest (genuinely OOS) while bounding runtime.
  # The last 20 one-step-ahead errors are representative of recent fit.
  eval_start <- max(min_train, n - 20L)
  for (i in seq(eval_start, n - 1L)) {
    y_tr  <- window(y_ts, end=time(y_ts)[i])
    xr_tr <- if (!is.null(xreg_mat)) xreg_mat[seq_len(i), , drop=FALSE] else NULL
    xr_te <- if (!is.null(xreg_mat)) xreg_mat[i+1L, , drop=FALSE]       else NULL

    fit_cv <- tryCatch(
      forecast::Arima(y_tr,
        order    = arima_ord,
        seasonal = list(order=if(has_seas) c(P_ord,D_ord,Q_ord) else c(0L,0L,0L),
                        period=4L),
        xreg     = xr_tr,
        method   = "CSS-ML"),
      error = function(e) NULL)
    if (is.null(fit_cv)) next

    fc_cv <- tryCatch(
      forecast::forecast(fit_cv, h=TSCV_H, xreg=xr_te),
      error = function(e) NULL)
    if (is.null(fc_cv)) next

    actual_h <- as.numeric(y_ts)[i + TSCV_H]
    pred_h   <- as.numeric(fc_cv$mean)[TSCV_H]
    if (!is.na(actual_h) && !is.na(pred_h))
      errs <- c(errs, (actual_h - pred_h)^2)
  }
  if (length(errs) == 0) return(Inf)
  sqrt(mean(errs))
}

fit_window_3a <- function(train_dt, test_row, dep_var, feats) {

  y_train <- train_dt[[dep_var]]
  if (sum(!is.na(y_train)) < TSCV_MIN_TRAIN)
    return(list(ok=FALSE, reason="insufficient training obs"))

  y_train_w <- winsorise(y_train)

  # Build ts object
  train_dates <- sort(train_dt$date)
  min_yq   <- zoo::as.yearqtr(min(train_dates))
  start_yr <- as.integer(format(min_yq, "%Y"))
  start_q  <- as.integer(format(min_yq, "%q"))
  y_ts     <- ts(y_train_w, frequency=4L, start=c(start_yr, start_q))

  # Build feature matrix
  X_train <- prep_X(train_dt, feats)
  if (is.null(X_train) || ncol(X_train) < 1L)
    return(list(ok=FALSE, reason="no features after prep"))

  n_obs       <- length(y_train_w)
  nfolds_use  <- max(3L, min(10L, floor(n_obs / 3L)))
  x_train_cols <- colnames(X_train)

  # ── Step 1: LASSO pre-screening ──────────────────────────
  cv_fit <- tryCatch(
    glmnet::cv.glmnet(X_train, y_train_w, alpha=1,
                      nfolds=nfolds_use, standardize=TRUE,
                      intercept=TRUE, type.measure="mse"),
    error=function(e) NULL)
  if (is.null(cv_fit))
    return(list(ok=FALSE, reason="glmnet failed"))

  lasso_coef <- as.matrix(coef(cv_fit, s="lambda.1se"))
  selected   <- rownames(lasso_coef)[lasso_coef[,1L] != 0 &
                  rownames(lasso_coef) != "(Intercept)"]
  selected   <- intersect(selected, x_train_cols)

  # Always include exit vars and seasonal dummies if available
  protected  <- intersect(c(EXIT_VARS, "q1","q2","q3"), x_train_cols)
  candidates <- unique(c(selected, protected))
  candidates <- intersect(candidates, x_train_cols)

  # Cap at MAX_XREG_VARS by LASSO magnitude
  if (length(candidates) > MAX_XREG_VARS) {
    lasso_abs  <- abs(lasso_coef[candidates, 1L])
    # Always keep protected vars; rank the rest by magnitude
    non_prot   <- setdiff(candidates, protected)
    prot_avail <- intersect(protected, candidates)
    budget     <- MAX_XREG_VARS - length(prot_avail)
    if (budget > 0 && length(non_prot) > 0) {
      top_np   <- names(sort(abs(lasso_coef[non_prot,1L]),
                             decreasing=TRUE))[seq_len(min(budget, length(non_prot)))]
      candidates <- unique(c(prot_avail, top_np))
    } else {
      candidates <- prot_avail[seq_len(min(MAX_XREG_VARS, length(prot_avail)))]
    }
    message(sprintf("        [XREG CAP] capped to %d vars", length(candidates)))
  }

  # Build training medians for test-row imputation
  train_meds <- setNames(
    lapply(x_train_cols, function(cn) median(X_train[,cn], na.rm=TRUE)),
    x_train_cols)

  # Build xreg matrix helper
  build_xreg_mat <- function(vars) {
    vars <- intersect(vars, x_train_cols)
    if (length(vars) == 0L) return(NULL)
    m    <- X_train[, vars, drop=FALSE]
    ok_v <- apply(m, 2, function(x) var(x, na.rm=TRUE) > 0)
    m    <- m[, ok_v, drop=FALSE]
    if (ncol(m) == 0L) return(NULL)
    m
  }

  # ── Step 2: auto.arima on y_ts alone (fixes ARIMA order) ──
  arima_base <- tryCatch(
    forecast::auto.arima(y_ts, stepwise=TRUE, approximation=TRUE,
                         max.p=3L, max.q=2L, max.P=1L, max.Q=1L,
                         seasonal=TRUE),
    error=function(e) NULL)
  if (is.null(arima_base))
    return(list(ok=FALSE, reason="auto.arima failed"))

  arima_ord <- forecast::arimaorder(arima_base)
  p_ord <- as.integer(arima_ord["p"]); d_ord <- as.integer(arima_ord["d"])
  q_ord <- as.integer(arima_ord["q"]); P_ord <- as.integer(arima_ord["P"])
  D_ord <- as.integer(arima_ord["D"]); Q_ord <- as.integer(arima_ord["Q"])
  has_seas <- any(c(P_ord, D_ord, Q_ord) != 0L)
  arima_order_vec <- c(p=p_ord, d=d_ord, q=q_ord,
                       P=P_ord, D=D_ord, Q=Q_ord)

  # Refit helper (always uses same ARIMA order)
  refit_fn <- function(vars) {
    xr <- build_xreg_mat(vars)
    fit <- tryCatch(
      forecast::Arima(y_ts,
        order    = c(p_ord, d_ord, q_ord),
        seasonal = list(order=if(has_seas) c(P_ord,D_ord,Q_ord) else c(0L,0L,0L),
                        period=4L),
        xreg     = xr,
        method   = "CSS-ML"),
      error=function(e) tryCatch(
        forecast::Arima(y_ts,
          order  = c(p_ord, d_ord, q_ord),
          seasonal = list(order=c(0L,0L,0L), period=4L),
          xreg   = xr, method="ML"),
        error=function(e2) NULL))
    actual_vars <- get_xreg_names(fit)
    list(fit=fit, vars=actual_vars)
  }

  # ── Step 3: TSCV backward elimination ────────────────────
  # Start: compute baseline TSCV RMSE with all candidates
  cur_vars    <- candidates
  xreg_cur    <- build_xreg_mat(cur_vars)
  best_rmse   <- tscv_rmse(y_ts, xreg_cur, c(p_ord,d_ord,q_ord),
                            has_seas, P_ord, D_ord, Q_ord)
  q_protect   <- intersect(c("q1","q2","q3"), cur_vars)
  exit_protect <- intersect(EXIT_VARS, cur_vars)
  protected_v  <- unique(c(q_protect, exit_protect))

  MAX_ELIM <- 20L
  for (iter in seq_len(MAX_ELIM)) {
    removable <- setdiff(cur_vars, protected_v)
    if (length(removable) == 0L) break

    # Try removing each non-protected var; keep the best improvement
    best_drop <- NULL
    best_new_rmse <- best_rmse

    for (v in removable) {
      trial_vars <- setdiff(cur_vars, v)
      trial_xreg <- build_xreg_mat(trial_vars)
      trial_rmse <- tscv_rmse(y_ts, trial_xreg, c(p_ord,d_ord,q_ord),
                               has_seas, P_ord, D_ord, Q_ord)
      if (isTRUE(trial_rmse < best_new_rmse)) {
        best_new_rmse <- trial_rmse
        best_drop     <- v
      }
    }

    if (is.null(best_drop)) break  # no improvement — stop

    cur_vars  <- setdiff(cur_vars, best_drop)
    best_rmse <- best_new_rmse
    message(sprintf("        [TSCV ELIM iter %d] dropped '%s'  RMSE=%.4f  vars=%d",
                    iter, best_drop, best_rmse, length(cur_vars)))
  }

  message(sprintf("        [TSCV ELIM] final: %d vars, TSCV RMSE=%.4f",
                  length(cur_vars), best_rmse))

  # ── Step 4: Final ARIMAX on full training window ──────────
  res_final <- refit_fn(cur_vars)
  arimax_final <- res_final$fit
  sig_vars     <- res_final$vars

  # If all vars eliminated, use pure ARIMA
  if (is.null(arimax_final)) {
    arimax_final <- arima_base
    sig_vars     <- character(0)
    message("        [FALLBACK] pure ARIMA (no xreg survived)")
  }

  # ── Build test xreg vector ───────────────────────────────
  build_test_xreg <- function(vars) {
    if (length(vars) == 0L) return(NULL)
    mat <- matrix(NA_real_, nrow=1L, ncol=length(vars),
                  dimnames=list(NULL, vars))
    test_df <- as.data.frame(test_row)[1L,,drop=FALSE]
    for (v in vars) {
      val <- if (v %in% names(test_df))
               suppressWarnings(as.numeric(test_df[[v]])) else NA_real_
      if (is.finite(val)) mat[1L,v] <- val
      else if (v %in% names(train_meds) && is.finite(train_meds[[v]]))
        mat[1L,v] <- train_meds[[v]]
      else mat[1L,v] <- 0
    }
    mat
  }
  xreg_test <- build_test_xreg(sig_vars)

  # ── Step 5: 1-step ahead forecast ────────────────────────
  fc_out <- tryCatch(
    forecast::forecast(arimax_final, h=1L, xreg=xreg_test, level=95),
    error=function(e) NULL)

  if (is.null(fc_out))
    return(list(ok=FALSE, reason="forecast() failed"))

  pred_final <- as.numeric(fc_out$mean)
  if (is.na(pred_final))
    return(list(ok=FALSE, reason="forecast returned NA"))

  # Clamp
  pred_final <- max(min(pred_final,  YOY_CAP), -YOY_CAP)
  pred_lo95  <- max(as.numeric(fc_out$lower[1L]), -YOY_CAP * 1.5)
  pred_hi95  <- min(as.numeric(fc_out$upper[1L]),  YOY_CAP * 1.5)

  # ── Coefficient table ─────────────────────────────────────
  cf_all  <- arimax_final$coef
  cf_se_v <- sqrt(diag(arimax_final$var.coef))
  cf_tval <- cf_all / cf_se_v
  n_df    <- max(length(y_train_w) - length(cf_all), 1L)
  cf_pval <- 2 * pt(-abs(cf_tval), df=n_df)

  coef_dt <- data.table(
    variable = names(cf_all),
    estimate = as.numeric(cf_all),
    std_err  = as.numeric(cf_se_v),
    t_stat   = as.numeric(cf_tval),
    p_value  = as.numeric(cf_pval),
    selected = names(cf_all) %in% sig_vars
  )

  # ── Adjusted R² ──────────────────────────────────────────
  resid_v  <- as.numeric(residuals(arimax_final))
  n_p      <- length(y_train_w)
  k_p      <- length(cf_all)
  ss_r     <- sum(resid_v^2, na.rm=TRUE)
  ss_t     <- sum((y_train_w - mean(y_train_w, na.rm=TRUE))^2, na.rm=TRUE)
  r2_raw   <- if (isTRUE(ss_t > 0)) 1 - ss_r/ss_t else NA_real_
  adj_r2   <- if (!is.na(r2_raw) && isTRUE(n_p > k_p+1L))
                1 - (1-r2_raw)*(n_p-1L)/(n_p-k_p-1L) else NA_real_

  # ── Screen print (last training window only) ─────────────
  # (called from rolling loop for final window)
  list(
    ok          = TRUE,
    pred_final  = pred_final,
    pred_lo95   = pred_lo95,
    pred_hi95   = pred_hi95,
    method_used = "ARIMAX_TSCV",
    sig_vars    = sig_vars,
    coef_dt     = coef_dt,
    arimax_fit  = arimax_final,
    arima_order = arima_order_vec,
    adj_r2      = adj_r2,
    tscv_rmse   = best_rmse,
    n_train     = n_obs,
    n_lasso_sel = length(selected),
    n_final     = length(sig_vars)
  )
}

# ════════════════════════════════════════════════════════════
# 6. ROLLING-WINDOW MODELLING  (14 models: 2 targets × 7 cats)
# ════════════════════════════════════════════════════════════
message("\n[4] Rolling-window TSCV-ARIMAX (14 models)...")
message(sprintf("    Training window start : 2005 Q1"))
message(sprintf("    First test quarter    : %s", as.character(TRAIN_END)))
message(sprintf("    Max xreg vars         : %d", MAX_XREG_VARS))
message(sprintf("    Mode                  : %s",
                if(DEBUG_MODE) "DEBUG" else "PRODUCTION"))

test_quarters <- all_quarters[all_quarters > TRAIN_END]
if (DEBUG_MODE && length(test_quarters) > DEBUG_ROLL_Q)
  test_quarters <- tail(test_quarters, DEBUG_ROLL_Q)

message(sprintf("    Rolling over %d quarters (%s → %s)",
                length(test_quarters),
                as.character(min(test_quarters)),
                as.character(max(test_quarters))))

all_forecasts <- list()
all_coefs     <- list()
all_metrics   <- list()
all_fits      <- list()   # final ARIMAX object per model

model_id <- 0L

for (dv in names(DEP_VARS)) {
  dv_label <- DEP_VARS[[dv]]$label
  dv_short <- DEP_VARS[[dv]]$short
  message(sprintf("\n  ── Target: %s ──", dv_label))

  for (cat in cats) {
    model_id <- model_id + 1L
    n_models <- length(DEP_VARS) * length(cats)

    cat_dt <- qtrly[cat_label == cat]
    setorderv(cat_dt, "date")

    message(sprintf("    [Model %02d/%02d] %s | %s",
                    model_id, n_models, dv_short, cat))
    tic(sprintf("Model %02d", model_id))

    fc_rows   <- list()
    coef_rows <- list()

    # Strip contemporaneous ficu/fiscu leakage for both targets
    CONTEMP_PAT <- "^(yoy|qoq)_.*(ficu|fiscu)|.*(ficu|fiscu).*(_accel|_cyc)$"
    feats_dv <- FEATS_ALL
    contemp_drop <- grep(CONTEMP_PAT, feats_dv, perl=TRUE, value=TRUE)
    contemp_drop <- contemp_drop[!grepl("_lag[0-9]", contemp_drop, perl=TRUE)]
    feats_dv <- setdiff(feats_dv, contemp_drop)

    last_res <- NULL  # store last window result for screen print + all_fits

    for (tq in test_quarters) {
      train_idx <- cat_dt$date >= zoo::as.yearqtr("2005 Q1") &
                   cat_dt$date <  tq
      test_idx  <- cat_dt$date == tq
      if (sum(train_idx) < TSCV_MIN_TRAIN || sum(test_idx) == 0) next

      train_dt <- cat_dt[train_idx]
      test_row  <- cat_dt[test_idx][1L]

      res <- fit_window_3a(train_dt, test_row, dv, feats_dv)
      last_res <- res

      # Actual value
      actual_val <- test_row[[dv]]
      if (is.na(actual_val)) {
        col_lv  <- if (dv=="yoy_ficu_pct") "ficu_count" else "fiscu_count"
        lag4_val <- cat_dt[date == tq - 1, get(col_lv)][1L]
        cur_val  <- test_row[[col_lv]]
        if (!is.na(lag4_val) && lag4_val > 0 && !is.na(cur_val))
          actual_val <- (cur_val - lag4_val) / lag4_val * 100
      }

      if (!res$ok) {
        fc_rows[[length(fc_rows)+1L]] <- data.table(
          dep_var=dv, dv_label=dv_label, cat_label=cat,
          date=tq, actual=actual_val,
          pred_final=NA_real_, pred_lo95=NA_real_, pred_hi95=NA_real_,
          method_used="FAILED", error_msg=res$reason,
          adj_r2=NA_real_, tscv_rmse=NA_real_,
          n_lasso_sel=NA_integer_, n_final=NA_integer_)
        next
      }

      fc_rows[[length(fc_rows)+1L]] <- data.table(
        dep_var=dv, dv_label=dv_label, cat_label=cat,
        date=tq, actual=actual_val,
        pred_final=res$pred_final,
        pred_lo95=res$pred_lo95,
        pred_hi95=res$pred_hi95,
        method_used=res$method_used,
        error_msg=NA_character_,
        adj_r2=res$adj_r2,
        tscv_rmse=res$tscv_rmse,
        n_lasso_sel=as.integer(res$n_lasso_sel),
        n_final=as.integer(res$n_final)
      )

      if (!is.null(res$coef_dt) && nrow(res$coef_dt) > 0) {
        cd <- copy(res$coef_dt)
        cd[, `:=`(dep_var=dv, cat_label=cat, date=tq)]
        coef_rows[[length(coef_rows)+1L]] <- cd
      }
    }  # end tq loop

    if (length(fc_rows) == 0) {
      message("        insufficient data")
      toc(); next
    }

    fc_dt <- rbindlist(fc_rows, fill=TRUE)
    all_forecasts[[paste(dv, cat, sep="|")]] <- fc_dt

    valid <- fc_dt[!is.na(actual) & !is.na(pred_final)]
    if (nrow(valid) >= 2L) {
      m_met <- reg_metrics(valid$actual, valid$pred_final)
      m_met$dep_var   <- dv
      m_met$dv_label  <- dv_label
      m_met$cat_label <- cat
      all_metrics[[paste(dv, cat, sep="|")]] <- m_met
      message(sprintf("        RMSE=%.3f  R²=%.3f  n=%d  TSCV_RMSE=%.3f",
                      m_met$rmse, m_met$r2_oos, m_met$n,
                      median(fc_dt$tscv_rmse, na.rm=TRUE)))
    } else {
      message("        insufficient valid pairs")
    }

    if (length(coef_rows) > 0)
      all_coefs[[paste(dv, cat, sep="|")]] <- rbindlist(coef_rows, fill=TRUE)

    # Screen print for last window — show ALL coefficients (incl AR/MA)
    if (!is.null(last_res) && last_res$ok && !is.null(last_res$coef_dt)) {
      cd <- last_res$coef_dt   # show all, not just selected
      if (nrow(cd) > 0) {
        cat(sprintf("\nCall:\n  ARIMAX via auto.arima() + TSCV backward elimination\n"))
        cat(sprintf("  ARIMA order: ARIMA(%d,%d,%d)  LASSO selected: %d  Final xreg: %d\n",
                    last_res$arima_order["p"], last_res$arima_order["d"],
                    last_res$arima_order["q"],
                    last_res$n_lasso_sel, last_res$n_final))
        cat(sprintf("  TSCV RMSE: %.4f  Adj R²: %.4f  Method: ARIMAX_TSCV\n\n",
                    last_res$tscv_rmse, last_res$adj_r2 %||% NA))
        cat("Coefficients:\n")
        cat(sprintf("  %-30s %12s %12s %8s %12s\n",
                    "variable","Estimate","Std.Error","t value","Pr(>|t|)"))
        cat("  ", strrep("-", 78), "\n", sep="")
        for (i in seq_len(nrow(last_res$coef_dt))) {
          row <- last_res$coef_dt[i]
          stars <- if (is.na(row$p_value)) "" else
                   if (row$p_value < 0.001) "***" else
                   if (row$p_value < 0.01)  "**"  else
                   if (row$p_value < 0.05)  "*"   else
                   if (row$p_value < 0.10)  "."   else " "
          cat(sprintf("  %-30s %12.7f %12.7f %8.3f %12.7f %s\n",
                      row$variable, row$estimate, row$std_err,
                      row$t_stat, row$p_value, stars))
        }
        cat(sprintf("  ---\n  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n"))
        cat(sprintf("  Adjusted R-squared: %.4f  (in-sample, full training window)\n\n",
                    last_res$adj_r2 %||% NA))
      }
    }

    # Store final fit for future forecast
    if (!is.null(last_res) && last_res$ok) {
      all_fits[[paste(dv, cat, sep="|")]] <- list(
        fit       = last_res$arimax_fit,
        sig_vars  = last_res$sig_vars,
        dep_var   = dv,
        cat_label = cat,
        n_train   = last_res$n_train,
        date_end  = max(test_quarters)
      )
    }

    toc()
  }  # end cat loop
}  # end dv loop

# ════════════════════════════════════════════════════════════
# 7. CONSOLIDATE RESULTS
# ════════════════════════════════════════════════════════════
message("\n[5] Consolidating results...")

forecasts_all <- rbindlist(all_forecasts, fill=TRUE)
metrics_all   <- if (length(all_metrics)>0)
                   rbindlist(lapply(all_metrics, as.data.table), fill=TRUE) else data.table()
coefs_all     <- if (length(all_coefs)>0)
                   rbindlist(all_coefs, fill=TRUE) else data.table()

message(sprintf("    Forecast rows    : %d", nrow(forecasts_all)))
message(sprintf("    Metrics rows     : %d", nrow(metrics_all)))
message(sprintf("    Coefficient rows : %d", nrow(coefs_all)))

fwrite(forecasts_all, file.path(RESULT_DIR, "forecasts_3a.csv"))
fwrite(metrics_all,   file.path(RESULT_DIR, "metrics_3a.csv"))
fwrite(coefs_all,     file.path(RESULT_DIR, "coefficients_3a.csv"))
message("    Saved CSVs to results_3a/")

# ════════════════════════════════════════════════════════════
# 8. BACK-TRANSFORM: YoY% → LEVEL (count)
# ════════════════════════════════════════════════════════════
message("\n[5b] Back-transforming to level space...")

full_levels <- qtrly[, .(cat_label, date, ficu_count, fiscu_count)]
setorderv(full_levels, c("cat_label","date"))

make_level_fc <- function(dv, count_col) {
  fc_sub <- forecasts_all[dep_var == dv,
    .(cat_label, date, pred_yoy=pred_final,
      lo95_yoy=pred_lo95, hi95_yoy=pred_hi95, method_used)]

  # Anchor = count 4 quarters prior (lag-4 of actual level)
  lv_sub <- full_levels[, c("cat_label","date",count_col), with=FALSE]
  setnames(lv_sub, count_col, "count_val")
  setorderv(lv_sub, c("cat_label","date"))
  lv_sub[, anchor       := shift(count_val, 4L, type="lag"), by=cat_label]
  lv_sub[, actual_level := count_val]

  fc_m <- merge(fc_sub,
                lv_sub[, .(cat_label, date, anchor, actual_level)],
                by=c("cat_label","date"), all.x=TRUE)
  fc_m[, pred_level := mapply(yoy_to_level, pred_yoy, anchor)]
  fc_m[, lo95_level := mapply(yoy_to_level, lo95_yoy, anchor)]
  fc_m[, hi95_level := mapply(yoy_to_level, hi95_yoy, anchor)]
  fc_m[, series := count_col]
  fc_m
}

level_ficu  <- make_level_fc("yoy_ficu_pct",  "ficu_count")
level_fiscu <- make_level_fc("yoy_fiscu_pct", "fiscu_count")

level_fc <- rbindlist(list(level_ficu, level_fiscu), fill=TRUE)
level_fc[, date := zoo::as.yearqtr(as.numeric(date))]

# Level-space OOS metrics
level_metrics <- level_fc[!is.na(actual_level) & !is.na(pred_level),
  .(rmse   = sqrt(mean((actual_level-pred_level)^2, na.rm=TRUE)),
    mae    = mean(abs(actual_level-pred_level), na.rm=TRUE),
    mape   = mean(abs((actual_level-pred_level)/actual_level)*100, na.rm=TRUE),
    r2_oos = {
      ss_r <- sum((actual_level-pred_level)^2, na.rm=TRUE)
      ss_t <- sum((actual_level-mean(actual_level,na.rm=TRUE))^2, na.rm=TRUE)
      if (isTRUE(ss_t>0)) 1-ss_r/ss_t else NA_real_},
    n=.N),
  by=.(series, cat_label)]

fwrite(level_fc,      file.path(RESULT_DIR, "level_forecasts_3a.csv"))
fwrite(level_metrics, file.path(RESULT_DIR, "level_metrics_3a.csv"))
message("    Level OOS metrics:")
print(level_metrics[order(series, cat_label),
  .(series, cat_label,
    rmse=round(rmse,1), mape=round(mape,2),
    r2_oos=round(r2_oos,3), n)])

# ════════════════════════════════════════════════════════════
# 9. FUTURE FORECAST  (last data → 2030 Q4)
# ════════════════════════════════════════════════════════════
message("\n[6] ARIMAX future forecast → 2030 Q4...")
message("    Exit vars (merger_rate, liquid_rate, acquisition_rate)")
message("    set to ZERO for future quarters (unknown future values).")

future_rows <- list()

for (key in names(all_fits)) {
  obj     <- all_fits[[key]]
  fit     <- obj$fit
  dv      <- obj$dep_var
  cat_lbl <- obj$cat_label
  sv      <- obj$sig_vars

  if (is.null(fit) || !inherits(fit, c("ARIMA","Arima"))) next

  last_obs <- max(qtrly[cat_label==cat_lbl, date], na.rm=TRUE)
  fc_qtrs  <- seq(last_obs+0.25, FC_END, by=0.25)
  fc_qtrs  <- fc_qtrs[fc_qtrs > last_obs & fc_qtrs <= FC_END]
  if (length(fc_qtrs) == 0L) next

  h_steps <- length(fc_qtrs)

  # Training medians for macro imputation
  cat_train  <- qtrly[cat_label==cat_lbl & date <= obj$date_end]
  num_cols_t <- names(cat_train)[vapply(names(cat_train),
                 function(cn) is.numeric(cat_train[[cn]]), logical(1))]
  train_meds <- setNames(
    lapply(num_cols_t, function(cn) median(cat_train[[cn]], na.rm=TRUE)),
    num_cols_t)

  # Build h-step xreg matrix
  # Exit vars → 0 for all future quarters (unknown)
  # Macro vars → use forward panel from qtrly if available, else training median
  future_xreg <- if (length(sv) > 0L) {
    mat <- matrix(NA_real_, nrow=h_steps, ncol=length(sv),
                  dimnames=list(NULL, sv))
    for (i in seq_along(fc_qtrs)) {
      fq_yqtr <- zoo::as.yearqtr(fc_qtrs[i])
      fwd_row <- qtrly[cat_label==cat_lbl & date==fq_yqtr]
      for (v in sv) {
        # Exit variables always set to zero — future rates unknown
        is_exit_v <- v %in% EXIT_VARS ||
                     grepl(paste(EXIT_VARS, collapse="|"), v, perl=TRUE)
        if (is_exit_v) {
          mat[i, v] <- 0
        } else if (nrow(fwd_row)==1L && v %in% names(fwd_row) &&
                   is.finite(as.numeric(fwd_row[[v]]))) {
          mat[i, v] <- as.numeric(fwd_row[[v]])
        } else if (v %in% names(train_meds) && is.finite(train_meds[[v]])) {
          mat[i, v] <- train_meds[[v]]
        } else {
          mat[i, v] <- 0
        }
      }
    }
    mat
  } else NULL

  fc_out <- tryCatch(
    forecast::forecast(fit, h=h_steps, xreg=future_xreg, level=95),
    error=function(e) NULL)
  if (is.null(fc_out)) {
    message(sprintf("    [WARN] Future forecast failed: %s", key))
    next
  }

  fc_mean <- pmax(pmin(as.numeric(fc_out$mean),   YOY_CAP), -YOY_CAP)
  fc_lo   <- pmax(as.numeric(fc_out$lower[,1L]), -YOY_CAP*1.5)
  fc_hi   <- pmin(as.numeric(fc_out$upper[,1L]),  YOY_CAP*1.5)

  # Level anchors
  count_col  <- if (dv=="yoy_ficu_pct") "ficu_count" else "fiscu_count"
  last_level <- tail(qtrly[cat_label==cat_lbl & !is.na(get(count_col)),
                            get(count_col)], 1L)
  if (length(last_level) == 0L) last_level <- NA_real_

  running_level <- last_level

  for (i in seq_along(fc_qtrs)) {
    fq_yqtr <- zoo::as.yearqtr(fc_qtrs[i])
    pred_lv  <- yoy_to_level(fc_mean[i], running_level)
    pred_lo  <- yoy_to_level(fc_lo[i],   running_level)
    pred_hi  <- yoy_to_level(fc_hi[i],   running_level)

    future_rows[[length(future_rows)+1L]] <- data.table(
      dep_var     = dv,
      cat_label   = cat_lbl,
      series      = count_col,
      date        = fq_yqtr,
      pred_yoy    = fc_mean[i],
      lo95_yoy    = fc_lo[i],
      hi95_yoy    = fc_hi[i],
      pred_level  = pred_lv,
      lo95_level  = pred_lo,
      hi95_level  = pred_hi,
      actual_level= NA_real_,
      method_used = "ARIMAX_FUTURE"
    )
    if (!is.na(pred_lv)) running_level <- pred_lv
  }
}

future_fc <- rbindlist(future_rows, fill=TRUE)
future_fc[, date := zoo::as.yearqtr(as.numeric(date))]
fwrite(future_fc, file.path(RESULT_DIR, "future_forecast_3a.csv"))
message(sprintf("    Future forecast rows: %d", nrow(future_fc)))

# ════════════════════════════════════════════════════════════
# 10. PLOTS
# ════════════════════════════════════════════════════════════
message("\n[7] Generating plots...")

# Common theme
theme_cu <- theme_bw(base_size=11) +
  theme(strip.background=element_rect(fill="grey92"),
        legend.position="bottom",
        plot.title=element_text(face="bold", size=13),
        plot.subtitle=element_text(size=10, colour="grey40"))

# ── P1: Actual vs Predicted (OOS rolling window) ─────────────
for (dv in names(DEP_VARS)) {
  fc_dv <- forecasts_all[dep_var==dv & !is.na(actual) & !is.na(pred_final)]
  if (nrow(fc_dv) == 0) next
  fc_dv[, date_num := as.numeric(date)]

  p <- ggplot(fc_dv, aes(x=date_num)) +
    geom_ribbon(aes(ymin=pred_lo95, ymax=pred_hi95), fill="steelblue", alpha=0.2) +
    geom_line(aes(y=actual,     colour="Actual"),    linewidth=0.9) +
    geom_line(aes(y=pred_final, colour="Predicted"), linewidth=0.8, linetype="dashed") +
    geom_hline(yintercept=0, linetype="dotted", colour="grey50") +
    facet_wrap(~cat_label, scales="free_y", ncol=3) +
    scale_colour_manual(values=c("Actual"="black","Predicted"="steelblue"),
                        name="") +
    scale_x_continuous(breaks=pretty(fc_dv$date_num, n=5),
                       labels=function(x) as.character(zoo::as.yearqtr(x))) +
    labs(title=sprintf("Actual vs Predicted — %s", DEP_VARS[[dv]]$label),
         subtitle="Shaded: 95% CI  |  ARIMAX with TSCV variable selection",
         x="Quarter", y="YoY % Change") +
    theme_cu
  save_plot(p, sprintf("P1_%s_actual_vs_pred.pdf", DEP_VARS[[dv]]$short), w=14, h=9)
}

# ── P2: OOS Metrics bar chart ─────────────────────────────────
if (nrow(metrics_all) > 0) {
  met_long <- melt(metrics_all[, .(dep_var, cat_label, rmse, r2_oos)],
                   id.vars=c("dep_var","cat_label"),
                   variable.name="metric", value.name="value")
  met_long[, dep_label := DEP_VARS[[dep_var]]$label, by=dep_var]

  p <- ggplot(met_long, aes(x=cat_label, y=value, fill=cat_label)) +
    geom_col(show.legend=FALSE) +
    geom_text(aes(label=round(value,3)), vjust=-0.3, size=3) +
    facet_grid(metric ~ dep_label, scales="free_y") +
    scale_fill_brewer(palette="Set2") +
    labs(title="OOS Metrics by Model", x="Asset Category", y="Value") +
    theme_cu + theme(axis.text.x=element_text(angle=35, hjust=1))
  save_plot(p, "P2_oos_metrics.pdf", w=14, h=8)
}

# ── P3: Coefficient importance heatmap ───────────────────────
if (nrow(coefs_all) > 0) {
  # Last window coefficients for each model
  last_coefs <- coefs_all[selected==TRUE,
    .SD[which.max(date)],
    by=.(dep_var, cat_label, variable)]
  last_coefs[, dep_label := DEP_VARS[[dep_var]]$label, by=dep_var]

  if (nrow(last_coefs) > 0) {
    p <- ggplot(last_coefs[p_value < SIG_LEVEL],
                aes(x=cat_label, y=variable, fill=estimate)) +
      geom_tile(colour="white", linewidth=0.4) +
      geom_text(aes(label=sprintf("%.2f", estimate)), size=2.8) +
      facet_wrap(~dep_label, ncol=2) +
      scale_fill_gradient2(low="firebrick3", mid="white", high="steelblue4",
                           midpoint=0, name="Coef") +
      labs(title="Significant ARIMAX Coefficients — Final Window",
           subtitle=sprintf("Shown: p < %.2f, TSCV-selected variables", SIG_LEVEL),
           x="Asset Category", y="Variable") +
      theme_cu + theme(axis.text.x=element_text(angle=35, hjust=1),
                       axis.text.y=element_text(size=8))
    save_plot(p, "P3_coef_heatmap.pdf", w=14, h=10)
  }
}

# ── P4: Residuals over time ───────────────────────────────────
for (dv in names(DEP_VARS)) {
  fc_dv <- forecasts_all[dep_var==dv & !is.na(actual) & !is.na(pred_final)]
  if (nrow(fc_dv) == 0) next
  fc_dv[, resid := actual - pred_final]
  fc_dv[, date_num := as.numeric(date)]

  p <- ggplot(fc_dv, aes(x=date_num, y=resid)) +
    geom_hline(yintercept=0, colour="grey50", linetype="dashed") +
    geom_point(colour="steelblue", alpha=0.7, size=1.8) +
    geom_smooth(method="loess", se=FALSE, colour="firebrick", linewidth=0.8) +
    facet_wrap(~cat_label, scales="free_y", ncol=3) +
    scale_x_continuous(breaks=pretty(fc_dv$date_num, n=4),
                       labels=function(x) as.character(zoo::as.yearqtr(x))) +
    labs(title=sprintf("OOS Residuals — %s", DEP_VARS[[dv]]$label),
         subtitle="Red line: loess trend (should be flat near zero)",
         x="Quarter", y="Residual (Actual − Predicted)") +
    theme_cu
  save_plot(p, sprintf("P4_%s_residuals.pdf", DEP_VARS[[dv]]$short), w=14, h=9)
}

# ── P5: Level-space forecast (OOS eval period) ───────────────
for (ser in c("ficu_count","fiscu_count")) {
  lv_ser <- level_fc[series==ser & !is.na(actual_level) & !is.na(pred_level)]
  if (nrow(lv_ser) == 0) next
  lv_ser[, date_num := as.numeric(date)]

  p <- ggplot(lv_ser, aes(x=date_num)) +
    geom_ribbon(aes(ymin=lo95_level, ymax=hi95_level), fill="steelblue", alpha=0.2) +
    geom_line(aes(y=actual_level, colour="Actual"), linewidth=0.9) +
    geom_line(aes(y=pred_level,   colour="Predicted"), linewidth=0.8, linetype="dashed") +
    facet_wrap(~cat_label, scales="free_y", ncol=3) +
    scale_colour_manual(values=c("Actual"="black","Predicted"="steelblue"), name="") +
    scale_x_continuous(breaks=pretty(lv_ser$date_num, n=4),
                       labels=function(x) as.character(zoo::as.yearqtr(x))) +
    scale_y_continuous(labels=comma) +
    labs(title=sprintf("Level Forecast — %s", toupper(gsub("_count","",ser))),
         subtitle="Back-transformed from YoY%  |  95% CI shaded",
         x="Quarter", y="Count") +
    theme_cu
  save_plot(p, sprintf("P5_%s_level.pdf", ser), w=14, h=9)
}

# ── P6: Future forecast (2025→2030) with historical context ──
for (ser in c("ficu_count","fiscu_count")) {
  dv_ser  <- if (ser=="ficu_count") "yoy_ficu_pct" else "yoy_fiscu_pct"
  fut_ser <- future_fc[series==ser]
  if (nrow(fut_ser) == 0) next

  # Historical level data
  hist_lv <- qtrly[, .(cat_label, date, lv=get(ser))]
  hist_lv[, date_num := as.numeric(date)]
  hist_lv <- hist_lv[!is.na(lv) & date >= zoo::as.yearqtr("2010 Q1")]

  fut_ser[, date_num := as.numeric(date)]

  # Combined plot per category
  p <- ggplot() +
    geom_line(data=hist_lv, aes(x=date_num, y=lv, colour="Historical"),
              linewidth=0.9) +
    geom_ribbon(data=fut_ser,
                aes(x=date_num, ymin=lo95_level, ymax=hi95_level),
                fill="steelblue", alpha=0.25) +
    geom_line(data=fut_ser,
              aes(x=date_num, y=pred_level, colour="Forecast"),
              linewidth=1.0, linetype="dashed") +
    facet_wrap(~cat_label, scales="free_y", ncol=3) +
    scale_colour_manual(values=c("Historical"="black","Forecast"="steelblue"),
                        name="") +
    scale_x_continuous(breaks=pretty(c(hist_lv$date_num, fut_ser$date_num), n=6),
                       labels=function(x) as.character(zoo::as.yearqtr(x))) +
    scale_y_continuous(labels=comma) +
    geom_vline(xintercept=as.numeric(max(hist_lv$date)), linetype="dotted",
               colour="grey40") +
    labs(title=sprintf("Future Forecast 2025–2030 — %s",
                       toupper(gsub("_count","",ser))),
         subtitle="Exit vars set to zero  |  Macro from FRB Baseline  |  95% CI shaded",
         x="Quarter", y="Count") +
    theme_cu
  save_plot(p, sprintf("P6_%s_future.pdf", ser), w=16, h=10)
}

# ── P7: System totals (sum across all categories) ────────────
for (ser in c("ficu_count","fiscu_count")) {
  fut_tot <- future_fc[series==ser,
    .(pred_level=sum(pred_level,  na.rm=TRUE),
      lo95_level=sum(lo95_level,  na.rm=TRUE),
      hi95_level=sum(hi95_level,  na.rm=TRUE)),
    by=date]
  if (nrow(fut_tot)==0) next
  hist_tot <- qtrly[date >= zoo::as.yearqtr("2010 Q1"),
    .(lv=sum(get(ser), na.rm=TRUE)), by=date]
  fut_tot[,  date_num := as.numeric(date)]
  hist_tot[, date_num := as.numeric(date)]

  p <- ggplot() +
    geom_line(data=hist_tot, aes(x=date_num, y=lv, colour="Historical"),
              linewidth=1.1) +
    geom_ribbon(data=fut_tot,
                aes(x=date_num, ymin=lo95_level, ymax=hi95_level),
                fill="steelblue", alpha=0.25) +
    geom_line(data=fut_tot,
              aes(x=date_num, y=pred_level, colour="Forecast"),
              linewidth=1.1, linetype="dashed") +
    scale_colour_manual(values=c("Historical"="black","Forecast"="steelblue"),
                        name="") +
    scale_x_continuous(breaks=pretty(c(hist_tot$date_num,fut_tot$date_num),n=6),
                       labels=function(x) as.character(zoo::as.yearqtr(x))) +
    scale_y_continuous(labels=comma) +
    geom_vline(xintercept=as.numeric(max(hist_tot$date)), linetype="dotted",
               colour="grey40") +
    labs(title=sprintf("System Total Forecast 2025–2030 — %s",
                       toupper(gsub("_count","",ser))),
         subtitle="Sum across all asset-size categories  |  95% CI shaded",
         x="Quarter", y="Total Count") +
    theme_cu
  save_plot(p, sprintf("P7_%s_system_total.pdf", ser), w=12, h=7)
}

# ── P8: TSCV RMSE heatmap ─────────────────────────────────────
if (nrow(forecasts_all) > 0 && "tscv_rmse" %in% names(forecasts_all)) {
  rmse_sum <- forecasts_all[!is.na(tscv_rmse),
    .(med_tscv_rmse=median(tscv_rmse, na.rm=TRUE)), by=.(dep_var, cat_label)]
  rmse_sum[, dep_label := DEP_VARS[[dep_var]]$label, by=dep_var]

  p <- ggplot(rmse_sum, aes(x=cat_label, y=dep_label, fill=med_tscv_rmse)) +
    geom_tile(colour="white", linewidth=0.5) +
    geom_text(aes(label=round(med_tscv_rmse,3)), size=3.5) +
    scale_fill_gradient(low="white", high="firebrick3", name="TSCV RMSE") +
    labs(title="Median TSCV RMSE by Model",
         subtitle="Lower = better out-of-sample fit during variable selection",
         x="Asset Category", y="Target") +
    theme_cu + theme(axis.text.x=element_text(angle=35, hjust=1))
  save_plot(p, "P8_tscv_rmse_heatmap.pdf", w=12, h=5)
}

# ════════════════════════════════════════════════════════════
# P9 / P10: POLICY-DECISION CHARTS  (publication quality)
#
# Equivalent to P16/P17 from Part 3 v4:
#   • Full history from 2005 Q1 — 7 categories, solid lines
#   • OOS rolling-window level forecast — dashed, same colour
#   • Future forecast 2025 → 2030 — dashed, continued
#   • Amber shading over full forecast region
#   • Dotted vertical line at OOS forecast start
#   • 1-year and 3-year horizon markers
#   • Endpoint label (final forecast value per category)
#   • Clean theme suitable for PDF / senior leadership
# ════════════════════════════════════════════════════════════
message("\n[8] Policy-decision charts (P9 FICU, P10 FISCU)...")

CAT_COLOURS <- c(
  "1_Less_10M"  = "#1f77b4", "2_10M_50M"   = "#ff7f0e",
  "3_50M_100M"  = "#2ca02c", "4_100M_500M" = "#d62728",
  "5_500M_1B"   = "#9467bd", "6_1B_10B"    = "#8c564b",
  "7_10B_Plus"  = "#e377c2"
)
LTY_CYCLE <- c("solid","dashed","dotdash","longdash",
                "twodash","solid","dashed")
names(LTY_CYCLE) <- names(CAT_COLOURS)

policy_theme_3a <- function() {
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_blank(),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    plot.caption     = element_text(colour = "grey55", size = 9, hjust = 0),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    legend.key.width = unit(1.8, "cm"),
    panel.grid.minor = element_blank()
  )
}

make_policy_chart_3a <- function(count_col, title_text, stem) {

  # Historical levels from 2005 Q1
  hist_dt <- qtrly[!is.na(get(count_col)) &
                     date >= zoo::as.yearqtr("2005 Q1"),
                   .(date, cat_label, value = get(count_col))]
  hist_dt[, cat_label := as.character(cat_label)]

  # OOS rolling-window forecast (level space)
  fc_oos <- level_fc[series == count_col & !is.na(pred_level),
                     .(date, cat_label, value = pred_level)]
  fc_oos[, cat_label := as.character(cat_label)]

  # Future forecast 2025->2030
  fc_fut <- future_fc[series == count_col & !is.na(pred_level),
                      .(date, cat_label, value = pred_level)]
  fc_fut[, cat_label := as.character(cat_label)]

  fc_all <- rbindlist(list(fc_oos, fc_fut), fill = TRUE)
  fc_all <- unique(fc_all, by = c("date", "cat_label"))

  if (nrow(fc_all) == 0) {
    message(sprintf("    [SKIP] %s - no forecast data", stem))
    return(invisible(NULL))
  }

  # Build plain data.frames to avoid yearqtr class conflicts on rbind
  to_df <- function(dt, seg) {
    data.frame(
      date_num  = as.numeric(dt$date),
      cat_label = as.character(dt$cat_label),
      value     = as.numeric(dt$value),
      segment   = seg,
      stringsAsFactors = FALSE
    )
  }
  all_df      <- rbind(to_df(hist_dt, "Historical"),
                       to_df(fc_all,  "Forecast"))
  all_df      <- all_df[!is.na(all_df$value) & !is.na(all_df$cat_label), ]
  all_df$date <- as.Date(zoo::as.yearqtr(all_df$date_num))

  fc_start_date <- as.Date(min(fc_all$date, na.rm = TRUE))
  fc_end_date   <- as.Date(max(all_df$date, na.rm = TRUE)) + 90
  h1yr          <- fc_start_date + 365
  h3yr          <- fc_start_date + 365 * 3

  # Endpoint labels — last forecast point per category
  last_pts <- as.data.table(all_df[all_df$segment == "Forecast", ])
  last_pts <- last_pts[, .SD[which.max(date)], by = cat_label]

  p <- ggplot(all_df, aes(x = date, y = value, colour = cat_label)) +

    # Amber forecast region
    annotate("rect",
             xmin = fc_start_date, xmax = fc_end_date,
             ymin = -Inf, ymax = Inf,
             fill = "#FFF3CD", alpha = 0.55) +

    # Forecast-start line
    geom_vline(xintercept = fc_start_date,
               linetype = "dotted", colour = "grey45", linewidth = 0.7) +

    # 1yr / 3yr horizon markers
    geom_vline(xintercept = h1yr,
               linetype = "dotdash", colour = "grey65", linewidth = 0.45) +
    geom_vline(xintercept = h3yr,
               linetype = "dotdash", colour = "grey65", linewidth = 0.45) +
    annotate("text", x = h1yr + 10, y = Inf, label = "1yr",
             colour = "grey50", size = 3.2, vjust = 1.6, hjust = 0) +
    annotate("text", x = h3yr + 10, y = Inf, label = "3yr",
             colour = "grey50", size = 3.2, vjust = 1.6, hjust = 0) +

    # Historical — solid lines
    geom_line(data = all_df[all_df$segment == "Historical", ],
              aes(linetype = cat_label),
              linewidth = 0.85, alpha = 0.9) +

    # Forecast — dashed lines (heavier)
    geom_line(data = all_df[all_df$segment == "Forecast", ],
              aes(linetype = cat_label),
              linewidth = 0.95, alpha = 1.0) +

    # Endpoint dot
    geom_point(data = last_pts,
               aes(x = date, y = value),
               size = 2.5, shape = 21, fill = "white", stroke = 1.2) +

    # Endpoint value annotation
    geom_text(data = last_pts,
              aes(x = date, y = value,
                  label = scales::comma(round(value))),
              hjust = -0.15, size = 2.9, fontface = "bold",
              show.legend = FALSE) +

    scale_colour_manual(values = CAT_COLOURS, name = "Asset Category") +
    scale_linetype_manual(values = LTY_CYCLE,  name = "Asset Category") +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years",
                 expand = expansion(mult = c(0.02, 0.13))) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = title_text,
      subtitle = sprintf(
        "Historical (solid) | ARIMAX-TSCV Forecast (dashed) | Amber = forecast period | Start: %s",
        as.character(zoo::as.yearqtr(fc_start_date))
      ),
      caption  = paste(
        "Model: ARIMAX with TSCV backward elimination.",
        "Features: FRB Baseline 2026 macro + merger/liquidation/acquisition rates.",
        "\nExit vars set to zero in future horizon. 7 NCUA asset-size categories.",
        "Source: NCUA Call Reports."
      ),
      x = NULL, y = "Count"
    ) +
    policy_theme_3a() +
    guides(colour   = guide_legend(nrow = 2, byrow = TRUE),
           linetype = guide_legend(nrow = 2, byrow = TRUE))

  save_plot(p, stem, w = 14, h = 7)
  invisible(p)
}

# P9: FICU policy chart
if ("ficu_count" %in% names(qtrly)) {
  message("    P9: FICU Count policy chart...")
  make_policy_chart_3a(
    count_col  = "ficu_count",
    title_text = "FICU Count — Historical Trend & ARIMAX Forecast by Asset Category",
    stem       = "P9_policy_ficu_count.pdf"
  )
} else {
  message("    P9: skipped - ficu_count not in data")
}

# P10: FISCU policy chart
if ("fiscu_count" %in% names(qtrly)) {
  message("    P10: FISCU Count policy chart...")
  make_policy_chart_3a(
    count_col  = "fiscu_count",
    title_text = "FISCU Count — Historical Trend & ARIMAX Forecast by Asset Category",
    stem       = "P10_policy_fiscu_count.pdf"
  )
} else {
  message("    P10: skipped - fiscu_count not in data")
}

message("    Policy charts complete (P9, P10)")

message("\n[9] Done. All outputs in results_3a/ and plots_3a/")
message(sprintf("    %d models  |  %d forecast rows  |  %d future rows",
                length(all_fits), nrow(forecasts_all), nrow(future_fc)))
message("    Plots: P1-P8 (diagnostics) + P9/P10 (policy charts)")
