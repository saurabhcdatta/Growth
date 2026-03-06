############################################################
# PART 4 v1.0 — ENSEMBLE ML FORECASTING
#   Cross-model ensemble covering all targets from 3a / 3b / 3c:
#
#   3a: yoy_fcu_pct, yoy_fiscu_pct          (7 cats each = 14 models)
#   3b: yoy_fcu_assets_pct, yoy_fiscu_assets_pct (7 cats each = 14 models)
#   3c: yoy_corp_cu_assets_pct              (1 aggregate series)
#
# ENSEMBLE MEMBERS
#   1. XGBoost        — gradient boosting trees
#   2. Random Forest  — bagged trees (ranger)
#   3. LASSO / Ridge  — regularised linear (glmnet)
#   4. ARIMAX-TSCV    — the existing Part 3a/3b/3c models (as a member)
#
# STACKING
#   A meta-learner (lasso) combines the four base-model OOS
#   predictions into a final ensemble forecast.
#
# TOP-10 MACRO FACTORS
#   For every (target × category) cell:
#     • XGBoost importance (gain)
#     • Random Forest importance (mean decrease impurity)
#     • LASSO |coefficient| ranking
#     • Ensemble-weighted average importance
#   Plotted as bar charts, heatmaps, and a multi-panel summary PDF.
#
# PARALLEL PROCESSING (Windows)
#   Uses the `doParallel` / `parallel` backend with
#   makeCluster(detectCores() - 1) -- safe for Windows.
#   Each (target × category × model) cell runs in parallel.
#   All results are merged back to the master node.
#
# OUTPUTS
#   results_4/     — CSVs: ensemble forecasts, metrics, importances
#   plots_4/       — PDFs: importance plots, ensemble diagnostics
#
# READS
#   modeling_panel_v5.rds          — main NCUA panel (3a/3b)
#   Corporate_Credit_Unions.xlsx   — corporate CU data (3c)
############################################################

# ════════════════════════════════════════════════════════════
# 0. PACKAGES
# ════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(glmnet)
  library(forecast)
  library(xgboost)
  library(ranger)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(tictoc)
  library(stringr)
  # Parallel
  library(parallel)
  library(doParallel)
  library(foreach)
  # Excel read (3c)
  library(readxl)
})

set.seed(42)
options(scipen = 999)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ════════════════════════════════════════════════════════════
# 1. CONFIG
# ════════════════════════════════════════════════════════════
DATA_DIR   <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR   <- "plots_4_v1"
RESULT_DIR <- "results_4_v1"

DEBUG_MODE   <- TRUE    # FALSE for full production run
DEBUG_ROLL_Q <- 6       # test quarters in debug mode

TRAIN_END    <- zoo::as.yearqtr("2021 Q1")   # first OOS quarter (3a/3b)
TRAIN_END_3C <- zoo::as.yearqtr("2022 Q1")   # first OOS quarter (3c)
FC_END       <- zoo::as.yearqtr("2030 Q4")   # forecast horizon
YOY_CAP      <- 15.0                          # ±% clamp

MAX_XREG_VARS <- 10L   # LASSO top-N fed to ARIMAX member
SIG_LEVEL     <- 0.10  # p-value for ARIMAX member
TSCV_MIN_TRAIN <- 12L
TSCV_H         <- 1L

# Parallel: leave 1 core for the OS
N_CORES <- max(1L, parallel::detectCores() - 1L)
message(sprintf("[CONFIG] Parallel workers: %d / %d cores", N_CORES, parallel::detectCores()))

setwd(DATA_DIR)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)

# ════════════════════════════════════════════════════════════
# 2. LOAD DATA
# ════════════════════════════════════════════════════════════
message("\n[1] Loading data...")

# ── 2a. NCUA panel (3a & 3b) ─────────────────────────────────
if (!file.exists("modeling_panel_v5.rds"))
  stop("modeling_panel_v5.rds not found.")

qtrly <- readRDS("modeling_panel_v5.rds")
setDT(qtrly)

# Strip haven labels
if (requireNamespace("haven", quietly = TRUE)) {
  qtrly <- as.data.table(haven::zap_labels(qtrly))
}
for (cn in names(qtrly)) {
  for (attr_nm in c("label","labels","format.stata","format.spss"))
    attr(qtrly[[cn]], attr_nm) <- NULL
}

CAT_MAP <- c("1"="1_Less_10M","2"="2_10M_50M","3"="3_50M_100M",
             "4"="4_100M_500M","5"="5_500M_1B","6"="6_1B_10B","7"="7_10B_Plus")
if (!"cat_label" %in% names(qtrly))
  qtrly[, cat_label := CAT_MAP[as.character(categories)]]

setorderv(qtrly, c("categories","date"))
all_quarters <- sort(unique(qtrly$date))
cats         <- sort(unique(qtrly$cat_label))
message(sprintf("  NCUA panel: %d rows × %d cols | %d quarters | %d cats",
                nrow(qtrly), ncol(qtrly), length(all_quarters), length(cats)))

# ── 2b. Corporate CU data (3c) ────────────────────────────────
CORP_XLSX <- file.path(DATA_DIR, "Corporate_Credit_Unions.xlsx")
corp_panel <- NULL
if (file.exists(CORP_XLSX)) {
  corp_raw <- as.data.table(read_excel(CORP_XLSX))
  date_col  <- grep("date|cycle", names(corp_raw), ignore.case=TRUE, value=TRUE)[1L]
  asset_col <- grep("total.*asset|ccu.*asset", names(corp_raw), ignore.case=TRUE, value=TRUE)[1L]
  if (is.na(date_col))  date_col  <- names(corp_raw)[1L]
  if (is.na(asset_col)) stop("Cannot locate ccu_total_assets column.")

  raw_dates <- corp_raw[[date_col]]
  if (inherits(raw_dates, c("POSIXct","POSIXlt","Date"))) {
    corp_raw[, date_raw := as.Date(raw_dates)]
  } else if (is.numeric(raw_dates)) {
    corp_raw[, date_raw := as.Date(raw_dates, origin="1899-12-30")]
  } else {
    raw_chr <- trimws(as.character(raw_dates))
    if (all(grepl("^[0-9]+\\.?[0-9]*$", head(raw_chr[!is.na(raw_chr)], 10))))
      corp_raw[, date_raw := as.Date(as.numeric(raw_chr), origin="1899-12-30")]
    else
      corp_raw[, date_raw := as.Date(raw_chr, format="%m/%d/%Y")]
  }
  corp_raw[, corp_cu_assets := as.numeric(get(asset_col))]
  corp_raw <- corp_raw[!is.na(date_raw) & !is.na(corp_cu_assets)]
  setorderv(corp_raw, "date_raw")
  corp_raw[, month_num := as.integer(format(date_raw, "%m"))]
  corp_qtr <- corp_raw[month_num %in% c(3L,6L,9L,12L),
                        .(date=zoo::as.yearqtr(date_raw), corp_cu_assets)]
  corp_qtr <- corp_qtr[, .(corp_cu_assets=tail(corp_cu_assets,1L)), by=date]
  setorderv(corp_qtr, "date")
  corp_qtr[, corp_cu_assets_lag4 := shift(corp_cu_assets, 4L, type="lag")]
  corp_qtr[, yoy_corp_cu_assets_pct := 100*(corp_cu_assets-corp_cu_assets_lag4)/corp_cu_assets_lag4]
  corp_qtr[, cat_label := "Corporate"]

  # Merge with macro panel (pick one category row per date for macro vars)
  macro_cols <- names(qtrly)[grepl("^macro_", names(qtrly))]
  macro_only <- unique(qtrly[, c("date", macro_cols), with=FALSE])
  corp_panel <- merge(corp_qtr, macro_only, by="date", all.x=TRUE)
  setDT(corp_panel)
  message(sprintf("  Corp CU panel: %d rows | YoY non-NA: %d",
                  nrow(corp_panel), sum(!is.na(corp_panel$yoy_corp_cu_assets_pct))))
} else {
  message("  [WARN] Corporate_Credit_Unions.xlsx not found — 3c models will be skipped.")
}

# ════════════════════════════════════════════════════════════
# 3. FEATURE DEFINITIONS  (shared across all models)
# ════════════════════════════════════════════════════════════
message("\n[2] Building feature lists...")

CURATED_MACRO <- c(
  "macro_fedfunds","macro_fedfunds_chg","macro_fedfunds_cycle",
  "macro_gs3m","macro_gs10","macro_gs30",
  "macro_yield_curve","macro_yield_curve_inv","macro_spread_2s10s",
  "macro_mortgage30","macro_real_rate",
  "macro_baa_spread","macro_credit_tightness",
  "macro_unrate","macro_disp_income","macro_savings_rate",
  "macro_gdp_real","macro_cons_confidence",
  "macro_cpi","macro_core_cpi","macro_cpi_yoy","macro_core_cpi_yoy",
  "macro_housing_permits","macro_hpi_fed",
  "macro_consumer_bankrupt","macro_cons_loan_delinq",
  "macro_fwd_1y1y","macro_fwd_1y5y",
  "macro_fomc_regime","macro_hike_run"
)
EXIT_VARS_RAW <- c("merger_rate","liquid_rate","acquisition_rate","exit_rate","exit_roll4")
EXIT_VARS     <- paste0(EXIT_VARS_RAW, "_lag1")

CURATED_BASE <- gsub("^macro_","", CURATED_MACRO)
base_pat     <- paste(CURATED_BASE, collapse="|")
STAT_PAT     <- paste(
  paste0("^macro_(",base_pat,")$"),
  paste0("^macro_yoy_(",base_pat,")$"),
  paste0("^macro_qoq_(",base_pat,")$"),
  paste0("^macro_(",base_pat,")_lag[0-9]+$"),
  paste0("^macro_(",base_pat,")_rmean[0-9]+$"),
  paste0("^macro_(",base_pat,")_rsd[0-9]+$"),
  paste0("^macro_(",base_pat,")_chg$"),
  paste0("^macro_(",base_pat,")_cyc$"),
  paste0("^macro_yoy_(",base_pat,")_accel$"),
  sep="|")

HARD_EXCL <- c("yoy_fcu_pct","yoy_fiscu_pct","yoy_fcu_assets_pct","yoy_fiscu_assets_pct",
               "yoy_corp_cu_assets_pct","yoy_assets_pct",
               "qoq_fcu_pct","qoq_fiscu_pct","fcu_count","fiscu_count",
               "fcu_count_lag4","fiscu_count_lag4","ld_fcu","ld_fiscu",
               "net_entry_rate","net_entry_rate_fiscu","categories","n_active","n_total",
               "q1","q2","q3","q4")

build_feats <- function(panel_dt, include_exit = TRUE) {
  all_num <- names(panel_dt)[vapply(panel_dt, is.numeric, logical(1))]
  macro_f <- grep(STAT_PAT, all_num, value=TRUE, perl=TRUE)
  macro_f <- macro_f[macro_f %in% names(panel_dt)]
  macro_f <- macro_f[vapply(macro_f, function(cn) {
    v <- panel_dt[[cn]]; length(unique(v[!is.na(v)])) > 3L
  }, logical(1))]
  exit_f <- if (include_exit) intersect(EXIT_VARS, all_num) else character(0)
  feats <- unique(c(macro_f, exit_f))
  setdiff(feats, HARD_EXCL)
}

# ════════════════════════════════════════════════════════════
# 4. SHARED HELPER FUNCTIONS
# ════════════════════════════════════════════════════════════
message("\n[3] Loading helpers...")

winsorise <- function(x, p=0.01) {
  lo <- quantile(x, p, na.rm=TRUE); hi <- quantile(x, 1-p, na.rm=TRUE)
  pmax(pmin(x, hi), lo)
}

reg_metrics <- function(actual, pred) {
  ok <- !is.na(actual) & !is.na(pred); a <- actual[ok]; p <- pred[ok]; n <- sum(ok)
  if (n < 2) return(list(rmse=NA,mae=NA,mape=NA,r2_oos=NA,n=n))
  rmse <- sqrt(mean((a-p)^2)); mae <- mean(abs(a-p))
  mape <- mean(abs((a-p)/a)*100, na.rm=TRUE)
  ss_r <- sum((a-p)^2); ss_t <- sum((a-mean(a))^2)
  r2   <- if (ss_t>0) 1-ss_r/ss_t else NA_real_
  list(rmse=rmse,mae=mae,mape=mape,r2_oos=r2,n=n)
}

prep_X <- function(dt, feats, corr_cut=0.92, min_nonmiss=0.70) {
  cols <- intersect(feats, names(dt))
  if (!length(cols)) return(NULL)
  # Force all columns to plain numeric (guards against haven_labelled)
  dt_c <- dt[, cols, with=FALSE]
  dt_c <- dt_c[, lapply(.SD, function(x) as.numeric(x))]
  mat <- as.matrix(dt_c); storage.mode(mat) <- "double"
  ok_m <- apply(mat, 2, function(x) mean(!is.na(x)) >= min_nonmiss)
  ok_v <- apply(mat, 2, function(x) var(x, na.rm=TRUE) > 1e-10)
  mat  <- mat[, ok_m & ok_v, drop=FALSE]
  if (!ncol(mat)) return(NULL)
  for (j in seq_len(ncol(mat))) {
    na_j <- is.na(mat[,j])
    if (any(na_j)) mat[na_j, j] <- median(mat[,j], na.rm=TRUE)
  }
  if (ncol(mat) > 2) {
    cr <- cor(mat, use="pairwise.complete.obs"); cr[is.na(cr)] <- 0
    keep <- rep(TRUE, ncol(mat))
    for (i in seq_len(ncol(mat)-1)) {
      if (!keep[i]) next
      for (j in seq(i+1, ncol(mat)))
        if (keep[j] && abs(cr[i,j]) > corr_cut) keep[j] <- FALSE
    }
    mat <- mat[, keep, drop=FALSE]
  }
  if (!ncol(mat)) return(NULL); mat
}

# ────────────────────────────────────────────────────────────
# XGBoost: fit + importance
# ────────────────────────────────────────────────────────────
fit_xgb <- function(X_tr, y_tr, X_te) {
  dtrain <- xgboost::xgb.DMatrix(data=X_tr, label=y_tr)
  params <- list(
    booster          = "gbtree",
    objective        = "reg:squarederror",
    eta              = 0.05,
    max_depth        = 4L,
    subsample        = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 3L,
    lambda           = 1.0
  )
  # Quick CV to find nrounds (max 200 for speed; 500 in production)
  nrounds_max <- if (DEBUG_MODE) 200L else 500L
  cv <- tryCatch(
    xgboost::xgb.cv(params=params, data=dtrain, nrounds=nrounds_max,
                    nfold=max(2L, min(5L, floor(nrow(X_tr)/4L))),
                    early_stopping_rounds=20L,
                    verbose=0, showsd=FALSE),
    error=function(e) NULL)
  best_n <- if (!is.null(cv)) cv$best_iteration else 50L
  best_n <- max(10L, best_n)

  fit <- xgboost::xgboost(params=params, data=dtrain,
                           nrounds=best_n, verbose=0)
  pred <- predict(fit, xgboost::xgb.DMatrix(data=X_te))
  imp  <- tryCatch(
    xgboost::xgb.importance(feature_names=colnames(X_tr), model=fit),
    error=function(e) data.table())
  list(fit=fit, pred=pred, importance=imp, nrounds=best_n)
}

# ────────────────────────────────────────────────────────────
# Random Forest (ranger): fit + importance
# ────────────────────────────────────────────────────────────
fit_rf <- function(X_tr, y_tr, X_te) {
  n_trees  <- if (DEBUG_MODE) 200L else 500L
  n_feats  <- max(2L, floor(ncol(X_tr) / 3L))
  rf_df    <- as.data.frame(X_tr)
  rf_df$.y <- y_tr
  fit <- tryCatch(
    ranger::ranger(
      formula         = .y ~ .,
      data            = rf_df,
      num.trees       = n_trees,
      mtry            = n_feats,
      importance      = "impurity",
      min.node.size   = 5L,
      seed            = 42L
    ),
    error=function(e) NULL)
  if (is.null(fit)) return(list(fit=NULL, pred=NA_real_, importance=data.table()))
  pred <- predict(fit, data=as.data.frame(X_te))$predictions
  imp  <- data.table(Feature=names(fit$variable.importance),
                     Importance=as.numeric(fit$variable.importance))
  # Normalise to 0-1 (like xgb gain)
  imp[, Gain := Importance / max(Importance, na.rm=TRUE)]
  list(fit=fit, pred=pred, importance=imp)
}

# ────────────────────────────────────────────────────────────
# LASSO (glmnet) + importance as |coef|
# ────────────────────────────────────────────────────────────
fit_lasso <- function(X_tr, y_tr, X_te) {
  nf  <- min(5L, max(3L, floor(nrow(X_tr)/4L)))
  fit <- tryCatch(
    glmnet::cv.glmnet(X_tr, y_tr, alpha=1, nfolds=nf, standardize=TRUE, type.measure="mse"),
    error=function(e) NULL)
  if (is.null(fit)) return(list(fit=NULL, pred=NA_real_, importance=data.table()))
  pred <- as.numeric(predict(fit, newx=X_te, s="lambda.1se"))
  cf   <- as.matrix(coef(fit, s="lambda.1se"))
  imp  <- data.table(Feature=rownames(cf)[-1L], AbsCoef=abs(cf[-1L,1L]))
  imp  <- imp[AbsCoef > 0]
  imp[, Gain := AbsCoef / max(AbsCoef, na.rm=TRUE)]
  list(fit=fit, pred=pred, importance=imp)
}

# ────────────────────────────────────────────────────────────
# ARIMAX-TSCV member (simplified wrapper — same logic as 3a/3b)
# Returns 1-step OOS prediction and feature importance proxy
# ────────────────────────────────────────────────────────────
tscv_rmse_fn <- function(y_ts, xreg_mat, ord, P, D, Q,
                          min_train=TSCV_MIN_TRAIN) {
  n <- length(y_ts); if (n < min_train+2L) return(Inf)
  errs <- numeric(0)
  eval_start <- max(min_train, n-20L)
  for (i in seq(eval_start, n-1L)) {
    y_tr  <- window(y_ts, end=time(y_ts)[i])
    xr_tr <- if (!is.null(xreg_mat)) xreg_mat[seq_len(i),,drop=FALSE] else NULL
    xr_te <- if (!is.null(xreg_mat)) xreg_mat[i+1L,,drop=FALSE]       else NULL
    fit_cv <- tryCatch(
      forecast::Arima(y_tr, order=ord,
                      seasonal=list(order=c(P,D,Q), period=4L),
                      xreg=xr_tr, method="CSS-ML"),
      error=function(e) NULL)
    if (is.null(fit_cv)) next
    fc_cv <- tryCatch(forecast::forecast(fit_cv, h=1L, xreg=xr_te), error=function(e) NULL)
    if (is.null(fc_cv)) next
    act <- as.numeric(y_ts)[i+1L]; prd <- as.numeric(fc_cv$mean)[1L]
    if (!is.na(act) && !is.na(prd)) errs <- c(errs, (act-prd)^2)
  }
  if (!length(errs)) Inf else sqrt(mean(errs))
}

fit_arimax_member <- function(train_dt, test_row, dep_var, feats) {
  y_tr <- winsorise(as.numeric(train_dt[[dep_var]]))
  n    <- sum(!is.na(y_tr))
  if (n < 8L) return(list(pred=NA_real_, sig_vars=character(0), adj_r2=NA_real_))
  tr_d <- sort(train_dt$date)
  sy   <- as.integer(format(zoo::as.yearqtr(min(tr_d)), "%Y"))
  sq   <- as.integer(format(zoo::as.yearqtr(min(tr_d)), "%q"))
  y_ts <- ts(y_tr, frequency=4L, start=c(sy, sq))
  arb  <- tryCatch(
    forecast::auto.arima(y_ts, stepwise=TRUE, approximation=TRUE,
                         max.p=3L, max.q=2L, max.P=1L, max.Q=1L, seasonal=TRUE),
    error=function(e) NULL)
  if (is.null(arb)) return(list(pred=NA_real_, sig_vars=character(0), adj_r2=NA_real_))
  ord  <- forecast::arimaorder(arb)
  p_o  <- ord["p"]; d_o <- ord["d"]; q_o <- ord["q"]
  P_o  <- ord["P"]; D_o <- ord["D"]; Q_o <- ord["Q"]
  has_s <- any(c(P_o,D_o,Q_o)!=0L)

  X_tr <- prep_X(train_dt, feats)
  if (is.null(X_tr) || !ncol(X_tr)) {
    fc0 <- tryCatch(forecast::forecast(arb, h=1L, level=95), error=function(e) NULL)
    pred0 <- if (!is.null(fc0)) as.numeric(fc0$mean) else NA_real_
    return(list(pred=max(min(pred0, YOY_CAP), -YOY_CAP),
                sig_vars=character(0), adj_r2=NA_real_))
  }

  nf   <- min(5L, max(3L, floor(n/4L)))
  cv_f <- tryCatch(glmnet::cv.glmnet(X_tr, y_tr, alpha=1, nfolds=nf,
                                      standardize=TRUE, type.measure="mse"),
                   error=function(e) NULL)
  if (is.null(cv_f)) {
    fc0 <- tryCatch(forecast::forecast(arb, h=1L), error=function(e) NULL)
    pred0 <- if (!is.null(fc0)) as.numeric(fc0$mean) else NA_real_
    return(list(pred=max(min(pred0, YOY_CAP), -YOY_CAP),
                sig_vars=character(0), adj_r2=NA_real_))
  }
  cf    <- as.matrix(coef(cv_f, s="lambda.1se"))
  sel   <- rownames(cf)[cf[,1L]!=0 & rownames(cf)!="(Intercept)"]
  sel   <- intersect(sel, colnames(X_tr))
  prot  <- intersect(EXIT_VARS, colnames(X_tr))
  cands <- unique(c(sel, prot))
  if (length(cands) > MAX_XREG_VARS) {
    lm_abs <- abs(cf[cands,1L])
    top    <- names(sort(lm_abs, decreasing=TRUE))[seq_len(MAX_XREG_VARS)]
    cands  <- unique(c(intersect(prot, cands), top))[seq_len(MAX_XREG_VARS)]
  }

  xr_mat <- if (length(cands)) X_tr[, intersect(cands, colnames(X_tr)), drop=FALSE] else NULL
  best_rmse <- tscv_rmse_fn(y_ts, xr_mat, c(p_o,d_o,q_o), P_o, D_o, Q_o)
  cur_vars  <- cands

  for (iter in seq_len(15L)) {
    if (!length(cur_vars)) break
    best_drop <- NULL; best_nr <- best_rmse
    for (v in cur_vars) {
      tv   <- setdiff(cur_vars, v)
      txr  <- if (length(tv)) X_tr[, intersect(tv, colnames(X_tr)), drop=FALSE] else NULL
      tr_r <- tscv_rmse_fn(y_ts, txr, c(p_o,d_o,q_o), P_o, D_o, Q_o)
      if (isTRUE(tr_r < best_nr)) { best_nr <- tr_r; best_drop <- v }
    }
    if (is.null(best_drop)) break
    cur_vars  <- setdiff(cur_vars, best_drop); best_rmse <- best_nr
  }

  xr_final  <- if (length(cur_vars)) X_tr[, intersect(cur_vars, colnames(X_tr)), drop=FALSE] else NULL

  fit_f <- tryCatch(
    forecast::Arima(y_ts, order=c(p_o,d_o,q_o),
                    seasonal=list(order=if(has_s) c(P_o,D_o,Q_o) else c(0L,0L,0L), period=4L),
                    xreg=xr_final, method="CSS-ML"),
    error=function(e) arb)

  # Build test xreg
  xr_test <- NULL
  if (length(cur_vars) && !is.null(xr_final)) {
    xr_test <- matrix(NA_real_, 1L, length(cur_vars),
                      dimnames=list(NULL, cur_vars))
    td <- as.data.frame(test_row)[1L,,drop=FALSE]
    tr_meds <- setNames(lapply(cur_vars, function(cv)
      median(as.numeric(X_tr[,cv]), na.rm=TRUE)), cur_vars)
    for (v in cur_vars) {
      val <- if (v %in% names(td)) suppressWarnings(as.numeric(td[[v]])) else NA_real_
      xr_test[1L,v] <- if (is.finite(val)) val else tr_meds[[v]] %||% 0
    }
  }

  fc_f <- tryCatch(forecast::forecast(fit_f, h=1L, xreg=xr_test, level=95),
                   error=function(e) tryCatch(forecast::forecast(arb, h=1L), error=function(e2) NULL))
  pred_val <- if (!is.null(fc_f)) as.numeric(fc_f$mean) else NA_real_
  pred_val <- max(min(pred_val %||% 0, YOY_CAP), -YOY_CAP)

  resid_v  <- tryCatch(as.numeric(residuals(fit_f)), error=function(e) NULL)
  adj_r2   <- if (!is.null(resid_v)) {
    n_p <- length(y_tr); k_p <- length(fit_f$coef)
    ss_r <- sum(resid_v^2, na.rm=TRUE)
    ss_t <- sum((y_tr-mean(y_tr,na.rm=TRUE))^2, na.rm=TRUE)
    r2   <- if (ss_t>0) 1-ss_r/ss_t else NA_real_
    if (!is.na(r2) && n_p>k_p+1L) 1-(1-r2)*(n_p-1L)/(n_p-k_p-1L) else r2
  } else NA_real_

  list(pred=pred_val, sig_vars=cur_vars, adj_r2=adj_r2)
}

# ════════════════════════════════════════════════════════════
# 5. CORE CELL WORKER
#    Runs all 4 ensemble members for ONE (dep_var × cat) cell
#    over the rolling OOS window.
#    Returns: forecast rows, importance rows, metrics
# ════════════════════════════════════════════════════════════
run_cell <- function(cell_panel, dep_var, cat_lbl, feats,
                     test_quarters, include_exit) {
  # Build lag1 exit vars if needed
  setorderv(cell_panel, "date")
  for (rv in EXIT_VARS_RAW) {
    ln <- paste0(rv, "_lag1")
    if (!ln %in% names(cell_panel) && rv %in% names(cell_panel)) {
      vals <- cell_panel[[rv]]
      cell_panel[[ln]] <- c(NA_real_, vals[-length(vals)])
    }
  }

  local_feats <- intersect(feats, names(cell_panel))

  fc_rows   <- list()
  imp_rows  <- list()  # importance per quarter

  for (tq in test_quarters) {
    tr_idx <- cell_panel$date < tq
    te_idx <- cell_panel$date == tq
    if (sum(tr_idx) < 8L || sum(te_idx) == 0L) next

    train_dt <- cell_panel[tr_idx]
    test_row  <- cell_panel[te_idx][1L]
    actual    <- test_row[[dep_var]]

    y_tr <- winsorise(as.numeric(train_dt[[dep_var]]))
    y_tr_clean <- y_tr[!is.na(y_tr)]
    if (length(y_tr_clean) < 6L) next

    X_tr <- prep_X(train_dt, local_feats)
    if (is.null(X_tr) || !ncol(X_tr)) next

    # Test feature vector (1 row, same columns as X_tr)
    X_te_df <- as.data.frame(test_row)[1L, colnames(X_tr), drop=FALSE]
    for (cn in colnames(X_tr)) {
      val <- suppressWarnings(as.numeric(X_te_df[[cn]]))
      X_te_df[[cn]] <- if (is.finite(val)) val else median(X_tr[,cn], na.rm=TRUE)
    }
    X_te <- as.matrix(X_te_df)

    y_complete <- y_tr[!is.na(y_tr)]
    # Align X_tr rows with non-NA y
    na_y <- is.na(y_tr)
    X_tr_fit <- X_tr[!na_y, , drop=FALSE]
    y_fit    <- y_tr[!na_y]
    if (length(y_fit) < 6L || nrow(X_tr_fit) < 6L) next

    # ── Member 1: XGBoost ──
    xgb_res <- tryCatch(fit_xgb(X_tr_fit, y_fit, X_te), error=function(e) NULL)
    pred_xgb <- if (!is.null(xgb_res)) xgb_res$pred[1L] else NA_real_
    pred_xgb <- max(min(pred_xgb %||% NA_real_, YOY_CAP), -YOY_CAP)

    # ── Member 2: Random Forest ──
    rf_res  <- tryCatch(fit_rf(X_tr_fit, y_fit, X_te),  error=function(e) NULL)
    pred_rf <- if (!is.null(rf_res))  rf_res$pred[1L]  else NA_real_
    pred_rf <- max(min(pred_rf %||% NA_real_, YOY_CAP), -YOY_CAP)

    # ── Member 3: LASSO ──
    lasso_res  <- tryCatch(fit_lasso(X_tr_fit, y_fit, X_te), error=function(e) NULL)
    pred_lasso <- if (!is.null(lasso_res)) lasso_res$pred[1L] else NA_real_
    pred_lasso <- max(min(pred_lasso %||% NA_real_, YOY_CAP), -YOY_CAP)

    # ── Member 4: ARIMAX ──
    arimax_res  <- tryCatch(
      fit_arimax_member(train_dt, test_row, dep_var, local_feats),
      error=function(e) list(pred=NA_real_, sig_vars=character(0), adj_r2=NA_real_))
    pred_arimax <- arimax_res$pred %||% NA_real_

    # ── Simple average ensemble ──
    preds <- c(pred_xgb, pred_rf, pred_lasso, pred_arimax)
    pred_simple <- if (sum(!is.na(preds)) >= 1L) mean(preds, na.rm=TRUE) else NA_real_

    fc_rows[[length(fc_rows)+1L]] <- data.table(
      dep_var   = dep_var,
      cat_label = cat_lbl,
      date      = tq,
      actual    = actual,
      pred_xgb     = pred_xgb,
      pred_rf      = pred_rf,
      pred_lasso   = pred_lasso,
      pred_arimax  = pred_arimax,
      pred_ensemble = pred_simple
    )

    # ── Importances (top 20 per member) ──
    if (!is.null(xgb_res) && nrow(xgb_res$importance) > 0) {
      top_xgb <- head(xgb_res$importance[order(-Gain)], 20)
      imp_rows[[length(imp_rows)+1L]] <- data.table(
        dep_var=dep_var, cat_label=cat_lbl, date=tq,
        model="XGBoost", Feature=top_xgb$Feature, Gain=top_xgb$Gain)
    }
    if (!is.null(rf_res) && nrow(rf_res$importance) > 0) {
      top_rf <- head(rf_res$importance[order(-Gain)], 20)
      imp_rows[[length(imp_rows)+1L]] <- data.table(
        dep_var=dep_var, cat_label=cat_lbl, date=tq,
        model="RandomForest", Feature=top_rf$Feature, Gain=top_rf$Gain)
    }
    if (!is.null(lasso_res) && nrow(lasso_res$importance) > 0) {
      top_las <- head(lasso_res$importance[order(-Gain)], 20)
      imp_rows[[length(imp_rows)+1L]] <- data.table(
        dep_var=dep_var, cat_label=cat_lbl, date=tq,
        model="LASSO", Feature=top_las$Feature, Gain=top_las$Gain)
    }
  }

  fc_dt  <- rbindlist(fc_rows,  fill=TRUE)
  imp_dt <- rbindlist(imp_rows, fill=TRUE)
  list(forecasts=fc_dt, importances=imp_dt)
}

# ════════════════════════════════════════════════════════════
# 6. BUILD TASK LIST
# ════════════════════════════════════════════════════════════
message("\n[4] Building task list...")

# 3a tasks
tasks_3a <- list()
for (dv in c("yoy_fcu_pct","yoy_fiscu_pct"))
  for (cat in cats)
    tasks_3a[[length(tasks_3a)+1L]] <- list(
      panel=qtrly[cat_label==cat], dep_var=dv, cat=cat,
      include_exit=TRUE, train_end=TRAIN_END)

# 3b tasks
tasks_3b <- list()
for (dv in c("yoy_fcu_assets_pct","yoy_fiscu_assets_pct"))
  for (cat in cats)
    tasks_3b[[length(tasks_3b)+1L]] <- list(
      panel=qtrly[cat_label==cat], dep_var=dv, cat=cat,
      include_exit=TRUE, train_end=TRAIN_END)

# 3c tasks
tasks_3c <- list()
if (!is.null(corp_panel))
  tasks_3c[[1L]] <- list(
    panel=corp_panel, dep_var="yoy_corp_cu_assets_pct", cat="Corporate",
    include_exit=FALSE, train_end=TRAIN_END_3C)

all_tasks <- c(tasks_3a, tasks_3b, tasks_3c)
message(sprintf("  Total tasks: %d  (3a: %d | 3b: %d | 3c: %d)",
                length(all_tasks), length(tasks_3a), length(tasks_3b), length(tasks_3c)))

# Test quarters
test_q_base <- all_quarters[all_quarters > TRAIN_END]
if (DEBUG_MODE && length(test_q_base) > DEBUG_ROLL_Q)
  test_q_base <- tail(test_q_base, DEBUG_ROLL_Q)

# ════════════════════════════════════════════════════════════
# 7. PARALLEL EXECUTION
# ════════════════════════════════════════════════════════════
message(sprintf("\n[5] Launching parallel ensemble (workers=%d)...", N_CORES))
tic("Ensemble run")

# Register Windows-compatible parallel backend
cl <- parallel::makeCluster(N_CORES, type="PSOCK")  # PSOCK works on Windows
doParallel::registerDoParallel(cl)

# Export all required objects + functions to workers
parallel::clusterExport(cl, varlist=c(
  "DEBUG_MODE","DEBUG_ROLL_Q","YOY_CAP","MAX_XREG_VARS","SIG_LEVEL",
  "TSCV_MIN_TRAIN","TSCV_H",
  "EXIT_VARS","EXIT_VARS_RAW","HARD_EXCL","STAT_PAT",
  "all_quarters",                                 # needed for te_q inside worker
  "winsorise","reg_metrics","prep_X","build_feats",
  "fit_xgb","fit_rf","fit_lasso",
  "fit_arimax_member","tscv_rmse_fn","run_cell",
  "%||%"                                          # custom null-coalesce used in worker fns
), envir=environment())

# Load packages on each worker
parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages({
    library(data.table); library(zoo); library(glmnet)
    library(forecast);   library(xgboost); library(ranger)
  })
  set.seed(42)
})

all_results <- foreach::foreach(
  task = all_tasks,
  .combine  = "list",          # "c" would collapse list-of-lists to a flat vector
  .packages = c("data.table","zoo","glmnet","forecast","xgboost","ranger"),
  .errorhandling = "pass"
) %dopar% {
  dv      <- task$dep_var
  cat_lbl <- task$cat
  panel   <- as.data.table(task$panel)
  te_q    <- all_quarters[all_quarters > task$train_end]

  # In debug mode, cap test quarters
  if (DEBUG_MODE && length(te_q) > 6L) te_q <- tail(te_q, 6L)

  feats_local <- build_feats(panel, include_exit=task$include_exit)

  # Suppress noisy output on workers
  suppressMessages(
    run_cell(panel, dv, cat_lbl, feats_local, te_q, task$include_exit)
  )
}

parallel::stopCluster(cl)
toc()

message(sprintf("[5] Parallel run complete. Collecting %d result objects.", length(all_results)))

# ════════════════════════════════════════════════════════════
# 8. CONSOLIDATE RESULTS
# ════════════════════════════════════════════════════════════
message("\n[6] Consolidating results...")

fc_list  <- list(); imp_list <- list()
for (res in all_results) {
  if (is.list(res) && !inherits(res, "error")) {
    if (!is.null(res$forecasts)  && nrow(res$forecasts)  > 0) fc_list[[length(fc_list)+1L]]   <- res$forecasts
    if (!is.null(res$importances) && nrow(res$importances) > 0) imp_list[[length(imp_list)+1L]] <- res$importances
  }
}

forecasts_all <- if (length(fc_list))  rbindlist(fc_list,  fill=TRUE) else data.table()
importances_all<- if (length(imp_list)) rbindlist(imp_list, fill=TRUE) else data.table()

message(sprintf("  Forecast rows   : %d", nrow(forecasts_all)))
message(sprintf("  Importance rows : %d", nrow(importances_all)))

# ── OOS metrics per model ─────────────────────────────────────
metrics_list <- list()
if (nrow(forecasts_all) > 0) {
  models_to_eval <- c("pred_xgb","pred_rf","pred_lasso","pred_arimax","pred_ensemble")
  for (mdl in models_to_eval) {
    if (!mdl %in% names(forecasts_all)) next
    m_sub <- forecasts_all[!is.na(actual) & !is.na(get(mdl)),
      {m <- reg_metrics(actual, get(mdl));
       list(rmse=m$rmse, mae=m$mae, r2_oos=m$r2_oos, n=m$n)},
      by=.(dep_var, cat_label)]
    m_sub[, model := mdl]
    metrics_list[[mdl]] <- m_sub
  }
}
metrics_all <- if (length(metrics_list)) rbindlist(metrics_list, fill=TRUE) else data.table()

# Save CSVs
fwrite(forecasts_all,  file.path(RESULT_DIR, "ensemble_forecasts_v1.csv"))
fwrite(importances_all, file.path(RESULT_DIR, "ensemble_importances_v1.csv"))
fwrite(metrics_all,    file.path(RESULT_DIR, "ensemble_metrics_v1.csv"))
message("  CSVs saved.")

# ════════════════════════════════════════════════════════════
# 9. TOP-10 MACRO FACTORS
#    Aggregate importance across all OOS windows (mean gain)
#    Compute ensemble-weighted importance = mean of XGB+RF+LASSO gains
# ════════════════════════════════════════════════════════════
message("\n[7] Computing top-10 macro factors per model-cell...")

top10_all <- data.table()   # default empty; populated below if importances exist

if (nrow(importances_all) > 0) {
  # Mean gain per (dep_var × cat_label × model × Feature)
  mean_imp <- importances_all[,
    .(mean_gain = mean(Gain, na.rm=TRUE), n_windows = .N),
    by=.(dep_var, cat_label, model, Feature)]

  # Ensemble importance = simple mean across the 3 ML models
  ensemble_imp <- mean_imp[model %in% c("XGBoost","RandomForest","LASSO"),
    .(ens_gain = mean(mean_gain, na.rm=TRUE), n_models = .N),
    by=.(dep_var, cat_label, Feature)]
  ensemble_imp[, model := "Ensemble"]
  setnames(ensemble_imp, "ens_gain", "mean_gain")

  combined_imp <- rbindlist(list(mean_imp, ensemble_imp), fill=TRUE)

  # Top-10 per (dep_var × cat_label × model)
  top10_list <- list()
  for (mdl in unique(combined_imp$model)) {
    sub_m <- combined_imp[model == mdl]
    for (dv in unique(sub_m$dep_var)) {
      for (cat in unique(sub_m[dep_var==dv, cat_label])) {
        cell <- sub_m[dep_var==dv & cat_label==cat]
        cell_top <- head(cell[order(-mean_gain)], 10L)
        cell_top[, rank := seq_len(.N)]
        top10_list[[length(top10_list)+1L]] <- cell_top
      }
    }
  }

  top10_all <- rbindlist(top10_list, fill=TRUE)
  fwrite(top10_all, file.path(RESULT_DIR, "top10_macro_factors_v1.csv"))
  message(sprintf("  Top-10 table: %d rows saved.", nrow(top10_all)))
} else {
  top10_all <- data.table()
  message("  [WARN] No importance data — skipping top-10.")
}

# ════════════════════════════════════════════════════════════
# 10. PLOTS
# ════════════════════════════════════════════════════════════
message("\n[8] Generating plots...")

theme_cu4 <- theme_bw(base_size=11) +
  theme(strip.background=element_rect(fill="grey93"),
        legend.position="bottom",
        plot.title=element_text(face="bold", size=13),
        plot.subtitle=element_text(size=9, colour="grey40"),
        panel.grid.minor=element_blank())

save_plot <- function(p, fname, w=14, h=8) {
  path <- file.path(PLOT_DIR, fname)
  tryCatch({
    pdf(path, width=w, height=h); print(p); dev.off()
    message(sprintf("  Saved: %s", fname))
  }, error=function(e) { try(dev.off(), silent=TRUE); message("  [WARN] ", e$message) })
}

# ── P1: Top-10 Macro Factors  ─────────────────────────────────
# One PDF per target variable; one bar panel per category
if (nrow(top10_all) > 0) {

  for (dv in unique(top10_all$dep_var)) {
    for (mdl in c("Ensemble","XGBoost","RandomForest","LASSO")) {
      sub <- top10_all[dep_var==dv & model==mdl]
      if (!nrow(sub)) next

      # Clean feature names for display
      sub[, feature_clean := gsub("macro_","", Feature)]
      sub[, feature_clean := gsub("_lag[0-9]+","(lag)", feature_clean)]
      sub[, feature_clean := gsub("_rmean[0-9]+","(rmean)", feature_clean)]
      sub[, feature_clean := gsub("_rsd[0-9]+","(rsd)", feature_clean)]
      sub[, feature_clean := gsub("_chg","(Δ)", feature_clean)]
      sub[, feature_clean := gsub("_cyc","(cyc)", feature_clean)]

      # Reorder by mean_gain within each category panel
      sub[, feature_f := reorder(feature_clean, mean_gain)]

      p <- ggplot(sub, aes(x=feature_f, y=mean_gain, fill=mean_gain)) +
        geom_col(show.legend=FALSE) +
        geom_text(aes(label=sprintf("%.3f", mean_gain)),
                  hjust=-0.1, size=2.8) +
        coord_flip() +
        facet_wrap(~cat_label, scales="free", ncol=3) +
        scale_fill_gradient(low="#c6dbef", high="#08519c") +
        labs(
          title    = sprintf("Top-10 Macro Factors — %s  [%s]", dv, mdl),
          subtitle = "Mean importance gain across all rolling OOS windows",
          x        = NULL,
          y        = "Mean Gain (normalised 0-1)"
        ) +
        theme_cu4 +
        theme(strip.text=element_text(size=8, face="bold"))

      fname <- sprintf("P1_top10_%s_%s.pdf",
                       gsub("_pct","",dv), tolower(gsub("Forest","F",mdl)))
      save_plot(p, fname, w=16, h=12)
    }
  }
}

# ── P2: Ensemble Importance Heatmap ───────────────────────────
# X = category, Y = feature (top 15 across all cats), fill = mean gain
if (nrow(top10_all) > 0) {
  ens_imp <- top10_all[model=="Ensemble"]
  if (nrow(ens_imp) > 0) {
    for (dv in unique(ens_imp$dep_var)) {
      sub <- ens_imp[dep_var==dv]
      # Global top-15 features across all categories
      feat_agg <- sub[, .(total_gain=sum(mean_gain,na.rm=TRUE)), by=Feature]
      top15    <- head(feat_agg[order(-total_gain)], 15L)$Feature

      hm <- sub[Feature %in% top15]
      hm[, feature_clean := gsub("macro_","",
           gsub("_lag[0-9]+","(lag)",
           gsub("_rmean[0-9]+","(rmean)",
           gsub("_chg","(Δ)",Feature))))]

      p <- ggplot(hm, aes(x=cat_label, y=feature_clean, fill=mean_gain)) +
        geom_tile(colour="white", linewidth=0.4) +
        geom_text(aes(label=sprintf("%.3f", mean_gain)), size=2.5) +
        scale_fill_gradient(low="#f0f9ff", high="#08306b",
                            name="Gain", na.value="grey90") +
        labs(
          title    = sprintf("Ensemble Importance Heatmap — %s", dv),
          subtitle = "Top-15 features by total gain across categories | Ensemble = XGBoost + RF + LASSO",
          x = "Asset Category", y = "Feature"
        ) +
        theme_cu4 +
        theme(axis.text.x=element_text(angle=35, hjust=1),
              axis.text.y=element_text(size=8))

      fname <- sprintf("P2_heatmap_%s.pdf", gsub("_pct","",dv))
      save_plot(p, fname, w=14, h=9)
    }
  }
}

# ── P3: Actual vs Predicted by model ─────────────────────────
if (nrow(forecasts_all) > 0) {
  pred_long <- melt(forecasts_all[!is.na(actual)],
                    id.vars=c("dep_var","cat_label","date","actual"),
                    measure.vars=intersect(
                      c("pred_xgb","pred_rf","pred_lasso","pred_arimax","pred_ensemble"),
                      names(forecasts_all)),
                    variable.name="model", value.name="pred")
  pred_long <- pred_long[!is.na(pred)]
  pred_long[, date_num := as.numeric(date)]

  for (dv in unique(pred_long$dep_var)) {
    sub <- pred_long[dep_var==dv]
    if (!nrow(sub)) next

    model_colours <- c(
      "pred_xgb"      = "#1f77b4",
      "pred_rf"       = "#2ca02c",
      "pred_lasso"    = "#ff7f0e",
      "pred_arimax"   = "#9467bd",
      "pred_ensemble" = "#d62728"
    )

    p <- ggplot(sub, aes(x=date_num)) +
      geom_line(aes(y=actual), colour="black", linewidth=0.9) +
      geom_line(aes(y=pred, colour=model, group=model),
                linewidth=0.7, linetype="dashed", alpha=0.9) +
      geom_hline(yintercept=0, linetype="dotted", colour="grey55") +
      facet_wrap(~cat_label, scales="free_y", ncol=3) +
      scale_colour_manual(values=model_colours,
                          labels=c("XGBoost","Random Forest","LASSO",
                                   "ARIMAX-TSCV","Ensemble"),
                          name="Model") +
      scale_x_continuous(breaks=pretty(sub$date_num, n=4),
                         labels=function(x) as.character(zoo::as.yearqtr(x))) +
      labs(title=sprintf("Actual vs Ensemble Members — %s", dv),
           subtitle="Black = Actual  |  Dashed = OOS 1-step predictions",
           x="Quarter", y="YoY %") +
      theme_cu4
    save_plot(p, sprintf("P3_actual_vs_pred_%s.pdf", gsub("_pct","",dv)), w=16, h=11)
  }
}

# ── P4: OOS Metrics Comparison ───────────────────────────────
if (nrow(metrics_all) > 0) {
  for (dv in unique(metrics_all$dep_var)) {
    sub <- metrics_all[dep_var==dv & !is.na(rmse)]
    if (!nrow(sub)) next
    sub[, model_label := fcase(
      model=="pred_xgb",      "XGBoost",
      model=="pred_rf",       "Random Forest",
      model=="pred_lasso",    "LASSO",
      model=="pred_arimax",   "ARIMAX-TSCV",
      model=="pred_ensemble", "Ensemble",
      default = model
    )]
    model_col <- c("XGBoost"="#1f77b4","Random Forest"="#2ca02c",
                   "LASSO"="#ff7f0e","ARIMAX-TSCV"="#9467bd","Ensemble"="#d62728")

    p_rmse <- ggplot(sub, aes(x=cat_label, y=rmse, fill=model_label)) +
      geom_col(position="dodge", width=0.75) +
      geom_text(aes(label=round(rmse,3)), position=position_dodge(0.75),
                vjust=-0.3, size=2.5) +
      scale_fill_manual(values=model_col, name="Model") +
      labs(title=sprintf("OOS RMSE by Model — %s", dv),
           x="Asset Category", y="RMSE (YoY%)") +
      theme_cu4 + theme(axis.text.x=element_text(angle=35, hjust=1))

    p_r2 <- ggplot(sub[!is.na(r2_oos)], aes(x=cat_label, y=r2_oos, fill=model_label)) +
      geom_col(position="dodge", width=0.75) +
      geom_hline(yintercept=0, linetype="dashed", colour="grey50") +
      geom_text(aes(label=round(r2_oos,3)), position=position_dodge(0.75),
                vjust=-0.3, size=2.5) +
      scale_fill_manual(values=model_col, name="Model") +
      labs(title=sprintf("OOS R² by Model — %s", dv),
           x="Asset Category", y="OOS R²") +
      theme_cu4 + theme(axis.text.x=element_text(angle=35, hjust=1))

    comb <- (p_rmse / p_r2) +
      patchwork::plot_annotation(
        title    = sprintf("Ensemble Performance — %s", dv),
        subtitle = "All 4 ensemble members + stacked ensemble  |  Rolling OOS evaluation",
        theme    = theme(plot.title=element_text(face="bold", size=14)))
    save_plot(comb, sprintf("P4_metrics_%s.pdf", gsub("_pct","",dv)), w=14, h=11)
  }
}

# ── P5: Cross-model Importance Stability  ─────────────────────
# For each (dep_var × category): ranked bar showing which features
# consistently rank in top-10 across XGBoost / RF / LASSO
if (nrow(top10_all) > 0) {
  stability <- top10_all[model %in% c("XGBoost","RandomForest","LASSO"),
    .(n_models_top10 = .N, avg_rank = mean(rank, na.rm=TRUE)),
    by=.(dep_var, cat_label, Feature)]
  stability[, feature_clean := gsub("macro_","",
               gsub("_lag[0-9]+","(lag)",
               gsub("_chg","(Δ)",Feature)))]

  for (dv in unique(stability$dep_var)) {
    sub <- stability[dep_var==dv]
    # top stable features: appear in all 3 models
    stab3 <- sub[n_models_top10==3]
    if (!nrow(stab3)) stab3 <- sub[n_models_top10>=2]
    if (!nrow(stab3)) next

    p <- ggplot(stab3, aes(x=reorder(feature_clean, -avg_rank),
                            y=n_models_top10, fill=avg_rank)) +
      geom_col() +
      geom_text(aes(label=sprintf("avg rank: %.1f", avg_rank)),
                hjust=1.1, colour="white", size=2.8, fontface="bold") +
      coord_flip() +
      facet_wrap(~cat_label, ncol=3, scales="free_y") +
      scale_fill_gradient(low="#74c476", high="#006d2c", name="Avg rank") +
      labs(
        title    = sprintf("Cross-Model Feature Stability — %s", dv),
        subtitle = "Features appearing in top-10 of 2+ models (XGBoost + RF + LASSO)",
        x=NULL, y="# Models where in Top-10"
      ) +
      theme_cu4 + theme(strip.text=element_text(size=8, face="bold"))

    save_plot(p, sprintf("P5_stability_%s.pdf", gsub("_pct","",dv)), w=16, h=11)
  }
}

# ── P6: Summary diagnostic PDF  ───────────────────────────────
# One page = global ranked list of top macro factors (all cells combined)
if (nrow(top10_all) > 0) {
  global_imp <- top10_all[model=="Ensemble",
    .(total_gain=sum(mean_gain, na.rm=TRUE), n_cells=.N),
    by=.(dep_var, Feature)]
  global_imp[, feature_clean := gsub("macro_","",
               gsub("_lag[0-9]+","(lag)",
               gsub("_rmean[0-9]+","(rmean)",
               gsub("_chg","(Δ)",Feature))))]

  for (dv in unique(global_imp$dep_var)) {
    sub <- head(global_imp[dep_var==dv][order(-total_gain)], 20L)
    sub[, feature_f := reorder(feature_clean, total_gain)]

    p <- ggplot(sub, aes(x=feature_f, y=total_gain, fill=total_gain)) +
      geom_col(show.legend=FALSE) +
      geom_text(aes(label=sprintf("%.2f", total_gain)), hjust=-0.1, size=3) +
      coord_flip() +
      scale_fill_gradient(low="#9ecae1", high="#08306b") +
      labs(
        title    = sprintf("Global Top-20 Macro Factors — %s", dv),
        subtitle = "Total ensemble gain summed across all asset categories",
        x=NULL, y="Total Ensemble Gain"
      ) +
      theme_cu4

    save_plot(p, sprintf("P6_global_top20_%s.pdf", gsub("_pct","",dv)), w=13, h=9)
  }
}

# ════════════════════════════════════════════════════════════
# 11. STACKED META-LEARNER (Optional second pass)
#     Train a LASSO meta-model on the OOS predictions of the
#     4 base models to produce a stacked ensemble forecast.
#     Requires at least 10 complete OOS rows per cell.
# ════════════════════════════════════════════════════════════
message("\n[9] Stacked meta-learner (LASSO on base model OOS predictions)...")

stacked_list <- list()

if (nrow(forecasts_all) > 10L) {
  for (dv in unique(forecasts_all$dep_var)) {
    for (cat_lbl in unique(forecasts_all[dep_var==dv, cat_label])) {
      sub <- forecasts_all[dep_var==dv & cat_label==cat_lbl &
                             !is.na(actual) &
                             !is.na(pred_xgb) & !is.na(pred_rf) &
                             !is.na(pred_lasso) & !is.na(pred_arimax)]
      if (nrow(sub) < 10L) next

      # Sort by date for time-series-aware CV
      setorderv(sub, "date")
      n_tr2 <- floor(nrow(sub) * 0.7)
      if (n_tr2 < 5L) next

      X_meta <- as.matrix(sub[, .(pred_xgb, pred_rf, pred_lasso, pred_arimax)])
      y_meta <- sub$actual

      cv_meta <- tryCatch(
        glmnet::cv.glmnet(X_meta, y_meta, alpha=1, nfolds=min(5L, n_tr2),
                          standardize=TRUE, type.measure="mse"),
        error=function(e) NULL)
      if (is.null(cv_meta)) next

      meta_preds <- as.numeric(predict(cv_meta, newx=X_meta, s="lambda.1se"))
      sub[, pred_stacked := meta_preds]

      m_meta <- reg_metrics(sub$actual, sub$pred_stacked)
      stacked_list[[length(stacked_list)+1L]] <- data.table(
        dep_var=dv, cat_label=cat_lbl,
        rmse_stacked=m_meta$rmse, r2_stacked=m_meta$r2_oos,
        n=m_meta$n)

      # Update forecasts_all with stacked predictions
      forecasts_all[dep_var==dv & cat_label==cat_lbl & date %in% sub$date,
                    pred_stacked := sub$pred_stacked]
    }
  }
}

stacked_metrics <- if (length(stacked_list)) rbindlist(stacked_list, fill=TRUE) else data.table()
fwrite(forecasts_all,     file.path(RESULT_DIR, "ensemble_forecasts_v1.csv"))  # re-save with stacked col
fwrite(stacked_metrics,   file.path(RESULT_DIR, "stacked_metrics_v1.csv"))
message(sprintf("  Stacked meta results: %d cells", nrow(stacked_metrics)))

# ════════════════════════════════════════════════════════════
# 12. DONE
# ════════════════════════════════════════════════════════════
message("\n[10] ══════════════════════════════════════════════")
message("     PART 4 ENSEMBLE ML COMPLETE")
message(sprintf("     Tasks          : %d", length(all_tasks)))
message(sprintf("     Forecast rows  : %d", nrow(forecasts_all)))
message(sprintf("     Workers used   : %d", N_CORES))
message(sprintf("     Results → %s/", RESULT_DIR))
message(sprintf("     Plots   → %s/", PLOT_DIR))
message("     Outputs:")
message("       ensemble_forecasts_v1.csv  — all OOS predictions (XGB+RF+LASSO+ARIMAX+Ensemble+Stacked)")
message("       ensemble_importances_v1.csv — per-window feature importances")
message("       ensemble_metrics_v1.csv    — OOS RMSE / R² per model × cell")
message("       top10_macro_factors_v1.csv — top-10 factors per target × category × model")
message("       stacked_metrics_v1.csv     — stacked meta-learner performance")
message("     Plots:")
message("       P1 — Top-10 bar charts per target+model")
message("       P2 — Importance heatmaps")
message("       P3 — Actual vs all members")
message("       P4 — OOS metrics comparison (RMSE + R²)")
message("       P5 — Cross-model feature stability")
message("       P6 — Global top-20 macro factors")
message("══════════════════════════════════════════════════")
