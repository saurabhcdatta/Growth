############################################################
# PART 3b — FICU & FISCU TOTAL ASSETS FORECASTING
#
# Targets  : yoy_ficu_assets_pct  (YoY % change in FICU total assets)
#             yoy_fiscu_assets_pct (YoY % change in FISCU total assets)
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
  library(tictoc)
  library(stringr)
})

set.seed(42)
options(scipen = 999)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 &&
                               !is.na(a[1])) a else b

# Safe rbindlist: normalises date/date_d class mismatches before binding.
# Prevents "class attribute on column N doesn't match" errors when
# combining qtrly-sourced (yearqtr) and forecast-sourced (numeric) tables.
safe_rbind <- function(..., fill = TRUE) {
  tbls <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(...))
  if (length(tbls) == 0L) return(data.table())
  tbls <- lapply(tbls, function(dt) {
    dt <- copy(dt)
    if ("date" %in% names(dt))
      dt[, date := as.numeric(date)]
    if ("date_d" %in% names(dt))
      dt[, date_d := as.Date(zoo::as.yearqtr(as.numeric(date)))]
    dt
  })
  rbindlist(tbls, fill = fill)
}

# ════════════════════════════════════════════════════════════
# 1. CONFIG
# ════════════════════════════════════════════════════════════
DATA_DIR   <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR   <- "plots_3b"
RESULT_DIR <- "results_3b"

DEBUG_MODE    <- TRUE    # FALSE for full production run
DEBUG_ROLL_Q  <- 6       # quarters to use in debug mode

# Rolling window: first test quarter
TRAIN_END  <- as.numeric(zoo::as.yearqtr("2021 Q1"))

# TSCV settings
TSCV_MIN_TRAIN        <- 12L   # minimum obs for standard categories
TSCV_MIN_TRAIN_SPARSE <- 8L    # floor for sparse categories (7_10B_Plus, 1_Less_10M)
TSCV_H                <- 1L    # forecast horizon per fold (1-step ahead)

# Categories with very few CUs — use lower TSCV floor and
# skip LASSO (go straight to pure ARIMA or small xreg set)
SPARSE_CATS <- c("7_10B_Plus", "1_Less_10M")

# Feature selection
MAX_XREG_VARS  <- 10L    # top N by LASSO magnitude passed to ARIMAX
SIG_LEVEL      <- 0.10   # p-value threshold for final model print

# Forecast horizon
FC_END <- as.numeric(zoo::as.yearqtr("2030 Q4"))

# YoY% clamp — hard ceiling on per-quarter forecast
YOY_CAP <- 25.0          # ±25% — assets swing wider than counts

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

required <- c("date","categories","cat_label","yoy_ficu_assets_pct","yoy_fiscu_assets_pct")
miss <- setdiff(required, names(qtrly))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse=", "))

# Replace Inf/NaN in dep vars with NA before any modelling
for (.dc in c("yoy_ficu_assets_pct", "yoy_fiscu_assets_pct")) {
  if (.dc %in% names(qtrly))
    qtrly[!is.finite(get(.dc)), (.dc) := NA_real_]
}
rm(.dc)

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
  yoy_ficu_assets_pct  = list(label="FICU Assets YoY % Change",  short="ficu_assets"),
  yoy_fiscu_assets_pct = list(label="FISCU Assets YoY % Change", short="fiscu_assets")
)

# ── CU exit variables (one-quarter lag) ──────────────────────
# Mergers/liquidations/acquisitions in quarter Q reduce the CU
# count in Q+1, not Q itself -- the NCUA de-charter process takes
# at least one quarter to finalise.  Using contemporaneous rates
# would be endogenous (count change and exit rate are jointly
# determined within the quarter).  Lagging by 1 breaks this link.
#
# We use the _lag1 transforms.  If absent from the panel they are
# created on-the-fly below before building FEATS_ALL.
#
# Sign expectations (enforced post-fit):
#   merger_rate_lag1      -> NEGATIVE & significant (required)
#   liquid_rate_lag1      -> NEGATIVE & significant (required)
#   acquisition_rate_lag1 -> NEGATIVE & significant (acquired CUs lose independent charter)
#   exit_rate_lag1        -> NEGATIVE (composite; no hard requirement)
#   exit_roll4_lag1       -> any sign (mean-reversion signal)
EXIT_VARS_RAW <- c("merger_rate", "liquid_rate", "acquisition_rate",
                   "exit_rate", "exit_roll4")
EXIT_VARS     <- c("merger_rate_lag1", "liquid_rate_lag1",
                   "acquisition_rate_lag1", "exit_rate_lag1",
                   "exit_roll4_lag1")

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
  # macro_v4 VAR_MAP: RFF->fedfunds, RS3M->gs3m, RS10Y->gs10, RS30Y->gs30
  # Derived in macro_v4 S2: fedfunds_chg, fedfunds_cycle, fomc_regime, hike_run
  "fedfunds", "fedfunds_chg", "fedfunds_cycle",
  "gs3m", "gs10", "gs30",
  # Derived in macro_v4 S2: yield_curve, yield_curve_inv, spread_2s10s
  "yield_curve", "yield_curve_inv", "spread_2s10s",
  # macro_v4 VAR_MAP: RMTG->mortgage30
  "mortgage30",
  # Derived in macro_v4 S2: real_rate = fedfunds - cpi_yoy
  "real_rate",
  # Credit — macro_v4 VAR_MAP: SRCB->baa_spread; credit_tightness derived S2
  "baa_spread", "credit_tightness",
  # Labour — macro_v4 VAR_MAP: LURC->unrate, YPDS->disp_income, UYPSAV->savings_rate
  "unrate", "disp_income", "savings_rate",
  # Activity — macro_v4 VAR_MAP: GDPS->gdp_real, CCONF->cons_confidence
  "gdp_real", "cons_confidence",
  # Inflation — macro_v4 VAR_MAP: PCPI->cpi, PCPIXFE->core_cpi
  # Derived in S2: cpi_yoy = PCPI YoY%, core_cpi_yoy = PCPIXFE YoY%
  "cpi", "core_cpi", "cpi_yoy", "core_cpi_yoy",
  # Housing — macro_v4 VAR_MAP: PHPI->hpi_fed, HP1->housing_permits
  "housing_permits", "hpi_fed",
  # Consumer credit — macro_v4 VAR_MAP: HDNNB->consumer_bankrupt, OCLDQ->cons_loan_delinq
  "consumer_bankrupt", "cons_loan_delinq",
  # FRB forward rates — macro_v4 VAR_MAP: RF1Y1Y->fwd_1y1y, RF1Y5Y->fwd_1y5y
  "fwd_1y1y", "fwd_1y5y",
  # FOMC regime indicators (derived S2)
  "fomc_regime", "hike_run"
)

# Stationary transforms to include for each curated macro var.
# Patterns cover:
#   ^fedfunds$          raw level (already stationary — rates/spreads)
#   ^yoy_fedfunds$      year-on-year % change prefix
#   ^fedfunds_yoy$      year-on-year % change suffix (alt naming)
#   ^qoq_fedfunds$      quarter-on-quarter % change
#   ^fedfunds_lag4$     lag transforms (lag1, lag4, lag8 etc.)
#   ^fedfunds_rmean4$   rolling mean transforms
#   ^fedfunds_rsd4$     rolling SD transforms
#   ^fedfunds_chg$      first-difference version
#   ^fedfunds_cyc$      cyclical deviation
cm_pat <- paste(CURATED_MACRO, collapse="|")
STATIONARY_TRANSFORMS <- paste(
  paste0("^(", cm_pat, ")$"),               # raw level
  paste0("^yoy_(", cm_pat, ")$"),            # yoy_ prefix
  paste0("^(", cm_pat, ")_yoy$"),            # _yoy suffix
  paste0("^qoq_(", cm_pat, ")$"),            # qoq_ prefix
  paste0("^(", cm_pat, ")_qoq$"),            # _qoq suffix
  paste0("^(", cm_pat, ")_lag[0-9]+$"),      # lag transforms
  paste0("^(", cm_pat, ")_rmean[0-9]+$"),    # rolling mean
  paste0("^(", cm_pat, ")_rsd[0-9]+$"),      # rolling SD
  paste0("^(", cm_pat, ")_chg$"),            # first difference
  paste0("^(", cm_pat, ")_cyc$"),            # cyclical
  paste0("^(", cm_pat, ")_accel$"),          # acceleration
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

# ── Forward-fill macro columns within each category ──────────────────────
# Macro values are broadcast from the quarterly time series — any NAs come
# from rolling/lag transforms at series starts (e.g. rmean8 needs 8 quarters
# before it is non-NA). LOCF within category ensures prep_X sees full
# coverage for all training windows without leaking future information.
macro_cols_present <- intersect(macro_feats, names(qtrly))
if (length(macro_cols_present) > 0) {
  setorderv(qtrly, c("categories","date"))
  qtrly[, (macro_cols_present) := lapply(.SD, function(x)
    data.table::nafill(x, type = "locf")
  ), by = categories, .SDcols = macro_cols_present]
  message(sprintf("    Forward-filled %d macro cols (LOCF by category)",
                  length(macro_cols_present)))
} else {
  message("    [WARN] No macro cols found in qtrly for forward-fill — check macro_v4 merge")
}

# ── Create _lag1 exit columns on-the-fly if not already present ──
# Part 1 may or may not have produced these; create them here to
# guarantee they exist before FEATS_ALL is assembled.
setorderv(qtrly, c("cat_label", "date"))
for (raw_v in EXIT_VARS_RAW) {
  lag1_name <- paste0(raw_v, "_lag1")
  if (!lag1_name %in% names(qtrly) && raw_v %in% names(qtrly)) {
    # Compute lag1 per category, preserving original row order.
    # split() reorders rows alphabetically, so we use row indices
    # to assign values back to the correct positions.
    lag1_vals <- rep(NA_real_, nrow(qtrly))
    for (grp in unique(qtrly[["cat_label"]])) {
      idx <- which(qtrly[["cat_label"]] == grp)
      # idx is already in date order because we setorderv above
      vals <- qtrly[[raw_v]][idx]
      lag1_vals[idx] <- c(NA_real_, vals[-length(vals)])
    }
    qtrly[[lag1_name]] <- lag1_vals
    message(sprintf("    [LAG1 CREATED] %s  (non-NA: %d)",
                    lag1_name, sum(!is.na(lag1_vals))))
  }
}
# Refresh numeric column list after potential new columns
all_num_cols <- names(qtrly)[vapply(qtrly, is.numeric, logical(1))]

# Exit variable features: only the _lag1 versions
exit_feats <- intersect(EXIT_VARS, all_num_cols)
# Also include exit_rate_lag1 and exit_roll4_lag1 transforms if present
exit_feats <- unique(c(exit_feats,
  grep("^(exit_rate|exit_roll4)_lag[0-9]|^(exit_rate|exit_roll4)_rmean",
       all_num_cols, value=TRUE, perl=TRUE)))

# NOTE: No quarterly dummies included.
# ARIMA's seasonal component (P,D,Q)_4 handles within-year seasonality
# directly in the ARIMA structure. Adding q1/q2/q3 dummies on top
# would double-count seasonal effects and create collinearity with
# the seasonal AR/MA terms.

# Combined feature set — macro + exit only
FEATS_ALL <- unique(c(macro_feats, exit_feats))

# Hard exclusions — targets themselves and any ficu/fiscu leakage
HARD_EXCL <- c("yoy_ficu_assets_pct","yoy_fiscu_assets_pct",
               "yoy_ficu_pct","yoy_fiscu_pct","yoy_assets_pct",
               "qoq_ficu_pct","qoq_fiscu_pct",
               "yoy_ficu_count","yoy_fiscu_count",
               "qoq_ficu_count","qoq_fiscu_count",
               "ficu_count","fiscu_count",
               "ficu_count_lag4","fiscu_count_lag4",
               "ld_ficu","ld_fiscu",
               "net_entry_rate","net_entry_rate_fiscu",
               "categories","n_active","n_total",
               "q1","q2","q3","q4")   # explicitly exclude all quarter dummies
FEATS_ALL <- setdiff(FEATS_ALL, HARD_EXCL)

message(sprintf("    Curated macro vars: %d base series", length(CURATED_MACRO)))
message(sprintf("    Macro features    : %d (incl. transforms)", length(macro_feats)))
message(sprintf("    Exit rate features: %d", length(exit_feats)))
message(sprintf("    Seasonal dummies  : EXCLUDED (ARIMA seasonal structure handles this)"))
message(sprintf("    Total features    : %d", length(FEATS_ALL)))

# ── HARD CHECK: Warn immediately if macro features are empty ─────────────
if (length(macro_feats) == 0) {
  message("")
  message("  ╔══════════════════════════════════════════════════════════════╗")
  message("  ║  WARNING: 0 macro features found in qtrly!                  ║")
  message("  ║  LASSO will run on exit vars only — no macroeconomic signal ║")
  message("  ║  Possible causes:                                            ║")
  message("  ║    1. macro_v4_frb.R has NOT been run yet                   ║")
  message("  ║    2. qtrly_enriched_v3.rds loaded BEFORE macro merge       ║")
  message("  ║    3. Column naming mismatch between CURATED_MACRO and data ║")
  message("  ║  Check: names(qtrly) for fedfunds, gs10, unrate etc.        ║")
  message("  ╚══════════════════════════════════════════════════════════════╝")
  message("")
  # Print first 30 numeric col names so user can see what IS present
  message(sprintf("  Numeric cols in qtrly (first 30): %s",
                  paste(head(all_num_cols, 30), collapse=", ")))
} else {
  # Verify macro cols are actually present in qtrly (not just named in FEATS_ALL)
  macro_in_qtrly <- intersect(macro_feats, names(qtrly))
  macro_missing  <- setdiff(macro_feats, names(qtrly))
  message(sprintf("    Macro feats in qtrly  : %d / %d present",
                  length(macro_in_qtrly), length(macro_feats)))
  if (length(macro_missing) > 0)
    message(sprintf("    Macro feats NOT in qtrly (%d): %s",
                    length(macro_missing),
                    paste(head(macro_missing, 10), collapse=", ")))
}
message(sprintf("    (was ~500+ with all FRB transforms; now focused on %d causal drivers)",
                length(FEATS_ALL)))

# ── Macro column diagnostic ───────────────────────────────────
# Wrapped in local({}) so the if/else block is paste-safe in R console.
local({
message("\n    [MACRO DIAG] Checking curated macro base vars in data...")
found_base  <- character(0)
missed_base <- character(0)
for (bv in CURATED_MACRO) {
  # Check all patterns: raw, yoy_, _yoy, qoq_, lags etc.
  pat_bv <- paste(
    paste0("^", bv, "$"),          # exact match (also catches derived: cpi_yoy, fomc_regime etc.)
    paste0("^yoy_", bv, "$"),      # yoy_ prefix (FE on base vars)
    paste0("^", bv, "_yoy$"),      # _yoy suffix
    paste0("^qoq_", bv, "$"),      # qoq_ prefix
    paste0("^", bv, "_qoq$"),      # _qoq suffix
    paste0("^", bv, "_lag"),       # lag variants
    paste0("^", bv, "_rmean"),     # rolling mean variants
    paste0("^lag[0-9]+_", bv, "$"),# lag_N_ prefix
    sep="|")
  hits <- grep(pat_bv, all_num_cols, value=TRUE, perl=TRUE)
  hits_in_feats <- intersect(hits, FEATS_ALL)
  if (length(hits_in_feats) > 0) {
    found_base <- c(found_base, bv)
  } else {
    missed_base <- c(missed_base, bv)
  }
}
message(sprintf("    Found  (%d): %s",
                length(found_base),
                paste(found_base, collapse=", ")))
if (length(missed_base) > 0) {
  message(sprintf("    MISSING(%d): %s  <-- check VAR_MAP naming in macro_v4",
                  length(missed_base),
                  paste(missed_base, collapse=", ")))
} else {
  message("    All curated macro base vars have at least one transform in FEATS_ALL")
}
message(sprintf("    Macro cols in FEATS_ALL (sample): %s",
                paste(head(macro_feats, 10), collapse=", ")))
})  # end local diagnostic block

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

# Build a clean feature matrix from a data.table.
# verbose=TRUE prints per-stage drop counts for diagnostics.
prep_X <- function(dt, feats, corr_cut=0.92, min_nonmiss=0.60, verbose=FALSE) {
  # Stage 0: which requested features actually exist in dt?
  not_in_dt <- setdiff(feats, names(dt))
  cols       <- intersect(feats, names(dt))
  if (verbose && length(not_in_dt) > 0)
    message(sprintf("        [prep_X] %d feats not in dt (missing cols): %s",
                    length(not_in_dt), paste(head(not_in_dt, 10), collapse=", ")))
  if (length(cols) == 0) {
    if (verbose) message("        [prep_X] 0 cols after intersect — returning NULL")
    return(NULL)
  }
  mat <- as.matrix(dt[, cols, with=FALSE])
  n_start <- ncol(mat)

  # Stage 1: missingness filter
  ok_miss <- apply(mat, 2, function(x) mean(!is.na(x)) >= min_nonmiss)
  dropped_miss <- colnames(mat)[!ok_miss]
  mat <- mat[, ok_miss, drop=FALSE]
  if (verbose && length(dropped_miss) > 0)
    message(sprintf("        [prep_X] dropped %d cols (>%.0f%% missing): %s",
                    length(dropped_miss), (1-min_nonmiss)*100,
                    paste(head(dropped_miss, 10), collapse=", ")))

  # Stage 2: near-zero variance filter
  ok_var <- apply(mat, 2, function(x) var(x, na.rm=TRUE) > 1e-10)
  dropped_var <- colnames(mat)[!ok_var]
  mat <- mat[, ok_var, drop=FALSE]
  if (verbose && length(dropped_var) > 0)
    message(sprintf("        [prep_X] dropped %d cols (near-zero var): %s",
                    length(dropped_var), paste(head(dropped_var, 10), collapse=", ")))

  if (ncol(mat) == 0) {
    if (verbose) message("        [prep_X] 0 cols after miss+var filter — returning NULL")
    return(NULL)
  }

  # Stage 3: impute remaining NAs with column median
  for (j in seq_len(ncol(mat))) {
    na_j <- is.na(mat[,j])
    if (any(na_j)) mat[na_j, j] <- median(mat[,j], na.rm=TRUE)
  }

  # Stage 4: correlation filter (keep first of each highly-correlated pair)
  n_before_corr <- ncol(mat)
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
  if (verbose)
    message(sprintf("        [prep_X] summary: %d requested → %d in dt → %d post-miss → %d post-var → %d post-corr",
                    length(feats), n_start,
                    n_start - length(dropped_miss),
                    n_start - length(dropped_miss) - length(dropped_var),
                    ncol(mat)))

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

# ── Exit variable sign priors (GLOBAL — used in fit_window_3a and screen print) ──
# -1 = must be negative, +1 = must be positive, 0 = no restriction
# Drop condition: wrong sign AND insignificant (both must be true)
EXIT_SIGN_PRIOR <- c(
  merger_rate_lag1      = -1L,   # mergers remove independent CUs
  liquid_rate_lag1      = -1L,   # liquidations remove CUs
  acquisition_rate_lag1 = -1L,   # acquisitions remove independent charter
  exit_rate_lag1        = -1L,   # composite exit
  exit_roll4_lag1       =  0L    # no restriction (mean-reversion)
)

# Helper: build coefficient data.table from any Arima/ARIMA fit
build_coef_dt <- function(fit, n_obs, sig_vars = character(0)) {
  if (is.null(fit)) return(data.table())

  cf_all <- tryCatch(fit$coef, error=function(e) numeric(0))

  # ARIMA(0,d,0) has no AR/MA terms — coef is numeric(0).
  # Return a single informative row so the screen print is never blank.
  if (length(cf_all) == 0L) {
    ord <- tryCatch(forecast::arimaorder(fit), error=function(e) NULL)
    ord_str <- if (!is.null(ord))
                 sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[4]",
                         ord["p"],ord["d"],ord["q"],
                         ord["P"],ord["D"],ord["Q"])
               else "ARIMA(0,d,0)"
    # Compute sigma² (variance of residuals) as the single "coefficient"
    resid_v <- tryCatch(as.numeric(residuals(fit)), error=function(e) NULL)
    sigma2  <- if (!is.null(resid_v)) var(resid_v, na.rm=TRUE) else NA_real_
    return(data.table(
      variable = c(paste0("[", ord_str, " — no AR/MA params]"), "sigma2"),
      estimate = c(NA_real_, sigma2),
      std_err  = c(NA_real_, NA_real_),
      t_stat   = c(NA_real_, NA_real_),
      p_value  = c(NA_real_, NA_real_),
      selected = c(FALSE, FALSE)
    ))
  }

  cf_se_v <- tryCatch(sqrt(diag(fit$var.coef)),
                      error=function(e) rep(NA_real_, length(cf_all)))
  # Ensure cf_se_v has same length (can be shorter if var.coef is smaller)
  if (length(cf_se_v) < length(cf_all))
    cf_se_v <- c(cf_se_v, rep(NA_real_, length(cf_all) - length(cf_se_v)))

  n_df    <- max(n_obs - length(cf_all), 1L)
  cf_tval <- cf_all / cf_se_v
  cf_pval <- 2 * pt(-abs(cf_tval), df=n_df)

  dt <- data.table(
    variable = names(cf_all),
    estimate = as.numeric(cf_all),
    std_err  = as.numeric(cf_se_v),
    t_stat   = as.numeric(cf_tval),
    p_value  = as.numeric(cf_pval),
    selected = names(cf_all) %in% sig_vars
  )

  # Append sigma² row so we always show model noise level
  resid_v <- tryCatch(as.numeric(residuals(fit)), error=function(e) NULL)
  sigma2  <- if (!is.null(resid_v)) var(resid_v, na.rm=TRUE) else NA_real_
  rbind(dt, data.table(variable="sigma2", estimate=sigma2,
                       std_err=NA_real_, t_stat=NA_real_,
                       p_value=NA_real_, selected=FALSE))
}

# Helper: compute adj R² from any fit
compute_adj_r2 <- function(fit, y_vec) {
  resid_v <- tryCatch(as.numeric(residuals(fit)), error=function(e) NULL)
  if (is.null(resid_v)) return(NA_real_)
  n_p   <- length(y_vec)
  k_p   <- length(tryCatch(fit$coef, error=function(e) numeric(0)))
  ss_r  <- sum(resid_v^2, na.rm=TRUE)
  ss_t  <- sum((y_vec - mean(y_vec, na.rm=TRUE))^2, na.rm=TRUE)
  r2    <- if (isTRUE(ss_t > 0)) 1 - ss_r/ss_t else NA_real_
  if (!is.na(r2) && isTRUE(n_p > k_p+1L)) {
    1 - (1-r2)*(n_p-1L)/(n_p-k_p-1L)
  } else {
    NA_real_
  }
}

fit_window_3a <- function(train_dt, test_row, dep_var, feats,
                          min_obs = TSCV_MIN_TRAIN,
                          verbose_prep = FALSE) {

  y_train <- train_dt[[dep_var]]
  n_valid <- sum(!is.na(y_train))

  # Absolute floor — 6 obs minimum to fit any ARIMA
  if (n_valid < 6L)
    return(list(ok=FALSE, reason=sprintf("only %d non-NA obs (need >=6)", n_valid)))

  # Flag sparse mode: below standard threshold → skip LASSO,
  # use pure ARIMA or a minimal hand-picked xreg set
  sparse_mode <- (n_valid < min_obs)
  if (sparse_mode)
    message(sprintf("        [SPARSE] %d obs < %d — skipping LASSO, using pure ARIMAX",
                    n_valid, min_obs))

  y_train_w <- winsorise(y_train)
  # Guard: if winsorised series is all NA/non-finite, cannot fit ARIMA
  y_train_w[!is.finite(y_train_w)] <- NA_real_
  if (sum(!is.na(y_train_w)) < 6L)
    return(list(ok=FALSE, reason="insufficient finite obs after winsorise"))

  # Build ts object
  train_dates <- sort(train_dt$date)
  min_yq   <- zoo::as.yearqtr(min(train_dates))
  start_yr <- as.integer(format(min_yq, "%Y"))
  start_q  <- as.integer(format(min_yq, "%q"))
  y_ts     <- ts(y_train_w, frequency=4L, start=c(start_yr, start_q))

  n_obs        <- length(y_train_w)
  nfolds_use   <- max(3L, min(10L, floor(n_obs / 3L)))

  # ── Step 2: auto.arima on y_ts alone (fixes ARIMA order) ──
  # Done first regardless of sparse/normal mode
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
  arima_order_vec <- c(p=p_ord, d=d_ord, q=q_ord, P=P_ord, D=D_ord, Q=Q_ord)

  # ── SPARSE PATH: pure ARIMA (no xreg) ────────────────────
  # Used when n_obs < min_obs OR when LASSO/TSCV cannot run reliably.
  # Still produces a valid 1-step forecast.
  if (sparse_mode) {
    fc_out <- tryCatch(
      forecast::forecast(arima_base, h=1L, level=95L),
      error=function(e) NULL)
    if (is.null(fc_out))
      return(list(ok=FALSE, reason="sparse pure ARIMA forecast failed"))

    pred_final <- as.numeric(fc_out$mean)
    if (!is.finite(pred_final))
      return(list(ok=FALSE, reason="sparse forecast returned non-finite"))
    pred_final <- max(min(pred_final,  YOY_CAP), -YOY_CAP)
    pred_lo95  <- max(as.numeric(fc_out$lower[1L]), -YOY_CAP*1.5)
    pred_hi95  <- min(as.numeric(fc_out$upper[1L]),  YOY_CAP*1.5)

    coef_dt <- build_coef_dt(arima_base, n_obs)
    adj_r2  <- compute_adj_r2(arima_base, y_train_w)

    return(list(ok=TRUE, pred_final=pred_final,
                pred_lo95=pred_lo95, pred_hi95=pred_hi95,
                method_used="PURE_ARIMA_SPARSE",
                sig_vars=character(0), coef_dt=coef_dt,
                arimax_fit=arima_base, arima_order=arima_order_vec,
                adj_r2=adj_r2, tscv_rmse=NA_real_,
                n_train=n_obs, n_lasso_sel=0L, n_final=0L))
  }

  # ── STANDARD PATH (n_obs >= min_obs) ────────────────────
  # Build feature matrix
  X_train <- prep_X(train_dt, feats, verbose=verbose_prep)
  if (is.null(X_train) || ncol(X_train) < 1L) {
    if (verbose_prep)
      message(sprintf("        [prep_X] ALL features dropped — pure ARIMA fallback"))
    return(list(ok=FALSE, reason="no features after prep"))
  }
  x_train_cols <- colnames(X_train)

  # ── Step 1: LASSO pre-screening ──────────────────────────
  # Guard: if y has near-zero variance, LASSO cannot run and
  # the category behaves like a sparse/constant series.
  # Treat as sparse → pure ARIMA fallback.
  y_var <- var(y_train_w, na.rm=TRUE)
  if (!is.finite(y_var) || y_var < 1e-6) {
    message(sprintf("        [NEAR-CONST] y variance=%.2e — falling back to pure ARIMA",
                    if (is.finite(y_var)) y_var else 0))
    fc_out <- tryCatch(
      forecast::forecast(arima_base, h=1L, level=95L),
      error=function(e) NULL)
    if (is.null(fc_out))
      return(list(ok=FALSE, reason="near-constant y: pure ARIMA forecast failed"))
    pred_final <- max(min(as.numeric(fc_out$mean), YOY_CAP), -YOY_CAP)
    pred_lo95  <- max(as.numeric(fc_out$lower[1L]), -YOY_CAP*1.5)
    pred_hi95  <- min(as.numeric(fc_out$upper[1L]),  YOY_CAP*1.5)
    if (!is.finite(pred_final))
      return(list(ok=FALSE, reason="near-constant y: forecast non-finite"))
    coef_dt <- build_coef_dt(arima_base, n_obs)
    adj_r2_fb <- compute_adj_r2(arima_base, y_train_w)
    return(list(ok=TRUE, pred_final=pred_final,
                pred_lo95=pred_lo95, pred_hi95=pred_hi95,
                method_used="PURE_ARIMA_NEARCONST",
                sig_vars=character(0), coef_dt=coef_dt,
                arimax_fit=arima_base, arima_order=arima_order_vec,
                adj_r2=adj_r2_fb, tscv_rmse=NA_real_,
                n_train=n_obs, n_lasso_sel=0L, n_final=0L,
                lasso_macro_top=character(0), in_final_macro=character(0),
                X_train=X_train))
  }

  cv_fit <- tryCatch(
    glmnet::cv.glmnet(X_train, y_train_w, alpha=1,
                      nfolds=nfolds_use, standardize=TRUE,
                      intercept=TRUE, type.measure="mse"),
    error=function(e) {
      message(sprintf("        [GLMNET FAIL] %s — falling back to pure ARIMA", e$message))
      NULL
    })
  # If glmnet fails for any reason, fall back to pure ARIMA
  if (is.null(cv_fit)) {
    fc_out <- tryCatch(
      forecast::forecast(arima_base, h=1L, level=95L),
      error=function(e) NULL)
    if (is.null(fc_out))
      return(list(ok=FALSE, reason="glmnet failed + pure ARIMA forecast failed"))
    pred_final <- max(min(as.numeric(fc_out$mean), YOY_CAP), -YOY_CAP)
    pred_lo95  <- max(as.numeric(fc_out$lower[1L]), -YOY_CAP*1.5)
    pred_hi95  <- min(as.numeric(fc_out$upper[1L]),  YOY_CAP*1.5)
    if (!is.finite(pred_final))
      return(list(ok=FALSE, reason="glmnet failed + forecast non-finite"))
    coef_dt <- build_coef_dt(arima_base, n_obs)
    adj_r2_fb <- compute_adj_r2(arima_base, y_train_w)
    return(list(ok=TRUE, pred_final=pred_final,
                pred_lo95=pred_lo95, pred_hi95=pred_hi95,
                method_used="PURE_ARIMA_GLMNET_FALLBACK",
                sig_vars=character(0), coef_dt=coef_dt,
                arimax_fit=arima_base, arima_order=arima_order_vec,
                adj_r2=adj_r2_fb, tscv_rmse=NA_real_,
                n_train=n_obs, n_lasso_sel=0L, n_final=0L,
                lasso_macro_top=character(0), in_final_macro=character(0),
                X_train=X_train))
  }

  lasso_coef <- as.matrix(coef(cv_fit, s="lambda.1se"))
  selected   <- rownames(lasso_coef)[lasso_coef[,1L] != 0 &
                  rownames(lasso_coef) != "(Intercept)"]
  selected   <- intersect(selected, x_train_cols)

  # Always protect exit vars (merger/liquidation rates)
  # No quarterly dummies — ARIMA seasonal structure handles seasonality
  protected  <- intersect(EXIT_VARS, x_train_cols)
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

  # ── Step 2: ARIMA order already determined above (before sparse check) ──
  # arima_base, p_ord, d_ord, q_ord, P_ord, D_ord, Q_ord, has_seas,
  # arima_order_vec are all available from the shared block above.

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
  q_protect    <- character(0)   # no quarterly dummies — ARIMA handles seasonality
  # Exit vars are NOT protected in TSCV: they compete on OOS RMSE like any macro var.
  # The sign+significance gate (Step 4b) handles economic constraints post-fit.
  # Protecting them here would force them in even when they hurt OOS performance.
  protected_v  <- character(0)

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
      if (is.finite(val)) {
        mat[1L,v] <- val
      } else if (v %in% names(train_meds) && is.finite(train_meds[[v]])) {
        mat[1L,v] <- train_meds[[v]]
      } else {
        mat[1L,v] <- 0
      }
    }
    mat
  }
  # ── Step 4b: Exit-var sign & significance gate ───────────────
  #
  # Economic priors (all variables are lagged 1 quarter):
  #   merger_rate_lag1      — must be NEGATIVE & significant
  #                           (mergers in Q reduce count in Q+1)
  #   liquid_rate_lag1      — must be NEGATIVE & significant
  #                           (liquidations in Q reduce count in Q+1)
  #   acquisition_rate_lag1 — must be NEGATIVE & significant
  #                           (acquired CUs lose independent charter)
  #   exit_rate_lag1        — must be NEGATIVE (composite exit)
  #   exit_roll4_lag1       — no sign restriction (mean-reversion)
  #
  # Any exit var with WRONG sign OR p >= SIG_LEVEL is dropped and
  # the model is refit without it.  This enforces economic logic
  # and avoids spurious coefficients contaminating forecasts.
  # ─────────────────────────────────────────────────────────────
  # EXIT_SIGN_PRIOR is defined globally above fit_window_3a

  check_exit_signs <- function(fit, vars, n_obs) {
    if (length(vars) == 0L || is.null(fit)) return(vars)
    cf    <- fit$coef
    se_v  <- sqrt(diag(fit$var.coef))
    df    <- max(n_obs - length(cf), 1L)
    drop_v <- character(0)

    for (ev in intersect(vars, names(EXIT_SIGN_PRIOR))) {
      prior <- EXIT_SIGN_PRIOR[[ev]]
      if (prior == 0L) next   # no restriction on exit_roll4

      if (!ev %in% names(cf)) next
      est  <- as.numeric(cf[ev])
      se_e <- as.numeric(se_v[match(ev, rownames(fit$var.coef))])
      tval <- est / se_e
      pval <- 2 * pt(-abs(tval), df=df)

      wrong_sign <- isTRUE((prior == -1L && est > 0) ||
                            (prior ==  1L && est < 0))
      insig      <- isTRUE(pval >= SIG_LEVEL)

      # Drop if EITHER condition is true:
      #   wrong sign  → positive merger/liquid/acquisition contradicts
      #                 economic theory regardless of significance
      #   insignificant → no statistical evidence it belongs
      # Keep ONLY when sign is correct AND p < SIG_LEVEL
      if (wrong_sign || insig) {
        reason <- if (wrong_sign && insig) "wrong sign + insignificant" else
                  if (wrong_sign)          "wrong sign (positive — dropped regardless of significance)" else
                                           "insignificant"
        message(sprintf("        [EXIT GATE] dropped '%s': est=%.4f  p=%.4f  (%s)",
                        ev, est, pval, reason))
        drop_v <- c(drop_v, ev)
      } else {
        message(sprintf("        [EXIT GATE] KEPT '%s': est=%.4f  p=%.4f  (correct sign + significant)",
                        ev, est, pval))
      }
    }
    setdiff(vars, drop_v)
  }

  # Apply gate: may remove some exit vars
  sig_vars_gated <- check_exit_signs(arimax_final, sig_vars, n_obs)

  if (length(sig_vars_gated) < length(sig_vars)) {
    dropped_exit <- setdiff(sig_vars, sig_vars_gated)
    message(sprintf("        [EXIT GATE] refitting without: %s",
                    paste(dropped_exit, collapse=", ")))

    if (length(sig_vars_gated) == 0L) {
      # All xreg vars gated out — use pre-fitted pure ARIMA directly.
      arimax_final <- arima_base
      sig_vars     <- character(0)
      message("        [EXIT GATE] all xreg dropped — using pure ARIMA")
    } else {
      res_gated <- refit_fn(sig_vars_gated)
      if (!is.null(res_gated$fit)) {
        arimax_final <- res_gated$fit
        sig_vars     <- res_gated$vars
        message("        [EXIT GATE] refit OK")
      } else {
        arimax_final <- arima_base
        sig_vars     <- character(0)
        message("        [EXIT GATE] refit failed — falling back to pure ARIMA")
      }
    }
    # Always rebuild coef_dt from the final model after gate adjustments
    # (sig_vars may have changed — coef_dt must reflect actual fitted model)
    coef_dt_final <- build_coef_dt(arimax_final, n_obs, sig_vars)
    adj_r2_final  <- compute_adj_r2(arimax_final, y_train_w)
  } else {
    message("        [EXIT GATE] all exit vars passed sign+significance check")
    coef_dt_final <- build_coef_dt(arimax_final, n_obs, sig_vars)
    adj_r2_final  <- compute_adj_r2(arimax_final, y_train_w)
  }

  # Build test xreg — NULL when sig_vars is empty (pure ARIMA path)
  xreg_test <- if (length(sig_vars) > 0L) build_test_xreg(sig_vars) else NULL

  # ── Step 5: 1-step ahead forecast ────────────────────────
  fc_out <- tryCatch(
    forecast::forecast(arimax_final, h=1L, xreg=xreg_test, level=95),
    error=function(e) {
      # If xreg mismatch, retry with pure ARIMA
      tryCatch(
        forecast::forecast(arima_base, h=1L, level=95),
        error=function(e2) NULL)
    })

  if (is.null(fc_out))
    return(list(ok=FALSE, reason="forecast() failed after all fallbacks"))

  pred_final <- as.numeric(fc_out$mean)
  if (is.na(pred_final))
    return(list(ok=FALSE, reason="forecast returned NA"))

  # Clamp
  pred_final <- max(min(pred_final,  YOY_CAP), -YOY_CAP)
  pred_lo95  <- max(as.numeric(fc_out$lower[1L]), -YOY_CAP * 1.5)
  pred_hi95  <- min(as.numeric(fc_out$upper[1L]),  YOY_CAP * 1.5)

  # ── Coefficient table + Adj R² (built above after exit gate) ────
  coef_dt <- coef_dt_final
  adj_r2  <- adj_r2_final

  # ── Top LASSO-scored macro vars (for screen print diagnostic) ──
  # Capture macro vars that had nonzero LASSO coef but did not
  # survive TSCV elimination — useful to know even when absent.
  exit_and_arima_pat <- paste0(
    "^(", paste(c(EXIT_VARS, EXIT_VARS_RAW), collapse="|"), ")|",
    "^(ar|ma|sar|sma)[0-9]+$|intercept|drift|mean")
  lasso_nonzero <- rownames(lasso_coef)[lasso_coef[,1L] != 0 &
                     rownames(lasso_coef) != "(Intercept)"]
  lasso_macro_candidates <- lasso_nonzero[
    !grepl(exit_and_arima_pat, lasso_nonzero, perl=TRUE)]
  # Sort by absolute LASSO coefficient, report top 5
  if (length(lasso_macro_candidates) > 0L) {
    lasso_macro_abs <- abs(lasso_coef[lasso_macro_candidates, 1L])
    lasso_macro_top <- names(sort(lasso_macro_abs, decreasing=TRUE))[
                         seq_len(min(5L, length(lasso_macro_candidates)))]
  } else {
    lasso_macro_top <- character(0)
  }
  in_final_macro <- sig_vars[!grepl(exit_and_arima_pat, sig_vars, perl=TRUE)]

  # Full macro LASSO tracking for screen print
  lasso_macro_all <- lasso_macro_candidates  # all macro vars with nonzero LASSO coef
  lasso_macro_coefs <- if (length(lasso_macro_all) > 0L)
    setNames(as.numeric(lasso_coef[lasso_macro_all, 1L]), lasso_macro_all)
  else numeric(0)
  # Classify each macro var's fate through the pipeline
  tscv_dropped_macro <- setdiff(lasso_macro_all, sig_vars)  # LASSO selected but TSCV dropped
  tscv_dropped_macro <- tscv_dropped_macro[
    !grepl(exit_and_arima_pat, tscv_dropped_macro, perl=TRUE)]

  list(
    ok                   = TRUE,
    pred_final           = pred_final,
    pred_lo95            = pred_lo95,
    pred_hi95            = pred_hi95,
    method_used          = "ARIMAX_TSCV",
    sig_vars             = sig_vars,
    coef_dt              = coef_dt,
    arimax_fit           = arimax_final,
    arima_order          = arima_order_vec,
    adj_r2               = adj_r2,
    tscv_rmse            = best_rmse,
    n_train              = n_obs,
    n_lasso_sel          = length(selected),
    n_final              = length(sig_vars),
    lasso_macro_top      = lasso_macro_top,      # top 5 by LASSO magnitude
    lasso_macro_all      = lasso_macro_all,      # ALL macro vars with nonzero LASSO coef
    lasso_macro_coefs    = lasso_macro_coefs,    # named vector of LASSO coefs for macro
    tscv_dropped_macro   = tscv_dropped_macro,   # LASSO-selected but TSCV-eliminated
    in_final_macro       = in_final_macro,       # survived all the way to final model
    X_train              = X_train               # passed to screen print for LASSO diag
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
all_fits      <- list()        # final ARIMAX object per model
all_arima_oos <- list()        # pure ARIMA 1-step OOS forecasts (benchmark)

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

    # Adaptive minimum training obs — sparse categories get a lower floor
    min_train_cat <- if (cat %in% SPARSE_CATS) TSCV_MIN_TRAIN_SPARSE else TSCV_MIN_TRAIN

    for (tq in test_quarters) {
      train_idx <- cat_dt$date >= zoo::as.yearqtr("2005 Q1") &
                   cat_dt$date <  tq
      test_idx  <- cat_dt$date == tq

      # Diagnostic on first test quarter if skipping
      if (tq == test_quarters[1L] &&
          (sum(train_idx) < min_train_cat || sum(test_idx) == 0)) {
        message(sprintf("        [DIAG] %s|%s: train_n=%d (min=%d), test_n=%d",
                        dv, cat, sum(train_idx), min_train_cat, sum(test_idx)))
        y_check <- cat_dt[train_idx][[dv]]
        message(sprintf("        [DIAG] non-NA in dep_var: %d / %d",
                        sum(!is.na(y_check)), length(y_check)))
      }

      if (sum(train_idx) < min_train_cat || sum(test_idx) == 0) next

      train_dt <- cat_dt[train_idx]
      test_row  <- cat_dt[test_idx][1L]

      # verbose=TRUE on first test quarter per cat/dv so we can see
      # exactly what features survive into X_train for each series
      is_first_window <- (tq == test_quarters[1L])

      # ── One-time macro diagnostic per cat/dv ─────────────────
      if (is_first_window) {
        macro_in_train <- intersect(macro_feats, names(train_dt))
        macro_nonmiss  <- vapply(macro_in_train, function(cn)
          mean(!is.na(train_dt[[cn]])), numeric(1))
        n_pass70  <- sum(macro_nonmiss >= 0.70)
        n_fail70  <- sum(macro_nonmiss <  0.70)
        message(sprintf("        [MACRO CHECK] %s | %s: %d macro cols in train_dt | pass 70%%: %d | fail 70%%: %d",
                        dv, cat, length(macro_in_train), n_pass70, n_fail70))
        if (n_fail70 > 0 && n_fail70 <= 10)
          message(sprintf("        [MACRO MISS]  failing cols: %s",
                          paste(names(macro_nonmiss)[macro_nonmiss < 0.70], collapse=", ")))
        if (length(macro_in_train) == 0)
          message("        [MACRO WARN]  NO macro cols found in train_dt — check qtrly merge!")
      }

      res <- fit_window_3a(train_dt, test_row, dv, feats_dv,
                           min_obs = min_train_cat,
                           verbose_prep = is_first_window)
      # Track last_res as the most recent SUCCESSFUL result for screen print.
      # Failed windows (ok=FALSE) are skipped so the print always reflects
      # a real fitted model — not a NULL/empty failure object.
      if (isTRUE(res$ok)) last_res <- res

      # ── Pure ARIMA 1-step OOS (benchmark — no xreg) ──────────
      arima_oos_pred <- tryCatch({
        y_tr_w  <- winsorise(train_dt[[dv]])
        if (sum(!is.na(y_tr_w)) < 6L) stop("too sparse")
        tr_dts  <- sort(train_dt$date)
        sy      <- as.integer(format(zoo::as.yearqtr(min(tr_dts)), "%Y"))
        sq      <- as.integer(format(zoo::as.yearqtr(min(tr_dts)), "%q"))
        y_ts_b  <- ts(y_tr_w, frequency=4L, start=c(sy, sq))
        fit_b   <- forecast::auto.arima(y_ts_b, stepwise=TRUE,
                                         approximation=TRUE,
                                         max.p=3L, max.q=2L,
                                         max.P=1L, max.Q=1L)
        fc_b    <- forecast::forecast(fit_b, h=1L, level=95L)
        list(
          pred  = as.numeric(fc_b$mean),
          lo95  = as.numeric(fc_b$lower[1L]),
          hi95  = as.numeric(fc_b$upper[1L]),
          order = paste0("ARIMA(",
                   paste(forecast::arimaorder(fit_b), collapse=","), ")")
        )
      }, error=function(e) NULL)

      # Actual value
      actual_val <- test_row[[dv]]
      if (is.na(actual_val)) {
        col_lv  <- if (dv=="yoy_ficu_assets_pct") "ficu_assets" else "fiscu_assets"
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

      # Always rebuild per-window coef_dt from the actual fit object so
      # pure ARIMA windows store their real AR/MA coefs, not an empty table.
      per_win_cd <- if (!is.null(res$arimax_fit)) {
        build_coef_dt(res$arimax_fit,
                      res$n_train %||% 20L,
                      res$sig_vars %||% character(0))
      } else {
        res$coef_dt
      }
      if (!is.null(per_win_cd) && nrow(per_win_cd) > 0) {
        cd_w <- copy(per_win_cd)
        cd_w[, `:=`(dep_var=dv, cat_label=cat, date=tq)]
        coef_rows[[length(coef_rows)+1L]] <- cd_w
      }

      # Store pure ARIMA OOS row
      if (!is.null(arima_oos_pred)) {
        all_arima_oos[[length(all_arima_oos)+1L]] <- data.table(
          dep_var    = dv,
          cat_label  = cat,
          date       = tq,
          actual     = actual_val,
          arima_pred = arima_oos_pred$pred,
          arima_lo95 = arima_oos_pred$lo95,
          arima_hi95 = arima_oos_pred$hi95,
          arima_order= arima_oos_pred$order
        )
      }
    }  # end tq loop

    if (length(fc_rows) == 0) {
      message("        insufficient data")
      toc(); next
    }

    fc_dt <- rbindlist(fc_rows, fill=TRUE)
    all_forecasts[[paste(dv, cat, sep="|")]] <- fc_dt

    valid <- fc_dt[!is.na(actual) & !is.na(pred_final)]
    n_total  <- nrow(fc_dt)
    n_failed <- sum(fc_dt$method_used == "FAILED", na.rm=TRUE)
    n_na_pred <- sum(!is.na(fc_dt$actual) & is.na(fc_dt$pred_final))
    if (n_failed > 0 || n_na_pred > 0)
      message(sprintf("        [WARN] %d/%d windows failed, %d had NA predictions",
                      n_failed, n_total, n_na_pred))
    if (nrow(valid) >= 2L) {
      m_met <- reg_metrics(valid$actual, valid$pred_final)
      m_met$dep_var   <- dv
      m_met$dv_label  <- dv_label
      m_met$cat_label <- cat
      all_metrics[[paste(dv, cat, sep="|")]] <- m_met
      message(sprintf("        RMSE=%.3f  R²=%.3f  n=%d  TSCV_RMSE=%.3f",
                      m_met$rmse, m_met$r2_oos, m_met$n,
                      median(fc_dt$tscv_rmse, na.rm=TRUE)))
    } else if (nrow(valid) == 1L) {
      message(sprintf("        only 1 valid pair — RMSE skipped (need >= 2)"))
    } else {
      # Diagnose why all predictions failed
      fail_reasons <- unique(fc_dt$error_msg[!is.na(fc_dt$error_msg)])
      message(sprintf("        insufficient valid pairs (0/%d)", n_total))
      if (length(fail_reasons) > 0)
        message(sprintf("        failure reasons: %s",
                        paste(head(fail_reasons, 3), collapse=" | ")))
    }

    if (length(coef_rows) > 0)
      all_coefs[[paste(dv, cat, sep="|")]] <- rbindlist(coef_rows, fill=TRUE)

    # ── Post-model screen note ─────────────────────────────────
    # Printed after every model. Shows:
    #   1. Full coefficient table (last training window)
    #   2. Exit var gate outcomes
    #   3. Macro variable status — which were selected vs screened out
    #   4. OOS performance summary
    # ─────────────────────────────────────────────────────────────
    if (!is.null(last_res) && last_res$ok) {

      # ── Determine model type ──────────────────────────────────
      is_pure_arima <- last_res$method_used %in%
                         c("PURE_ARIMA_SPARSE", "PURE_ARIMA_NEARCONST",
                           "PURE_ARIMA_GLMNET_FALLBACK") ||
                       isTRUE((last_res$n_final %||% 0L) == 0L)

      # ── Rebuild coef_dt from actual final fit — always, unconditionally ──
      # build_coef_dt() handles ALL cases:
      #   - ARIMAX with xreg: shows AR/MA + macro/exit terms
      #   - Pure ARIMA with AR/MA terms: shows those terms
      #   - Pure ARIMA(0,d,0) — zero coef: returns sigma2 + placeholder row
      # We NEVER skip this step or fall back to a stored coef_dt, because
      # the stored version may belong to a pre-gate ARIMAX model.
      fit_for_print <- last_res$arimax_fit
      n_for_print   <- last_res$n_train %||% 20L
      cd <- if (!is.null(fit_for_print)) {
              build_coef_dt(fit_for_print, n_for_print,
                            last_res$sig_vars %||% character(0))
            } else if (!is.null(last_res$coef_dt) && nrow(last_res$coef_dt) > 0) {
              last_res$coef_dt   # only fall back if fit object is truly NULL
            } else {
              data.table(variable="[fit object unavailable]",
                         estimate=NA_real_, std_err=NA_real_,
                         t_stat=NA_real_, p_value=NA_real_, selected=FALSE)
            }

      # ── Header ───────────────────────────────────────────────
      cat(sprintf("\n%s\n", strrep("=", 80)))
      cat(sprintf("  MODEL %02d/%02d  |  %s  |  %s\n",
                  model_id, length(DEP_VARS)*length(cats), dv_label, cat))
      cat(sprintf("%s\n", strrep("=", 80)))
      if (is_pure_arima) {
        cat(sprintf("  *** PURE ARIMA (no xreg survived)  Reason: %s ***\n",
                    last_res$method_used %||% "unknown"))
      }
      cat(sprintf("  ARIMA order : ARIMA(%d,%d,%d)(%d,%d,%d)[4]  Method: %s\n",
                  last_res$arima_order["p"], last_res$arima_order["d"],
                  last_res$arima_order["q"],
                  last_res$arima_order["P"], last_res$arima_order["D"],
                  last_res$arima_order["Q"],
                  last_res$method_used))
      cat(sprintf("  LASSO sel.  : %d vars  ->  Final xreg: %d vars\n",
                  last_res$n_lasso_sel %||% 0L, last_res$n_final %||% 0L))
      cat(sprintf("  TSCV RMSE   : %.4f  |  In-sample Adj R2: %.4f\n",
                  last_res$tscv_rmse %||% NA_real_, last_res$adj_r2 %||% NA_real_))

      # ── Coefficient table ─────────────────────────────────────
      # Always prints — for pure ARIMA shows AR/MA/seasonal/drift terms
      if (!is.null(cd) && nrow(cd) > 0) {
        if (is_pure_arima) {
          cat(sprintf("\n  Coefficients (Pure ARIMA — no macro/exit xreg survived):\n"))
          cat("  Terms: ar=autoregressive  ma=moving-average  sar/sma=seasonal\n")
          cat("         intercept/mean=level  drift=trend  sigma2=residual variance\n")
        } else {
          cat(sprintf("\n  Coefficients (ARIMAX-TSCV):\n"))
        }
        cat(sprintf("  %-28s %12s %11s %8s %11s\n",
                    "variable", "Estimate", "Std.Error", "t value", "Pr(>|t|)"))
        cat(sprintf("  %s\n", strrep("-", 74)))
        for (i in seq_len(nrow(cd))) {
          r <- cd[i]
          # sigma2 row and placeholder rows: print without t/p columns
          is_meta <- is.na(r$p_value) && (r$variable == "sigma2" ||
                       grepl("^\\[ARIMA", r$variable))
          if (is_meta) {
            if (r$variable == "sigma2") {
              cat(sprintf("  %-28s %12.6f   (residual variance)\n",
                          r$variable,
                          if (is.finite(r$estimate)) r$estimate else NA_real_))
            } else {
              cat(sprintf("  %s\n", r$variable))
            }
            next
          }
          stars <- if (is.na(r$p_value)) "   " else
                   if (r$p_value < 0.001) "***" else
                   if (r$p_value < 0.01)  "** " else
                   if (r$p_value < 0.05)  "*  " else
                   if (r$p_value < 0.10)  ".  " else "   "
          is_exit_v <- r$variable %in% EXIT_VARS ||
                       (grepl(paste(EXIT_VARS_RAW, collapse="|"), r$variable, perl=TRUE) &&
                        grepl("_lag", r$variable, fixed=TRUE))
          vtype <- if (is_exit_v) "[EXIT]" else
                   if (grepl("^(ar|ma|sar|sma)[0-9]+$|^(intercept|drift|mean)$",
                              r$variable, ignore.case=TRUE, perl=TRUE)) "[ARIMA]" else
                   if (r$variable == "sigma2") "" else "[MACRO]"
          cat(sprintf("  %-28s %12.6f %11.6f %8.3f %11.6f %s  %s\n",
                      r$variable,
                      if (is.finite(r$estimate)) r$estimate else NA_real_,
                      if (is.finite(r$std_err))  r$std_err  else NA_real_,
                      if (is.finite(r$t_stat))   r$t_stat   else NA_real_,
                      if (is.finite(r$p_value))  r$p_value  else NA_real_,
                      stars, vtype))
        }
        cat(sprintf("  %s\n", strrep("-", 74)))
        cat("  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
        cat(sprintf("  Adjusted R-squared: %.4f  (in-sample, full training window)\n",
                    last_res$adj_r2 %||% NA_real_))
      } else {
        cat("  [WARN] Coefficient table is empty — fit object may be NULL\n")
      }

      # ── Exit variable note ────────────────────────────────────
      cat(sprintf("\n  EXIT VAR SUMMARY:\n"))
      if (is_pure_arima) {
        cat("    [PURE ARIMA MODEL] No xreg — all exit vars absent:\n")
        for (ev in names(EXIT_SIGN_PRIOR)) {
          cat(sprintf("    %-28s  (not in model — no xreg survived)\n", ev))
        }
      } else {
        fit_coef_names <- if (!is.null(last_res$arimax_fit) &&
                               !is.null(last_res$arimax_fit$coef))
                            names(last_res$arimax_fit$coef) else character(0)
        for (ev in names(EXIT_SIGN_PRIOR)) {
          if (!ev %in% fit_coef_names) {
            cat(sprintf("    %-28s  not in final model", ev))
            in_lasso <- if (!is.null(last_res$sig_vars)) ev %in% last_res$sig_vars else FALSE
            if (!in_lasso) {
              cat("  (LASSO/TSCV eliminated)\n")
            } else {
              cat("  (exit-gate: wrong sign or insignificant)\n")
            }
          } else {
            cf_ev  <- last_res$arimax_fit$coef[ev]
            vcov_m <- tryCatch(last_res$arimax_fit$var.coef, error=function(e) NULL)
            se_ev  <- if (!is.null(vcov_m)) {
                        sqrt(diag(vcov_m))[match(ev, rownames(vcov_m))]
                      } else { NA_real_ }
            pv_ev  <- if (is.finite(se_ev) && se_ev > 0) {
                        2 * pt(-abs(cf_ev/se_ev),
                               df=max(last_res$n_train - length(last_res$arimax_fit$coef), 1L))
                      } else { NA_real_ }
            prior   <- EXIT_SIGN_PRIOR[[ev]]
            sign_ok <- prior == 0L ||
                       (prior == -1L && cf_ev < 0) ||
                       (prior ==  1L && cf_ev > 0)
            cat(sprintf("    %-28s  est=%+.4f  p=%.4f  sign=%s  sig=%s\n",
                        ev, cf_ev, pv_ev %||% NA_real_,
                        if (sign_ok) "OK (correct)" else "WRONG",
                        if (is.finite(pv_ev %||% NA_real_) &&
                            (pv_ev %||% 1) < SIG_LEVEL) "sig" else "n.s."))
          }
        }
      }

      # ── Macro variable note ─────────────────────────────────
      cat(sprintf("\n  MACRO VAR SUMMARY (LASSO pipeline):\n"))
      cat(sprintf("  %s\n", strrep("-", 74)))

      # Recover full tracking from last_res
      lasso_mac_all   <- last_res$lasso_macro_all   %||% character(0)
      lasso_mac_coefs <- last_res$lasso_macro_coefs %||% numeric(0)
      tscv_drop_mac   <- last_res$tscv_dropped_macro %||% character(0)
      final_mac       <- last_res$in_final_macro     %||% character(0)

      # Get p-values and estimates from coef table for final macro vars
      get_coef_row <- function(vname) {
        if (is.null(cd) || nrow(cd) == 0) return(NULL)
        r <- cd[cd$variable == vname]
        if (nrow(r) == 0) return(NULL)
        r[1]
      }

      # Classify every candidate macro var and print one row per var
      # exactly like the exit var summary
      avail_m <- if (!is.null(last_res$X_train))
        length(intersect(macro_feats, colnames(last_res$X_train))) else 0L

      cat(sprintf("  Macro features into LASSO : %d  |  LASSO-selected (nonzero): %d  |  Final model: %d\n",
                  avail_m, length(lasso_mac_all), length(final_mac)))
      cat(sprintf("  %s\n", strrep("-", 74)))

      if (is_pure_arima) {
        cat("    [PURE ARIMA — no xreg, all macro absent]\n")
      } else if (avail_m == 0L) {
        cat("    >>> ALL macro vars dropped by prep_X before LASSO\n")
        cat("        (check FRB forward panel coverage for this category)\n")
      } else if (length(lasso_mac_all) == 0L) {
        cat("    LASSO zeroed all macro vars — none entered TSCV stage\n")
      } else {
        # Header row
        cat(sprintf("  %-34s  %-10s  %-12s  %-8s  %-8s  %s\n",
                    "Macro Variable", "LASSO coef", "Estimate", "p-value",
                    "Sig", "Stage reached"))
        cat(sprintf("  %s\n", strrep("-", 92)))

        # All macro vars that had nonzero LASSO coef — sorted by |LASSO coef|
        sorted_mac <- if (length(lasso_mac_coefs) > 0)
          names(sort(abs(lasso_mac_coefs), decreasing=TRUE))
        else lasso_mac_all

        for (mv in sorted_mac) {
          lc  <- if (mv %in% names(lasso_mac_coefs))
                   lasso_mac_coefs[[mv]] else NA_real_
          lc_s <- if (is.finite(lc)) sprintf("%+.4f", lc) else "     NA"

          if (mv %in% final_mac) {
            # ── Survived all the way to final model ──────────────
            cr <- get_coef_row(mv)
            est <- if (!is.null(cr)) cr$estimate %||% NA_real_ else NA_real_
            pv  <- if (!is.null(cr)) cr$p_value  %||% NA_real_ else NA_real_
            est_s <- if (is.finite(est)) sprintf("%+.4f", est) else "      NA"
            pv_s  <- if (is.finite(pv))  sprintf("%.4f",  pv)  else "    NA"
            sig_s <- if (is.finite(pv) && pv < SIG_LEVEL) {
              if (pv < 0.001) "*** " else if (pv < 0.01) "**  " else "*   "
            } else if (is.finite(pv)) ".   " else "    "
            stage <- "[FINAL MODEL]"
          } else if (mv %in% tscv_drop_mac) {
            # ── LASSO selected, TSCV eliminated ──────────────────
            est_s <- "      --"; pv_s <- "    --"; sig_s <- "    "
            stage <- "[LASSO only — TSCV eliminated]"
          } else {
            # ── LASSO selected but dropped for another reason ─────
            est_s <- "      --"; pv_s <- "    --"; sig_s <- "    "
            stage <- "[selected but dropped]"
          }

          cat(sprintf("  %-34s  %10s  %12s  %8s  %4s  %s\n",
                      mv, lc_s, est_s, pv_s, sig_s, stage))
        }
        cat(sprintf("  %s\n", strrep("-", 92)))
        cat("  Stage key: [FINAL MODEL]=survived LASSO+TSCV+gate  [LASSO only]=TSCV eliminated\n")
      }

      # ── OOS performance note ──────────────────────────────────
      if (nrow(valid) >= 2L) {
        cat(sprintf("\n  OOS PERFORMANCE (n=%d quarters):\n", m_met$n))
        cat(sprintf("    RMSE=%.4f  MAE=%.4f  OOS R²=%.4f\n",
                    m_met$rmse, m_met$mae, m_met$r2_oos))
        perf_note <- dplyr::case_when(
          isTRUE(m_met$r2_oos > 0.5)  ~ "  -> Good: model explains >50% of OOS variance",
          isTRUE(m_met$r2_oos > 0.2)  ~ "  -> Moderate: ARIMA dynamics dominant, macro adds limited lift",
          isTRUE(m_met$r2_oos > 0)    ~ "  -> Weak: minimal OOS predictability beyond naive mean",
          TRUE                          ~ "  -> Negative R2: worse than naive mean -- consider pure ARIMA"
        )
        cat(perf_note, "\n")
      }

      cat(sprintf("%s\n\n", strrep("─", 80)))
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

fwrite(forecasts_all, file.path(RESULT_DIR, "forecasts_3b.csv"))
fwrite(metrics_all,   file.path(RESULT_DIR, "metrics_3b.csv"))
fwrite(coefs_all,     file.path(RESULT_DIR, "coefficients_3b.csv"))

# Consolidate pure ARIMA OOS
arima_oos_all <- if (length(all_arima_oos)>0) {
                   rbindlist(all_arima_oos, fill=TRUE)
                 } else {
                   data.table()
                 }
if (nrow(arima_oos_all)>0)
  fwrite(arima_oos_all, file.path(RESULT_DIR, "arima_oos_benchmark_3b.csv"))

message("    Saved CSVs to results_3b/")

# ════════════════════════════════════════════════════════════
# 8. BACK-TRANSFORM: YoY% → LEVEL (count)
# ════════════════════════════════════════════════════════════
message("\n[5b] Back-transforming to level space...")

full_levels <- qtrly[, .(cat_label, date, ficu_assets, fiscu_assets)]
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

level_ficu  <- make_level_fc("yoy_ficu_assets_pct",  "ficu_assets")
level_fiscu <- make_level_fc("yoy_fiscu_assets_pct", "fiscu_assets")

level_fc <- rbindlist(list(level_ficu, level_fiscu), fill=TRUE)
level_fc[, date := as.numeric(date)]

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

fwrite(level_fc,      file.path(RESULT_DIR, "level_forecasts_3b.csv"))
fwrite(level_metrics, file.path(RESULT_DIR, "level_metrics_3b.csv"))
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

  last_obs <- as.numeric(max(qtrly[cat_label==cat_lbl, date], na.rm=TRUE))
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
      fq_yqtr <- as.numeric(fc_qtrs[i])
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
  count_col  <- if (dv=="yoy_ficu_assets_pct") "ficu_assets" else "fiscu_assets"
  last_level <- tail(qtrly[cat_label==cat_lbl & !is.na(get(count_col)),
                            get(count_col)], 1L)
  if (length(last_level) == 0L) last_level <- NA_real_

  running_level <- last_level

  for (i in seq_along(fc_qtrs)) {
    fq_yqtr <- as.numeric(fc_qtrs[i])
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
future_fc[, date := as.numeric(date)]
fwrite(future_fc, file.path(RESULT_DIR, "future_forecast_3b.csv"))
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
for (ser in c("ficu_assets","fiscu_assets")) {
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
         x="Quarter", y="Total Assets ($)") +
    theme_cu
  save_plot(p, sprintf("P5_%s_level.pdf", ser), w=14, h=9)
}

# ── P6: Future forecast (2025→2030) with historical context ──
for (ser in c("ficu_assets","fiscu_assets")) {
  dv_ser  <- if (ser=="ficu_assets") "yoy_ficu_assets_pct" else "yoy_fiscu_assets_pct"
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
         x="Quarter", y="Total Assets ($)") +
    theme_cu
  save_plot(p, sprintf("P6_%s_future.pdf", ser), w=16, h=10)
}

# ── P7: System totals (sum across all categories) ────────────
for (ser in c("ficu_assets","fiscu_assets")) {
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
         x="Quarter", y="Total Assets ($)") +
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

make_policy_chart_3b <- function(count_col, title_text, stem) {

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
      x = NULL, y = "Total Assets ($)"
    ) +
    policy_theme_3a() +
    guides(colour   = guide_legend(nrow = 2, byrow = TRUE),
           linetype = guide_legend(nrow = 2, byrow = TRUE))

  save_plot(p, stem, w = 14, h = 7)
  invisible(p)
}

# P9: FICU policy chart
if ("ficu_assets" %in% names(qtrly)) {
  message("    P9: FICU Assets policy chart...")
  make_policy_chart_3b(
    count_col  = "ficu_assets",
    title_text = "FICU Count — Historical Trend & ARIMAX Forecast by Asset Category",
    stem       = "P9_policy_ficu_assets.pdf"
  )
} else {
  message("    P9: skipped - ficu_assets not in data")
}

# P10: FISCU policy chart
if ("fiscu_assets" %in% names(qtrly)) {
  message("    P10: FISCU Assets policy chart...")
  make_policy_chart_3b(
    count_col  = "fiscu_assets",
    title_text = "FISCU Count — Historical Trend & ARIMAX Forecast by Asset Category",
    stem       = "P10_policy_fiscu_assets.pdf"
  )
} else {
  message("    P10: skipped - fiscu_assets not in data")
}

message("    Policy charts complete (P9, P10)")

# ════════════════════════════════════════════════════════════
# P11: FORECAST PANELS  (equivalent to P19 in Part 3 v4)
#
# One PDF per series (ficu / fiscu). 7 category panels arranged
# in a 3-column patchwork grid. Each panel shows:
#   • Historical actual from 2005 Q1           (solid blue)
#   • ARIMAX-TSCV rolling OOS forecast         (dashed blue, eval window)
#   • ARIMAX-TSCV future 2025Q4→2030Q4        (dashed red + red CI ribbon)
#   • Pure ARIMA future 2025Q4→2030Q4          (dotdash green + green CI)
#   • Amber shading over forecast region
#   • Dotted vline at last observed quarter
#   • 1yr / 3yr / 5yr horizon markers with labels
#   • ARIMA order in subtitle
# ════════════════════════════════════════════════════════════
message("\n[10] P11: Forecast panels (7-category patchwork)...")

if (!requireNamespace("patchwork", quietly=TRUE))
  install.packages("patchwork", repos="https://cloud.r-project.org", quiet=TRUE)
suppressPackageStartupMessages(library(patchwork))

# ── Config ────────────────────────────────────────────────────
PLOT_START_P11 <- zoo::as.yearqtr("2005 Q1")
plot_start_d   <- as.Date(PLOT_START_P11)
fc_end_d       <- as.Date(zoo::as.yearqtr(FC_END))   # 2030 Q4
LAST_OBS       <- max(qtrly$date, na.rm = TRUE)
last_obs_d     <- as.Date(zoo::as.yearqtr(LAST_OBS))

HORIZON_MARKS  <- c(4L, 12L, 20L)         # quarters: 1yr / 3yr / 5yr
HORIZON_LABS   <- c("1-yr", "3-yr", "5-yr")
horizon_dates  <- last_obs_d + HORIZON_MARKS * (365.25 / 4)

HORIZON_QTR    <- max(as.integer(round((as.numeric(FC_END) - as.numeric(LAST_OBS)) * 4)), 20L)

p11_dir <- file.path(PLOT_DIR, "P11_forecast_panels")
if (!dir.exists(p11_dir)) dir.create(p11_dir, recursive = TRUE)

# ── Series metadata ───────────────────────────────────────────
series_meta_3b <- list(
  ficu_assets  = list(label = "FICU Total Assets",  hist_col = "ficu_assets",
                      short = "ficu_assets",  dep_var = "yoy_ficu_assets_pct"),
  fiscu_assets = list(label = "FISCU Total Assets", hist_col = "fiscu_assets",
                      short = "fiscu_assets", dep_var = "yoy_fiscu_assets_pct")
)

# ── Pure ARIMA baseline on full history (comparison benchmark) ─
# Fit auto.arima() on observed level series (no xreg).
# Lets the reader compare: does adding macro xreg improve on
# a simple univariate ARIMA benchmark?
message("    Fitting pure ARIMA baselines (benchmark)...")
arima_base_list <- list()

for (sr in names(series_meta_3b)) {
  sm   <- series_meta_3b[[sr]]
  hcol <- sm$hist_col

  for (cat in cats) {
    key      <- paste(sr, cat, sep = "|")
    cat_hist <- qtrly[cat_label == cat & date >= PLOT_START_P11 &
                        !is.na(get(hcol)) & is.finite(get(hcol)) & get(hcol) > 0]
    setorderv(cat_hist, "date")
    if (nrow(cat_hist) < 12L) next

    y_raw <- as.numeric(cat_hist[[hcol]])
    y_raw[!is.finite(y_raw)] <- NA_real_
    if (sum(!is.na(y_raw)) < 12L) {
      message(sprintf("        [P11 SKIP ARIMA] %s | %s — only %d finite obs",
                      sr, cat, sum(!is.na(y_raw))))
      next
    }
    # Use start year/quarter from actual data range
    min_yq_h <- zoo::as.yearqtr(min(cat_hist$date, na.rm = TRUE))
    y_ts <- ts(y_raw, frequency = 4L,
               start = c(as.integer(format(min_yq_h, "%Y")),
                         as.integer(format(min_yq_h, "%q"))))

    fit_a <- tryCatch(
      forecast::auto.arima(y_ts, stepwise = TRUE, approximation = TRUE,
                           max.p = 3L, max.q = 2L, max.P = 1L, max.Q = 1L),
      error = function(e) NULL)
    if (is.null(fit_a)) {
      message(sprintf("        [P11 ARIMA FAIL] %s | %s", sr, cat))
      next
    }

    fc_a <- tryCatch(
      forecast::forecast(fit_a, h = HORIZON_QTR, level = 95),
      error = function(e) NULL)
    if (is.null(fc_a)) next

    fc_dates <- as.numeric(LAST_OBS) + seq_len(HORIZON_QTR) / 4

    arima_base_list[[key]] <- data.table(
      series      = sr,
      cat_label   = cat,
      date        = fc_dates,
      arima_point = as.numeric(fc_a$mean),
      arima_lo95  = as.numeric(fc_a$lower[, 1L]),
      arima_hi95  = as.numeric(fc_a$upper[, 1L]),
      arima_order = paste0("ARIMA(",
                     paste(forecast::arimaorder(fit_a), collapse = ","), ")")
    )
  }
}

arima_base_all <- if (length(arima_base_list) > 0) {
                    rbindlist(arima_base_list, fill = TRUE)
                  } else {
                    data.table()
                  }
message(sprintf("    Pure ARIMA benchmark: %d models fitted", length(arima_base_list)))
if (length(arima_base_list) > 0) {
  message(sprintf("    Keys: %s", paste(names(arima_base_list), collapse=", ")))
} else {
  message("    [WARN] arima_base_list is empty — green lines will not appear in P11")
}

# ── Build 7-panel patchwork per series ───────────────────────
for (sr in names(series_meta_3b)) {
  sm    <- series_meta_3b[[sr]]
  hcol  <- sm$hist_col
  label <- sm$label

  # Historical actuals
  hist_dt <- qtrly[date >= PLOT_START_P11 & !is.na(get(hcol)),
                   .(date = as.numeric(date), cat_label, actual = get(hcol))]
  hist_dt[, date_d := as.Date(zoo::as.yearqtr(as.numeric(date)))]

  # ARIMAX-TSCV rolling OOS (evaluation window)
  oos_dt <- level_fc[series == sr & !is.na(pred_level),
                     .(cat_label, date, pred_level, lo95_level, hi95_level)]
  oos_dt[, date_d := as.Date(zoo::as.yearqtr(as.numeric(date)))]

  # ARIMAX-TSCV future forecast
  fut_dt <- future_fc[series == sr & !is.na(pred_level),
                      .(cat_label, date, pred_level, lo95_level, hi95_level)]
  fut_dt[, date_d := as.Date(zoo::as.yearqtr(as.numeric(date)))]

  # Pure ARIMA benchmark
  ar_dt <- if (nrow(arima_base_all) > 0) {
              arima_base_all[series == sr]
          } else {
              data.table()
          }
  if (nrow(ar_dt) > 0) ar_dt[, date_d := as.Date(zoo::as.yearqtr(as.numeric(date)))]

  panel_plots <- list()

  for (cat in cats) {
    h_c <- hist_dt[cat_label == cat]
    o_c <- oos_dt[cat_label  == cat]
    f_c <- fut_dt[cat_label  == cat]
    a_c <- if (nrow(ar_dt) > 0) ar_dt[cat_label == cat] else data.table()

    if (nrow(h_c) == 0) next
    arima_str <- if (nrow(a_c) > 0) a_c$arima_order[1L] else "N/A"

    p <- ggplot() +
      # Amber forecast shading from last observation
      annotate("rect",
               xmin = last_obs_d, xmax = fc_end_d,
               ymin = -Inf,       ymax = Inf,
               fill = "#fff3cd",  alpha = 0.45) +
      # Historical actual line
      geom_line(data = h_c,
                aes(x = date_d, y = actual, colour = "Actual"),
                linewidth = 0.9) +
      # ARIMAX OOS CI ribbon
      {if (nrow(o_c) > 0 && any(!is.na(o_c$lo95_level))) {
         geom_ribbon(data = o_c,
                     aes(x = date_d, ymin = lo95_level, ymax = hi95_level),
                     fill = "#2171b5", alpha = 0.13)
       } else { list() }} +
      # ARIMAX OOS line
      {if (nrow(o_c) > 0) {
         geom_line(data = o_c,
                   aes(x = date_d, y = pred_level,
                       colour = "ARIMAX (rolling OOS)"),
                   linewidth = 0.85, linetype = "dashed")
       } else { list() }} +
      # ARIMAX future CI ribbon
      {if (nrow(f_c) > 0 && any(!is.na(f_c$lo95_level))) {
         geom_ribbon(data = f_c,
                     aes(x = date_d, ymin = lo95_level, ymax = hi95_level),
                     fill = "#d62728", alpha = 0.13)
       } else { list() }} +
      # ARIMAX future line
      {if (nrow(f_c) > 0) {
         geom_line(data = f_c,
                   aes(x = date_d, y = pred_level,
                       colour = "ARIMAX-TSCV (2025Q4\u21922030Q4)"),
                   linewidth = 0.9, linetype = "dashed")
       } else { list() }} +
      # Pure ARIMA CI ribbon
      {if (nrow(a_c) > 0) {
         geom_ribbon(data = a_c,
                     aes(x = date_d, ymin = arima_lo95, ymax = arima_hi95),
                     fill = "#2ca02c", alpha = 0.10)
       } else { list() }} +
      # Pure ARIMA line
      {if (nrow(a_c) > 0) {
         geom_line(data = a_c,
                   aes(x = date_d, y = arima_point,
                       colour = "Pure ARIMA (2025Q4\u21922030Q4)"),
                   linewidth = 0.85, linetype = "dotdash")
       } else { list() }} +
      # Dotted vline at last observed data point
      geom_vline(xintercept = last_obs_d,
                 linetype = "dotted", colour = "grey40", linewidth = 0.5) +
      # Horizon marker lines (+1yr / +3yr / +5yr)
      geom_vline(xintercept = horizon_dates,
                 linetype = "longdash", colour = "#7f7f7f", linewidth = 0.35) +
      annotate("text",
               x = horizon_dates, y = Inf,
               label = HORIZON_LABS,
               vjust = 1.4, hjust = -0.15, size = 2.5, colour = "#555555") +
      scale_x_date(limits     = c(plot_start_d, fc_end_d),
                   date_labels = "%Y",
                   date_breaks = "2 years") +
      scale_y_continuous(labels = scales::comma) +
      scale_colour_manual(
        values = c(
          "Actual"                            = "#1f77b4",
          "ARIMAX (rolling OOS)"              = "#7b9ec7",
          "ARIMAX-TSCV (2025Q4\u21922030Q4)" = "#d62728",
          "Pure ARIMA (2025Q4\u21922030Q4)"  = "#2ca02c"),
        name = NULL) +
      labs(title    = cat,
           subtitle = sprintf("ARIMA benchmark: %s  |  95%% CI shaded", arima_str),
           x = NULL, y = label) +
      theme_cu +
      theme(legend.position = "bottom",
            legend.text     = element_text(size = 7),
            plot.title      = element_text(size = 10, face = "bold"),
            plot.subtitle   = element_text(size = 7.5, colour = "grey40"))

    panel_plots[[cat]] <- p
  }

  if (length(panel_plots) == 0) {
    message(sprintf("    [P11 SKIP] %s — no panels built", sr)); next
  }

  combined <- patchwork::wrap_plots(panel_plots, ncol = 3) +
    patchwork::plot_annotation(
      title    = sprintf("%s \u2014 Actual vs ARIMAX (rolling OOS) vs ARIMA (future)",
                         label),
      subtitle = sprintf(
        paste("Historical: 2005Q1 \u2192 %s  |  Amber region: forecast 2025Q4\u20132030Q4",
              " |  Horizon markers: +1yr / +3yr / +5yr from %s"),
        as.character(LAST_OBS),
        as.character(LAST_OBS)
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 9, colour = "grey30")
      )
    )

  pdf_path <- file.path(p11_dir,
                         sprintf("P11_%s_forecast_panels.pdf", sm$short))
  tryCatch({
    pdf_dev <- tryCatch(
      { grDevices::cairo_pdf(pdf_path, width = 18, height = 13); "cairo" },
      error = function(e) { grDevices::pdf(pdf_path, width = 18, height = 13); "pdf" })
    print(combined)
    grDevices::dev.off()
    message(sprintf("    Saved: P11_%s_forecast_panels.pdf  [%s]",
                    sm$short, pdf_dev))
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    message(sprintf("    [WARN] P11 failed for %s: %s", sr, e$message))
  })
}

# ════════════════════════════════════════════════════════════
# P12: LEVEL-VALUE HEATMAPS  (equivalent to P20/P21 in Part 3)
#
# One PDF per series (P12a = ficu, P12b = fiscu).
# Three stacked panels per PDF:
#   Top    — ARIMAX-TSCV forecast level values (blue gradient)
#   Middle — Pure ARIMA forecast level values   (blue gradient)
#   Bottom — ARIMAX minus ARIMA difference      (diverging:
#             blue = ARIMAX higher, red = ARIMA higher)
#
# X axis = forecast quarter (2025Q4 → 2030Q4)
# Y axis = 7 asset-size categories
# First column = last observed actual (visual anchor)
# Cell label   = count value printed inside tile
# ════════════════════════════════════════════════════════════
message("\n[11] P12: Level-value heatmaps (ARIMAX-TSCV vs ARIMA)...")

for (sr in names(series_meta_3b)) {
  sm    <- series_meta_3b[[sr]]
  label <- sm$label
  hcol  <- sm$hist_col

  fmt_cell <- function(x)
    ifelse(is.na(x), "", scales::comma(round(x)))

  # Last observed actual — anchor column in both panels
  last_actual_dt <- qtrly[date == LAST_OBS & !is.na(get(hcol)),
                           .(cat_label, date = as.numeric(date), value = get(hcol))]
  last_actual_dt[, date_d    := as.Date(zoo::as.yearqtr(as.numeric(date)))]
  last_actual_dt[, label_txt := fmt_cell(value)]

  # ── ARIMAX-TSCV future levels ─────────────────────────────
  arimax_hm <- future_fc[series == sr & !is.na(pred_level),
                          .(cat_label, date = as.numeric(date), value = pred_level)]
  arimax_hm[, date_d    := as.Date(zoo::as.yearqtr(as.numeric(date)))]
  arimax_hm[, label_txt := fmt_cell(value)]

  arimax_hm_full <- safe_rbind(
    last_actual_dt[, .(cat_label, date, date_d, value, label_txt)],
    arimax_hm)

  # ── Pure ARIMA benchmark levels ───────────────────────────
  ar_hm <- if (nrow(arima_base_all) > 0) {
              arima_base_all[series == sr & !is.na(arima_point),
                              .(cat_label, date = as.numeric(date), value = arima_point)]
          } else {
              data.table()
          }
  if (nrow(ar_hm) > 0) {
    ar_hm[, date_d    := as.Date(zoo::as.yearqtr(as.numeric(date)))]
    ar_hm[, label_txt := fmt_cell(value)]
  }

  ar_hm_full <- if (nrow(ar_hm) > 0) {
    safe_rbind(
      last_actual_dt[, .(cat_label, date, date_d, value, label_txt)],
      ar_hm)
  } else {
    data.table()
  }

  if (nrow(arimax_hm) == 0 && nrow(ar_hm) == 0) {
    message(sprintf("    [HM SKIP] %s — no forecast data", sr)); next
  }

  # ── Difference: ARIMAX minus pure ARIMA ──────────────────
  diff_hm <- merge(
    arimax_hm[, .(cat_label, date, arimax_val = value)],
    ar_hm[,    .(cat_label, date, arima_val  = value)],
    by = c("cat_label", "date"), all = TRUE)
  diff_hm[, value     := arimax_val - arima_val]
  diff_hm[, label_txt := fmt_cell(value)]
  diff_hm[, date_d    := as.Date(zoo::as.yearqtr(as.numeric(date)))]

  # Category factor (largest category at top)
  cat_order <- rev(sort(unique(
    c(arimax_hm$cat_label, ar_hm$cat_label))))

  arimax_hm_full[, cat_f := factor(cat_label, levels = cat_order)]
  if (nrow(ar_hm_full) > 0)
    ar_hm_full[, cat_f := factor(cat_label, levels = cat_order)]
  diff_hm[, cat_f := factor(cat_label, levels = cat_order)]

  # Heatmap tile builder
  make_hm_tile <- function(dt, title_txt, diverging = FALSE) {
    if (nrow(dt) == 0)
      return(ggplot() + labs(title = title_txt) + theme_cu)

    vr  <- range(dt$value, na.rm = TRUE)
    rng <- max(vr[2] - vr[1], 1e-9)
    dt[, text_col := ifelse(
      !is.na(value) & (value - vr[1]) / rng > 0.55,
      "white", "grey15")]

    fill_sc <- if (diverging) {
      scale_fill_gradient2(low     = "#2166ac", mid = "#f7f7f7",
                           high    = "#d6604d", midpoint = 0,
                           na.value = "grey85", name = label,
                           labels  = scales::comma)
    } else {
      scale_fill_gradient(low      = "#deebf7", high = "#08306b",
                          na.value = "grey85",  name = label,
                          labels   = scales::comma)
    }

    ggplot(dt, aes(x = date_d, y = cat_f, fill = value)) +
      geom_tile(colour = "white", linewidth = 0.25) +
      geom_text(aes(label = label_txt, colour = text_col),
                size = 2.3, fontface = "plain") +
      fill_sc +
      scale_colour_identity() +
      scale_x_date(date_labels = "%Y-Q%q",
                   date_breaks = "1 year",
                   expand      = expansion(mult = c(0.01, 0.01))) +
      labs(title = title_txt, x = NULL, y = NULL) +
      theme_cu +
      theme(axis.text.x       = element_text(angle = 55, hjust = 1, size = 7),
            axis.text.y       = element_text(size = 9),
            plot.title        = element_text(size = 11, face = "bold"),
            legend.position   = "right",
            legend.key.height = unit(1.2, "cm"))
  }

  p_arimax_hm <- make_hm_tile(
    arimax_hm_full,
    sprintf("ARIMAX-TSCV Forecast \u2014 %s", label))

  p_arima_hm  <- make_hm_tile(
    ar_hm_full,
    sprintf("Pure ARIMA Forecast \u2014 %s", label))

  p_diff_hm   <- make_hm_tile(
    diff_hm,
    sprintf("ARIMAX \u2212 ARIMA \u2014 %s  (blue = ARIMAX higher, red = ARIMA higher)",
            label),
    diverging = TRUE)

  p_hm <- (p_arimax_hm / p_arima_hm / p_diff_hm) +
    patchwork::plot_annotation(
      title    = sprintf(
        "%s \u2014 Forecast Heatmap: ARIMAX-TSCV vs Pure ARIMA vs Difference",
        label),
      subtitle = paste(
        sprintf("First column = last actual (%s).", as.character(LAST_OBS)),
        "Count values.",
        "Diverging panel: blue = ARIMAX-TSCV > ARIMA, red = ARIMA > ARIMAX-TSCV."
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, colour = "grey30")
      )
    )

  hm_pnum <- if (sr == "ficu_assets") "P12a" else "P12b"
  hm_path <- file.path(PLOT_DIR,
                sprintf("%s_heatmap_%s_arimax_vs_arima.pdf",
                        hm_pnum, sm$short))

  tryCatch({
    pdf_dev <- tryCatch(
      { grDevices::cairo_pdf(hm_path, width = 16, height = 15); "cairo" },
      error = function(e) { grDevices::pdf(hm_path, width = 16, height = 15); "pdf" })
    print(p_hm)
    grDevices::dev.off()
    message(sprintf("    Saved: %s  [%s]", basename(hm_path), pdf_dev))
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    message(sprintf("    [WARN] Heatmap failed for %s: %s", sr, e$message))
  })
}

message("    P11 + P12 complete")

# ════════════════════════════════════════════════════════════
# P13: OOS PERFORMANCE COMPARISON PDF
#      Pure ARIMA (benchmark) vs ARIMAX-TSCV (final model)
#
# One PDF with multiple pages / sections:
#   Page 1 — Summary metrics table: for each of the 14 models
#             RMSE, MAE, MAPE, OOS R² for both ARIMA and ARIMAX
#             side by side, with ΔR² column (ARIMAX gain over ARIMA)
#             Colour: green = ARIMAX wins, red = ARIMA wins
#
#   Page 2 — RMSE comparison bar chart: grouped by model,
#             ARIMA vs ARIMAX bars
#
#   Page 3+ — Scatter: actual vs predicted per model,
#              ARIMA (left column) vs ARIMAX (right column)
#              for all 14 models
# ════════════════════════════════════════════════════════════
message("\n[13] P13: OOS comparison PDF (ARIMA benchmark vs ARIMAX-TSCV)...")

oos_pdf_path <- file.path(RESULT_DIR, "P13_oos_comparison_arima_vs_arimax_3b.pdf")

if (nrow(arima_oos_all) > 0 && nrow(forecasts_all) > 0) {

  # ── Build comparison table ──────────────────────────────────
  # Compute OOS metrics for both models per dep_var × cat_label
  arima_met_list  <- list()
  arimax_met_list <- list()

  for (dv in names(DEP_VARS)) {
    for (cat in cats) {
      key <- paste(dv, cat, sep="|")

      # Pure ARIMA OOS
      ar_sub <- arima_oos_all[dep_var==dv & cat_label==cat &
                                !is.na(actual) & !is.na(arima_pred)]
      if (nrow(ar_sub) >= 2L) {
        m <- reg_metrics(ar_sub$actual, ar_sub$arima_pred)
        arima_met_list[[key]] <- data.table(
          dep_var=dv, cat_label=cat,
          model    = paste(DEP_VARS[[dv]]$short, cat, sep="|"),
          dv_label = DEP_VARS[[dv]]$label,
          method   = "Pure ARIMA",
          rmse     = m$rmse, mae=m$mae, mape=m$mape, r2_oos=m$r2_oos, n=m$n
        )
      }

      # ARIMAX-TSCV OOS
      ax_sub <- forecasts_all[dep_var==dv & cat_label==cat &
                                !is.na(actual) & !is.na(pred_final)]
      if (nrow(ax_sub) >= 2L) {
        m <- reg_metrics(ax_sub$actual, ax_sub$pred_final)
        arimax_met_list[[key]] <- data.table(
          dep_var=dv, cat_label=cat,
          model    = paste(DEP_VARS[[dv]]$short, cat, sep="|"),
          dv_label = DEP_VARS[[dv]]$label,
          method   = "ARIMAX-TSCV",
          rmse     = m$rmse, mae=m$mae, mape=m$mape, r2_oos=m$r2_oos, n=m$n
        )
      }
    }
  }

  comp_ar  <- if (length(arima_met_list)>0)
                rbindlist(arima_met_list,  fill=TRUE) else data.table()
  comp_ax  <- if (length(arimax_met_list)>0)
                rbindlist(arimax_met_list, fill=TRUE) else data.table()

  # Wide comparison table with delta columns
  comp_wide <- merge(
    comp_ar[,  .(dep_var, cat_label, model, dv_label,
                 ar_rmse=rmse, ar_mae=mae, ar_r2=r2_oos, ar_n=n)],
    comp_ax[,  .(dep_var, cat_label,
                 ax_rmse=rmse, ax_mae=mae, ax_r2=r2_oos)],
    by=c("dep_var","cat_label"), all=TRUE
  )
  comp_wide[, delta_rmse := ar_rmse - ax_rmse]   # positive = ARIMAX better
  comp_wide[, delta_r2   := ax_r2   - ar_r2]     # positive = ARIMAX better
  comp_wide[, winner     := fcase(
    is.na(delta_r2),       "Insufficient data",
    delta_r2 > 0.01,       "ARIMAX-TSCV",
    delta_r2 < -0.01,      "Pure ARIMA",
    default =              "Comparable"
  )]
  setorderv(comp_wide, c("dep_var","cat_label"))

  fwrite(comp_wide, file.path(RESULT_DIR, "P13_oos_comparison_table_3b.csv"))

  # ── Open PDF device ─────────────────────────────────────────
  pdf_ok <- tryCatch({
    grDevices::pdf(oos_pdf_path, width=16, height=10, onefile=TRUE)
    TRUE
  }, error=function(e) { message("[WARN] Cannot open P13 PDF: ", e$message); FALSE })

  if (pdf_ok) {

    # ── PAGE 1: Summary metrics table ──────────────────────────
    if (nrow(comp_wide) > 0) {

      # Colour winner column
      win_col <- ifelse(comp_wide$winner=="ARIMAX-TSCV", "#2ca02c",
                 ifelse(comp_wide$winner=="Pure ARIMA",  "#d62728",
                                                          "#888888"))

      tbl_data <- comp_wide[, .(
        Model      = model,
        Series     = dv_label,
        Category   = cat_label,
        `AR RMSE`  = round(ar_rmse, 3),
        `AX RMSE`  = round(ax_rmse, 3),
        `ΔRMSE`    = round(delta_rmse, 3),
        `AR R²`    = round(ar_r2, 3),
        `AX R²`    = round(ax_r2, 3),
        `ΔR²`      = round(delta_r2, 3),
        `N`        = ar_n,
        Winner     = winner
      )]

      p_tbl <- ggplot(data.frame(x=1), aes(x=x)) +
        annotate("text", x=0.5, y=0.5,
                 label=paste(
                   "OOS Performance: Pure ARIMA vs ARIMAX-TSCV\n",
                   sprintf("%-28s  %7s  %7s  %7s  %7s  %7s  %7s  %s",
                           "Model", "AR RMSE", "AX RMSE", "ΔRMSE",
                           "AR R²", "AX R²", "ΔR²", "Winner"),
                   strrep("-", 85),
                   paste(apply(tbl_data, 1, function(r)
                     sprintf("%-28s  %7s  %7s  %7s  %7s  %7s  %7s  %s",
                             r["Model"], r["AR RMSE"], r["AX RMSE"], r["ΔRMSE"],
                             r["AR R²"], r["AX R²"], r["ΔR²"], r["Winner"])
                   ), collapse="\n"),
                   sep="\n"),
                 hjust=0, vjust=1, size=3.2, family="mono") +
        theme_void() +
        labs(title="OOS Comparison: Pure ARIMA vs ARIMAX-TSCV",
             subtitle="ΔRMSE = AR − AX (positive = ARIMAX better)  |  ΔR² = AX − AR (positive = ARIMAX better)") +
        theme(plot.title    = element_text(face="bold", size=14),
              plot.subtitle = element_text(size=10, colour="grey40"))
      print(p_tbl)
    }

    # ── PAGE 2: RMSE comparison bar chart ──────────────────────
    comp_long <- rbindlist(list(
      comp_ar[,  .(model, dep_var, cat_label, dv_label, rmse, r2_oos, method="Pure ARIMA")],
      comp_ax[,  .(model, dep_var, cat_label, dv_label, rmse, r2_oos, method="ARIMAX-TSCV")]
    ), fill=TRUE)

    if (nrow(comp_long) > 0) {
      p_rmse <- ggplot(comp_long,
                       aes(x=cat_label, y=rmse, fill=method)) +
        geom_col(position="dodge", width=0.7) +
        geom_text(aes(label=round(rmse,2)),
                  position=position_dodge(width=0.7),
                  vjust=-0.4, size=2.8) +
        facet_wrap(~dv_label, ncol=1, scales="free_y") +
        scale_fill_manual(values=c("Pure ARIMA"="#7b9ec7",
                                   "ARIMAX-TSCV"="#d62728"),
                          name="Model") +
        labs(title="OOS RMSE: Pure ARIMA vs ARIMAX-TSCV",
             subtitle="Lower RMSE = better out-of-sample accuracy  |  YoY% units",
             x="Asset Category", y="OOS RMSE (YoY%)") +
        theme_cu +
        theme(axis.text.x=element_text(angle=35, hjust=1))
      print(p_rmse)

      # OOS R² comparison
      p_r2 <- ggplot(comp_long[!is.na(r2_oos)],
                     aes(x=cat_label, y=r2_oos, fill=method)) +
        geom_col(position="dodge", width=0.7) +
        geom_hline(yintercept=0, linetype="dashed", colour="grey50") +
        geom_text(aes(label=round(r2_oos,3)),
                  position=position_dodge(width=0.7),
                  vjust=-0.4, size=2.8) +
        facet_wrap(~dv_label, ncol=1, scales="free_y") +
        scale_fill_manual(values=c("Pure ARIMA"="#7b9ec7",
                                   "ARIMAX-TSCV"="#d62728"),
                          name="Model") +
        labs(title="OOS R²: Pure ARIMA vs ARIMAX-TSCV",
             subtitle="Higher R² = better  |  Negative R² = worse than naive mean",
             x="Asset Category", y="OOS R²") +
        theme_cu +
        theme(axis.text.x=element_text(angle=35, hjust=1))
      print(p_r2)
    }

    # ── PAGE 3+: Actual vs Predicted scatter per model ─────────
    # 2-column layout: left = Pure ARIMA, right = ARIMAX-TSCV
    for (dv in names(DEP_VARS)) {
      for (cat in cats) {

        ar_sub <- arima_oos_all[dep_var==dv & cat_label==cat &
                                  !is.na(actual) & !is.na(arima_pred)]
        ax_sub <- forecasts_all[dep_var==dv & cat_label==cat &
                                  !is.na(actual) & !is.na(pred_final)]
        if (nrow(ar_sub)==0 && nrow(ax_sub)==0) next

        model_label <- sprintf("%s | %s", DEP_VARS[[dv]]$label, cat)

        # Shared axis limits
        all_vals <- c(ar_sub$actual, ar_sub$arima_pred,
                      ax_sub$actual, ax_sub$pred_final)
        lims <- range(all_vals, na.rm=TRUE)
        lims <- lims + diff(lims) * c(-0.05, 0.05)

        # Pure ARIMA scatter
        p_ar_sc <- if (nrow(ar_sub) >= 2L) {
          ar_m  <- reg_metrics(ar_sub$actual, ar_sub$arima_pred)
          ggplot(ar_sub, aes(x=actual, y=arima_pred)) +
            geom_abline(intercept=0, slope=1, colour="grey60", linetype="dashed") +
            geom_point(colour="#7b9ec7", size=2.2, alpha=0.8) +
            geom_smooth(method="lm", se=FALSE, colour="#1f77b4", linewidth=0.8) +
            coord_equal(xlim=lims, ylim=lims) +
            labs(title=sprintf("Pure ARIMA  |  RMSE=%.3f  R²=%.3f",
                               ar_m$rmse, ar_m$r2_oos),
                 x="Actual (YoY%)", y="Predicted (YoY%)") +
            theme_cu
        } else {
          ggplot() + labs(title="Pure ARIMA — insufficient data") + theme_cu
        }

        # ARIMAX-TSCV scatter
        p_ax_sc <- if (nrow(ax_sub) >= 2L) {
          ax_m  <- reg_metrics(ax_sub$actual, ax_sub$pred_final)
          ggplot(ax_sub, aes(x=actual, y=pred_final)) +
            geom_abline(intercept=0, slope=1, colour="grey60", linetype="dashed") +
            geom_point(colour="#d62728", size=2.2, alpha=0.8) +
            geom_smooth(method="lm", se=FALSE, colour="#a61717", linewidth=0.8) +
            coord_equal(xlim=lims, ylim=lims) +
            labs(title=sprintf("ARIMAX-TSCV  |  RMSE=%.3f  R²=%.3f",
                               ax_m$rmse, ax_m$r2_oos),
                 x="Actual (YoY%)", y="Predicted (YoY%)") +
            theme_cu
        } else {
          ggplot() + labs(title="ARIMAX-TSCV — insufficient data") + theme_cu
        }

        # Time-series overlay: actual vs both models
        ar_ts <- ar_sub[, .(date, actual, pred=arima_pred, model="Pure ARIMA")]
        ax_ts <- ax_sub[, .(date, actual, pred=pred_final,  model="ARIMAX-TSCV")]
        ts_dt <- rbindlist(list(ar_ts, ax_ts), fill=TRUE)
        ts_dt[, date_num := as.numeric(date)]

        p_ts <- ggplot(ts_dt) +
          geom_line(aes(x=date_num, y=actual), colour="black",
                    linewidth=0.9, data=unique(ts_dt[,.(date_num,actual)])) +
          geom_line(aes(x=date_num, y=pred, colour=model, linetype=model),
                    linewidth=0.8) +
          geom_hline(yintercept=0, linetype="dotted", colour="grey50") +
          scale_colour_manual(values=c("Pure ARIMA"="#7b9ec7",
                                       "ARIMAX-TSCV"="#d62728"), name="") +
          scale_linetype_manual(values=c("Pure ARIMA"="dashed",
                                          "ARIMAX-TSCV"="solid"), name="") +
          scale_x_continuous(
            labels=function(x) as.character(zoo::as.yearqtr(x)),
            breaks=pretty(ts_dt$date_num, n=5)) +
          labs(title="Time Series: Actual vs Both Models",
               x="Quarter", y="YoY %") +
          theme_cu

        # Assemble 3-panel page using patchwork
        page <- (p_ar_sc | p_ax_sc) / p_ts +
          patchwork::plot_annotation(
            title    = model_label,
            subtitle = "Left: Pure ARIMA  |  Right: ARIMAX-TSCV  |  Bottom: Time series overlay",
            theme    = theme(plot.title    = element_text(face="bold", size=13),
                             plot.subtitle = element_text(size=9, colour="grey40"))
          )
        print(page)
      }
    }

    grDevices::dev.off()
    message(sprintf("    Saved: P13_oos_comparison_arima_vs_arimax.pdf"))

  }  # end if pdf_ok

} else {
  message("    [SKIP] P13 — arima_oos_all or forecasts_all is empty")
}


# ════════════════════════════════════════════════════════════
# P14: ALL-MODELS REGRESSION OUTPUT PDF
#      One page per model (all 14: 2 targets × 7 categories).
#      Shows the screen-print regression output in visual form.
#
# Each page — 2×2 layout:
#   Top-left  : Actual vs Fitted (in-sample) + OOS 1-step forecasts
#   Top-right : Coefficient table (estimates, SE, t, p, stars)
#               — for pure ARIMA: AR/MA/seasonal terms
#               — for ARIMAX-TSCV: macro/exit xreg + ARIMA terms
#   Bot-left  : Residuals over time (loess trend)
#   Bot-right : Stats box: order, method, Adj R², AIC, OOS metrics
# ════════════════════════════════════════════════════════════
message("\n[14] P14: All-models regression output PDF (all 14 models)...")

p14_path <- file.path(RESULT_DIR, "P14_all_models_regression_3b.pdf")
pdf_ok14 <- tryCatch({
  grDevices::pdf(p14_path, width=16, height=11, onefile=TRUE)
  TRUE
}, error=function(e) { message("[WARN] P14 PDF open failed: ", e$message); FALSE })

if (pdf_ok14) {

  pg_count <- 0L

  for (dv in names(DEP_VARS)) {
    dv_lbl <- DEP_VARS[[dv]]$label

    for (cat_lbl in cats) {
      key    <- paste(dv, cat_lbl, sep="|")
      fit_ob <- all_fits[[key]]$fit

      # Skip if no fit was stored (category had no valid windows)
      if (is.null(fit_ob)) {
        message(sprintf("    [P14 SKIP] %s | %s — no fit stored", dv_lbl, cat_lbl))
        next
      }

      fc_sub  <- forecasts_all[dep_var == dv & cat_label == cat_lbl]
      cat_dt  <- qtrly[cat_label == cat_lbl]
      setorderv(cat_dt, "date")

      # Series history (non-NA)
      hist_dt <- cat_dt[!is.na(get(dv)),
                         .(date = as.numeric(date), y = get(dv))]

      # Fitted + residuals from stored fit
      fitted_v <- tryCatch(as.numeric(fitted(fit_ob)),   error=function(e) NULL)
      resid_v  <- tryCatch(as.numeric(residuals(fit_ob)), error=function(e) NULL)

      # OOS metrics
      valid_fc <- fc_sub[!is.na(actual) & !is.na(pred_final)]
      oos_met  <- if (nrow(valid_fc) >= 2L) {
                    reg_metrics(valid_fc$actual, valid_fc$pred_final)
                  } else {
                    list(rmse=NA_real_, mae=NA_real_, r2_oos=NA_real_, n=nrow(valid_fc))
                  }

      # Model metadata
      n_final_v  <- all_fits[[key]]$sig_vars
      n_final_n  <- length(n_final_v)
      method_lbl <- {
        ms <- unique(fc_sub$method_used[!is.na(fc_sub$method_used)])
        if (length(ms) > 0) ms[1L] else "UNKNOWN"
      }
      is_pure <- method_lbl %in% c("PURE_ARIMA_SPARSE","PURE_ARIMA_NEARCONST",
                                    "PURE_ARIMA_GLMNET_FALLBACK") ||
                 n_final_n == 0L
      type_lbl <- if (is_pure) "PURE ARIMA" else "ARIMAX-TSCV"

      arima_ord <- tryCatch(forecast::arimaorder(fit_ob), error=function(e) NULL)
      ord_str   <- if (!is.null(arima_ord))
                     sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[4]",
                             as.integer(arima_ord["p"]), as.integer(arima_ord["d"]),
                             as.integer(arima_ord["q"]), as.integer(arima_ord["P"]),
                             as.integer(arima_ord["D"]), as.integer(arima_ord["Q"]))
                   else "ARIMA(?,?,?)"

      adj_r2_v <- tryCatch(
        compute_adj_r2(fit_ob, as.numeric(na.omit(cat_dt[[dv]]))),
        error=function(e) NA_real_)
      aic_v <- tryCatch(AIC(fit_ob), error=function(e) NA_real_)
      n_tr  <- all_fits[[key]]$n_train %||% NA_integer_

      # Coefficient table from fit
      cd_p14 <- build_coef_dt(fit_ob, n_tr %||% 20L, n_final_v)

      # ── Panel 1: Actual vs Fitted + OOS ────────────────────
      plot_base <- copy(hist_dt)[, type := "Actual"]

      if (!is.null(fitted_v) && length(fitted_v) == nrow(hist_dt)) {
        fit_dt  <- data.table(date=hist_dt$date, y=fitted_v, type="Fitted")
        combo   <- rbindlist(list(plot_base, fit_dt), fill=TRUE)
      } else {
        combo   <- plot_base
      }

      if (nrow(valid_fc) > 0) {
        oos_pts <- data.table(date=as.numeric(valid_fc$date),
                              y=valid_fc$pred_final, type="OOS Forecast")
        combo   <- rbindlist(list(combo, oos_pts), fill=TRUE)
      }

      p1 <- ggplot(combo, aes(x=date, y=y, colour=type, linetype=type)) +
        geom_line(linewidth=0.8, na.rm=TRUE) +
        geom_point(data=combo[type=="OOS Forecast"], size=2.5, na.rm=TRUE) +
        geom_hline(yintercept=0, linetype="dotted", colour="grey55") +
        scale_colour_manual(
          values=c("Actual"="black","Fitted"="#2166ac","OOS Forecast"="#d73027"),
          name="") +
        scale_linetype_manual(
          values=c("Actual"="solid","Fitted"="dashed","OOS Forecast"="dotted"),
          name="") +
        scale_x_continuous(
          labels=function(x) as.character(zoo::as.yearqtr(x)),
          breaks=pretty(combo$date, n=6)) +
        labs(title="Actual vs Fitted (in-sample) + OOS Forecast",
             x=NULL, y="YoY %") +
        theme_cu + theme(legend.position="bottom")

      # ── Panel 2: Coefficient table ──────────────────────────
      if (!is.null(cd_p14) && nrow(cd_p14) > 0) {
        coef_lines <- apply(cd_p14, 1L, function(r) {
          vname <- r["variable"]
          est   <- suppressWarnings(as.numeric(r["estimate"]))
          se    <- suppressWarnings(as.numeric(r["std_err"]))
          tv    <- suppressWarnings(as.numeric(r["t_stat"]))
          pv    <- suppressWarnings(as.numeric(r["p_value"]))

          # sigma2 / placeholder rows
          if (is.na(pv) || grepl("^\\[ARIMA", vname)) {
            if (vname == "sigma2")
              return(sprintf("  %-22s  %10.5f   (residual variance)",
                             vname, if(is.finite(est)) est else NA_real_))
            else
              return(sprintf("  %s", vname))
          }

          stars <- if (pv < 0.001) "***" else if (pv < 0.01) "**" else
                   if (pv < 0.05)  "*"   else if (pv < 0.10) "."  else ""

          is_exit_v <- grepl(paste(c(EXIT_VARS, EXIT_VARS_RAW), collapse="|"),
                             vname, perl=TRUE)
          vtype <- if (is_exit_v) "[EXIT]" else
                   if (grepl("^(ar|ma|sar|sma)[0-9]+$|^(intercept|drift|mean)$",
                             vname, ignore.case=TRUE, perl=TRUE)) "[ARIMA]" else "[MACRO]"

          sprintf("  %-22s  %+9.5f  %9.5f  %7.3f  %8.5f %s %s",
                  vname,
                  if(is.finite(est)) est else NA_real_,
                  if(is.finite(se))  se  else NA_real_,
                  if(is.finite(tv))  tv  else NA_real_,
                  if(is.finite(pv))  pv  else NA_real_,
                  stars, vtype)
        })

        coef_header <- paste0(
          sprintf("  %-22s  %9s  %9s  %7s  %8s\n", "variable","Estimate","Std.Err","t","p"),
          "  ", strrep("-", 70)
        )
        coef_body <- paste(c(coef_header, coef_lines,
                             "  Signif: *** p<.001  ** p<.01  * p<.05  . p<.1"),
                           collapse="\n")
      } else {
        coef_body <- "  [no coefficients available]"
      }

      coef_title <- if (is_pure) "Coefficients (Pure ARIMA)" else
                    sprintf("Coefficients (ARIMAX-TSCV,  %d xreg)", n_final_n)

      p2 <- ggplot(data.frame(x=0,y=0), aes(x,y)) +
        annotate("text", x=0.02, y=0.97, label=coef_body,
                 hjust=0, vjust=1, size=2.9, family="mono") +
        expand_limits(x=c(0,1), y=c(0,1)) +
        labs(title=coef_title) +
        theme_void() +
        theme(plot.title    = element_text(face="bold", size=10, hjust=0),
              plot.margin   = margin(6,6,6,6),
              plot.background = element_rect(fill="grey98", colour="grey80"))

      # ── Panel 3: Residuals over time ────────────────────────
      if (!is.null(resid_v) && length(resid_v) > 3L) {
        r_dt <- data.table(
          date  = hist_dt$date[seq_along(resid_v)],
          resid = resid_v)
        p3 <- ggplot(r_dt, aes(x=date, y=resid)) +
          geom_hline(yintercept=0, colour="grey50", linetype="dashed") +
          geom_line(colour="#636363", linewidth=0.6) +
          geom_point(colour="#636363", size=1.2, alpha=0.7) +
          geom_smooth(method="loess", se=FALSE, colour="#e6550d",
                      linewidth=0.8, span=0.5, na.rm=TRUE) +
          scale_x_continuous(
            labels=function(x) as.character(zoo::as.yearqtr(x)),
            breaks=pretty(r_dt$date, n=6)) +
          labs(title="Residuals over time  (orange = loess)",
               subtitle="Flat near zero = well-specified | Trend = misspecification",
               x=NULL, y="Residual") +
          theme_cu
      } else {
        p3 <- ggplot() + labs(title="Residuals — unavailable") + theme_cu
      }

      # ── Panel 4: Stats summary box ──────────────────────────
      tscv_med <- median(fc_sub$tscv_rmse, na.rm=TRUE)

      stats_txt <- paste0(
        sprintf("Model  : %s  |  %s\n",    dv_lbl, cat_lbl),
        sprintf("Type   : %s\n",            type_lbl),
        sprintf("Order  : %s\n",            ord_str),
        sprintf("Method : %s\n",            method_lbl),
        sprintf("N train: %s\n",
                if (!is.na(n_tr)) as.character(n_tr) else "?"),
        sprintf("xreg   : %d var(s)\n\n",   n_final_n),
        sprintf("In-sample Adj R2 : %s\n",
                if(is.finite(adj_r2_v)) sprintf("%.4f", adj_r2_v) else "NA"),
        sprintf("AIC              : %s\n",
                if(is.finite(aic_v))   sprintf("%.2f",  aic_v)    else "NA"),
        sprintf("TSCV RMSE (med)  : %s\n\n",
                if(is.finite(tscv_med)) sprintf("%.4f", tscv_med)  else "NA"),
        sprintf("OOS n    : %d quarters\n", oos_met$n),
        sprintf("OOS RMSE : %s\n",
                if(is.finite(oos_met$rmse))   sprintf("%.4f", oos_met$rmse)   else "NA"),
        sprintf("OOS MAE  : %s\n",
                if(is.finite(oos_met$mae))    sprintf("%.4f", oos_met$mae)    else "NA"),
        sprintf("OOS R2   : %s\n",
                if(is.finite(oos_met$r2_oos)) sprintf("%.4f", oos_met$r2_oos) else "NA")
      )

      p4 <- ggplot(data.frame(x=0,y=0), aes(x,y)) +
        annotate("text", x=0.02, y=0.97, label=stats_txt,
                 hjust=0, vjust=1, size=3.2, family="mono") +
        expand_limits(x=c(0,1), y=c(0,1)) +
        labs(title="Model Statistics") +
        theme_void() +
        theme(plot.title    = element_text(face="bold", size=10, hjust=0),
              plot.margin   = margin(6,6,6,6),
              plot.background = element_rect(fill="grey98", colour="grey80"))

      # ── Assemble 2×2 page ───────────────────────────────────
      page14 <- (p1 | p2) / (p3 | p4) +
        patchwork::plot_annotation(
          title    = sprintf("[%s]  %s  |  %s",
                             type_lbl, dv_lbl, cat_lbl),
          subtitle = sprintf("%s  |  Adj R²=%s  |  OOS RMSE=%s  |  xreg=%d",
                             ord_str,
                             if(is.finite(adj_r2_v)) sprintf("%.4f",adj_r2_v) else "NA",
                             if(is.finite(oos_met$rmse)) sprintf("%.4f",oos_met$rmse) else "NA",
                             n_final_n),
          theme = theme(
            plot.title    = element_text(face="bold", size=13),
            plot.subtitle = element_text(size=9, colour="grey35"))
        )

      print(page14)
      pg_count <- pg_count + 1L
      message(sprintf("    P14: page %d  — %s | %s  [%s]",
                      pg_count, dv_lbl, cat_lbl, type_lbl))
    }
  }

  grDevices::dev.off()
  message(sprintf("    Saved: %s  (%d pages)", basename(p14_path), pg_count))

} else {
  message("    [SKIP] P14 — no valid fits found")
}
# ════════════════════════════════════════════════════════════
# P15: STL DECOMPOSITION PDF
#      Decomposes the fitted ARIMA series (full training window)
#      into Trend, Seasonal, Cyclical (HP filter) and Irregular
#      components. One page per model (all 14 models).
#
# Layout (4 stacked panels per page):
#   1. Observed  — raw YoY % series
#   2. Trend     — loess/STL trend component
#   3. Seasonal  — within-year pattern (STL seasonal)
#   4. Irregular — remainder after trend + seasonal
#   + Cyclical panel (HP filter cycle, lambda=1600 for quarterly)
# ════════════════════════════════════════════════════════════
message("\n[15] P15: STL decomposition PDF (all 14 models)...")

p15_path <- file.path(RESULT_DIR, "P15_stl_decomposition_3b.pdf")
pdf_ok15 <- tryCatch({
  grDevices::pdf(p15_path, width=12, height=14, onefile=TRUE)
  TRUE
}, error=function(e) { message("[WARN] P15 PDF open failed: ", e$message); FALSE })

if (pdf_ok15) {

  for (dv in names(DEP_VARS)) {
    dv_label <- DEP_VARS[[dv]]$label

    for (cat in cats) {
      key     <- paste(dv, cat, sep="|")
      cat_dt  <- qtrly[cat_label == cat]
      setorderv(cat_dt, "date")

      # Raw series (non-NA)
      y_raw <- cat_dt[[dv]]
      dates <- as.numeric(cat_dt$date)
      valid_idx <- which(!is.na(y_raw))

      if (length(valid_idx) < 12L) {
        message(sprintf("    [P15 SKIP] %s|%s — fewer than 12 obs", dv, cat))
        next
      }

      y_v    <- y_raw[valid_idx]
      d_v    <- dates[valid_idx]
      min_yq <- zoo::as.yearqtr(min(d_v))
      y_ts   <- ts(y_v, frequency=4L,
                   start=c(as.integer(format(min_yq,"%Y")),
                           as.integer(format(min_yq,"%q"))))

      # ── STL decomposition ──────────────────────────────────
      stl_out <- tryCatch(
        stl(y_ts, s.window="periodic", robust=TRUE),
        error=function(e) NULL)

      if (is.null(stl_out)) {
        message(sprintf("    [P15 WARN] STL failed for %s|%s", dv, cat))
        next
      }

      stl_comp <- as.data.frame(stl_out$time.series)
      stl_dt   <- data.table(
        date       = d_v,
        observed   = y_v,
        trend      = stl_comp$trend,
        seasonal   = stl_comp$seasonal,
        remainder  = stl_comp$remainder
      )

      # ── HP filter for cyclical component ──────────────────
      # lambda=1600 is standard for quarterly data (Hodrick-Prescott)
      hp_out <- tryCatch({
        if (requireNamespace("mFilter", quietly=TRUE)) {
          hp <- mFilter::hpfilter(y_v, freq=1600, type="lambda")
          list(trend=as.numeric(hp$trend), cycle=as.numeric(hp$cycle))
        } else {
          # Manual HP filter via sparse linear algebra
          n   <- length(y_v)
          lam <- 1600
          I   <- diag(n)
          D   <- diff(I, differences=2)
          hp_trend <- as.numeric(solve(I + lam * t(D) %*% D, y_v))
          list(trend=hp_trend, cycle=y_v - hp_trend)
        }
      }, error=function(e) NULL)

      if (!is.null(hp_out)) {
        stl_dt[, hp_cycle := hp_out$cycle]
        stl_dt[, hp_trend := hp_out$trend]
      }

      # ── Build panels ───────────────────────────────────────
      theme_decomp <- theme_cu +
        theme(axis.title.x = element_blank(),
              panel.grid.minor = element_blank())

      fmt_x <- function(x) as.character(zoo::as.yearqtr(x))

      p_obs <- ggplot(stl_dt, aes(x=date, y=observed)) +
        geom_line(colour="#333333", linewidth=0.8) +
        geom_hline(yintercept=0, linetype="dotted", colour="grey60") +
        labs(title="Observed  (raw YoY %)", y="YoY %") +
        scale_x_continuous(labels=fmt_x, breaks=pretty(d_v, n=7)) +
        theme_decomp

      p_trend <- ggplot(stl_dt, aes(x=date, y=trend)) +
        geom_line(colour="#1a6faf", linewidth=0.9) +
        geom_hline(yintercept=0, linetype="dotted", colour="grey60") +
        labs(title="Trend  (STL loess)", y="YoY %") +
        scale_x_continuous(labels=fmt_x, breaks=pretty(d_v, n=7)) +
        theme_decomp

      p_seas <- ggplot(stl_dt, aes(x=date, y=seasonal)) +
        geom_line(colour="#2ca02c", linewidth=0.8) +
        geom_hline(yintercept=0, linetype="dotted", colour="grey60") +
        labs(title="Seasonal  (within-year pattern)", y="Seasonal") +
        scale_x_continuous(labels=fmt_x, breaks=pretty(d_v, n=7)) +
        theme_decomp

      # Cyclical panel — HP cycle if available, else STL remainder
      if (!is.null(hp_out)) {
        p_cycle <- ggplot(stl_dt, aes(x=date, y=hp_cycle)) +
          geom_line(colour="#d6470e", linewidth=0.8) +
          geom_ribbon(aes(ymin=pmin(hp_cycle,0), ymax=pmax(hp_cycle,0)),
                      fill="#d6470e", alpha=0.15) +
          geom_hline(yintercept=0, linetype="dotted", colour="grey60") +
          labs(title="Cyclical  (HP filter, lambda=1600)", y="Cycle") +
          scale_x_continuous(labels=fmt_x, breaks=pretty(d_v, n=7)) +
          theme_decomp
      } else {
        p_cycle <- ggplot() + labs(title="Cyclical (HP filter unavailable)") + theme_cu
      }

      p_irreg <- ggplot(stl_dt, aes(x=date, y=remainder)) +
        geom_hline(yintercept=0, linetype="dashed", colour="grey50") +
        geom_segment(aes(xend=date, yend=0), colour="#9467bd", linewidth=0.6,
                     alpha=0.7) +
        geom_point(colour="#9467bd", size=1.5, alpha=0.8) +
        labs(title="Irregular  (STL remainder after trend + seasonal)", y="Remainder") +
        scale_x_continuous(labels=fmt_x, breaks=pretty(d_v, n=7)) +
        theme_decomp

      # ── Variance shares ────────────────────────────────────
      var_obs  <- var(stl_dt$observed,  na.rm=TRUE)
      var_trd  <- var(stl_dt$trend,     na.rm=TRUE)
      var_seas  <- var(stl_dt$seasonal, na.rm=TRUE)
      var_irr  <- var(stl_dt$remainder, na.rm=TRUE)
      var_cyc  <- if (!is.null(hp_out)) var(stl_dt$hp_cycle, na.rm=TRUE) else NA_real_

      pct <- function(v) if (is.finite(var_obs) && var_obs>0) sprintf("%.1f%%", 100*v/var_obs) else "NA"

      subtitle_txt <- paste0(
        sprintf("Trend: %s  |  Seasonal: %s  |  ",    pct(var_trd), pct(var_seas)),
        sprintf("Cyclical: %s  |  Irregular: %s",      pct(var_cyc), pct(var_irr)),
        "  — share of total series variance"
      )

      method_lbl <- if (!is.null(all_fits[[key]]$method_used))
                      all_fits[[key]]$method_used else "?"
      arima_lbl  <- if (!is.null(all_fits[[key]]$fit)) {
                      tryCatch({
                        ord <- forecast::arimaorder(all_fits[[key]]$fit)
                        sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[4]",
                                ord["p"],ord["d"],ord["q"],
                                ord["P"],ord["D"],ord["Q"])
                      }, error=function(e) "ARIMA(?)")
                    } else { "ARIMA(?)" }

      page15 <- (p_obs / p_trend / p_seas / p_cycle / p_irreg) +
        patchwork::plot_annotation(
          title    = sprintf("STL Decomposition  |  %s  |  %s", dv_label, cat),
          subtitle = paste0(arima_lbl, "  |  ", method_lbl, "\n", subtitle_txt),
          theme    = theme(
            plot.title    = element_text(face="bold", size=13),
            plot.subtitle = element_text(size=8.5, colour="grey35"))
        )

      print(page15)
      message(sprintf("    P15: %s | %s", dv_label, cat))
    }
  }

  grDevices::dev.off()
  message(sprintf("    Saved: %s", basename(p15_path)))
}

# ════════════════════════════════════════════════════════════
# P16: PURE ARIMA FORECAST HEATMAPS
#      Three panels per series (P16a = ficu, P16b = fiscu):
#        a) Mean forecast  (point estimate)
#        b) Lower 95% CI
#        c) Upper 95% CI
#
# X axis = forecast quarter (LAST_OBS+1 → 2030Q4)
# Y axis = 7 asset-size categories
# First column = last observed actual (visual anchor)
# Cell label   = count value inside tile
# Colour scale = blue gradient (consistent across panels)
# ════════════════════════════════════════════════════════════
message("\n[16] P16: Pure ARIMA forecast heatmaps (mean / lo95 / hi95)...")

if (nrow(arima_base_all) == 0) {
  message("    [SKIP] P16 — arima_base_all is empty")
} else {

  for (sr in names(series_meta_3b)) {
    sm    <- series_meta_3b[[sr]]
    label <- sm$label
    hcol  <- sm$hist_col

    fmt_cell <- function(x) ifelse(is.na(x), "", scales::comma(round(x)))

    # Last observed actual — anchor column
    last_act <- qtrly[date == LAST_OBS & !is.na(get(hcol)),
                       .(cat_label, date = as.numeric(date), value = get(hcol))]
    last_act[, date_d    := as.Date(zoo::as.yearqtr(as.numeric(date)))]
    last_act[, label_txt := fmt_cell(value)]

    ar_sub <- arima_base_all[series == sr]
    if (nrow(ar_sub) == 0) {
      message(sprintf("    [P16 SKIP] %s — no data", sr)); next
    }
    ar_sub[, date_d := as.Date(zoo::as.yearqtr(as.numeric(date)))]

    cat_order <- rev(sort(unique(ar_sub$cat_label)))

    # Helper: build one heatmap panel
    make_arima_hm <- function(value_col, title_txt) {
      dt <- ar_sub[, .(cat_label, date_d, value = get(value_col))]
      dt[, label_txt := fmt_cell(value)]

      # Prepend anchor column
      anch <- last_act[cat_label %in% dt$cat_label,
                        .(cat_label, date_d, value, label_txt)]
      dt_full <- safe_rbind(anch, dt)
      dt_full[, cat_f := factor(cat_label, levels=cat_order)]

      vr  <- range(dt_full$value, na.rm=TRUE)
      rng <- max(vr[2] - vr[1], 1e-9)
      dt_full[, text_col := ifelse(
        !is.na(value) & (value - vr[1]) / rng > 0.55,
        "white", "grey15")]

      ggplot(dt_full, aes(x=date_d, y=cat_f, fill=value)) +
        geom_tile(colour="white", linewidth=0.25) +
        geom_text(aes(label=label_txt, colour=text_col),
                  size=2.3, fontface="plain") +
        scale_fill_gradient(low="#deebf7", high="#08306b",
                            na.value="grey85", name=label,
                            labels=scales::comma) +
        scale_colour_identity() +
        scale_x_date(date_labels="%Y-Q%q",
                     date_breaks="1 year",
                     expand=expansion(mult=c(0.01,0.01))) +
        labs(title=title_txt, x=NULL, y=NULL) +
        theme_cu +
        theme(axis.text.x       = element_text(angle=55, hjust=1, size=7),
              axis.text.y       = element_text(size=9),
              plot.title        = element_text(size=11, face="bold"),
              legend.position   = "right",
              legend.key.height = unit(1.2,"cm"))
    }

    p_mean <- make_arima_hm("arima_point",
                             sprintf("Pure ARIMA — Mean Forecast  |  %s", label))
    p_lo   <- make_arima_hm("arima_lo95",
                             sprintf("Pure ARIMA — Lower 95%% CI  |  %s", label))
    p_hi   <- make_arima_hm("arima_hi95",
                             sprintf("Pure ARIMA — Upper 95%% CI  |  %s", label))

    p16 <- (p_mean / p_lo / p_hi) +
      patchwork::plot_annotation(
        title    = sprintf("%s — Pure ARIMA Forecast Heatmap (Mean / Lo95 / Hi95)",
                           label),
        subtitle = paste(
          sprintf("First column = last actual (%s).", as.character(LAST_OBS)),
          "Level counts (CUs). Blue gradient = higher count.",
          "CI based on 95% forecast interval."
        ),
        theme = theme(
          plot.title    = element_text(face="bold", size=13),
          plot.subtitle = element_text(size=9, colour="grey30"))
      )

    hm_pnum <- if (sr == "ficu_assets") "P16a" else "P16b"
    hm_path <- file.path(PLOT_DIR,
                  sprintf("%s_pure_arima_heatmap_%s.pdf",
                          hm_pnum, sm$short))

    tryCatch({
      pdf_dev <- tryCatch(
        { grDevices::cairo_pdf(hm_path, width=16, height=15); "cairo" },
        error=function(e) { grDevices::pdf(hm_path, width=16, height=15); "pdf" })
      print(p16)
      grDevices::dev.off()
      message(sprintf("    Saved: %s  [%s]", basename(hm_path), pdf_dev))
    }, error=function(e) {
      try(grDevices::dev.off(), silent=TRUE)
      message(sprintf("    [WARN] P16 failed for %s: %s", sr, e$message))
    })
  }
}


message("\n[12] Done. All outputs in results_3b/ and plots_3b/")
message(sprintf("    %d models  |  %d forecast rows  |  %d future rows",
                length(all_fits), nrow(forecasts_all), nrow(future_fc)))
message(paste("    Plots:",
              "P1-P8 (diagnostics) |",
              "P9/P10 (policy) |",
              "P11 (7-panel forecast) |",
              "P12a/P12b (heatmaps) |",
              "P13 (OOS comparison PDF) |",
              "P14 (All-models regression PDF) |",
              "P15 (STL decomposition) |",
              "P16a/P16b (Pure ARIMA heatmaps)"))
