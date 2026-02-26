############################################################
# PART 3 — LASSO + POST-LASSO OLS  (Regression Pipeline)
#
# Models 21 outcomes:
#   (a) yoy_ficu_pct   — YoY % change in FICU count
#   (b) yoy_fiscu_pct  — YoY % change in FISCU count
#   (c) ln_assets_tot  — log total assets
#   ... each fitted for asset categories 1-7 separately
#
# Strategy:
#   1. Rolling-window Lasso  (glmnet, alpha=1)
#      - Fixed initial window 2005 Q1 → TRAIN_END (end of COVID)
#      - Roll one quarter at a time; re-fit LASSO each roll
#      - Lambda chosen by 10-fold CV (lambda.1se for parsimony)
#   2. Post-LASSO OLS on each rolling window
#      - Take LASSO-selected variables
#      - Fit OLS; keep only statistically significant (p < SIG_LEVEL)
#        AND economically sensible coefficients
#      - If OLS drops all vars, fall back to Lasso prediction
#   3. Back-transform forecasts to level space:
#        yoy_ficu_pct  → ficu_count   (anchor × (1+yoy/100))
#        yoy_fiscu_pct → fiscu_count  (anchor × (1+yoy/100))
#        ln_assets_tot → total_assets (exp(ln_pred))
#   4. Evaluation metrics: RMSE, MAE, MAPE, OOS R² (transformed + level)
#   5. Plots: actual vs predicted, residuals, coefficient paths,
#             significance heatmap, rolling R², feature selection,
#             level-space time series, scatter, error, system totals
#
# Outputs:
#   plots_regression/  (PDF — 15 plot files)
#   results_regression/  (CSV: forecasts, metrics, coefficients,
#                               forecasts_levels, metrics_levels)
#
# Runtime: ~5-20 min depending on features & window count
############################################################

DEBUG_MODE <- TRUE   # ← set FALSE for full production run

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(glmnet)
  library(ggplot2)
  library(scales)
  library(httr)
  library(tictoc)
  library(stringr)
  library(stargazer)
})
set.seed(42)
options(scipen = 999)

# Null-coalescing helper used throughout
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nchar(a[1]) > 0) a else b

# ════════════════════════════════════════════════════════════
# CONFIG
# ════════════════════════════════════════════════════════════

# Rolling-window initial training period ends here
# (this is the FIRST test quarter — model trains on everything before it)
TRAIN_END <- zoo::as.yearqtr("2021 Q1")   # end of COVID era

# Significance level for post-LASSO OLS variable retention
SIG_LEVEL <- 0.10    # keep vars with p < 0.10 (10% significance)

# Minimum number of observations needed to fit a model
MIN_OBS <- 20

# Correlation cutoff for pre-filtering collinear features
CORR_CUT <- 0.92

# Minimum fraction of non-NA values required to include a feature
MIN_NONMISS <- 0.70

# Debug: only roll over last N quarters for speed
DEBUG_ROLL_Q <- 6

# ── ntfy ─────────────────────────────────────────────────────
NTFY_TOPIC   <- "your-unique-topic-name"   # ← CHANGE THIS
NTFY_ENABLED <- TRUE
notify <- function(title, msg, tags = NULL) {
  if (!NTFY_ENABLED) return(invisible(NULL))
  tryCatch({
    h <- list(Title = title)
    if (!is.null(tags)) h$Tags <- paste(tags, collapse = ",")
    httr::POST(paste0("https://ntfy.sh/", NTFY_TOPIC),
               body = msg, encode = "raw",
               do.call(httr::add_headers, h))
  }, error = function(e) NULL)
}

# ── Output folders ────────────────────────────────────────────
PLOT_DIR   <- "plots_regression"
RESULT_DIR <- "results_regression"
for (d in c(PLOT_DIR, RESULT_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}
message(sprintf("Plots   → %s/", PLOT_DIR))
message(sprintf("Results → %s/", RESULT_DIR))

# ── Plot helper ───────────────────────────────────────────────
save_plot <- function(p, filename, width = 10, height = 6) {
  path <- file.path(PLOT_DIR, paste0(filename, ".pdf"))
  tryCatch(
    ggplot2::ggsave(path, plot = p, width = width, height = height,
                    device = cairo_pdf),
    error = function(e)
      ggplot2::ggsave(path, plot = p, width = width, height = height,
                      device = "pdf")
  )
  invisible(path)
}

# ── Colour palette consistent across plots ────────────────────
CAT_COLOURS <- c(
  "1_Less_10M"   = "#1f77b4", "2_10M_50M"    = "#ff7f0e",
  "3_50M_100M"   = "#2ca02c", "4_100M_500M"  = "#d62728",
  "5_500M_1B"    = "#9467bd", "6_1B_10B"     = "#8c564b",
  "7_10B_Plus"   = "#e377c2"
)

t0 <- proc.time()
tic("Part 3 total")
message("=======================================================")
message(sprintf("PART 3 [%s]  %s",
                if (DEBUG_MODE) "DEBUG" else "PROD",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("=======================================================")
notify("Part 3 Started",
       sprintf("[%s] %s", if (DEBUG_MODE) "DEBUG" else "PROD",
               format(Sys.time(), "%H:%M")), tags = "rocket")

# ════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ════════════════════════════════════════════════════════════
message("\n[1] Loading qtrly_enriched.rds...")
if (!file.exists("qtrly_enriched.rds"))
  stop("qtrly_enriched.rds not found. Run Part 1 first.")

qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)
message(sprintf("    %s rows x %s cols",
                format(nrow(qtrly), big.mark = ","),
                format(ncol(qtrly), big.mark = ",")))

# Category label map (consistent with Part 1 & 2)
CAT_MAP <- c("1"="1_Less_10M", "2"="2_10M_50M",  "3"="3_50M_100M",
             "4"="4_100M_500M","5"="5_500M_1B",  "6"="6_1B_10B",
             "7"="7_10B_Plus")
if (!"cat_label" %in% names(qtrly))
  qtrly[, cat_label := CAT_MAP[as.character(categories)]]

# Required columns check
required <- c("date", "categories", "cat_label",
               "yoy_ficu_pct", "yoy_fiscu_pct", "ln_assets_tot")
missing_req <- setdiff(required, names(qtrly))
if (length(missing_req) > 0) {
  # ln_assets_tot might be named differently
  if ("assets_tot" %in% names(qtrly) && !"ln_assets_tot" %in% names(qtrly)) {
    qtrly[, ln_assets_tot := log(pmax(assets_tot, 1))]
    message("    Created ln_assets_tot from assets_tot")
    missing_req <- setdiff(missing_req, "ln_assets_tot")
  }
  if (length(missing_req) > 0)
    stop(sprintf("Missing required cols: %s", paste(missing_req, collapse = ", ")))
}

setorderv(qtrly, c("categories", "date"))
all_quarters <- sort(unique(qtrly$date))
cats         <- sort(unique(qtrly$cat_label))
message(sprintf("    Categories: %s", paste(cats, collapse = " | ")))
message(sprintf("    Date range: %s to %s  (%d quarters)",
                as.character(min(all_quarters)),
                as.character(max(all_quarters)),
                length(all_quarters)))

# ════════════════════════════════════════════════════════════
# 2. DEFINE DEPENDENT VARIABLES AND FEATURE CANDIDATES
# ════════════════════════════════════════════════════════════
message("\n[2] Defining targets and features...")

# Three dependent variables
DEP_VARS <- list(
  yoy_ficu_pct  = list(label = "FICU YoY % Change",      short = "ficu"),
  yoy_fiscu_pct = list(label = "FISCU YoY % Change",     short = "fiscu"),
  ln_assets_tot = list(label = "Log Total Assets",        short = "lnassets")
)

# Feature candidates: engineered, stationary, interpretable
# Excludes raw levels and target leakage
FEAT_PAT <- paste(
  "^yoy_(?!ficu|fiscu)",      # YoY transforms but NOT our targets
  "^qoq_",                     # QoQ transforms
  "_lag[0-9]",                 # lagged levels
  "_rmean[0-9]",               # rolling means
  "_rsd[0-9]",                 # rolling SDs (volatility)
  "_accel$",                   # momentum / acceleration
  "_cyc$",                     # cyclical deviations
  "^regime_",                  # regime dummies
  "^time_idx$",                # time trend
  "^qtrs_from_",               # distance from regimes
  "^q[1-4]$",                  # seasonal dummies
  # Macro series (levels ok — they're stationary rates/spreads)
  "^fedfunds$", "^fedfunds_cycle$", "^fedfunds_chg$",
  "^gs2$",      "^gs10$",      "^yield_curve$",  "^yield_curve_inv$",
  "^hy_spread$","^baa_spread$","^credit_tightness$",
  "^unrate$",   "^cpi_yoy$",   "^real_rate$",    "^spread_2s10s$",
  "^mortgage30$","^oil_qoq_pct$","^oil_shock$",
  "^fomc_regime$","^hike_run$",
  # M&A / exit rates
  "merger_rate$","liquid_rate$","exit_rate$","exit_roll4$",
  "^share_",                   # cross-category share features
  sep = "|"
)

all_num_cols <- names(qtrly)[vapply(qtrly, is.numeric, logical(1))]
FEATS_ALL <- grep(FEAT_PAT, all_num_cols, value = TRUE, perl = TRUE)

# Hard exclusions — anything that IS or directly encodes the target
HARD_EXCL <- c(
  "yoy_ficu_pct", "yoy_fiscu_pct", "ln_assets_tot",
  "qoq_ficu_pct", "qoq_fiscu_pct",
  "ficu_count",   "fiscu_count",
  "ficu_count_lag4", "fiscu_count_lag4",
  "ld_ficu", "ld_fiscu",
  "net_entry_rate", "net_entry_rate_fiscu",
  "categories", "n_active", "n_total",
  "q4"   # q4 = base category for quarterly dummies (q1/q2/q3 enter the model)
)
FEATS_ALL <- setdiff(FEATS_ALL, HARD_EXCL)
message(sprintf("    Total candidate features: %d", length(FEATS_ALL)))

# ════════════════════════════════════════════════════════════
# 3. HELPER FUNCTIONS
# ════════════════════════════════════════════════════════════
message("\n[3] Building helper functions...")

# ── 3a: Feature matrix prep ──────────────────────────────────
prep_X <- function(dt, feats, corr_cut = CORR_CUT,
                   min_nonmiss = MIN_NONMISS) {

  # Keep only features present in dt
  use <- intersect(feats, names(dt))
  use <- use[vapply(use, function(cn) is.numeric(dt[[cn]]), logical(1))]
  if (length(use) == 0L) return(NULL)

  mat <- as.matrix(dt[, use, with = FALSE])
  colnames(mat) <- use

  # Drop columns with too many NAs
  ok_miss <- colMeans(!is.na(mat)) >= min_nonmiss
  mat <- mat[, ok_miss, drop = FALSE]
  if (ncol(mat) == 0L) return(NULL)

  # Median impute remaining NAs
  for (j in seq_len(ncol(mat))) {
    nas <- is.na(mat[, j])
    if (any(nas)) {
      med <- median(mat[!nas, j], na.rm = TRUE)
      mat[nas, j] <- if (is.finite(med)) med else 0
    }
  }

  # Drop near-zero variance  (fewer than 3 unique values or CV < 1%)
  ok_nzv <- vapply(seq_len(ncol(mat)), function(j) {
    u <- length(unique(mat[, j]))
    cv <- if (abs(mean(mat[, j])) > 1e-8)
      sd(mat[, j]) / abs(mean(mat[, j])) else 0
    u >= 3L && cv > 0.01
  }, logical(1))
  mat <- mat[, ok_nzv, drop = FALSE]
  if (ncol(mat) == 0L) return(NULL)

  # Drop high-correlation pairs (keep first of pair)
  if (ncol(mat) >= 2L) {
    cm <- suppressWarnings(cor(mat, use = "pairwise.complete.obs"))
    cm[is.na(cm)] <- 0
    diag(cm) <- 0
    hi <- which(abs(cm) > corr_cut, arr.ind = TRUE)
    hi <- hi[hi[, 1] < hi[, 2], , drop = FALSE]  # upper triangle only
    drop_idx <- unique(hi[, 2])
    if (length(drop_idx) > 0)
      mat <- mat[, -drop_idx, drop = FALSE]
  }

  mat
}

# ── 3b: Winsorise a vector ────────────────────────────────────
winsorise <- function(x, p = 0.01) {
  lo <- quantile(x, p,     na.rm = TRUE)
  hi <- quantile(x, 1 - p, na.rm = TRUE)
  pmax(pmin(x, hi), lo)
}

# ── 3c: Compute regression metrics ───────────────────────────
reg_metrics <- function(actual, predicted) {
  r   <- actual - predicted
  n   <- sum(!is.na(r))
  mse <- mean(r^2, na.rm = TRUE)
  mae <- mean(abs(r), na.rm = TRUE)
  mape <- mean(abs(r / actual) * 100,
               na.rm = TRUE)   # can be Inf if actual = 0
  ss_res <- sum(r^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  r2  <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  data.table(n = n, rmse = sqrt(mse), mae = mae,
             mape = mape, r2_oos = r2)
}

# ── 3d: Sign check — are coefficients economically sensible? ──
# Returns TRUE for variables where sign is ambiguous / hard to enforce.
# Returns FALSE for known variables where wrong sign = red flag.
sign_ok <- function(varname, coef_val) {
  v <- tolower(varname)
  # Seasonal dummies: always accept — sign depends on base quarter
  if (grepl("^q[1-4]$", v)) return(TRUE)
  # Variables that should have positive association with growth
  pos_expect <- c("gdp", "payems", "housing", "umcsent", "inf_exp",
                  "fedfunds_cycle",  # relative easing = more CUs
                  "exit_roll4",      # lagged exits reduce competition → growth?
                  "share_ficu")
  # Variables that should have negative association with growth
  neg_expect <- c("unrate", "hy_spread", "baa_spread", "credit_tightness",
                  "merger_rate", "liquid_rate", "exit_rate",
                  "yield_curve_inv",  # inversion = recession = CU contraction
                  "oil_shock")
  if (any(vapply(pos_expect, function(p) grepl(p, v), logical(1))))
    return(coef_val >= 0)
  if (any(vapply(neg_expect, function(p) grepl(p, v), logical(1))))
    return(coef_val <= 0)
  TRUE   # no prior — accept any sign
}

# ── 3e: Core model fit for one rolling window ────────────────
fit_window <- function(train_dt, test_row,
                       dep_var, feats,
                       sig_level = SIG_LEVEL) {

  # ---- guard: enough data? ----
  y_train <- train_dt[[dep_var]]
  if (sum(!is.na(y_train)) < MIN_OBS)
    return(list(ok = FALSE, reason = "insufficient training obs"))

  # ---- Winsorise target ----
  y_train_w <- winsorise(y_train)

  # ---- Build feature matrix ----
  X_train <- prep_X(train_dt, feats)
  if (is.null(X_train) || ncol(X_train) < 2L)
    return(list(ok = FALSE, reason = "too few features after prep"))

  # ---- Step 1: LASSO via cross-validated glmnet ----
  cv_fit <- tryCatch(
    glmnet::cv.glmnet(
      x        = X_train,
      y        = y_train_w,
      alpha    = 1,            # Lasso
      nfolds   = 10,
      type.measure = "mse",
      standardize  = TRUE,
      intercept    = TRUE
    ),
    error = function(e)
      return(list(ok = FALSE, reason = paste("glmnet:", e$message)))
  )
  if (!inherits(cv_fit, "cv.glmnet"))
    return(cv_fit)   # propagate error list

  lasso_lambda <- cv_fit$lambda.1se   # parsimonious lambda

  # LASSO coefficients (excluding intercept)
  lasso_coef <- as.matrix(coef(cv_fit, s = "lambda.1se"))
  selected   <- rownames(lasso_coef)[lasso_coef != 0 &
                                       rownames(lasso_coef) != "(Intercept)"]

  # LASSO prediction for test observation
  # IMPORTANT: Do NOT run prep_X on test_row — it's only 1 row so the
  # missingness filter would drop almost every column.
  # Instead: pull values for the exact columns that survived X_train prep,
  # then impute any NAs with the training-set column median.
  train_col_medians <- apply(X_train, 2, median, na.rm = TRUE)

  X_test_aligned <- matrix(train_col_medians,   # start from training medians
                            nrow = 1,
                            dimnames = list(NULL, colnames(X_train)))

  # Overwrite with actual test-row values where available
  test_vals <- as.data.frame(test_row)[1, , drop = FALSE]
  for (cn in colnames(X_train)) {
    if (cn %in% names(test_vals)) {
      v <- as.numeric(test_vals[[cn]])
      if (!is.na(v) && is.finite(v))
        X_test_aligned[1, cn] <- v
      # else keep the training median (already set above)
    }
  }

  pred_lasso <- tryCatch(
    as.numeric(predict(cv_fit, newx = X_test_aligned, s = "lambda.1se")),
    error = function(e) NA_real_
  )

  # ---- Step 2: Post-LASSO OLS ----
  pred_ols   <- NA_real_
  ols_coefs  <- NULL
  sig_vars   <- character(0)

  if (length(selected) >= 1L) {
    # Build OLS data frame (only LASSO-selected features present in train)
    ols_feats <- intersect(selected, colnames(X_train))

    if (length(ols_feats) >= 1L) {
      ols_dt <- as.data.frame(X_train[, ols_feats, drop = FALSE])
      ols_dt$y__ <- y_train_w

      ols_fit <- tryCatch(
        lm(y__ ~ ., data = ols_dt),
        error = function(e) NULL
      )

      if (!is.null(ols_fit)) {
        sm      <- summary(ols_fit)
        cf      <- as.data.frame(sm$coefficients)
        cf$var  <- rownames(cf)
        cf      <- cf[cf$var != "(Intercept)", , drop = FALSE]
        names(cf)[4] <- "pval"   # Pr(>|t|) column

        # Keep: significant AND correct sign
        cf$sign_ok <- mapply(sign_ok, cf$var, cf$Estimate)
        sig_vars   <- cf$var[cf$pval < sig_level & cf$sign_ok]

        # Always retain quarterly dummies present in X_train
        # regardless of p-value — they control structural seasonality.
        # Use q1/q2/q3 as dummies (q4 omitted as base to avoid multicollinearity).
        q_dummies  <- intersect(c("q1","q2","q3"), colnames(X_train))
        sig_vars   <- unique(c(sig_vars, q_dummies))

        # Store all coefficients for diagnostics
        ols_coefs <- data.table(
          variable   = cf$var,
          estimate   = cf$Estimate,
          std_err    = cf$`Std. Error`,
          t_stat     = cf$`t value`,
          p_value    = cf$pval,
          sign_ok    = cf$sign_ok,
          selected   = cf$var %in% sig_vars
        )

        # Re-fit OLS with only sig + sign-ok vars
        if (length(sig_vars) >= 1L) {
          ols_dt2  <- as.data.frame(X_train[, sig_vars, drop = FALSE])
          ols_dt2$y__ <- y_train_w
          ols_fit2 <- tryCatch(lm(y__ ~ ., data = ols_dt2), error = function(e) NULL)

          if (!is.null(ols_fit2)) {
            # Build OLS test frame from the already-prepared X_test_aligned
            X_test_ols <- as.data.frame(
              matrix(NA_real_, nrow = 1, ncol = length(sig_vars),
                     dimnames = list(NULL, sig_vars)))
            for (sv in sig_vars) {
              if (sv %in% colnames(X_test_aligned))
                X_test_ols[1, sv] <- X_test_aligned[1, sv]
              else
                X_test_ols[1, sv] <- 0   # fallback: zero (mean-centered)
            }
            # Point forecast + 95% prediction interval from OLS
            ols_pred_full <- tryCatch(
              predict(ols_fit2, newdata = X_test_ols,
                      interval = "prediction", level = 0.95),
              error = function(e) NULL
            )
            if (!is.null(ols_pred_full) && is.matrix(ols_pred_full)) {
              pred_ols    <- as.numeric(ols_pred_full[1, "fit"])
              pred_lo95   <- as.numeric(ols_pred_full[1, "lwr"])
              pred_hi95   <- as.numeric(ols_pred_full[1, "upr"])
            } else {
              # Fallback: point prediction only, no interval
              pred_ols    <- tryCatch(
                as.numeric(predict(ols_fit2, newdata = X_test_ols)),
                error = function(e) NA_real_)
              pred_lo95   <- NA_real_
              pred_hi95   <- NA_real_
            }
          }
        }
      }
    }
  }

  # If OLS prediction failed, fall back to LASSO
  pred_final  <- if (!is.na(pred_ols)) pred_ols else pred_lasso
  method_used <- if (!is.na(pred_ols)) "OLS" else "LASSO"

  # For LASSO-only rows approximate 95% PI as ±2*train RMSE
  if (method_used == "LASSO" || is.na(pred_lo95)) {
    train_rmse <- if (exists("ols_fit2") && !is.null(ols_fit2))
                    sqrt(mean(residuals(ols_fit2)^2, na.rm=TRUE))
                  else {
                    # Crude: SD of residuals from raw target
                    sd(y_train_w - mean(y_train_w, na.rm=TRUE), na.rm=TRUE)
                  }
    pred_lo95 <- pred_final - 1.96 * train_rmse
    pred_hi95 <- pred_final + 1.96 * train_rmse
  }

  list(
    ok           = TRUE,
    pred_lasso   = pred_lasso,
    pred_ols     = pred_ols,
    pred_final   = pred_final,
    pred_lo95    = pred_lo95,
    pred_hi95    = pred_hi95,
    method_used  = method_used,
    lasso_lambda = lasso_lambda,
    lasso_selected = selected,
    sig_vars     = sig_vars,
    ols_coefs    = ols_coefs,
    ols_fit2     = if (exists("ols_fit2")) ols_fit2 else NULL,  # for stargazer
    n_train      = nrow(X_train),
    n_lasso_sel  = length(selected),
    n_ols_sel    = length(sig_vars)
  )
}

# ════════════════════════════════════════════════════════════
# 4. ROLLING-WINDOW MODELLING  (21 models)
# ════════════════════════════════════════════════════════════
message("\n[4] Rolling-window LASSO + post-LASSO OLS...")
message(sprintf("    Training window: 2005 Q1 → %s (initial)",
                as.character(TRAIN_END)))
message(sprintf("    Significance level: p < %.2f", SIG_LEVEL))
message(sprintf("    Mode: %s", if (DEBUG_MODE) "DEBUG" else "PRODUCTION"))

# Quarters AFTER TRAIN_END → these are the "test" / forecast quarters
test_quarters <- all_quarters[all_quarters > TRAIN_END]
if (DEBUG_MODE && length(test_quarters) > DEBUG_ROLL_Q)
  test_quarters <- tail(test_quarters, DEBUG_ROLL_Q)

message(sprintf("    Rolling over %d test quarters (%s to %s)",
                length(test_quarters),
                as.character(min(test_quarters)),
                as.character(max(test_quarters))))

# Store all results
all_forecasts <- list()   # one row per (dep_var, cat, test_quarter)
all_coefs     <- list()   # coefficient records
all_metrics   <- list()   # per-model summary metrics
all_ols_fits  <- list()   # final OLS model objects (for stargazer)

model_id <- 0L

for (dv in names(DEP_VARS)) {
  dv_label <- DEP_VARS[[dv]]$label
  dv_short <- DEP_VARS[[dv]]$short
  message(sprintf("\n  ── Target: %s ──", dv_label))

  for (cat in cats) {
    model_id <- model_id + 1L
    cat_dt <- qtrly[cat_label == cat]
    setorderv(cat_dt, "date")

    message(sprintf("    [Model %02d/21] %s | %s", model_id, dv_short, cat))
    tic(sprintf("Model %02d", model_id))

    fc_rows  <- list()   # forecast rows for this model
    coef_rows <- list()  # coefficient rows for this model

    for (tq in test_quarters) {

      # Training data: 2005 Q1 up to (but not including) test quarter
      train_idx <- cat_dt$date >= zoo::as.yearqtr("2005 Q1") &
                   cat_dt$date <  tq
      test_idx  <- cat_dt$date == tq

      if (sum(train_idx) < MIN_OBS || sum(test_idx) == 0) next

      train_dt <- cat_dt[train_idx]
      test_row  <- cat_dt[test_idx][1]   # one row

      # Exclude features that directly encode the target (leakage)
      feats_use <- FEATS_ALL
      if (dv == "ln_assets_tot") {
        # Also exclude yoy_assets and log-level relatives
        feats_use <- feats_use[!grepl("assets", feats_use)]
      }
      if (dv == "yoy_ficu_pct") {
        feats_use <- feats_use[!grepl("^yoy_ficu", feats_use)]
      }
      if (dv == "yoy_fiscu_pct") {
        feats_use <- feats_use[!grepl("^yoy_fiscu", feats_use)]
      }

      # Always include q1/q2/q3 dummies (q4 = base, omitted to avoid collinearity)
      # These capture within-year seasonal patterns in CU metrics.
      q_dum_available <- intersect(c("q1","q2","q3"), names(train_dt))
      q_dum_available <- q_dum_available[
        vapply(q_dum_available, function(cn) is.numeric(train_dt[[cn]]), logical(1))
      ]
      feats_use <- unique(c(feats_use, q_dum_available))

      res <- fit_window(train_dt, test_row, dv, feats_use)

      # ── Compute actual value — with on-the-fly fallback ──────
      # test_row[[dv]] can be NA if Part 1 lag-4 wasn't available
      # for the most recent quarters. Recompute from raw cols if needed.
      actual_val <- test_row[[dv]]
      if (is.na(actual_val)) {
        if (dv == "yoy_ficu_pct" && !is.na(test_row$ficu_count)) {
          lag4_anchor <- cat_dt[date == tq - 1, ficu_count][1]  # tq-1 in yearqtr = 4 qtrs back
          if (!is.na(lag4_anchor) && lag4_anchor > 0)
            actual_val <- (test_row$ficu_count - lag4_anchor) / lag4_anchor * 100
        } else if (dv == "yoy_fiscu_pct" && !is.na(test_row$fiscu_count)) {
          lag4_anchor <- cat_dt[date == tq - 1, fiscu_count][1]
          if (!is.na(lag4_anchor) && lag4_anchor > 0)
            actual_val <- (test_row$fiscu_count - lag4_anchor) / lag4_anchor * 100
        } else if (dv == "ln_assets_tot" && !is.na(test_row$assets_tot)) {
          actual_val <- log(pmax(test_row$assets_tot, 1))
        }
      }

      # Store the best OLS fit object for this window (overwritten each roll;
      # last window's model is stored per model_id for stargazer)
      if (res$ok && !is.null(res$ols_fit2)) {
        all_ols_fits[[paste(dv, cat, sep="|")]] <- list(
          fit       = res$ols_fit2,
          dep_var   = dv,
          dv_label  = dv_label,
          cat_label = cat,
          n_train   = res$n_train,
          sig_vars  = res$sig_vars,
          date_end  = tq
        )
      }

      # ── Capture raw anchor values for back-transformation ──
      # For YoY% models we need the level 4 quarters ago (lag-4 anchor).
      # For ln_assets we need the actual assets_tot to convert exp() back.
      # We also store actual raw levels so level-space plots are self-contained.
      anchor_ficu   <- if ("ficu_count_lag4"  %in% names(test_row))
                         test_row$ficu_count_lag4  else NA_real_
      anchor_fiscu  <- if ("fiscu_count_lag4" %in% names(test_row))
                         test_row$fiscu_count_lag4 else NA_real_
      # If lag4 cols are absent fall back to shifting within cat_dt
      if (is.na(anchor_ficu)) {
        lag4_row <- cat_dt[date == tq - 1]   # yearqtr arithmetic: -1 = -4 qtrs
        anchor_ficu  <- if (nrow(lag4_row)>0) lag4_row$ficu_count[1]  else NA_real_
        anchor_fiscu <- if (nrow(lag4_row)>0) lag4_row$fiscu_count[1] else NA_real_
      }
      actual_ficu   <- if ("ficu_count"  %in% names(test_row)) test_row$ficu_count  else NA_real_
      actual_fiscu  <- if ("fiscu_count" %in% names(test_row)) test_row$fiscu_count else NA_real_
      actual_assets <- if ("assets_tot"  %in% names(test_row)) test_row$assets_tot  else NA_real_
      # ln_assets anchor = exp(ln_assets_lag4) if available, else assets_tot of lag4 row
      anchor_lnassets <- if ("ln_assets_tot" %in% names(test_row)) test_row$ln_assets_tot else NA_real_

      # Record forecast
      fc_rows[[length(fc_rows) + 1L]] <- data.table(
        dep_var          = dv,
        dv_label         = dv_label,
        cat_label        = cat,
        date             = tq,
        actual           = actual_val,
        pred_lasso       = if (res$ok) res$pred_lasso  else NA_real_,
        pred_ols         = if (res$ok) res$pred_ols     else NA_real_,
        pred_final       = if (res$ok) res$pred_final   else NA_real_,
        method_used      = if (res$ok) res$method_used  else "FAILED",
        n_train          = if (res$ok) res$n_train      else NA_integer_,
        n_lasso_sel      = if (res$ok) res$n_lasso_sel  else NA_integer_,
        n_ols_sel        = if (res$ok) res$n_ols_sel    else NA_integer_,
        lasso_lambda     = if (res$ok) res$lasso_lambda else NA_real_,
        pred_lo95        = if (res$ok) res$pred_lo95   else NA_real_,
        pred_hi95        = if (res$ok) res$pred_hi95   else NA_real_,
        error_msg        = if (!res$ok) res$reason      else NA_character_,
        # Raw level anchors (for back-transformation)
        anchor_ficu      = anchor_ficu,
        anchor_fiscu     = anchor_fiscu,
        anchor_lnassets  = anchor_lnassets,
        actual_ficu      = actual_ficu,
        actual_fiscu     = actual_fiscu,
        actual_assets    = actual_assets
      )

      # Record OLS coefficients
      if (res$ok && !is.null(res$ols_coefs)) {
        coef_rows[[length(coef_rows) + 1L]] <- cbind(
          data.table(dep_var = dv, cat_label = cat, date = tq),
          res$ols_coefs
        )
      }
    }  # end test_quarters loop

    # Aggregate forecasts for this model
    if (length(fc_rows) > 0) {
      fc_dt <- rbindlist(fc_rows, fill = TRUE)
      all_forecasts[[paste(dv, cat, sep = "|")]] <- fc_dt

      # Metrics — require at least 1 valid pair (debug) or 3 (prod)
      valid <- fc_dt[!is.na(actual) & !is.na(pred_final)]
      min_valid <- if (DEBUG_MODE) 1L else 3L
      if (nrow(valid) >= min_valid) {
        m <- reg_metrics(valid$actual, valid$pred_final)
        m$dep_var   <- dv
        m$dv_label  <- dv_label
        m$cat_label <- cat
        all_metrics[[paste(dv, cat, sep = "|")]] <- m
      } else {
        # Verbose diagnostics to help diagnose why
        n_actual_na  <- sum(is.na(fc_dt$actual))
        n_pred_na    <- sum(is.na(fc_dt$pred_final))
        n_total      <- nrow(fc_dt)
        message(sprintf("        [DIAG] %s|%s: %d rows, actual_NA=%d, pred_NA=%d, valid=%d",
                        dv, cat, n_total, n_actual_na, n_pred_na, nrow(valid)))
        if (n_actual_na == n_total)
          message("        [DIAG] ALL actual values are NA — check dep var is populated in qtrly_enriched.rds")
        if (n_pred_na == n_total)
          message("        [DIAG] ALL predictions are NA — check fit_window error reasons:")
        # Print any fit_window error reasons recorded
        err_rows <- fc_dt[!is.na(error_msg)]
        if (nrow(err_rows) > 0) {
          for (i in seq_len(nrow(err_rows)))
            message(sprintf("          date=%s error=%s",
                            as.character(err_rows$date[i]),
                            err_rows$error_msg[i]))
        } else {
          message("          (no error_msg recorded — pred_final became NA inside fit_window)")
          message("          Most likely cause: X_test_aligned construction issue or glmnet predict failed")
        }
      }
    }

    if (length(coef_rows) > 0)
      all_coefs[[paste(dv, cat, sep = "|")]] <- rbindlist(coef_rows, fill = TRUE)

    msg_m <- if (!is.null(all_metrics[[paste(dv, cat, sep = "|")]])) {
      m <- all_metrics[[paste(dv, cat, sep = "|")]]
      sprintf("RMSE=%.3f  R²=%.3f  n=%d", m$rmse, m$r2_oos, m$n)
    } else "insufficient data"
    message(sprintf("        %s", msg_m))
    toc()
  }  # end cat loop
}  # end dv loop

# ════════════════════════════════════════════════════════════
# 5. CONSOLIDATE RESULTS
# ════════════════════════════════════════════════════════════
message("\n[5] Consolidating results...")

forecasts_all <- rbindlist(all_forecasts, fill = TRUE)
metrics_all   <- if (length(all_metrics) > 0)
                   rbindlist(all_metrics, fill = TRUE) else data.table()
coefs_all     <- if (length(all_coefs) > 0)
                   rbindlist(all_coefs, fill = TRUE) else data.table()

message(sprintf("    Forecast rows    : %d", nrow(forecasts_all)))
message(sprintf("    Metrics rows     : %d", nrow(metrics_all)))
message(sprintf("    Coefficient rows : %d", nrow(coefs_all)))

# Save CSVs
fwrite(forecasts_all, file.path(RESULT_DIR, "forecasts.csv"))
fwrite(metrics_all,   file.path(RESULT_DIR, "metrics.csv"))
fwrite(coefs_all,     file.path(RESULT_DIR, "coefficients.csv"))
message("    Saved CSVs to results_regression/")

# ════════════════════════════════════════════════════════════
# 5b. BACK-TRANSFORM FORECASTS TO LEVEL SPACE
# ════════════════════════════════════════════════════════════
message("\n[5b] Back-transforming forecasts to level space...")

# Helper: YoY% -> level:  level_t = anchor_lag4 * (1 + yoy_pct/100)
yoy_to_level <- function(yoy_pct, anchor_lag4) {
  ifelse(!is.na(yoy_pct) & !is.na(anchor_lag4) & anchor_lag4 > 0,
         anchor_lag4 * (1 + yoy_pct / 100),
         NA_real_)
}

# Full history panel (in-sample + out-of-sample) for plotting
full_levels <- unique(qtrly[date >= zoo::as.yearqtr("2005 Q1"),
                             .(date, cat_label,
                               ficu_count, fiscu_count, assets_tot)])

# ── (a) ficu_count from yoy_ficu_pct ────────────────────────
fc_ficu <- forecasts_all[dep_var == "yoy_ficu_pct",
                          .(cat_label, date,
                            actual_level = actual_ficu,
                            pred_yoy     = pred_final,
                            lo95_yoy     = pred_lo95,
                            hi95_yoy     = pred_hi95,
                            anchor       = anchor_ficu,
                            method_used)]
fc_ficu[, pred_level  := yoy_to_level(pred_yoy,  anchor)]
fc_ficu[, pred_lo95   := yoy_to_level(lo95_yoy,  anchor)]
fc_ficu[, pred_hi95   := yoy_to_level(hi95_yoy,  anchor)]
fc_ficu[, series  := "ficu_count"]
fc_ficu[, y_label := "FICU Count"]

# ── (b) fiscu_count from yoy_fiscu_pct ──────────────────────
fc_fiscu <- forecasts_all[dep_var == "yoy_fiscu_pct",
                           .(cat_label, date,
                             actual_level = actual_fiscu,
                             pred_yoy     = pred_final,
                             lo95_yoy     = pred_lo95,
                             hi95_yoy     = pred_hi95,
                             anchor       = anchor_fiscu,
                             method_used)]
fc_fiscu[, pred_level  := yoy_to_level(pred_yoy,  anchor)]
fc_fiscu[, pred_lo95   := yoy_to_level(lo95_yoy,  anchor)]
fc_fiscu[, pred_hi95   := yoy_to_level(hi95_yoy,  anchor)]
fc_fiscu[, series  := "fiscu_count"]
fc_fiscu[, y_label := "FISCU Count"]

# ── (c) total_assets from ln_assets_tot via exp() ───────────
fc_assets <- forecasts_all[dep_var == "ln_assets_tot",
                            .(cat_label, date,
                              actual_level = actual_assets,
                              pred_ln      = pred_final,
                              lo95_ln      = pred_lo95,
                              hi95_ln      = pred_hi95,
                              method_used)]
fc_assets[, pred_level  := ifelse(!is.na(pred_ln),  exp(pred_ln),  NA_real_)]
fc_assets[, pred_lo95   := ifelse(!is.na(lo95_ln),  exp(lo95_ln),  NA_real_)]
fc_assets[, pred_hi95   := ifelse(!is.na(hi95_ln),  exp(hi95_ln),  NA_real_)]
fc_assets[, series  := "total_assets"]
fc_assets[, y_label := "Total Assets ($000s)"]

# Combine — now includes CI columns
level_fc <- rbindlist(
  list(
    fc_ficu  [, .(series, y_label, cat_label, date,
                  actual_level, pred_level, pred_lo95, pred_hi95, method_used)],
    fc_fiscu [, .(series, y_label, cat_label, date,
                  actual_level, pred_level, pred_lo95, pred_hi95, method_used)],
    fc_assets[, .(series, y_label, cat_label, date,
                  actual_level, pred_level, pred_lo95, pred_hi95, method_used)]
  ),
  fill = TRUE
)

# Level-space OOS metrics
level_metrics <- level_fc[!is.na(actual_level) & !is.na(pred_level),
  .(rmse   = sqrt(mean((actual_level - pred_level)^2, na.rm=TRUE)),
    mae    = mean(abs(actual_level - pred_level),     na.rm=TRUE),
    mape   = mean(abs((actual_level - pred_level) /
                        actual_level) * 100,          na.rm=TRUE),
    r2_oos = {
      ss_r <- sum((actual_level - pred_level)^2,                  na.rm=TRUE)
      ss_t <- sum((actual_level - mean(actual_level, na.rm=TRUE))^2, na.rm=TRUE)
      if (ss_t > 0) 1 - ss_r/ss_t else NA_real_
    },
    n = .N),
  by = .(series, y_label, cat_label)]

# Normalise date class on level_fc once, preventing class-mismatch
# errors in any subsequent rbindlist that mixes level_fc with qtrly
level_fc[, date := zoo::as.yearqtr(as.numeric(date))]

fwrite(level_fc,      file.path(RESULT_DIR, "forecasts_levels.csv"))
fwrite(level_metrics, file.path(RESULT_DIR, "metrics_levels.csv"))
message(sprintf("    Level forecast rows : %d", nrow(level_fc)))
message("    Saved: forecasts_levels.csv, metrics_levels.csv")
message("\n    Level-space OOS metrics:")
print(level_metrics[order(series, cat_label),
                    .(series, cat_label,
                      rmse   = round(rmse,   1),
                      mape   = round(mape,   2),
                      r2_oos = round(r2_oos, 3),
                      n)])

# ════════════════════════════════════════════════════════════
# 6. PLOTS
# ════════════════════════════════════════════════════════════
message("\n[6] Generating plots...")

# Helper: ggplot2 theme
theme_cu <- function() {
  theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#e8f0f7"),
      strip.text       = element_text(face = "bold"),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey40"),
      legend.position  = "bottom"
    )
}

# ── P1: Actual vs Predicted — per dep var, all categories ────
message("    P1: Actual vs predicted...")

for (dv in names(DEP_VARS)) {
  dv_label <- DEP_VARS[[dv]]$label
  dv_short <- DEP_VARS[[dv]]$short

  fc_dv <- forecasts_all[dep_var == dv & !is.na(actual) & !is.na(pred_final)]
  if (nrow(fc_dv) == 0) next

  fc_dv[, date_num := as.numeric(date)]

  p <- ggplot(fc_dv, aes(x = as.Date(date))) +
    geom_line(aes(y = actual, colour = "Actual"), linewidth = 0.8) +
    geom_line(aes(y = pred_final, colour = "Predicted"), linewidth = 0.7,
              linetype = "dashed") +
    facet_wrap(~ cat_label, scales = "free_y", ncol = 3) +
    scale_colour_manual(values = c("Actual" = "#1f77b4",
                                   "Predicted" = "#d62728"),
                        name = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    labs(title    = sprintf("Actual vs Predicted — %s", dv_label),
         subtitle = "Post-LASSO OLS (falls back to LASSO if OLS insufficient)",
         x = NULL, y = dv_label) +
    theme_cu()

  save_plot(p, sprintf("01_actual_vs_pred_%s", dv_short),
            width = 13, height = 9)
}

# ── P2: Residual plots ────────────────────────────────────────
message("    P2: Residuals...")

for (dv in names(DEP_VARS)) {
  dv_label <- DEP_VARS[[dv]]$label
  dv_short <- DEP_VARS[[dv]]$short

  fc_dv <- forecasts_all[dep_var == dv & !is.na(actual) & !is.na(pred_final)]
  if (nrow(fc_dv) == 0) next
  fc_dv[, resid := actual - pred_final]
  if (nrow(fc_dv) == 0 || all(is.na(fc_dv$cat_label))) next
  fc_dv <- fc_dv[!is.na(cat_label)]

  # Only add loess if enough points per facet
  enough_pts <- fc_dv[, .N, by = cat_label][N >= 5, cat_label]
  smooth_layer <- if (length(enough_pts) > 0)
    geom_smooth(data = fc_dv[cat_label %in% enough_pts],
                method = "loess", se = FALSE, colour = "black",
                linewidth = 0.6)
  else
    NULL

  p <- ggplot(fc_dv, aes(x = pred_final, y = resid)) +
    geom_point(aes(colour = cat_label), alpha = 0.6, size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    smooth_layer +
    facet_wrap(~ cat_label, scales = "free", ncol = 3) +
    scale_colour_manual(values = CAT_COLOURS, guide = "none") +
    labs(title    = sprintf("Residuals vs Fitted — %s", dv_label),
         subtitle = "Loess smoother in black; should be flat at 0",
         x = "Fitted value", y = "Residual") +
    theme_cu()

  save_plot(p, sprintf("02_residuals_%s", dv_short),
            width = 13, height = 9)
}

# ── P3: Out-of-sample R² by category ─────────────────────────
message("    P3: OOS R² scorecard...")

if (nrow(metrics_all) > 0 && any(!is.na(metrics_all$dv_label))) {
  metrics_all <- metrics_all[!is.na(dv_label) & !is.na(cat_label)]
  p <- ggplot(metrics_all,
              aes(x = reorder(cat_label, r2_oos),
                  y = r2_oos, fill = r2_oos > 0)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.3f", r2_oos),
                  hjust = ifelse(r2_oos >= 0, -0.1, 1.1)),
              size = 3) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
    coord_flip() +
    facet_wrap(~ dv_label, scales = "free_x", ncol = 3) +
    scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"),
                      guide = "none") +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
    labs(title    = "Out-of-Sample R² by Model",
         subtitle = "Green = positive predictive skill; red = worse than mean",
         x = NULL, y = "OOS R²") +
    theme_cu()

  save_plot(p, "03_oos_r2_scorecard", width = 14, height = 6)

  # Also print metrics table
  message("\n    OOS Metrics Summary:")
  print(metrics_all[order(dep_var, cat_label),
                    .(dep_var, cat_label, n, rmse = round(rmse, 3),
                      mae = round(mae, 3), r2_oos = round(r2_oos, 3))])
}

# ── P4: Rolling R² over time ─────────────────────────────────
message("    P4: Rolling OOS R² over time...")

# Compute rolling 4-quarter R² for each model
rolling_r2 <- list()
for (key in names(all_forecasts)) {
  fc <- all_forecasts[[key]]
  if (is.null(fc) || nrow(fc) < 5) next
  fc <- fc[!is.na(actual) & !is.na(pred_final)]
  if (nrow(fc) < 5) next

  fc[, resid2 := (actual - pred_final)^2]
  fc[, act_dm2 := (actual - mean(actual, na.rm=TRUE))^2]

  # 4-quarter rolling R²
  dts <- sort(fc$date)
  r2_roll <- vapply(seq_along(dts), function(i) {
    idx <- max(1, i-3):i
    sub <- fc[date %in% dts[idx]]
    ss_r <- sum(sub$resid2, na.rm=TRUE)
    ss_t <- sum((sub$actual - mean(sub$actual, na.rm=TRUE))^2, na.rm=TRUE)
    if (ss_t > 0) 1 - ss_r/ss_t else NA_real_
  }, numeric(1))

  rolling_r2[[key]] <- data.table(
    dep_var   = fc$dep_var[1],
    dv_label  = fc$dv_label[1],
    cat_label = fc$cat_label[1],
    date      = dts,
    r2_roll4  = r2_roll
  )
}

if (length(rolling_r2) > 0) {
  r2_roll_dt <- rbindlist(rolling_r2, fill = TRUE)

  for (dv in names(DEP_VARS)) {
    dv_label <- DEP_VARS[[dv]]$label
    dv_short <- DEP_VARS[[dv]]$short
    sub_r2   <- r2_roll_dt[dep_var == dv & !is.na(cat_label) & !is.na(dv_label)]
    if (nrow(sub_r2) == 0) next

    p <- ggplot(sub_r2, aes(x = as.Date(date), y = r2_roll4,
                             colour = cat_label)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      facet_wrap(~ cat_label, ncol = 3, scales = "free_y") +
      scale_colour_manual(values = CAT_COLOURS, guide = "none") +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      labs(title    = sprintf("Rolling 4-Quarter OOS R² — %s", dv_label),
           subtitle = "Each point = R² computed over trailing 4 quarters",
           x = NULL, y = "Rolling R²") +
      theme_cu()

    save_plot(p, sprintf("04_rolling_r2_%s", dv_short),
              width = 13, height = 9)
  }
}

# ── P5: LASSO selection frequency heatmap ────────────────────
message("    P5: LASSO selection frequency...")

if (nrow(forecasts_all) > 0) {

  # Unnest selected features from forecast table
  # We stored n_lasso_sel count; for actual var names use coefs_all
  if (nrow(coefs_all) > 0) {
    # For each dep_var x cat_label, count fraction of windows each var was selected
    sel_freq <- coefs_all[selected == TRUE,
                           .(n_selected = .N),
                           by = .(dep_var, cat_label, variable)]
    # Total windows per model
    win_count <- forecasts_all[!is.na(pred_final),
                                .(n_windows = .N),
                                by = .(dep_var, cat_label)]
    sel_freq <- merge(sel_freq, win_count, by = c("dep_var", "cat_label"),
                      all.x = TRUE)
    sel_freq[, sel_pct := n_selected / n_windows * 100]

    # Top 20 variables overall
    top_vars <- sel_freq[, .(mean_sel = mean(sel_pct, na.rm=TRUE)), by=variable]
    top_vars <- head(top_vars[order(-mean_sel)], 20)

    sel_top <- sel_freq[variable %in% top_vars$variable]

    for (dv in names(DEP_VARS)) {
      dv_label <- DEP_VARS[[dv]]$label
      dv_short <- DEP_VARS[[dv]]$short
      sub_sel  <- sel_top[dep_var == dv & !is.na(cat_label) & !is.na(variable)]
      if (nrow(sub_sel) == 0) next

      p <- ggplot(sub_sel, aes(x = cat_label, y = variable, fill = sel_pct)) +
        geom_tile(colour = "white") +
        geom_text(aes(label = sprintf("%.0f%%", sel_pct)),
                  size = 2.8, colour = "black") +
        scale_fill_gradient2(low = "white", mid = "#9ecae1",
                             high = "#08306b", midpoint = 50,
                             name = "% windows\nselected") +
        scale_x_discrete(guide = guide_axis(angle = 30)) +
        labs(title    = sprintf("Post-LASSO OLS Variable Selection — %s", dv_label),
             subtitle = "% of rolling windows where each variable was selected (p<0.10, sign OK)",
             x = NULL, y = NULL) +
        theme_cu() +
        theme(axis.text.y = element_text(size = 7))

      save_plot(p, sprintf("05_selection_heatmap_%s", dv_short),
                width = 12, height = 9)
    }
  }
}

# ── P6: Coefficient boxplots — top selected variables ─────────
message("    P6: Coefficient distributions...")

if (nrow(coefs_all) > 0) {
  for (dv in names(DEP_VARS)) {
    dv_label <- DEP_VARS[[dv]]$label
    dv_short <- DEP_VARS[[dv]]$short

    coef_dv <- coefs_all[dep_var == dv & selected == TRUE]
    if (nrow(coef_dv) == 0) next

    # Top 20 most frequently selected variables
    top_sel <- coef_dv[, .(n = .N), by = variable][order(-n)]
    top_20  <- head(top_sel$variable, 20)
    coef_dv <- coef_dv[variable %in% top_20]

    p <- ggplot(coef_dv,
                aes(x = reorder(variable, estimate, FUN = median),
                    y = estimate, colour = cat_label)) +
      geom_boxplot(aes(fill = cat_label), alpha = 0.3,
                   outlier.size = 0.8, outlier.alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      coord_flip() +
      scale_colour_manual(values = CAT_COLOURS, guide = "none") +
      scale_fill_manual(values = CAT_COLOURS, name = "Category") +
      labs(title    = sprintf("Coefficient Distributions — %s", dv_label),
           subtitle = "Post-LASSO OLS; boxes show distribution across rolling windows & categories",
           x = NULL, y = "OLS Coefficient") +
      theme_cu()

    save_plot(p, sprintf("06_coef_boxplot_%s", dv_short),
              width = 12, height = 9)
  }
}

# ── P7: Forecast bias — mean error by category ────────────────
message("    P7: Forecast bias...")

if (nrow(forecasts_all) > 0) {
  bias_dt <- forecasts_all[!is.na(actual) & !is.na(pred_final) &
                             !is.na(dv_label) & !is.na(cat_label),
                            .(mean_error  = mean(actual - pred_final, na.rm=TRUE),
                              rmse        = sqrt(mean((actual-pred_final)^2, na.rm=TRUE)),
                              n           = .N),
                            by = .(dep_var, dv_label, cat_label)]

  if (nrow(bias_dt) == 0 || length(unique(bias_dt$dv_label)) == 0) {
    message("    P7: skipped — no valid rows in bias_dt (debug mode?)")
  } else {
  p <- ggplot(bias_dt,
              aes(x = reorder(cat_label, mean_error),
                  y = mean_error, fill = mean_error > 0)) +
    geom_col() +
    geom_errorbar(aes(ymin = mean_error - rmse,
                      ymax = mean_error + rmse),
                  width = 0.3, colour = "grey40") +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
    coord_flip() +
    facet_wrap(~ dv_label, scales = "free_x", ncol = 3) +
    scale_fill_manual(values = c("TRUE" = "#d62728", "FALSE" = "#1f77b4"),
                      guide = "none") +
    scale_x_discrete(limits = rev) +
    labs(title    = "Mean Forecast Error (Bias) by Model",
         subtitle = "Error bars = ±RMSE; positive = under-prediction",
         x = NULL, y = "Mean Error (Actual − Predicted)") +
    theme_cu()

  save_plot(p, "07_forecast_bias", width = 14, height = 6)
  }  # end else bias_dt non-empty
}

# ── P8: LASSO lambda path — average by category ───────────────
message("    P8: LASSO lambda paths...")

if (nrow(forecasts_all) > 0) {
  lambda_dt <- forecasts_all[!is.na(lasso_lambda),
                              .(mean_lambda = mean(lasso_lambda, na.rm=TRUE),
                                sd_lambda   = sd(lasso_lambda, na.rm=TRUE),
                                n           = .N),
                              by = .(dep_var, dv_label, cat_label, date)]

  for (dv in names(DEP_VARS)) {
    dv_label <- DEP_VARS[[dv]]$label
    dv_short <- DEP_VARS[[dv]]$short
    sub_lam  <- lambda_dt[dep_var == dv & !is.na(cat_label)]
    if (nrow(sub_lam) == 0) next

    p <- ggplot(sub_lam, aes(x = as.Date(date), y = mean_lambda,
                              colour = cat_label)) +
      geom_line(linewidth = 0.8) +
      facet_wrap(~ cat_label, ncol = 3, scales = "free_y") +
      scale_colour_manual(values = CAT_COLOURS, guide = "none") +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      scale_y_log10() +
      labs(title    = sprintf("LASSO Lambda (1SE) Over Time — %s", dv_label),
           subtitle = "Higher lambda = stronger regularisation (fewer variables selected)",
           x = NULL, y = "Lambda (log scale)") +
      theme_cu()

    save_plot(p, sprintf("08_lasso_lambda_%s", dv_short),
              width = 13, height = 9)
  }
}

# ── P9: Variable selection count over time ────────────────────
message("    P9: Variable selection counts...")

if (nrow(forecasts_all) > 0) {
  sel_over_time <- forecasts_all[!is.na(n_ols_sel) & !is.na(dv_label),
                                  .(mean_ols = mean(n_ols_sel, na.rm=TRUE),
                                    mean_lasso = mean(n_lasso_sel, na.rm=TRUE)),
                                  by = .(dep_var, dv_label, date)]

  if (nrow(sel_over_time) == 0 || length(unique(sel_over_time$dv_label)) == 0) {
    message("    P9: skipped — no valid sel_over_time rows")
  } else {
  p <- ggplot(sel_over_time, aes(x = as.Date(date))) +
    geom_line(aes(y = mean_lasso, colour = "LASSO selected"),
              linewidth = 0.8, linetype = "dotted") +
    geom_line(aes(y = mean_ols, colour = "Post-LASSO OLS kept"),
              linewidth = 0.8) +
    facet_wrap(~ dv_label, ncol = 3, scales = "free_y") +
    scale_colour_manual(values = c("LASSO selected"     = "#9467bd",
                                   "Post-LASSO OLS kept" = "#2ca02c"),
                        name = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(title    = "Model Size Over Rolling Windows",
         subtitle = "Average # variables selected across all categories",
         x = NULL, y = "# Variables") +
    theme_cu()

  save_plot(p, "09_selection_count_over_time", width = 13, height = 6)
  }  # end else sel_over_time non-empty
}

# ── P10: OLS method vs LASSO fallback frequency ───────────────
message("    P10: OLS vs LASSO usage...")

if (nrow(forecasts_all) > 0) {
  method_dt <- forecasts_all[!is.na(method_used) & !is.na(dv_label) & !is.na(cat_label),
                              .(n = .N),
                              by = .(dep_var, dv_label, cat_label, method_used)]
  method_dt[, tot := sum(n), by = .(dep_var, cat_label)]
  method_dt[, pct := n / tot * 100]

  if (nrow(method_dt) == 0 || length(unique(method_dt$dv_label)) == 0) {
    message("    P10: skipped — no valid method_dt rows")
  } else {
  p <- ggplot(method_dt,
              aes(x = cat_label, y = pct, fill = method_used)) +
    geom_col(position = "stack") +
    geom_text(aes(label = sprintf("%.0f%%", pct)),
              position = position_stack(vjust = 0.5), size = 2.8) +
    facet_wrap(~ dv_label, ncol = 3) +
    scale_fill_manual(values = c("OLS" = "#2ca02c", "LASSO" = "#9467bd",
                                 "FAILED" = "#d62728"),
                      name = "Method") +
    scale_x_discrete(guide = guide_axis(angle = 40)) +
    labs(title    = "OLS vs LASSO Fallback by Model",
         subtitle = "Green = post-LASSO OLS used; purple = LASSO fallback",
         x = NULL, y = "% of rolling windows") +
    theme_cu()

  save_plot(p, "10_method_usage", width = 13, height = 6)
  }  # end else method_dt non-empty
}

# ════════════════════════════════════════════════════════════
# 6b. LEVEL-SPACE PLOTS  (back-transformed)
# ════════════════════════════════════════════════════════════
message("\n[6b] Level-space plots (back-transformed)...")

if (nrow(level_fc) > 0) {

  # Merge full historical series for in-sample backdrop
  # Long format of full_levels for all three series
  hist_long <- melt(
    full_levels,
    id.vars      = c("date", "cat_label"),
    measure.vars = c("ficu_count", "fiscu_count", "assets_tot"),
    variable.name = "series",
    value.name    = "actual_level"
  )
  hist_long[, series := as.character(series)]
  hist_long[series == "assets_tot", series := "total_assets"]

  # ── P11: Full time-series (history + OOS forecast) per series ──
  message("    P11: Full time-series actual vs forecast...")

  series_meta <- list(
    ficu_count   = list(label = "FICU Count",          unit = "count",   fmt = "comma"),
    fiscu_count  = list(label = "FISCU Count",          unit = "count",   fmt = "comma"),
    total_assets = list(label = "Total Assets ($000s)", unit = "dollars", fmt = "dollar")
  )

  for (sr in names(series_meta)) {
    sr_label <- series_meta[[sr]]$label
    sr_fmt   <- series_meta[[sr]]$fmt

    hist_sr <- hist_long[series == sr & !is.na(actual_level) & !is.na(cat_label)]
    fc_sr   <- level_fc[series == sr & !is.na(pred_level) & !is.na(cat_label)]
    if (nrow(fc_sr) == 0 || nrow(hist_sr) == 0) next

    # Shade the forecast period
    fc_start <- min(fc_sr$date, na.rm = TRUE)

    p <- ggplot() +
      # Shaded forecast region
      annotate("rect",
               xmin = as.Date(fc_start), xmax = Inf,
               ymin = -Inf,              ymax = Inf,
               fill = "#fff3cd", alpha = 0.5) +
      # Historical actual (full series including forecast period)
      geom_line(data = hist_sr,
                aes(x = as.Date(date), y = actual_level,
                    colour = "Actual"),
                linewidth = 0.9) +
      # OOS predicted
      geom_line(data = fc_sr,
                aes(x = as.Date(date), y = pred_level,
                    colour = "Forecast"),
                linewidth = 0.85, linetype = "dashed") +
      # OOS actual points (for easy comparison)
      geom_point(data = fc_sr[!is.na(actual_level)],
                 aes(x = as.Date(date), y = actual_level,
                     colour = "Actual"),
                 size = 1.8, shape = 16) +
      geom_point(data = fc_sr,
                 aes(x = as.Date(date), y = pred_level,
                     colour = "Forecast"),
                 size = 1.8, shape = 17) +
      # Vertical line at forecast start
      geom_vline(xintercept = as.Date(fc_start),
                 linetype = "dotted", colour = "grey40", linewidth = 0.6) +
      facet_wrap(~ cat_label, scales = "free_y", ncol = 3) +
      scale_colour_manual(
        values = c("Actual" = "#1f77b4", "Forecast" = "#d62728"),
        name   = NULL) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      {if (sr_fmt == "comma")
         scale_y_continuous(labels = scales::comma)
       else
         scale_y_continuous(labels = scales::dollar_format(scale = 1e-6,
                                                           suffix = "M"))} +
      labs(title    = sprintf("%s — Actual vs Forecast by Category", sr_label),
           subtitle = sprintf(
             "Yellow region = out-of-sample forecast period (from %s); dashed = forecast",
             as.character(fc_start)),
           x = NULL, y = sr_label) +
      theme_cu() +
      theme(legend.key.width = unit(1.5, "cm"))

    save_plot(p, sprintf("11_levels_%s", sr), width = 14, height = 10)
  }

  # ── P12: Actual vs Predicted scatter in level space ──────────
  message("    P12: Level-space scatter (actual vs predicted)...")

  for (sr in names(series_meta)) {
    sr_label <- series_meta[[sr]]$label
    sr_fmt   <- series_meta[[sr]]$fmt

    fc_sr <- level_fc[series == sr & !is.na(actual_level) & !is.na(pred_level)
                       & !is.na(cat_label)]
    if (nrow(fc_sr) < 3) next   # need >=3 pts for meaningful scatter

    # 45-degree perfect-forecast line
    rng <- range(c(fc_sr$actual_level, fc_sr$pred_level), na.rm = TRUE)

    p <- ggplot(fc_sr, aes(x = actual_level, y = pred_level,
                            colour = cat_label)) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", colour = "grey40", linewidth = 0.7) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(aes(group = cat_label), method = "lm",
                  se = FALSE, linewidth = 0.5, alpha = 0.6) +
      facet_wrap(~ cat_label, scales = "free", ncol = 3) +
      scale_colour_manual(values = CAT_COLOURS, guide = "none") +
      {if (sr_fmt == "comma")
         list(scale_x_continuous(labels = scales::comma),
              scale_y_continuous(labels = scales::comma))
       else
         list(scale_x_continuous(labels = scales::dollar_format(scale=1e-6, suffix="M")),
              scale_y_continuous(labels = scales::dollar_format(scale=1e-6, suffix="M")))} +
      labs(title    = sprintf("%s — Actual vs Predicted Scatter", sr_label),
           subtitle = "Dashed line = perfect forecast (45°); points = OOS quarters",
           x = paste("Actual", sr_label),
           y = paste("Predicted", sr_label)) +
      theme_cu()

    save_plot(p, sprintf("12_scatter_%s", sr), width = 13, height = 9)
  }

  # ── P13: Level forecast error (actual minus predicted) over time ─
  message("    P13: Level forecast error over time...")

  level_fc[, level_error := actual_level - pred_level]

  for (sr in names(series_meta)) {
    sr_label <- series_meta[[sr]]$label
    sr_fmt   <- series_meta[[sr]]$fmt

    fc_sr <- level_fc[series == sr & !is.na(level_error) & !is.na(cat_label)]
    if (nrow(fc_sr) == 0) next

    p <- ggplot(fc_sr, aes(x = as.Date(date), y = level_error,
                            fill = level_error >= 0)) +
      geom_col(width = 60) +
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
      facet_wrap(~ cat_label, scales = "free_y", ncol = 3) +
      scale_fill_manual(values = c("TRUE" = "#2171b5", "FALSE" = "#cb181d"),
                        guide = "none") +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
      {if (sr_fmt == "comma")
         scale_y_continuous(labels = scales::comma)
       else
         scale_y_continuous(labels = scales::dollar_format(scale=1e-6, suffix="M"))} +
      labs(title    = sprintf("%s — Forecast Error Over Time", sr_label),
           subtitle = "Actual minus Predicted; blue = over-predicted, red = under-predicted",
           x = NULL, y = "Forecast Error") +
      theme_cu()

    save_plot(p, sprintf("13_level_error_%s", sr), width = 14, height = 10)
  }

  # ── P14: Level OOS R² scorecard ──────────────────────────────
  message("    P14: Level-space R² scorecard...")

  if (nrow(level_metrics) > 0 && any(!is.na(level_metrics$y_label))) {
    level_metrics_p <- level_metrics[!is.na(y_label) & !is.na(cat_label)]
    p <- ggplot(level_metrics_p,
                aes(x = reorder(cat_label, ifelse(is.na(r2_oos), 0, r2_oos)),
                    y = r2_oos, fill = r2_oos > 0)) +
      geom_col() +
      geom_text(aes(label = sprintf("%.3f", r2_oos),
                    hjust = ifelse(r2_oos >= 0, -0.1, 1.1)),
                size = 3) +
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
      coord_flip() +
      facet_wrap(~ y_label, scales = "free_x", ncol = 3) +
      scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"),
                        guide = "none") +
      scale_x_discrete(limits = rev) +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.25))) +
      labs(title    = "Level-Space Out-of-Sample R² (Back-Transformed)",
           subtitle = "Green = positive predictive skill in original units",
           x = NULL, y = "OOS R²") +
      theme_cu()

    save_plot(p, "14_level_r2_scorecard", width = 14, height = 6)
  }

  # ── P15: System-wide totals — sum across all categories ──────
  message("    P15: System-wide totals...")

  sys_actual <- full_levels[,
    .(total_ficu   = sum(ficu_count,  na.rm=TRUE),
      total_fiscu  = sum(fiscu_count, na.rm=TRUE),
      total_assets = sum(assets_tot,  na.rm=TRUE)),
    by = date]

  sys_pred <- level_fc[!is.na(pred_level),
    .(pred_ficu   = sum(pred_level[series=="ficu_count"],  na.rm=TRUE),
      pred_fiscu  = sum(pred_level[series=="fiscu_count"], na.rm=TRUE),
      pred_assets = sum(pred_level[series=="total_assets"],na.rm=TRUE)),
    by = date]

  sys_all <- merge(sys_actual, sys_pred, by = "date", all = TRUE)

  # Long format for plotting
  sys_long_act <- melt(sys_all[, .(date, total_ficu, total_fiscu, total_assets)],
                       id.vars = "date", variable.name = "series",
                       value.name = "actual_sys")
  sys_long_pred <- melt(sys_all[, .(date, pred_ficu, pred_fiscu, pred_assets)],
                        id.vars = "date", variable.name = "series",
                        value.name = "pred_sys")
  sys_long_act[,  series := gsub("total_", "", series)]
  sys_long_pred[, series := gsub("pred_",  "", series)]
  sys_long <- merge(sys_long_act, sys_long_pred, by = c("date", "series"), all = TRUE)

  sys_long[, sr_label := fcase(
    series == "ficu",   "System FICU Count",
    series == "fiscu",  "System FISCU Count",
    series == "assets", "System Total Assets ($000s)",
    default = series
  )]

  fc_start_sys <- min(level_fc$date, na.rm = TRUE)

  sys_long <- sys_long[!is.na(sr_label)]
  if (nrow(sys_long) == 0 || length(unique(sys_long$sr_label)) == 0) {
    message("    P15: skipped — sys_long empty after filter")
  } else {
  p <- ggplot(sys_long, aes(x = as.Date(date))) +
    annotate("rect",
             xmin = as.Date(fc_start_sys), xmax = Inf,
             ymin = -Inf, ymax = Inf,
             fill = "#fff3cd", alpha = 0.5) +
    geom_line(aes(y = actual_sys, colour = "Actual"),  linewidth = 1.0) +
    geom_line(aes(y = pred_sys,   colour = "Forecast"),
              linewidth = 0.9, linetype = "dashed") +
    geom_vline(xintercept = as.Date(fc_start_sys),
               linetype = "dotted", colour = "grey40", linewidth = 0.6) +
    facet_wrap(~ sr_label, scales = "free_y", ncol = 3) +
    scale_colour_manual(values = c("Actual" = "#1f77b4", "Forecast" = "#d62728"),
                        name = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    scale_y_continuous(labels = scales::comma) +
    labs(title    = "System-Wide Totals — Actual vs Forecast (All Categories Summed)",
         subtitle = "Yellow = OOS forecast period; dashed = model forecast",
         x = NULL, y = NULL) +
    theme_cu()

  save_plot(p, "15_system_totals", width = 14, height = 6)
  }  # end else sys_long non-empty

}  # end if nrow(level_fc) > 0


# ════════════════════════════════════════════════════════════
# 6c. POLICY-DECISION PLOTS  (P16–P18)
#
# Three publication-quality charts for senior leadership:
#   P16 — FICU Count:        full history + OOS forecast,
#                             all 7 categories on one chart
#   P17 — FISCU Count:        same
#   P18 — Log Total Assets:   same (exp-transformed to $ levels)
#
# Design principles:
#   • Full historical time series from 2005 Q1
#   • OOS forecast region shaded in amber
#   • Distinct colour + linetype per category
#   • Dotted vertical line at forecast start
#   • 1-year and 3-year horizon markers
#   • Annotation of final (most recent OOS) predicted value
#   • Clean theme suitable for PDF / Word embedding
# ════════════════════════════════════════════════════════════
message("\n[6c] Policy-decision plots (P16–P18)...")

# ── Build a combined history + forecast data frame per series ──
make_policy_data <- function(series_name, hist_col, fc_series_id,
                              scale_fn = identity) {
  # Historical
  hist_dt <- unique(qtrly[, .(date, cat_label, value = get(hist_col))])
  hist_dt[, segment  := "Historical"]
  hist_dt[, value    := scale_fn(value)]
  hist_dt[, cat_label := as.character(cat_label)]
  # Convert date to plain numeric to strip ALL class attributes before bind
  hist_dt[, date_num := as.numeric(date)]

  # OOS forecast
  fc_dt <- level_fc[series == fc_series_id & !is.na(pred_level),
                     .(date, cat_label, value = scale_fn(pred_level))]
  fc_dt[, segment   := "Forecast"]
  fc_dt[, cat_label := as.character(cat_label)]
  fc_dt[, date_num  := as.numeric(date)]

  # Build plain data.frames with identical column types — no class attributes
  h_df <- data.frame(
    date_num  = as.numeric(hist_dt$date_num),
    cat_label = as.character(hist_dt$cat_label),
    value     = as.numeric(hist_dt$value),
    segment   = "Historical",
    stringsAsFactors = FALSE
  )
  f_df <- data.frame(
    date_num  = as.numeric(fc_dt$date_num),
    cat_label = as.character(fc_dt$cat_label),
    value     = as.numeric(fc_dt$value),
    segment   = "Forecast",
    stringsAsFactors = FALSE
  )

  # Safe bind — plain data.frames, no yearqtr class to clash
  all_df <- rbind(h_df, f_df)

  # Restore yearqtr for axis formatting in ggplot
  all_dt <- as.data.table(all_df)
  all_dt[, date := zoo::as.yearqtr(date_num)]
  all_dt[, date_num := NULL]

  all_dt <- all_dt[!is.na(value) & !is.na(cat_label)]
  setorderv(all_dt, c("cat_label", "date"))
  all_dt
}

policy_theme <- function() {
  theme_bw(base_size = 12) +
  theme(
    strip.background   = element_blank(),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(colour = "grey40", size = 11),
    plot.caption       = element_text(colour = "grey55", size = 9,
                                      hjust = 0),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold"),
    legend.key.width   = unit(1.8, "cm"),
    panel.grid.minor   = element_blank(),
    axis.title.y       = element_text(angle = 90, vjust = 0.5)
  )
}

# Linetype cycle for 7 categories (colour already distinguishes them)
LTY_CYCLE <- c("solid","dashed","dotdash","longdash",
                "twodash","solid","dashed")
names(LTY_CYCLE) <- names(CAT_COLOURS)

# Horizon dates for vertical reference lines
fc_start_pol <- if (nrow(level_fc) > 0)
                  as.Date(min(level_fc$date, na.rm=TRUE)) else NULL
h1yr <- as.Date(zoo::as.yearqtr("2022 Q1"))   # adjust if TRAIN_END changes
h3yr <- as.Date(zoo::as.yearqtr("2024 Q1"))

plot_policy_chart <- function(pd, y_label, title_text,
                               y_scale = "comma", stem = NULL) {

  if (nrow(pd) == 0 || is.null(fc_start_pol)) return(invisible(NULL))

  # Final forecast quarter and value per category (for labels)
  fc_pd    <- pd[segment == "Forecast"]
  last_fc  <- fc_pd[, .SD[which.max(as.numeric(date))], by = cat_label]

  scale_y_fn <- if (y_scale == "dollar")
    scale_y_continuous(labels = scales::dollar_format(scale=1e-6, suffix="B"))
  else
    scale_y_continuous(labels = scales::comma)

  p <- ggplot(pd, aes(x = as.Date(date), y = value,
                       colour = cat_label)) +
    # Amber forecast region
    annotate("rect",
             xmin = fc_start_pol, xmax = as.Date(max(pd$date)) + 90,
             ymin = -Inf, ymax = Inf,
             fill = "#FFF3CD", alpha = 0.55) +
    # Vertical line at forecast start
    geom_vline(xintercept = fc_start_pol,
               linetype = "dotted", colour = "grey45", linewidth = 0.7) +
    # Horizon markers
    geom_vline(xintercept = h1yr,
               linetype = "dotdash", colour = "grey65", linewidth = 0.45) +
    geom_vline(xintercept = h3yr,
               linetype = "dotdash", colour = "grey65", linewidth = 0.45) +
    # Horizon labels at top
    annotate("text", x = h1yr + 10, y = Inf,
             label = "1yr", colour = "grey50", size = 3.2,
             vjust = 1.6, hjust = 0) +
    annotate("text", x = h3yr + 10, y = Inf,
             label = "3yr", colour = "grey50", size = 3.2,
             vjust = 1.6, hjust = 0) +
    # Historical: solid lines
    geom_line(data = pd[segment == "Historical"],
              aes(linetype = cat_label),
              linewidth = 0.85, alpha = 0.9) +
    # Forecast: dashed lines (heavier)
    geom_line(data = pd[segment == "Forecast"],
              aes(linetype = cat_label),
              linewidth = 0.9, alpha = 1.0) +
    # Endpoint dot on forecast line
    geom_point(data = last_fc,
               aes(x = as.Date(date), y = value),
               size = 2.5, shape = 21, fill = "white", stroke = 1.2) +
    # Value label at forecast endpoint
    ggplot2::geom_text(
      data = last_fc,
      aes(x = as.Date(date), y = value,
          label = if (y_scale == "dollar")
            sprintf("$%.1fB", value / 1e6)
          else
            scales::comma(round(value))),
      hjust = -0.15, size = 2.9, fontface = "bold",
      show.legend = FALSE
    ) +
    scale_colour_manual(values = CAT_COLOURS, name = "Asset Category") +
    scale_linetype_manual(values = LTY_CYCLE,  name = "Asset Category") +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years",
                 expand = expansion(mult = c(0.02, 0.12))) +
    scale_y_fn +
    labs(
      title    = title_text,
      subtitle = sprintf(
        "Historical (solid) | Post-LASSO OLS Forecast (dashed) | Amber = OOS period | Forecast start: %s",
        format(fc_start_pol, "%Y Q?")
      ),
      caption  = paste(
        "Model: Rolling-window LASSO → Post-LASSO OLS with quarterly seasonal dummies.",
        "Variables selected at p<", SIG_LEVEL, "with sign consistency checks.",
        "\n7 NCUA asset-size categories. Source: NCUA Call Reports."
      ),
      x = NULL, y = y_label
    ) +
    policy_theme() +
    guides(colour   = guide_legend(nrow = 2, byrow = TRUE),
           linetype = guide_legend(nrow = 2, byrow = TRUE))

  if (!is.null(stem)) save_plot(p, stem, width = 14, height = 7)
  invisible(p)
}

# ── P16: FICU Count ──────────────────────────────────────────
message("    P16: FICU Count policy chart...")
if ("ficu_count" %in% names(qtrly) && nrow(level_fc[series=="ficu_count"]) > 0) {
  pd_ficu <- make_policy_data("ficu_count", "ficu_count", "ficu_count")
  plot_policy_chart(
    pd          = pd_ficu,
    y_label     = "Number of FICUs",
    title_text  = "FICU Count — Historical Trend & Model Forecast by Asset Category",
    y_scale     = "comma",
    stem        = "P16_policy_ficu_count"
  )
} else {
  message("    P16: skipped — ficu_count not available in level_fc")
}

# ── P17: FISCU Count ─────────────────────────────────────────
message("    P17: FISCU Count policy chart...")
if ("fiscu_count" %in% names(qtrly) && nrow(level_fc[series=="fiscu_count"]) > 0) {
  pd_fiscu <- make_policy_data("fiscu_count", "fiscu_count", "fiscu_count")
  plot_policy_chart(
    pd          = pd_fiscu,
    y_label     = "Number of FISCUs",
    title_text  = "FISCU Count — Historical Trend & Model Forecast by Asset Category",
    y_scale     = "comma",
    stem        = "P17_policy_fiscu_count"
  )
} else {
  message("    P17: skipped — fiscu_count not available in level_fc")
}

# ── P18: Total Assets (exp-transformed from ln_assets_tot) ───
message("    P18: Total Assets policy chart...")
if ("assets_tot" %in% names(qtrly) && nrow(level_fc[series=="total_assets"]) > 0) {
  pd_assets <- make_policy_data("assets_tot", "assets_tot", "total_assets")
  plot_policy_chart(
    pd          = pd_assets,
    y_label     = "Total Assets ($B)",
    title_text  = "Total Assets — Historical Trend & Model Forecast by Asset Category",
    y_scale     = "dollar",
    stem        = "P18_policy_assets"
  )
} else {
  message("    P18: skipped — assets_tot / total_assets not available in level_fc")
}

message("    Policy charts complete (P16, P17, P18)")

# ════════════════════════════════════════════════════════════
# 7. FINAL COEFFICIENT SUMMARY TABLE
# ════════════════════════════════════════════════════════════
message("\n[7] Building coefficient summary table...")

if (nrow(coefs_all) > 0) {
  coef_summary <- coefs_all[selected == TRUE,
                              .(mean_est  = mean(estimate,  na.rm=TRUE),
                                sd_est    = sd(estimate,    na.rm=TRUE),
                                mean_pval = mean(p_value,   na.rm=TRUE),
                                pct_pos   = mean(estimate > 0, na.rm=TRUE)*100,
                                n_windows = .N),
                              by = .(dep_var, cat_label, variable)]
  coef_summary <- coef_summary[order(dep_var, cat_label, -n_windows)]
  fwrite(coef_summary, file.path(RESULT_DIR, "coefficient_summary.csv"))

  message("\n    Top 5 most consistently selected variables per dep var:")
  for (dv in names(DEP_VARS)) {
    top5 <- head(coef_summary[dep_var == dv][order(-n_windows)], 5)
    if (nrow(top5) > 0) {
      message(sprintf("\n    %s:", DEP_VARS[[dv]]$label))
      print(top5[, .(variable, cat_label,
                     mean_est  = round(mean_est,  3),
                     mean_pval = round(mean_pval, 3),
                     n_windows)])
    }
  }
}

# ════════════════════════════════════════════════════════════
# 7b. STARGAZER REGRESSION TABLES
# ════════════════════════════════════════════════════════════
message("\n[7b] Building stargazer regression tables...")

# We produce three output files — one per dep var — each containing
# 7 side-by-side OLS columns (one per asset category).
# The model stored is the LAST rolling window's final post-LASSO OLS fit,
# which trains on the longest available history.
#
# Additionally a single combined LaTeX/HTML file is produced with all 21.

if (length(all_ols_fits) == 0) {
  message("    No OLS fit objects found — all models fell back to LASSO only.")
  message("    Stargazer tables skipped. Try relaxing SIG_LEVEL or CORR_CUT.")
} else {

  # Helper: clean variable names for display (remove prefix clutter)
  clean_varname <- function(x) {
    x <- gsub("^yoy_",  "YoY: ",  x)
    x <- gsub("^qoq_",  "QoQ: ",  x)
    x <- gsub("_lag([0-9]+)$", " (lag \\1q)", x)
    x <- gsub("_rmean([0-9]+)$", " (rmean \\1q)", x)
    x <- gsub("_rsd([0-9]+)$",   " (rsd \\1q)",   x)
    x <- gsub("_accel$",  " (accel)",  x)
    x <- gsub("_cyc$",    " (cyc)",    x)
    x <- gsub("_",  " ",  x)
    x
  }

  # Category display labels (short for table headers)
  cat_short <- c(
    "1_Less_10M"   = "<$10M",
    "2_10M_50M"    = "$10-50M",
    "3_50M_100M"   = "$50-100M",
    "4_100M_500M"  = "$100-500M",
    "5_500M_1B"    = "$500M-1B",
    "6_1B_10B"     = "$1-10B",
    "7_10B_Plus"   = ">$10B"
  )

  sg_out_dir <- RESULT_DIR

  for (dv in names(DEP_VARS)) {
    dv_label <- DEP_VARS[[dv]]$label
    dv_short <- DEP_VARS[[dv]]$short

    message(sprintf("    Building table: %s...", dv_label))

    # Collect one fit per category (7 models)
    fits_dv   <- list()
    col_names <- character(0)
    n_obs_vec <- integer(0)

    for (cat in cats) {
      key <- paste(dv, cat, sep = "|")
      obj <- all_ols_fits[[key]]
      if (!is.null(obj) && !is.null(obj$fit) && inherits(obj$fit, "lm")) {
        fits_dv[[length(fits_dv) + 1L]] <- obj$fit
        col_names <- c(col_names,
                       cat_short[cat] %||% cat)
        n_obs_vec <- c(n_obs_vec, nobs(obj$fit))
      }
    }

    if (length(fits_dv) == 0) {
      message(sprintf("    No OLS fits available for %s — skipping.", dv_label))
      next
    }

    # ── Collect all variable names across all models ──────────
    all_vars <- unique(unlist(lapply(fits_dv, function(f)
      names(coef(f))[names(coef(f)) != "(Intercept)"])))
    all_vars_clean <- clean_varname(all_vars)

    # ── HTML table (always works, no LaTeX dependency) ────────
    html_file <- file.path(sg_out_dir,
                           sprintf("stargazer_%s.html", dv_short))
    tryCatch({
      stargazer::stargazer(
        fits_dv,
        type             = "html",
        out              = html_file,
        title            = sprintf("Post-LASSO OLS Results — %s", dv_label),
        dep.var.labels   = dv_label,
        column.labels    = col_names,
        covariate.labels = all_vars_clean,
        omit.stat        = c("f", "ser"),
        add.lines        = list(
          c("Asset category",  col_names),
          c("Obs (training)",  as.character(n_obs_vec))
        ),
        notes            = paste(
          sprintf("Post-LASSO OLS. Variables selected by LASSO (lambda.1se) then retained if p < %.2f", SIG_LEVEL),
          "and sign is economically consistent. Last rolling window shown.",
          sprintf("Training period: 2005 Q1 to latest available. Dep var: %s.", dv_label)
        ),
        notes.align      = "l",
        star.cutoffs     = c(0.10, 0.05, 0.01),
        star.char        = c("+", "*", "**"),
        digits           = 4,
        no.space         = TRUE
      )
      message(sprintf("      Saved: %s", html_file))
    }, error = function(e) {
      message(sprintf("      stargazer HTML failed for %s: %s", dv_label, e$message))
    })

    # ── Text table (for console / plain-text archive) ─────────
    txt_file <- file.path(sg_out_dir,
                          sprintf("stargazer_%s.txt", dv_short))
    tryCatch({
      stargazer::stargazer(
        fits_dv,
        type             = "text",
        out              = txt_file,
        title            = sprintf("Post-LASSO OLS Results — %s", dv_label),
        dep.var.labels   = dv_label,
        column.labels    = col_names,
        covariate.labels = all_vars_clean,
        omit.stat        = c("f", "ser"),
        add.lines        = list(
          c("Asset category",  col_names),
          c("Obs (training)",  as.character(n_obs_vec))
        ),
        star.cutoffs     = c(0.10, 0.05, 0.01),
        star.char        = c("+", "*", "**"),
        digits           = 4,
        no.space         = TRUE
      )
      message(sprintf("      Saved: %s", txt_file))
    }, error = function(e) {
      message(sprintf("      stargazer TEXT failed for %s: %s", dv_label, e$message))
    })
  }

  # ── Combined HTML: all 21 models in one file ─────────────────
  message("\n    Building combined HTML (all 21 models)...")
  combined_html <- file.path(sg_out_dir, "stargazer_ALL_MODELS.html")

  # Open HTML wrapper
  html_header <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<meta charset='UTF-8'>\n",
    "<title>Credit Union Growth Forecast — Regression Tables</title>\n",
    "<style>\n",
    "  body { font-family: Arial, sans-serif; margin: 40px; }\n",
    "  h1   { color: #1a3a5c; border-bottom: 2px solid #1a3a5c; }\n",
    "  h2   { color: #2c5f8a; margin-top: 40px; }\n",
    "  table { border-collapse: collapse; margin-bottom: 30px; font-size: 12px; }\n",
    "  td, th { padding: 4px 10px; }\n",
    "  tr:nth-child(even) { background-color: #f2f7fc; }\n",
    "  .section-break { border-top: 3px solid #1a3a5c; margin: 50px 0 20px 0; }\n",
    "</style>\n</head>\n<body>\n",
    "<h1>Credit Union Growth Forecast — Post-LASSO OLS Regression Tables</h1>\n",
    sprintf("<p><em>Generated: %s | Training: 2005 Q1 onward | ",
            format(Sys.time(), "%Y-%m-%d %H:%M")),
    sprintf("Significance: p &lt; %.2f | Last rolling window</em></p>\n", SIG_LEVEL)
  )

  tryCatch({
    sink(combined_html)
    cat(html_header)

    for (dv in names(DEP_VARS)) {
      dv_label <- DEP_VARS[[dv]]$label
      dv_short <- DEP_VARS[[dv]]$short

      cat(sprintf("<div class='section-break'></div>\n"))
      cat(sprintf("<h2>%s</h2>\n", dv_label))

      fits_dv   <- list()
      col_names <- character(0)
      n_obs_vec <- integer(0)
      r2_vec    <- numeric(0)

      for (cat_i in cats) {
        key <- paste(dv, cat_i, sep = "|")
        obj <- all_ols_fits[[key]]
        m_key <- all_metrics[[key]]
        if (!is.null(obj) && !is.null(obj$fit) && inherits(obj$fit, "lm")) {
          fits_dv[[length(fits_dv) + 1L]] <- obj$fit
          col_names <- c(col_names, cat_short[cat_i] %||% cat_i)
          n_obs_vec <- c(n_obs_vec, nobs(obj$fit))
          r2_vec    <- c(r2_vec,
                         if (!is.null(m_key)) round(m_key$r2_oos, 3) else NA_real_)
        }
      }

      if (length(fits_dv) == 0) {
        cat(sprintf("<p><em>No OLS fits available for %s.</em></p>\n", dv_label))
        next
      }

      all_vars       <- unique(unlist(lapply(fits_dv, function(f)
        names(coef(f))[names(coef(f)) != "(Intercept)"])))
      all_vars_clean <- clean_varname(all_vars)

      r2_line <- ifelse(is.na(r2_vec), "—", as.character(r2_vec))

      # Capture stargazer HTML output as string then write
      sg_html <- capture.output(
        stargazer::stargazer(
          fits_dv,
          type             = "html",
          title            = "",
          dep.var.labels   = dv_label,
          column.labels    = col_names,
          covariate.labels = all_vars_clean,
          omit.stat        = c("f", "ser"),
          add.lines        = list(
            c("OOS R²",         r2_line),
            c("Obs (training)", as.character(n_obs_vec))
          ),
          notes            = paste0(
            "Post-LASSO OLS. Vars selected by LASSO then kept if p<",
            SIG_LEVEL, " and sign consistent. ",
            "+ p<0.10, * p<0.05, ** p<0.01."
          ),
          notes.align      = "l",
          star.cutoffs     = c(0.10, 0.05, 0.01),
          star.char        = c("+", "*", "**"),
          digits           = 4,
          no.space         = TRUE
        )
      )
      cat(paste(sg_html, collapse = "\n"), "\n")
    }

    cat("</body>\n</html>\n")
    sink()
    message(sprintf("    Saved combined: %s", combined_html))
  }, error = function(e) {
    if (sink.number() > 0) sink()
    message(sprintf("    Combined HTML failed: %s", e$message))
  })

  message(sprintf("\n    Stargazer files saved to %s/:", sg_out_dir))
  sg_files <- list.files(sg_out_dir, pattern = "stargazer_.*[.](html|txt)$")
  for (f in sg_files) message(sprintf("      %s", f))
}


# ════════════════════════════════════════════════════════════
# 7c. STARGAZER PDF  — all 21 models in one PDF
#
# Strategy (tried in order, first success wins):
#   1. LaTeX path  — write .tex wrapper, compile with pdflatex/xelatex
#   2. HTML→PDF    — convert combined HTML via chrome/wkhtmltopdf
#   3. ggplot path — render each table as a ggplot text panel and
#                    save via ggsave(cairo_pdf) — always available
#
# Output: results_regression/stargazer_regression_tables.pdf
# ════════════════════════════════════════════════════════════
message("\n[7c] Building stargazer PDF...")

sg_pdf_path <- file.path(RESULT_DIR, "stargazer_regression_tables.pdf")

# ── Shared helper already defined above: clean_varname(), cat_short ──
# ── Collect per-dv fits (same logic as 7b) ────────────────────────────
collect_fits_for_dv <- function(dv) {
  fits_dv   <- list()
  col_names <- character(0)
  n_obs_vec <- integer(0)
  r2_vec    <- numeric(0)
  for (cat_i in cats) {
    key <- paste(dv, cat_i, sep = "|")
    obj <- all_ols_fits[[key]]
    m_k <- all_metrics[[paste(dv, cat_i, sep="|")]]
    if (!is.null(obj) && !is.null(obj$fit) && inherits(obj$fit, "lm")) {
      fits_dv[[length(fits_dv)+1L]] <- obj$fit
      col_names <- c(col_names, cat_short[cat_i] %||% cat_i)
      n_obs_vec <- c(n_obs_vec, nobs(obj$fit))
      r2_vec    <- c(r2_vec, if (!is.null(m_k)) round(m_k$r2_oos, 3) else NA_real_)
    }
  }
  list(fits=fits_dv, cols=col_names, nobs=n_obs_vec, r2=r2_vec)
}

sg_pdf_done <- FALSE

# ══════════════════════════════════════════════════════════════
# PATH 1: LaTeX → pdflatex/xelatex
# ══════════════════════════════════════════════════════════════
latex_engine <- tryCatch({
  if (nchar(Sys.which("pdflatex")) > 0) "pdflatex"
  else if (nchar(Sys.which("xelatex")) > 0) "xelatex"
  else ""
}, error = function(e) "")

if (!sg_pdf_done && nchar(latex_engine) > 0 && length(all_ols_fits) > 0) {
  message(sprintf("    LaTeX engine found: %s — trying LaTeX path...", latex_engine))

  tryCatch({
    tex_lines <- character(0)

    # ── LaTeX preamble ──────────────────────────────────────
    tex_lines <- c(tex_lines,
      "\\documentclass[10pt,a4paper]{article}",
      "\\usepackage[a4paper, margin=1.8cm, top=2cm, bottom=2cm]{geometry}",
      "\\usepackage{booktabs}",
      "\\usepackage{dcolumn}",
      "\\usepackage{rotating}",
      "\\usepackage{caption}",
      "\\usepackage{xcolor}",
      "\\usepackage{lmodern}",
      "\\usepackage[T1]{fontenc}",
      "\\definecolor{navyblue}{RGB}{26,58,92}",
      "\\captionsetup{font=small,labelfont=bf}",
      "\\setlength{\\parindent}{0pt}",
      "\\begin{document}",
      "",
      "\\begin{center}",
      "{\\Large\\bfseries\\color{navyblue} Credit Union Growth Forecast}\\\\[4pt]",
      "{\\large Post-LASSO OLS Regression Tables}\\\\[2pt]",
      sprintf("{\\small Generated: %s \\quad Training: 2005 Q1 onward \\quad p-threshold: %.2f}",
              format(Sys.time(), "%Y-%m-%d %H:%M"), SIG_LEVEL),
      "\\end{center}",
      "\\vspace{6pt}",
      "\\hrule",
      "\\vspace{10pt}",
      ""
    )

    # ── One section per dep var ──────────────────────────────
    for (dv in names(DEP_VARS)) {
      dv_label <- DEP_VARS[[dv]]$label
      dv_short <- DEP_VARS[[dv]]$short
      info     <- collect_fits_for_dv(dv)

      tex_lines <- c(tex_lines,
        sprintf("\\section*{%s}", gsub("_", "\\\\_", dv_label)),
        sprintf("\\textit{Dependent variable: \\texttt{%s}. Last rolling window per asset category.}",
                gsub("_", "\\\\_", dv)),
        "\\vspace{4pt}"
      )

      if (length(info$fits) == 0) {
        tex_lines <- c(tex_lines,
          "\\textit{No OLS fits available for this outcome.}", "")
        next
      }

      all_vars_dv   <- unique(unlist(lapply(info$fits, function(f)
        names(coef(f))[names(coef(f)) != "(Intercept)"])))
      all_vars_clean <- clean_varname(all_vars_dv)
      r2_line <- ifelse(is.na(info$r2), "---", as.character(info$r2))

      # Capture LaTeX output from stargazer
      sg_latex <- capture.output(
        stargazer::stargazer(
          info$fits,
          type              = "latex",
          title             = "",
          dep.var.labels    = gsub("%", "\\\\%", dv_label),
          column.labels     = info$cols,
          covariate.labels  = all_vars_clean,
          omit.stat         = c("f", "ser"),
          add.lines         = list(
            c("OOS $R^2$",       r2_line),
            c("Obs (training)",  as.character(info$nobs))
          ),
          notes             = sprintf(
            "Post-LASSO OLS. Vars selected by LASSO ($\\\\lambda_{1SE}$) then kept if $p < %.2f$ and sign consistent. + $p<0.10$, * $p<0.05$, ** $p<0.01$.",
            SIG_LEVEL),
          notes.align       = "l",
          font.size         = "footnotesize",
          column.sep.width  = "1pt",
          star.cutoffs      = c(0.10, 0.05, 0.01),
          star.char         = c("+", "*", "**"),
          digits            = 4,
          no.space          = TRUE,
          float             = TRUE,
          float.env         = "table",
          table.placement   = "H",
          label             = sprintf("tab:%s", dv_short)
        )
      )

      # Wrap wide tables in landscape if many columns
      if (length(info$fits) >= 5) {
        tex_lines <- c(tex_lines,
          "\\begin{sidewaystable}", sg_latex, "\\end{sidewaystable}", "")
      } else {
        tex_lines <- c(tex_lines, sg_latex, "")
      }

      tex_lines <- c(tex_lines, "\\vspace{8pt}", "\\hrule", "\\vspace{8pt}", "")
    }

    tex_lines <- c(tex_lines, "\\end{document}")

    # Write .tex file
    tex_file <- file.path(RESULT_DIR, "stargazer_regression_tables.tex")
    writeLines(tex_lines, tex_file)

    # Compile twice (for cross-refs)
    old_wd <- getwd()
    setwd(RESULT_DIR)
    for (pass in 1:2) {
      compile_res <- system2(
        latex_engine,
        args    = c("-interaction=nonstopmode",
                    "-halt-on-error",
                    "stargazer_regression_tables.tex"),
        stdout  = TRUE, stderr = TRUE
      )
      if (!file.exists("stargazer_regression_tables.pdf"))
        stop("pdflatex ran but no PDF produced")
    }
    setwd(old_wd)

    # Copy compiled PDF to sg_pdf_path (same dir but explicit)
    compiled <- file.path(RESULT_DIR, "stargazer_regression_tables.pdf")
    if (compiled != sg_pdf_path && file.exists(compiled))
      file.copy(compiled, sg_pdf_path, overwrite = TRUE)

    sg_pdf_done <- TRUE
    message(sprintf("    [LaTeX] PDF saved: %s", sg_pdf_path))

  }, error = function(e) {
    message(sprintf("    [LaTeX] FAILED: %s — falling back to next method", e$message))
  })
}

# ══════════════════════════════════════════════════════════════
# PATH 2: HTML → PDF via chrome headless / webshot2 / pagedown
# ══════════════════════════════════════════════════════════════
if (!sg_pdf_done && length(all_ols_fits) > 0) {
  message("    Trying HTML→PDF path (webshot2/pagedown/chrome)...")

  have_webshot2 <- requireNamespace("webshot2", quietly = TRUE)
  have_pagedown <- requireNamespace("pagedown",  quietly = TRUE)

  combined_html_path <- file.path(RESULT_DIR, "stargazer_ALL_MODELS.html")

  converted <- FALSE

  if (!converted && have_webshot2 && file.exists(combined_html_path)) {
    tryCatch({
      webshot2::webshot(
        url    = paste0("file:///", normalizePath(combined_html_path)),
        file   = sg_pdf_path,
        vwidth = 1200, vheight = 900,
        zoom   = 1.5
      )
      if (file.exists(sg_pdf_path) && file.info(sg_pdf_path)$size > 1000) {
        converted  <- TRUE
        sg_pdf_done <- TRUE
        message(sprintf("    [webshot2] PDF saved: %s", sg_pdf_path))
      }
    }, error = function(e)
      message(sprintf("    [webshot2] failed: %s", e$message)))
  }

  if (!converted && have_pagedown && file.exists(combined_html_path)) {
    tryCatch({
      pagedown::chrome_print(
        input  = normalizePath(combined_html_path),
        output = sg_pdf_path,
        options = list(paperWidth = 11, paperHeight = 8.5,
                       marginTop = 0.5, marginBottom = 0.5,
                       marginLeft = 0.5, marginRight = 0.5)
      )
      if (file.exists(sg_pdf_path) && file.info(sg_pdf_path)$size > 1000) {
        converted  <- TRUE
        sg_pdf_done <- TRUE
        message(sprintf("    [pagedown] PDF saved: %s", sg_pdf_path))
      }
    }, error = function(e)
      message(sprintf("    [pagedown] failed: %s", e$message)))
  }
}

# ══════════════════════════════════════════════════════════════
# PATH 3: ggplot text panels — always available
# Renders each table as a formatted text grob via ggplot2 + cairo_pdf.
# Not as pretty as real LaTeX but guaranteed to work anywhere.
# ══════════════════════════════════════════════════════════════
if (!sg_pdf_done && length(all_ols_fits) > 0) {
  message("    Using ggplot text-panel fallback for PDF...")

  tryCatch({
    # Produce a text-format stargazer string per dep var, then render
    # each as a monospace text page.

    pdf(sg_pdf_path, width = 13, height = 9, onefile = TRUE)

    # Cover page
    plot.new()
    text(0.5, 0.72,
         "Credit Union Growth Forecast",
         cex = 1.8, font = 2, col = "#1A3A5C")
    text(0.5, 0.60,
         "Post-LASSO OLS Regression Tables",
         cex = 1.3, font = 1, col = "#1A3A5C")
    text(0.5, 0.50,
         sprintf("Generated: %s   |   Training: 2005 Q1+   |   p < %.2f",
                 format(Sys.time(), "%Y-%m-%d %H:%M"), SIG_LEVEL),
         cex = 0.9, col = "grey40")
    text(0.5, 0.40,
         sprintf("Models: %d  (%d dep vars x 7 asset categories)",
                 length(all_ols_fits), length(DEP_VARS)),
         cex = 0.9, col = "grey40")

    for (dv in names(DEP_VARS)) {
      dv_label <- DEP_VARS[[dv]]$label
      info     <- collect_fits_for_dv(dv)

      if (length(info$fits) == 0) {
        plot.new()
        text(0.5, 0.5, sprintf("No OLS fits for:\n%s", dv_label),
             cex = 1.2, col = "grey50")
        next
      }

      all_vars_dv    <- unique(unlist(lapply(info$fits, function(f)
        names(coef(f))[names(coef(f)) != "(Intercept)"])))
      all_vars_clean <- clean_varname(all_vars_dv)
      r2_line        <- ifelse(is.na(info$r2), "---", as.character(info$r2))

      # Capture text table from stargazer
      txt_tbl <- capture.output(
        stargazer::stargazer(
          info$fits,
          type             = "text",
          title            = sprintf("%s", dv_label),
          dep.var.labels   = dv_label,
          column.labels    = info$cols,
          covariate.labels = all_vars_clean,
          omit.stat        = c("f", "ser"),
          add.lines        = list(
            c("OOS R2",         r2_line),
            c("Obs (training)", as.character(info$nobs))
          ),
          notes            = sprintf(
            "Post-LASSO OLS. lambda.1se LASSO then p<%.2f + sign check. +p<0.10 *p<0.05 **p<0.01",
            SIG_LEVEL),
          star.cutoffs     = c(0.10, 0.05, 0.01),
          star.char        = c("+", "*", "**"),
          digits           = 4,
          no.space         = TRUE
        )
      )

      # How many pages does this table need?
      # Each page can fit ~55 lines of 8pt monospace
      lines_per_page <- 54L
      n_pages <- ceiling(length(txt_tbl) / lines_per_page)

      for (pg in seq_len(n_pages)) {
        idx_start <- (pg - 1L) * lines_per_page + 1L
        idx_end   <- min(pg * lines_per_page, length(txt_tbl))
        page_lines <- txt_tbl[idx_start:idx_end]

        plot.new()
        par(mar = c(1,1,2,1))

        # Section header (first page only)
        if (pg == 1L) {
          title_txt <- sprintf("%s   [%d/%d]", dv_label, pg, n_pages)
          mtext(title_txt, side = 3, cex = 1.1, font = 2,
                col = "#1A3A5C", line = 0.2)
        } else {
          mtext(sprintf("%s (cont'd — page %d)", dv_label, pg),
                side = 3, cex = 0.9, col = "grey40", line = 0.2)
        }

        text(0.0, 0.98,
             paste(page_lines, collapse = "\n"),
             family = "mono", cex = 0.62,
             adj    = c(0, 1),
             xpd    = TRUE)
      }
    }

    # Metrics summary page
    plot.new()
    par(mar = c(1,1,2,1))
    mtext("OOS Metrics Summary", side = 3, cex = 1.1, font = 2,
          col = "#1A3A5C", line = 0.2)

    if (nrow(metrics_all) > 0) {
      m_txt <- capture.output(
        print(metrics_all[order(dep_var, cat_label),
              .(dep_var, cat_label,
                n, rmse = round(rmse, 3),
                mae  = round(mae, 3),
                r2   = round(r2_oos, 3))],
              row.names = FALSE)
      )
      text(0.0, 0.95,
           paste(m_txt, collapse = "\n"),
           family = "mono", cex = 0.72, adj = c(0,1), xpd = TRUE)
    } else {
      text(0.5, 0.5, "No metrics available", cex = 1, col = "grey50")
    }

    dev.off()
    sg_pdf_done <- TRUE
    message(sprintf("    [ggplot text] PDF saved: %s", sg_pdf_path))

  }, error = function(e) {
    if (dev.cur() > 1) dev.off()
    message(sprintf("    [ggplot text] FAILED: %s", e$message))
  })
}

if (sg_pdf_done)
  message(sprintf("    Stargazer PDF complete: %s", sg_pdf_path))
else
  message("    [WARN] All PDF paths failed — check LaTeX / chrome installation.")

# ════════════════════════════════════════════════════════════
# 9. EXCEL EXPORT — ONE TAB PER ASSET CATEGORY
#
# Uses openxlsx2 (the actively maintained successor to openxlsx,
# identical API). Falls back to writexl (no Java, always available)
# for plain data export if openxlsx2 is also unavailable.
#
# Each sheet contains:
#   A  : Quarter
#   B  : Row type (Historical / Forecast)
#   C  : FICU Count
#   D  : FICU Lo 95%
#   E  : FICU Hi 95%
#   F  : FISCU Count
#   G  : FISCU Lo 95%
#   H  : FISCU Hi 95%
#   I  : Total Assets ($000s)
#   J  : Assets Lo 95%
#   K  : Assets Hi 95%
#   L  : Method
#
# Formatting (openxlsx2 path):
#   • Header row  : navy fill, white bold text
#   • Historical  : white background
#   • Forecast    : amber (#FFF3CD) background
#   • CI columns  : light blue (#DEEBF7)
#   • Freeze pane at row 3 / col 2
#   • README tab with model notes
# ════════════════════════════════════════════════════════════
message("\n[9] Excel export — one tab per asset category...")

xl_path <- file.path(RESULT_DIR, "CU_Forecast_by_Category.xlsx")

# ── Package detection ────────────────────────────────────────
have_oxl2  <- requireNamespace("openxlsx2", quietly = TRUE)
have_writexl <- requireNamespace("writexl",  quietly = TRUE)

if (!have_oxl2) {
  message("    openxlsx2 not found — attempting install...")
  tryCatch({
    install.packages("openxlsx2", repos = "https://cloud.r-project.org",
                     quiet = TRUE)
    have_oxl2 <- requireNamespace("openxlsx2", quietly = TRUE)
  }, error = function(e)
    message(sprintf("    openxlsx2 install failed: %s", e$message)))
}

if (!have_writexl) {
  tryCatch({
    install.packages("writexl", repos = "https://cloud.r-project.org",
                     quiet = TRUE)
    have_writexl <- requireNamespace("writexl", quietly = TRUE)
  }, error = function(e) NULL)
}

message(sprintf("    openxlsx2: %s   writexl: %s",
                if(have_oxl2) "available" else "NOT available",
                if(have_writexl) "available" else "NOT available"))

# ── Short category → sheet name map ─────────────────────────
cat_sheet_names <- c(
  "1_Less_10M"   = "Less than 10M",
  "2_10M_50M"    = "10M to 50M",
  "3_50M_100M"   = "50M to 100M",
  "4_100M_500M"  = "100M to 500M",
  "5_500M_1B"    = "500M to 1B",
  "6_1B_10B"     = "1B to 10B",
  "7_10B_Plus"   = "Over 10B"
)

cats_xl <- sort(unique(level_fc$cat_label))

# ── Helper: build the data table for one category ───────────
# NOTE ON DESIGN:
#   level_fc holds OOS rolling-window predictions — these are quarters
#   that ALSO exist in full_levels as actual observations (the model was
#   evaluated on them).  We therefore keep BOTH rows per quarter:
#     • Historical row  = actual values from full_levels
#     • Forecast row    = model prediction + 95% CI from level_fc
#   Rows are distinguished by the Row_Type column.
#   The old deduplication  fc_rows_xl[!Quarter %in% hist_rows$Quarter]
#   was silently deleting every forecast row — that is now removed.
#
# MATCH SAFETY:
#   level_fc$date may carry yearqtr class; fc_dates inherits it via c().
#   match() on yearqtr vs yearqtr is safe, but we convert both sides to
#   numeric to guarantee correct alignment regardless of class state.
build_cat_table <- function(cat) {

  # ── Historical actuals ────────────────────────────────────
  hist_cat <- full_levels[cat_label == cat]
  setorderv(hist_cat, "date")

  hist_rows <- data.table(
    Quarter     = as.character(hist_cat$date),
    Row_Type    = "Historical",
    FICU_Count  = hist_cat$ficu_count,
    FICU_Lo95   = NA_real_,
    FICU_Hi95   = NA_real_,
    FISCU_Count = hist_cat$fiscu_count,
    FISCU_Lo95  = NA_real_,
    FISCU_Hi95  = NA_real_,
    Assets_000s = hist_cat$assets_tot,
    Assets_Lo95 = NA_real_,
    Assets_Hi95 = NA_real_,
    Method      = "Historical"
  )

  # ── OOS forecasts from level_fc ───────────────────────────
  fc_cat_ficu   <- level_fc[series == "ficu_count"   & cat_label == cat]
  fc_cat_fiscu  <- level_fc[series == "fiscu_count"  & cat_label == cat]
  fc_cat_assets <- level_fc[series == "total_assets" & cat_label == cat]

  # Collect all unique forecast dates (numeric for safe match)
  fc_dates_num <- sort(unique(as.numeric(c(
    fc_cat_ficu$date, fc_cat_fiscu$date, fc_cat_assets$date
  ))))

  if (length(fc_dates_num) > 0) {

    # match() on numeric — immune to yearqtr class mismatches
    gv <- function(fc_dt, dvec_num) {
      fc_num <- as.numeric(fc_dt$date)
      idx    <- match(dvec_num, fc_num)
      list(
        pred   = fc_dt$pred_level[idx],
        lo95   = if ("pred_lo95" %in% names(fc_dt))
                   fc_dt$pred_lo95[idx]
                 else rep(NA_real_, length(dvec_num)),
        hi95   = if ("pred_hi95" %in% names(fc_dt))
                   fc_dt$pred_hi95[idx]
                 else rep(NA_real_, length(dvec_num)),
        method = fc_dt$method_used[idx]
      )
    }

    fv <- gv(fc_cat_ficu,  fc_dates_num)
    sv <- gv(fc_cat_fiscu, fc_dates_num)
    av <- gv(fc_cat_assets,fc_dates_num)

    method_vec <- ifelse(!is.na(fv$method), fv$method,
                  ifelse(!is.na(sv$method), sv$method, av$method))

    # Convert numeric dates back to readable quarter strings
    fc_dates_chr <- as.character(zoo::as.yearqtr(fc_dates_num))

    fc_rows_xl <- data.table(
      Quarter     = fc_dates_chr,
      Row_Type    = "Forecast",
      FICU_Count  = round(fv$pred),
      FICU_Lo95   = round(fv$lo95),
      FICU_Hi95   = round(fv$hi95),
      FISCU_Count = round(sv$pred),
      FISCU_Lo95  = round(sv$lo95),
      FISCU_Hi95  = round(sv$hi95),
      Assets_000s = round(av$pred),
      Assets_Lo95 = round(av$lo95),
      Assets_Hi95 = round(av$hi95),
      Method      = method_vec
    )

    # Drop forecast rows where ALL three predictions are NA
    # (means level_fc had no usable prediction for that quarter)
    fc_rows_xl <- fc_rows_xl[
      !(is.na(FICU_Count) & is.na(FISCU_Count) & is.na(Assets_000s))
    ]

    # Stack: historical first, then forecast
    # Both row types are kept even when quarters overlap —
    # Row_Type distinguishes actual vs model prediction.
    all_rows <- rbindlist(list(hist_rows, fc_rows_xl), fill = TRUE)

  } else {
    message(sprintf("    [WARN] build_cat_table: no forecast dates found for %s", cat))
    all_rows <- hist_rows
  }

  # Sort by Quarter string — yearqtr format "YYYY QN" sorts correctly
  setorderv(all_rows, c("Quarter", "Row_Type"))
  all_rows
}

# ════════════════════════════════════════════════════════════
# PATH A: openxlsx2  (formatted workbook)
# ════════════════════════════════════════════════════════════
xl_done <- FALSE

if (have_oxl2) {
  tryCatch({
    library(openxlsx2)

    wb <- wb_workbook(creator = "Part3_Regression",
                      title   = "CU Forecast by Asset Category")

    # ── Colour constants ──
    NAVY   <- "#1A3A5C"
    AMBER  <- "#FFF3CD"
    BLUE   <- "#DEEBF7"
    WHITE  <- "#FFFFFF"
    LGREY  <- "#E0E0E0"

    # ── Shared cell styles (openxlsx2 uses wb_add_cell_style / wbCellStyle) ──
    # openxlsx2 applies styles via wb_add_fill / wb_add_font / wb_add_border
    # We use the functional helpers directly on ranges.

    # ── README sheet ──────────────────────────────────────────
    wb <- wb_add_worksheet(wb, sheet = "README", tab_color = NAVY)

    readme_df <- data.frame(
      Field = c("Report", "Generated", "Model",
                "Dep vars", "Level vars", "Training period",
                "Significance", "Prediction intervals",
                "Amber rows", "Blue CI cols", "Method col",
                "FICU", "FISCU", "Total Assets"),
      Value = c(
        "Credit Union Growth Forecast — Level Forecasts by Asset Category",
        format(Sys.time(), "%Y-%m-%d %H:%M"),
        "Rolling-window LASSO -> Post-LASSO OLS with quarterly seasonal dummies (Q1/Q2/Q3)",
        "yoy_ficu_pct | yoy_fiscu_pct | ln_assets_tot",
        "FICU Count | FISCU Count | Total Assets ($000s)",
        sprintf("2005 Q1 onwards; OOS forecast from %s", as.character(TRAIN_END)),
        sprintf("p < %.2f; variables pass sign-consistency check", SIG_LEVEL),
        "95% PI from OLS predict(interval='prediction'); LASSO fallback = +/-1.96*RMSE",
        "Out-of-sample forecast quarters",
        "95% PI lower and upper bounds",
        "OLS | LASSO | Historical",
        "Federally Insured Credit Union",
        "Federally Insured State-Chartered Credit Union",
        "Sum of total assets within category ($000s)"
      ), stringsAsFactors = FALSE
    )

    # Title row
    wb <- wb_add_data(wb, sheet = "README",
                      x = "Credit Union Growth Forecast — Level Forecasts by Asset Category",
                      start_row = 1, start_col = 1, col_names = FALSE)
    wb <- wb_add_font(wb, sheet = "README",
                      dims = "A1", bold = TRUE, size = 14, color = NAVY)

    # Data table
    wb <- wb_add_data_table(wb, sheet = "README",
                            x = readme_df,
                            start_row = 2, start_col = 1,
                            table_name = "README_tbl",
                            table_style = "TableStyleMedium9")
    wb <- wb_set_col_widths(wb, sheet = "README",
                             cols = 1:2, widths = c(28, 85))

    # ── Category sheets ────────────────────────────────────────
    col_header_labels <- c(
      "Quarter", "Type",
      "FICU Count", "FICU Lo 95%", "FICU Hi 95%",
      "FISCU Count", "FISCU Lo 95%", "FISCU Hi 95%",
      "Total Assets ($000s)", "Assets Lo 95%", "Assets Hi 95%",
      "Method"
    )
    col_widths <- c(12, 12, 14, 14, 14, 14, 14, 14, 18, 16, 16, 11)

    for (cat in cats_xl) {
      sn <- cat_sheet_names[cat]
      if (is.na(sn) || nchar(sn) == 0) sn <- cat

      wb <- wb_add_worksheet(wb, sheet = sn, tab_color = "#2171B5")

      all_rows <- build_cat_table(cat)
      n_data   <- nrow(all_rows)

      # Title row (row 1)
      title_txt <- sprintf("CU Forecast — %s  |  %s", sn,
                            format(Sys.time(), "%Y-%m-%d"))
      wb <- wb_add_data(wb, sheet = sn, x = title_txt,
                        start_row = 1, start_col = 1, col_names = FALSE)
      wb <- wb_merge_cells(wb, sheet = sn, dims = "A1:L1")
      wb <- wb_add_font(wb, sheet = sn,
                        dims = "A1", bold = TRUE, size = 12, color = NAVY)

      # Header row (row 2)
      wb <- wb_add_data(wb, sheet = sn,
                        x = as.data.frame(t(col_header_labels)),
                        start_row = 2, start_col = 1, col_names = FALSE)
      wb <- wb_add_fill(wb, sheet = sn,
                        dims = sprintf("A2:L2"), color = NAVY)
      wb <- wb_add_font(wb, sheet = sn,
                        dims = "A2:L2", bold = TRUE,
                        color = WHITE)
      wb <- wb_set_row_heights(wb, sheet = sn, rows = 2, heights = 22)

      # Data (rows 3+)
      wb <- wb_add_data(wb, sheet = sn, x = as.data.frame(all_rows),
                        start_row = 3, start_col = 1, col_names = FALSE)

      # ── Row fills: amber for Forecast, white for Historical ──
      fc_idx   <- which(all_rows$Row_Type == "Forecast")
      hist_idx <- which(all_rows$Row_Type == "Historical")

      # Amber rows (all cols)
      if (length(fc_idx) > 0) {
        for (ri in fc_idx) {
          r <- ri + 2L  # offset by 2 header rows
          wb <- wb_add_fill(wb, sheet = sn,
                            dims = sprintf("A%d:L%d", r, r),
                            color = AMBER)
          # CI columns blue even on forecast rows
          wb <- wb_add_fill(wb, sheet = sn,
                            dims = sprintf("D%d:E%d", r, r), color = BLUE)
          wb <- wb_add_fill(wb, sheet = sn,
                            dims = sprintf("G%d:H%d", r, r), color = BLUE)
          wb <- wb_add_fill(wb, sheet = sn,
                            dims = sprintf("J%d:K%d", r, r), color = BLUE)
        }
      }

      # Blue CI cols on historical rows too (NA values, greyed font)
      if (length(hist_idx) > 0) {
        for (ri in hist_idx) {
          r <- ri + 2L
          wb <- wb_add_fill(wb, sheet = sn,
                            dims = sprintf("D%d:E%d", r, r), color = BLUE)
          wb <- wb_add_fill(wb, sheet = sn,
                            dims = sprintf("G%d:H%d", r, r), color = BLUE)
          wb <- wb_add_fill(wb, sheet = sn,
                            dims = sprintf("J%d:K%d", r, r), color = BLUE)
          wb <- wb_add_font(wb, sheet = sn,
                            dims = sprintf("D%d:E%d", r, r), color = "#AAAAAA")
          wb <- wb_add_font(wb, sheet = sn,
                            dims = sprintf("G%d:H%d", r, r), color = "#AAAAAA")
          wb <- wb_add_font(wb, sheet = sn,
                            dims = sprintf("J%d:K%d", r, r), color = "#AAAAAA")
        }
      }

      # Number formats — counts (C,D,E,F,G,H) and assets (I,J,K)
      data_range <- sprintf("C3:K%d", 2 + n_data)
      wb <- wb_add_numfmt(wb, sheet = sn, dims = data_range,
                          numfmt = "#,##0")

      # Freeze pane
      wb <- wb_freeze_pane(wb, sheet = sn,
                           first_active_row = 3, first_active_col = 2)

      # Column widths
      wb <- wb_set_col_widths(wb, sheet = sn,
                               cols = 1:12, widths = col_widths)

      message(sprintf("    Sheet '%s': %d hist + %d forecast rows",
                      sn,
                      sum(all_rows$Row_Type == "Historical"),
                      sum(all_rows$Row_Type == "Forecast")))
    }

    wb_save(wb, xl_path)
    xl_done <- TRUE
    message(sprintf("\n    [openxlsx2] Excel saved: %s", xl_path))
    message(sprintf("    Sheets: README + %d category tabs", length(cats_xl)))

  }, error = function(e) {
    message(sprintf("    [openxlsx2] FAILED: %s", e$message))
    message("    Falling back to writexl...")
  })
}

# ════════════════════════════════════════════════════════════
# PATH B: writexl  (plain, no formatting — always available)
# Produces one sheet per category plus a metadata sheet.
# writexl has zero system dependencies (pure C, no Java/LibreOffice).
# ════════════════════════════════════════════════════════════
if (!xl_done && have_writexl) {
  tryCatch({
    library(writexl)

    sheet_list <- list()

    # README sheet
    sheet_list[["README"]] <- data.frame(
      Field = c("Report","Generated","Model","Dep vars","Training",
                "Significance","FICU","FISCU","Total Assets",
                "Amber rows note","Blue CI cols note"),
      Value = c(
        "Credit Union Growth Forecast",
        format(Sys.time(), "%Y-%m-%d %H:%M"),
        "Rolling-window LASSO -> Post-LASSO OLS + quarterly dummies",
        "yoy_ficu_pct | yoy_fiscu_pct | ln_assets_tot",
        sprintf("2005 Q1+; OOS from %s", as.character(TRAIN_END)),
        sprintf("p < %.2f + sign check", SIG_LEVEL),
        "Federally Insured Credit Union",
        "Federally Insured State-Chartered Credit Union",
        "Sum of total assets ($000s)",
        "Forecast rows (no colour formatting in writexl fallback)",
        "95% CI lower/upper bounds — NAs for historical rows"
      ), stringsAsFactors = FALSE
    )

    for (cat in cats_xl) {
      sn <- cat_sheet_names[cat]
      if (is.na(sn) || nchar(sn) == 0) sn <- cat
      dt <- build_cat_table(cat)
      # Add human-readable column headers as first row
      hdr <- data.table(
        Quarter     = "Quarter",
        Row_Type    = "Type",
        FICU_Count  = "FICU Count",
        FICU_Lo95   = "FICU Lo 95%",
        FICU_Hi95   = "FICU Hi 95%",
        FISCU_Count = "FISCU Count",
        FISCU_Lo95  = "FISCU Lo 95%",
        FISCU_Hi95  = "FISCU Hi 95%",
        Assets_000s = "Total Assets ($000s)",
        Assets_Lo95 = "Assets Lo 95%",
        Assets_Hi95 = "Assets Hi 95%",
        Method      = "Method"
      )
      combined <- rbindlist(list(hdr, dt), fill = TRUE)
      sheet_list[[sn]] <- as.data.frame(combined)
      message(sprintf("    Sheet '%s': %d hist + %d forecast rows",
                      sn,
                      sum(dt$Row_Type == "Historical"),
                      sum(dt$Row_Type == "Forecast")))
    }

    write_xlsx(sheet_list, xl_path)
    xl_done <- TRUE
    message(sprintf("\n    [writexl] Excel saved: %s", xl_path))
    message("    Note: writexl fallback — no cell colouring; data is complete.")

  }, error = function(e)
    message(sprintf("    [writexl] FAILED: %s", e$message)))
}

if (!xl_done)
  message("    [WARN] All Excel paths failed — check package availability.")

# ════════════════════════════════════════════════════════════
# 8. FINAL SUMMARY
# ════════════════════════════════════════════════════════════
tot   <- as.numeric((proc.time() - t0)["elapsed"])
toc()  # Part 3 total

n_plots <- length(list.files(PLOT_DIR, pattern = "\\.pdf$"))

message("\n=======================================================")
message(sprintf("PART 3 [%s] COMPLETE  %dh %02dm %02ds",
                if (DEBUG_MODE) "DEBUG" else "PROD",
                floor(tot / 3600),
                floor((tot %% 3600) / 60),
                round(tot %% 60)))
message(sprintf("Models fitted : 21 (%d dep vars × 7 categories)",
                length(DEP_VARS)))
message(sprintf("Test quarters : %d  (%s to %s)",
                length(test_quarters),
                as.character(min(test_quarters)),
                as.character(max(test_quarters))))
message(sprintf("Forecast rows : %d", nrow(forecasts_all)))
message(sprintf("Plots saved   : %d PDF files → %s/", n_plots, PLOT_DIR))
message(sprintf("CSVs saved    : forecasts, metrics, coefficients → %s/",
                RESULT_DIR))
message(sprintf("Level CSVs    : forecasts_levels.csv, metrics_levels.csv → %s/",
                RESULT_DIR))
message(sprintf("Level plots   : 11_levels_*, 12_scatter_*, 13_level_error_*,"))
message(sprintf("                14_level_r2_scorecard, 15_system_totals → %s/", PLOT_DIR))
message(sprintf("Policy charts : P16_policy_ficu_count, P17_policy_fiscu_count,"))
message(sprintf("                P18_policy_assets → %s/", PLOT_DIR))
n_sg <- length(list.files(RESULT_DIR, pattern = "stargazer_.*[.](html|txt)$"))
message(sprintf("Stargazer     : %d files (HTML+TXT per dep var + combined) → %s/",
                n_sg, RESULT_DIR))
sg_pdf_exists <- file.exists(file.path(RESULT_DIR, "stargazer_regression_tables.pdf"))
message(sprintf("Reg tables PDF: stargazer_regression_tables.pdf  (%s) → %s/",
                if(sg_pdf_exists) "saved" else "MISSING — check log", RESULT_DIR))
xl_exists <- file.exists(file.path(RESULT_DIR, "CU_Forecast_by_Category.xlsx"))
message(sprintf("Excel         : CU_Forecast_by_Category.xlsx  (%s) → %s/",
                if(xl_exists) "saved" else "MISSING", RESULT_DIR))

if (nrow(metrics_all) > 0) {
  message("\n── OOS R² Summary (transformed space) ──")
  for (dv in names(DEP_VARS)) {
    sub <- metrics_all[dep_var == dv]
    if (nrow(sub) == 0) next
    message(sprintf("\n  %s:", DEP_VARS[[dv]]$label))
    print(sub[order(cat_label),
               .(cat_label,
                 rmse    = round(rmse,    3),
                 mae     = round(mae,     3),
                 r2_oos  = round(r2_oos,  3),
                 n)])
  }
}

if (exists("level_metrics") && nrow(level_metrics) > 0) {
  message("\n── OOS Metrics Summary (LEVEL space — original units) ──")
  for (sr in c("ficu_count","fiscu_count","total_assets")) {
    sub <- level_metrics[series == sr]
    if (nrow(sub) == 0) next
    message(sprintf("\n  %s:", unique(sub$y_label)))
    print(sub[order(cat_label),
               .(cat_label,
                 rmse   = round(rmse,   1),
                 mape   = round(mape,   2),
                 r2_oos = round(r2_oos, 3),
                 n)])
  }
}

if (DEBUG_MODE)
  message("\n*** Set DEBUG_MODE <- FALSE for full production run ***")
message("=======================================================")

n_level_rows <- if (exists("level_fc")) nrow(level_fc) else 0L
notify("Part 3 DONE",
       sprintf("[%s] %dh%02dm | 21 models | %d forecasts | %d plots | Excel: %s | PDF: %s",
               if (DEBUG_MODE) "DEBUG" else "PROD",
               floor(tot / 3600), floor((tot %% 3600) / 60),
               nrow(forecasts_all), n_plots,
               if(file.exists(file.path(RESULT_DIR,"CU_Forecast_by_Category.xlsx")))
                 "saved" else "missing",
               if(file.exists(file.path(RESULT_DIR,"stargazer_regression_tables.pdf")))
                 "saved" else "missing"),
       tags = if (nrow(metrics_all) > 0) "tada" else "warning")

############################################################
# END
############################################################
