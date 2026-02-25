############################################################
# PART 2 — XGBOOST  ficu_count forecast
#
# ENHANCEMENTS:
#   1. Overall (pooled, no asset split) model vs disaggregated
#   2. Rolling-window 1-step-ahead forecast (fixed window slides
#      forward one quarter at a time)
#   3. All plots saved as PDF to ./plots/ folder (vector quality)
#   4. Merger / liquidity / acquisition variable analysis:
#      - Gain importance across all categories
#      - Whether each var appears in top 10 per category
#
# Strategy  : model YoY% change, back-transform to counts
# Window    : ROLLING fixed-size (WINDOW_Q quarters wide)
# Runtime   : ~10 min debug  /  ~60-90 min production
############################################################

DEBUG_MODE <- TRUE   # ← set FALSE after debug passes

library(data.table); library(zoo);  library(xgboost)
library(ggplot2);    library(httr); library(scales)
set.seed(42)

# ── ntfy ──────────────────────────────────────────────────
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

# ── Config ────────────────────────────────────────────────
if (DEBUG_MODE) {
  WINDOW_Q         <- 20   # rolling window size (quarters)
  NR_MAX           <- 100
  ESTOP            <- 10
  N_ITER           <- 5
  DEBUG_FORECAST_Q <- 8    # predict only last 8 quarters in debug
} else {
  WINDOW_Q         <- 32   # rolling window size (quarters = ~8 years)
  NR_MAX           <- 1500
  ESTOP            <- 40
  N_ITER           <- 40
  DEBUG_FORECAST_Q <- NULL # forecast all available quarters
}

TRAIN_CUTOFF <- zoo::as.yearqtr("2020 Q4")  # evaluation period starts here
CORR_CUT     <- 0.97

# Known M&A / liquidity / acquisition variable name patterns
# (update this list if your Part 1 used different naming)
MAQ_PATTERN <- paste(
  "merger", "mergers", "acquisition", "acqui", "liquid",
  "liquidity", "liq_", "_liq", "consol", "charter",
  sep = "|"
)

# ── Plot output folder ────────────────────────────────────
PLOT_DIR <- "plots"
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR)
message(sprintf("Plots will be saved to: %s/  (PDF format)", PLOT_DIR))

# Helper: save plot as PDF (vector quality) and print to screen
save_plot <- function(p, filename, width = 10, height = 6) {
  path <- file.path(PLOT_DIR, paste0(filename, ".pdf"))
  tryCatch(
    ggplot2::ggsave(path, plot = p, width = width, height = height,
                    device = cairo_pdf),
    error = function(e) {
      # Fallback to standard pdf device if cairo_pdf unavailable
      tryCatch(
        ggplot2::ggsave(path, plot = p, width = width, height = height,
                        device = "pdf"),
        error = function(e2) message("  [save_plot] ", e2$message)
      )
    }
  )
  print(p)
  invisible(path)
}

t0_script <- proc.time()
message("=======================================================")
message(sprintf("PART 2  [%s]  %s",
                if (DEBUG_MODE) "DEBUG" else "PROD",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("=======================================================")
notify("CU Forecast Started",
       sprintf("[%s] %s", if(DEBUG_MODE)"DEBUG" else "PROD",
               format(Sys.time(),"%H:%M")), tags = "rocket")

# ════════════════════════════════════════════════════════
# 1. LOAD & PREP DATA
# ════════════════════════════════════════════════════════
message("\n[1] Loading data...")
qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)
message(sprintf("    %d rows x %d cols", nrow(qtrly), ncol(qtrly)))

stopifnot(
  "ficu_count missing" = "ficu_count"  %in% names(qtrly),
  "date missing"       = "date"        %in% names(qtrly),
  "categories missing" = "categories"  %in% names(qtrly)
)

# Fix categories
CAT_MAP <- c("1"="1_Less_10M",  "2"="2_10M_50M",   "3"="3_50M_100M",
             "4"="4_100M_500M", "5"="5_500M_1B",   "6"="6_1B_10B",
             "7"="7_10B_Plus")
qtrly[, cat_label := CAT_MAP[as.character(categories)]]
stopifnot(sum(is.na(qtrly$cat_label)) == 0)
message(sprintf("    Categories: %s",
                paste(sort(unique(qtrly$cat_label)), collapse=" | ")))

# Build YoY% target (within category)
setorderv(qtrly, c("cat_label","date"))
qtrly[, lag4_count := shift(ficu_count, 4L, type="lag"), by = cat_label]
qtrly[, yoy_pct := fifelse(
  !is.na(lag4_count) & lag4_count > 0,
  (ficu_count - lag4_count) / lag4_count * 100, NA_real_
)]

# Winsorise
p01 <- quantile(qtrly$yoy_pct, 0.01, na.rm=TRUE)
p99 <- quantile(qtrly$yoy_pct, 0.99, na.rm=TRUE)
qtrly[, yoy_pct := pmax(pmin(yoy_pct, p99), p01)]

r <- range(qtrly$yoy_pct, na.rm=TRUE)
message(sprintf("    yoy_pct range: [%.2f%%, %.2f%%]", r[1], r[2]))

# Feature candidates (inclusion-pattern — engineered/stationary only)
FEAT_PAT <- paste(
  "^yoy_","^qoq_","_lag[0-9]","_rmean[0-9]","_rsd[0-9]",
  "^regime_","^time_idx$","^qtrs_from_",
  "^fedfunds_cycle$","^yield_curve_inv$",
  "^fedfunds$","^gs10$","^gs2$","^mortgage30$",
  "^hy_spread$","^unrate$","^yield_curve$","^housing_starts$",
  sep="|"
)
all_num <- names(qtrly)[vapply(qtrly, is.numeric, logical(1))]
FEATS   <- grep(FEAT_PAT, all_num, value=TRUE, perl=TRUE)
FEATS   <- setdiff(FEATS, c("yoy_pct","ficu_count","lag4_count",
                             "yoy_ficu_count","categories"))
message(sprintf("    %d candidate features", length(FEATS)))

# Identify M&A / liquidity variables present in data
MAQ_VARS <- grep(MAQ_PATTERN, FEATS, value=TRUE, ignore.case=TRUE)
message(sprintf("    M&A/liquidity vars in features: %d  (%s)",
                length(MAQ_VARS),
                if(length(MAQ_VARS)>0)
                  paste(head(MAQ_VARS,6), collapse=", ")
                else "none found — check MAQ_PATTERN"))

# Filter date range
qtrly <- qtrly[date >= zoo::as.yearqtr("2002 Q1")]
setorderv(qtrly, c("cat_label","date"))
all_quarters <- sort(unique(qtrly$date))
cats         <- sort(unique(qtrly$cat_label))

message(sprintf("    Quarters: %s to %s  (%d total)",
                as.character(min(all_quarters)),
                as.character(max(all_quarters)),
                length(all_quarters)))

# ════════════════════════════════════════════════════════
# 2. CORE MODELLING FUNCTIONS
# ════════════════════════════════════════════════════════

# ── Feature prep ─────────────────────────────────────────
prep_features <- function(dt, feats, corr_cut = CORR_CUT) {
  x <- intersect(feats, names(dt))
  x <- x[vapply(x, function(cn) is.numeric(dt[[cn]]), logical(1))]
  if (length(x) == 0) return(NULL)

  mat <- as.matrix(dt[, x, with=FALSE])
  colnames(mat) <- x

  # Impute
  for (j in seq_len(ncol(mat))) {
    nas <- is.na(mat[,j])
    if (any(nas)) {
      m <- median(mat[!nas,j], na.rm=TRUE)
      mat[nas,j] <- if(is.finite(m)) m else 0
    }
  }

  # NZV
  ok <- vapply(seq_len(ncol(mat)), function(j) {
    u <- length(unique(mat[,j]))
    u >= 3 && (u/nrow(mat)) > 0.03
  }, logical(1))
  mat <- mat[, ok, drop=FALSE]
  if (ncol(mat) == 0) return(NULL)

  # High corr
  if (ncol(mat) >= 2) {
    cm <- suppressWarnings(cor(mat, use="pairwise.complete.obs"))
    cm[is.na(cm)] <- 0; diag(cm) <- 0
    keep <- rep(TRUE, ncol(cm))
    for (i in seq_len(ncol(cm)-1)) {
      if (!keep[i]) next
      for (j in (i+1):ncol(cm))
        if (keep[j] && abs(cm[i,j]) > corr_cut) keep[j] <- FALSE
    }
    mat <- mat[, keep, drop=FALSE]
  }
  if (ncol(mat) == 0) return(NULL)
  mat
}

# ── Fit XGBoost with simple CV ────────────────────────────
fit_xgb <- function(X, y, n_iter = N_ITER,
                     nr_max = NR_MAX, estop = ESTOP,
                     n_cv_folds = 3) {
  # Time-series CV: last 20% as validation
  n <- nrow(X)
  val_idx <- seq(floor(n * 0.8) + 1L, n)
  tr_idx  <- seq_len(floor(n * 0.8))

  if (length(tr_idx) < 10 || length(val_idx) < 2) {
    # Not enough for CV — use simple hold-out 80/20
    val_idx <- seq(floor(n * 0.8) + 1L, n)
    tr_idx  <- seq_len(max(floor(n * 0.8), 1L))
  }

  folds <- list(val_idx)   # single fold time-series CV

  set.seed(42)
  grid <- data.frame(
    eta=sample(c(0.01,0.05,0.1),n_iter,replace=TRUE),
    max_depth=sample(2:4,n_iter,replace=TRUE),
    min_child_weight=sample(c(3,5,10),n_iter,replace=TRUE),
    subsample=sample(c(0.7,0.8,0.9),n_iter,replace=TRUE),
    colsample_bytree=sample(c(0.6,0.8,1.0),n_iter,replace=TRUE),
    gamma=sample(c(0,0.1,0.5),n_iter,replace=TRUE),
    lambda=sample(c(1,5,10),n_iter,replace=TRUE),
    alpha=sample(c(0,0.5,1),n_iter,replace=TRUE),
    stringsAsFactors=FALSE
  )

  dmat <- xgb.DMatrix(data=X, label=y)
  best_rmse <- Inf; best_nr <- 30L; best_p <- NULL

  for (i in seq_len(nrow(grid))) {
    p <- list(booster="gbtree", objective="reg:squarederror",
              eval_metric="rmse", nthread=1L,
              eta=grid$eta[i], max_depth=grid$max_depth[i],
              min_child_weight=grid$min_child_weight[i],
              subsample=grid$subsample[i],
              colsample_bytree=grid$colsample_bytree[i],
              gamma=grid$gamma[i], lambda=grid$lambda[i],
              alpha=grid$alpha[i])
    sc <- tryCatch({
      cv <- xgb.cv(params=p, data=dmat, nrounds=nr_max,
                   folds=folds, early_stopping_rounds=estop,
                   verbose=0, showsd=FALSE)
      list(rmse=min(cv$evaluation_log$test_rmse_mean, na.rm=TRUE),
           nr=max(cv$best_iteration, 5L))
    }, error=function(e) list(rmse=Inf, nr=30L))
    if (is.finite(sc$rmse) && sc$rmse < best_rmse) {
      best_rmse <- sc$rmse; best_nr <- sc$nr; best_p <- p
    }
  }

  if (is.null(best_p)) {
    best_p <- list(booster="gbtree", objective="reg:squarederror",
                   eval_metric="rmse", nthread=1L,
                   eta=0.05, max_depth=3, min_child_weight=5,
                   subsample=0.8, colsample_bytree=0.8,
                   gamma=0, lambda=5, alpha=0)
    best_nr <- 30L; best_rmse <- NA_real_
  }

  mdl <- xgb.train(params=best_p, data=dmat,
                    nrounds=best_nr, verbose=0)
  list(model=mdl, best_rmse=best_rmse, best_nr=best_nr,
       x_cols=colnames(X))
}

# ── Rolling-window one-step-ahead forecast ───────────────
# For each target quarter q:
#   - Train on a fixed window of WINDOW_Q quarters ending at q-1
#   - Predict q  (window slides forward one quarter each step)
#   - Re-fit at every step with fresh window
rolling_forecast <- function(dt, feats, window_q = WINDOW_Q,
                              debug_q   = DEBUG_FORECAST_Q,
                              label     = "") {

  dt     <- dt[!is.na(yoy_pct)]
  qtrs   <- sort(unique(dt$date))
  n_qtrs <- length(qtrs)

  # Need at least window_q + 1 quarters total
  if (n_qtrs <= window_q) {
    message(sprintf("    [%s] SKIP: only %d quarters, need >%d",
                    label, n_qtrs, window_q))
    return(NULL)
  }

  # In debug mode, only forecast the last debug_q quarters
  if (!is.null(debug_q)) {
    first_pred_idx <- max(window_q + 1L, n_qtrs - debug_q + 1L)
  } else {
    first_pred_idx <- window_q + 1L
  }

  pred_rows  <- list()
  imp_rows   <- list()
  last_model <- NULL

  for (q_idx in seq(first_pred_idx, n_qtrs)) {
    target_qtr <- qtrs[q_idx]

    # ROLLING: fixed window of window_q quarters immediately before target
    window_start <- q_idx - window_q
    window_end   <- q_idx - 1L
    train_qtrs   <- qtrs[window_start:window_end]

    train_dt  <- dt[date %in% train_qtrs]
    test_row  <- dt[date == target_qtr]

    if (nrow(test_row) == 0 || is.na(test_row$yoy_pct)) next
    if (nrow(train_dt) < 10) next

    # Prep features on the training window
    mat_tr <- prep_features(train_dt, feats)
    if (is.null(mat_tr) || ncol(mat_tr) == 0) next

    cols_use <- intersect(colnames(mat_tr), names(test_row))
    if (length(cols_use) == 0) next

    # Restrict training matrix to cols also present in test
    mat_tr2 <- mat_tr[, cols_use, drop = FALSE]

    # Build test matrix, impute NAs with window medians
    mat_te <- as.matrix(test_row[, cols_use, with = FALSE])
    for (j in seq_along(cols_use)) {
      if (is.na(mat_te[1, j])) {
        m <- median(mat_tr2[, j], na.rm = TRUE)
        mat_te[1, j] <- if (is.finite(m)) m else 0
      }
    }

    y_tr <- train_dt$yoy_pct[seq_len(nrow(mat_tr2))]

    # Fit model on this window
    fit <- tryCatch(fit_xgb(mat_tr2, y_tr),
                    error = function(e) NULL)
    if (is.null(fit)) next

    # Predict YoY%
    pred_pct <- predict(fit$model,
                         xgb.DMatrix(data = mat_te))

    # Back-transform to count
    lag4     <- test_row$lag4_count
    pred_cnt <- if (!is.na(lag4) && lag4 > 0)
      round(lag4 * (1 + pred_pct / 100)) else NA_real_
    naive_pct <- tail(train_dt$yoy_pct, 1L)
    naive_cnt <- if (!is.na(lag4) && lag4 > 0)
      round(lag4 * (1 + naive_pct / 100)) else NA_real_

    pred_rows[[length(pred_rows) + 1L]] <- data.table(
      date        = target_qtr,
      actual_pct  = test_row$yoy_pct,
      pred_pct    = pred_pct,
      naive_pct   = naive_pct,
      actual_cnt  = test_row$ficu_count,
      pred_cnt    = pred_cnt,
      naive_cnt   = naive_cnt,
      n_train     = nrow(train_dt),
      n_feats     = length(cols_use),
      window_start = as.character(train_qtrs[1]),
      window_end   = as.character(tail(train_qtrs, 1))
    )

    # Capture importance every 4 quarters and at last step
    if ((q_idx %% 4 == 0) || q_idx == n_qtrs) {
      imp <- tryCatch(
        xgb.importance(feature_names = cols_use, model = fit$model),
        error = function(e) NULL
      )
      if (!is.null(imp))
        imp_rows[[length(imp_rows) + 1L]] <-
          data.table(date = target_qtr, imp)
    }
    last_model <- fit
  }

  if (length(pred_rows) == 0) return(NULL)

  preds <- rbindlist(pred_rows)
  imps  <- if (length(imp_rows) > 0) rbindlist(imp_rows) else data.table()

  list(preds = preds, imps = imps, last_model = last_model)
}

# ════════════════════════════════════════════════════════
# 3. RUN DISAGGREGATED (per-category) MODELS
# ════════════════════════════════════════════════════════
message(sprintf("\n[3] Disaggregated models (%d categories)...", length(cats)))
notify("Disaggregated Models Started",
       sprintf("[%s] %d categories", if(DEBUG_MODE)"DEBUG" else "PROD",
               length(cats)), tags="hourglass_flowing_sand")

disag_results <- list()

for (cc in cats) {
  t_cat <- proc.time()
  message(sprintf("\n    ── %s ──", cc))

  dt_cat <- qtrly[cat_label == cc]
  res    <- rolling_forecast(dt_cat, FEATS, label=cc)

  if (is.null(res)) {
    disag_results[[cc]] <- list(cat=cc, err="no predictions generated")
    message(sprintf("    %s: FAILED", cc))
    next
  }

  cat_secs <- as.numeric((proc.time()-t_cat)["elapsed"])

  # Per-category metrics
  p <- res$preds
  rmse_fn <- function(a,b) sqrt(mean((a-b)^2, na.rm=TRUE))
  mae_fn  <- function(a,b) mean(abs(a-b), na.rm=TRUE)

  xgb_cnt_rmse   <- rmse_fn(p$actual_cnt, p$pred_cnt)
  naive_cnt_rmse  <- rmse_fn(p$actual_cnt, p$naive_cnt)
  skill_cnt       <- if(is.finite(naive_cnt_rmse) && naive_cnt_rmse>0)
    (1-xgb_cnt_rmse/naive_cnt_rmse)*100 else NA_real_
  dir_acc         <- if(nrow(p)>1)
    mean(sign(diff(p$actual_cnt))==sign(diff(p$pred_cnt)), na.rm=TRUE)*100
  else NA_real_
  xgb_pct_rmse    <- rmse_fn(p$actual_pct, p$pred_pct)
  naive_pct_rmse  <- rmse_fn(p$actual_pct, p$naive_pct)
  skill_pct       <- if(is.finite(naive_pct_rmse) && naive_pct_rmse>0)
    (1-xgb_pct_rmse/naive_pct_rmse)*100 else NA_real_

  metrics <- data.table(
    model           = "Disaggregated",
    categories      = cc,
    n_predictions   = nrow(p),
    test_count_rmse = xgb_cnt_rmse,
    naive_count_rmse= naive_cnt_rmse,
    skill_count     = skill_cnt,
    test_count_mae  = mae_fn(p$actual_cnt, p$pred_cnt),
    dir_acc         = dir_acc,
    test_pct_rmse   = xgb_pct_rmse,
    naive_pct_rmse  = naive_pct_rmse,
    skill_pct       = skill_pct,
    total_seconds   = round(cat_secs, 1)
  )

  disag_results[[cc]] <- list(
    cat=cc, err=NULL, preds=p,
    imps=res$imps, last_model=res$last_model,
    metrics=metrics
  )

  message(sprintf("    count RMSE=%.1f  skill=%.1f%%  dir=%.1f%%  time=%.0fs",
                  xgb_cnt_rmse, skill_cnt %||% NA, dir_acc %||% NA, cat_secs))
}

# null-coalescing helper (used above)
`%||%` <- function(a,b) if(!is.null(a) && length(a)>0 && !is.na(a)) a else b

notify("Disaggregated Done",
       sprintf("%d/%d OK",
               sum(sapply(disag_results, function(r) is.null(r$err))),
               length(cats)), tags="white_check_mark")

# ════════════════════════════════════════════════════════
# 4. RUN OVERALL (POOLED) MODEL
# ════════════════════════════════════════════════════════
message("\n[4] Overall (pooled, no asset split) model...")
notify("Overall Model Started", "Pooled rolling-window forecast",
       tags="hourglass_flowing_sand")

# Add cat_idx as a feature so the pooled model knows about asset size
qtrly[, cat_idx := as.integer(factor(cat_label))]
pooled_feats <- c(FEATS, "cat_idx")
pooled_feats <- intersect(pooled_feats, names(qtrly))

# For the pooled model we aggregate ficu_count across all categories
# per quarter, then model total system-wide YoY%
system_dt <- qtrly[, .(
  ficu_count = sum(ficu_count, na.rm=TRUE),
  date       = date[1]
), by = date]
setorderv(system_dt, "date")
system_dt[, lag4_count := shift(ficu_count, 4L, type="lag")]
system_dt[, yoy_pct := fifelse(
  !is.na(lag4_count) & lag4_count>0,
  (ficu_count-lag4_count)/lag4_count*100, NA_real_
)]
system_dt[, cat_label := "OVERALL"]

# Merge macro/regime features from one representative category slice
# (they are the same across all categories — take first)
macro_cols <- intersect(FEATS, names(qtrly))
macro_cols <- macro_cols[!startsWith(macro_cols, "yoy_") &
                          !startsWith(macro_cols, "qoq_")]
# Only keep macro cols not already present in system_dt
macro_cols <- setdiff(macro_cols, names(system_dt))
if (length(macro_cols) > 0) {
  cat1_macro <- unique(qtrly[cat_label == cats[1],
                              c("date", macro_cols), with=FALSE])
  system_dt  <- merge(system_dt, cat1_macro, by="date", all.x=TRUE)
}

# Add system-level yoy features derived from aggregated data
system_dt[, lag4_count_sys := shift(ficu_count, 4L)]
# Use same FEATS that are available in merged data
sys_feats_avail <- intersect(FEATS, names(system_dt))
sys_feats_avail <- setdiff(sys_feats_avail,
                           c("yoy_pct","ficu_count","lag4_count",
                             "yoy_ficu_count","categories","cat_idx"))

t_pool <- proc.time()
pooled_res <- rolling_forecast(system_dt, sys_feats_avail,
                                label="OVERALL",
                                debug_q = DEBUG_FORECAST_Q)
pool_secs <- as.numeric((proc.time()-t_pool)["elapsed"])

if (!is.null(pooled_res)) {
  pp <- pooled_res$preds
  rmse_fn <- function(a,b) sqrt(mean((a-b)^2, na.rm=TRUE))
  overall_metrics <- data.table(
    model           = "Overall",
    categories      = "OVERALL",
    n_predictions   = nrow(pp),
    test_count_rmse = rmse_fn(pp$actual_cnt, pp$pred_cnt),
    naive_count_rmse= rmse_fn(pp$actual_cnt, pp$naive_cnt),
    skill_count     = {
      nr <- rmse_fn(pp$actual_cnt, pp$naive_cnt)
      xr <- rmse_fn(pp$actual_cnt, pp$pred_cnt)
      if(is.finite(nr) && nr>0) (1-xr/nr)*100 else NA_real_
    },
    test_count_mae  = mean(abs(pp$actual_cnt-pp$pred_cnt), na.rm=TRUE),
    dir_acc         = if(nrow(pp)>1)
      mean(sign(diff(pp$actual_cnt))==sign(diff(pp$pred_cnt)),na.rm=TRUE)*100
    else NA_real_,
    test_pct_rmse   = rmse_fn(pp$actual_pct, pp$pred_pct),
    naive_pct_rmse  = rmse_fn(pp$actual_pct, pp$naive_pct),
    skill_pct       = {
      nr <- rmse_fn(pp$actual_pct, pp$naive_pct)
      xr <- rmse_fn(pp$actual_pct, pp$pred_pct)
      if(is.finite(nr) && nr>0) (1-xr/nr)*100 else NA_real_
    },
    total_seconds   = round(pool_secs, 1)
  )
  message(sprintf("    Overall count RMSE=%.1f  skill=%.1f%%",
                  overall_metrics$test_count_rmse,
                  overall_metrics$skill_count))
} else {
  overall_metrics <- NULL
  message("    Overall model: no predictions generated")
}
notify("Overall Model Done",
       if(!is.null(overall_metrics))
         sprintf("Count RMSE=%.1f", overall_metrics$test_count_rmse)
       else "Failed", tags="white_check_mark")

# ════════════════════════════════════════════════════════
# 5. DISAGGREGATED AGGREGATE vs OVERALL COMPARISON
# Compare: sum of per-category forecasts vs overall model
# ════════════════════════════════════════════════════════
message("\n[5] Aggregating disaggregated forecasts for comparison...")

# Collect all disaggregated predictions
ok_cats  <- cats[sapply(disag_results, function(r) is.null(r$err))]
disag_pred_all <- rbindlist(lapply(ok_cats, function(cc)
  disag_results[[cc]]$preds[, .(date, actual_cnt, pred_cnt, naive_cnt,
                                 actual_pct, pred_pct, naive_pct)]
), fill=TRUE, idcol="categories")
if ("categories" %in% names(disag_pred_all))
  disag_pred_all[, categories := ok_cats[categories]]

# Sum per quarter across categories
disag_agg <- disag_pred_all[, .(
  actual_cnt_agg = sum(actual_cnt, na.rm=TRUE),
  pred_cnt_agg   = sum(pred_cnt,   na.rm=TRUE),
  naive_cnt_agg  = sum(naive_cnt,  na.rm=TRUE)
), by = date]
setorderv(disag_agg, "date")

# Merge with overall model predictions on common dates
if (!is.null(pooled_res)) {
  compare_dt <- merge(
    disag_agg,
    pooled_res$preds[, .(date,
                          overall_pred  = pred_cnt,
                          overall_naive = naive_cnt,
                          overall_actual= actual_cnt)],
    by="date"
  )

  rmse_fn <- function(a,b) sqrt(mean((a-b)^2, na.rm=TRUE))
  comp_metrics <- data.table(
    approach    = c("Disaggregated (sum)", "Overall model", "Naive RW"),
    count_rmse  = c(
      rmse_fn(compare_dt$actual_cnt_agg, compare_dt$pred_cnt_agg),
      rmse_fn(compare_dt$overall_actual, compare_dt$overall_pred),
      rmse_fn(compare_dt$actual_cnt_agg, compare_dt$naive_cnt_agg)
    ),
    count_mae   = c(
      mean(abs(compare_dt$actual_cnt_agg - compare_dt$pred_cnt_agg), na.rm=TRUE),
      mean(abs(compare_dt$overall_actual - compare_dt$overall_pred),  na.rm=TRUE),
      mean(abs(compare_dt$actual_cnt_agg - compare_dt$naive_cnt_agg), na.rm=TRUE)
    )
  )
  comp_metrics[, skill_vs_naive := (1 - count_rmse/count_rmse[3])*100]

  message("\n    ── Comparison: Disaggregated vs Overall ──")
  print(comp_metrics)
} else {
  compare_dt   <- NULL
  comp_metrics <- NULL
}

# ════════════════════════════════════════════════════════
# 6. COLLECT ALL METRICS & SAVE
# ════════════════════════════════════════════════════════
message("\n[6] Saving outputs...")

disag_metrics <- rbindlist(lapply(ok_cats, function(cc)
  disag_results[[cc]]$metrics), fill=TRUE)

all_metrics <- rbindlist(
  list(disag_metrics,
       if(!is.null(overall_metrics)) overall_metrics else NULL),
  fill=TRUE
)

all_imps <- rbindlist(lapply(ok_cats, function(cc) {
  im <- disag_results[[cc]]$imps
  if(!is.null(im) && nrow(im)>0) data.table(categories=cc, im)
  else NULL
}), fill=TRUE)

sfx <- if(DEBUG_MODE) "_debug" else "_enriched"
fwrite(all_metrics,  paste0("xgb_metrics",    sfx, ".csv"))
fwrite(all_imps,     paste0("xgb_importance", sfx, ".csv"))
if (!is.null(comp_metrics))
  fwrite(comp_metrics, paste0("xgb_comparison", sfx, ".csv"))
saveRDS(disag_results, paste0("xgb_disag",     sfx, ".rds"))
if (!is.null(pooled_res))
  saveRDS(pooled_res,  paste0("xgb_overall",   sfx, ".rds"))

message("    Metrics:")
print(all_metrics[, .(model, categories, n_predictions,
                       test_count_rmse, skill_count, dir_acc,
                       test_pct_rmse, skill_pct)])

# ════════════════════════════════════════════════════════
# 7. PLOTS
# ════════════════════════════════════════════════════════
message(sprintf("\n[7] Generating plots → %s/", PLOT_DIR))

# ── P1: Overall vs Disaggregated comparison ──────────────
if (!is.null(compare_dt) && nrow(compare_dt) > 0) {
  compare_dt[, dd := as.Date(date)]
  cm_long <- melt(
    compare_dt[, .(dd, actual_cnt_agg, pred_cnt_agg, overall_pred)],
    id.vars="dd", variable.name="series", value.name="count"
  )
  cm_long[, series := fcase(
    series=="actual_cnt_agg", "Actual (system total)",
    series=="pred_cnt_agg",   "Disaggregated forecast",
    series=="overall_pred",   "Overall model forecast",
    default="Other"
  )]
  p <- ggplot(cm_long, aes(x=dd, y=count, colour=series, linetype=series)) +
    geom_line(linewidth=0.9) +
    scale_colour_manual(
      values=c("Actual (system total)"="black",
               "Disaggregated forecast"="#2171b5",
               "Overall model forecast"="#d62728")) +
    scale_linetype_manual(
      values=c("Actual (system total)"="solid",
               "Disaggregated forecast"="dashed",
               "Overall model forecast"="dotted")) +
    scale_y_continuous(labels=comma) +
    labs(title="System FICU Count: Overall vs Disaggregated Forecast",
         subtitle=if(!is.null(comp_metrics))
           sprintf("Disaggregated RMSE=%.0f  |  Overall RMSE=%.0f  |  Naive RMSE=%.0f",
                   comp_metrics$count_rmse[1],
                   comp_metrics$count_rmse[2],
                   comp_metrics$count_rmse[3]) else "",
         x=NULL, y="Total Credit Unions", colour=NULL, linetype=NULL) +
    theme_bw(base_size=12) + theme(legend.position="bottom")
  save_plot(p, "00_overall_vs_disaggregated_comparison")

  # Comparison bar chart
  if (!is.null(comp_metrics)) {
    p2 <- ggplot(comp_metrics,
                 aes(x=reorder(approach,-count_rmse), y=count_rmse,
                     fill=approach)) +
      geom_col(width=0.6) +
      geom_text(aes(label=sprintf("RMSE=%.0f\nSkill=%.1f%%",
                                  count_rmse, skill_vs_naive)),
                vjust=-0.3, size=3.5) +
      scale_fill_manual(
        values=c("Disaggregated (sum)"="#2171b5",
                 "Overall model"="#d62728",
                 "Naive RW"="#74c476")) +
      scale_y_continuous(expand=expansion(mult=c(0,0.2))) +
      labs(title="Forecast Accuracy Comparison",
           subtitle="Lower RMSE = better",
           x=NULL, y="Count RMSE (# CUs)", fill=NULL) +
      theme_bw(base_size=12) + theme(legend.position="none")
    save_plot(p2, "00_comparison_bar", width=7, height=5)
  }
}

# ── P2: Per-category actual vs predicted (count space) ────
for (cc in ok_cats) {
  p_dt  <- disag_results[[cc]]$preds
  if (is.null(p_dt) || nrow(p_dt)==0) next

  # Full history: training (in-sample) + predictions
  full_dt <- qtrly[cat_label==cc, .(date, ficu_count)]
  p_dt[, dd := as.Date(date)]
  full_dt[, dd := as.Date(date)]
  m <- disag_results[[cc]]$metrics

  p <- ggplot() +
    geom_line(data=full_dt, aes(x=dd, y=ficu_count, colour="Actual"),
              linewidth=0.9) +
    geom_line(data=p_dt,
              aes(x=dd, y=pred_cnt, colour="XGBoost (1-step)"),
              linewidth=0.75, linetype="dashed") +
    geom_line(data=p_dt,
              aes(x=dd, y=naive_cnt, colour="Naive RW"),
              linewidth=0.6, linetype="dotted") +
    geom_vline(xintercept=as.Date(zoo::as.yearqtr("2020 Q4")),
               linetype="dotted", colour="grey40") +
    annotate("text", x=as.Date(zoo::as.yearqtr("2020 Q4"))+10,
             y=max(full_dt$ficu_count, na.rm=TRUE)*0.98,
             label="2020Q4", angle=90, hjust=1, size=3, colour="grey40") +
    scale_colour_manual(
      values=c("Actual"="#1f77b4",
               "XGBoost (1-step)"="#d62728",
               "Naive RW"="#2ca02c")) +
    scale_y_continuous(labels=comma) +
    labs(title=sprintf("FICU Count Forecast | %s", cc),
         subtitle=sprintf(
           "Expanding window 1-step-ahead  |  Count RMSE=%.1f  |  Skill=%.1f%%  |  Dir=%.1f%%",
           m$test_count_rmse,
           if(!is.na(m$skill_count)) m$skill_count else 0,
           if(!is.na(m$dir_acc)) m$dir_acc else 0),
         x=NULL, y="Number of Credit Unions", colour=NULL) +
    theme_bw(base_size=11) + theme(legend.position="bottom")
  save_plot(p, sprintf("01_count_forecast_%s", cc))
}

# Overall model forecast plot
if (!is.null(pooled_res)) {
  pp <- pooled_res$preds
  pp[, dd := as.Date(date)]
  sys_hist <- qtrly[, .(ficu_count=sum(ficu_count,na.rm=TRUE)), by=date]
  sys_hist[, dd := as.Date(date)]

  p <- ggplot() +
    geom_line(data=sys_hist, aes(x=dd,y=ficu_count,colour="Actual"),
              linewidth=0.9) +
    geom_line(data=pp, aes(x=dd,y=pred_cnt,colour="Overall XGBoost"),
              linewidth=0.75, linetype="dashed") +
    geom_line(data=pp, aes(x=dd,y=naive_cnt,colour="Naive RW"),
              linewidth=0.6, linetype="dotted") +
    scale_colour_manual(
      values=c("Actual"="#1f77b4","Overall XGBoost"="#d62728",
               "Naive RW"="#2ca02c")) +
    scale_y_continuous(labels=comma) +
    labs(title="System-Wide FICU Count | Overall Model",
         subtitle=if(!is.null(overall_metrics))
           sprintf("Count RMSE=%.1f  |  Skill=%.1f%%",
                   overall_metrics$test_count_rmse,
                   overall_metrics$skill_count) else "",
         x=NULL, y="Total Credit Unions", colour=NULL) +
    theme_bw(base_size=11) + theme(legend.position="bottom")
  save_plot(p, "01_count_forecast_OVERALL")
}

# ── P3: YoY% plots (model diagnostic space) ──────────────
for (cc in ok_cats) {
  p_dt <- disag_results[[cc]]$preds
  if (is.null(p_dt) || nrow(p_dt)==0) next
  p_dt[, dd := as.Date(date)]

  p <- ggplot(p_dt, aes(x=dd)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
    geom_line(aes(y=actual_pct, colour="Actual"),   linewidth=0.9) +
    geom_line(aes(y=pred_pct,   colour="XGBoost"),  linewidth=0.75,
              linetype="dashed") +
    geom_line(aes(y=naive_pct,  colour="Naive RW"), linewidth=0.6,
              linetype="dotted") +
    scale_colour_manual(
      values=c("Actual"="#1f77b4","XGBoost"="#d62728","Naive RW"="#2ca02c")) +
    labs(title=sprintf("YoY%% Change (model space) | %s", cc),
         x=NULL, y="YoY % Change in FICU Count", colour=NULL) +
    theme_bw(base_size=11) + theme(legend.position="bottom")
  save_plot(p, sprintf("02_yoy_pct_%s", cc))
}

# ── P4: Residuals ─────────────────────────────────────────
for (cc in ok_cats) {
  p_dt <- disag_results[[cc]]$preds
  if (is.null(p_dt) || nrow(p_dt)==0) next
  p_dt[, dd := as.Date(date)]
  p_dt[, resid := actual_cnt - pred_cnt]
  sd_r <- sd(p_dt$resid, na.rm=TRUE)

  p <- ggplot(p_dt, aes(x=dd, y=resid)) +
    geom_hline(yintercept=0, colour="grey50") +
    geom_hline(yintercept= sd_r, linetype="dashed", colour="#fdae61") +
    geom_hline(yintercept=-sd_r, linetype="dashed", colour="#fdae61") +
    geom_col(aes(fill=resid>0), alpha=0.8) +
    scale_fill_manual(values=c("TRUE"="#2171b5","FALSE"="#d62728"),
                      guide="none") +
    labs(title=sprintf("Count Residuals | %s", cc),
         subtitle="Blue=under-predicted  Red=over-predicted  Dashed=+/-1SD",
         x=NULL, y="Actual - Predicted (CUs)") +
    theme_bw(base_size=11)
  save_plot(p, sprintf("03_residuals_%s", cc))
}

# ── P5: Scorecard (all categories) ───────────────────────
if (nrow(disag_metrics) > 0) {
  p_sc <- ggplot(disag_metrics[!is.na(skill_count)],
                 aes(x=reorder(categories, skill_count),
                     y=skill_count, fill=skill_count>0)) +
    geom_col() +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_text(aes(label=sprintf("%.1f%%",skill_count)),
              hjust=ifelse(disag_metrics[!is.na(skill_count)]$skill_count>0,
                           -0.1,1.1), size=3.5) +
    coord_flip() +
    scale_fill_manual(values=c("TRUE"="#2171b5","FALSE"="#d62728"),
                      labels=c("TRUE"="Beats naive","FALSE"="Worse"),
                      name=NULL) +
    labs(title="Count Skill Score vs Naive RW (per category)",
         x=NULL, y="% RMSE improvement over naive") +
    theme_bw(base_size=11) + theme(legend.position="bottom")
  save_plot(p_sc, "04_scorecard_skill", width=9, height=5)

  p_dir <- ggplot(disag_metrics[!is.na(dir_acc)],
                  aes(x=reorder(categories, dir_acc),
                      y=dir_acc, fill=dir_acc>=50)) +
    geom_col() +
    geom_hline(yintercept=50, linetype="dashed", colour="grey40") +
    geom_text(aes(label=sprintf("%.1f%%",dir_acc)),
              hjust=-0.15, size=3.5) +
    coord_flip() +
    scale_fill_manual(values=c("TRUE"="#2171b5","FALSE"="#d62728"),
                      guide="none") +
    scale_y_continuous(limits=c(0,120)) +
    labs(title="Directional Accuracy of Count Forecast",
         x=NULL, y="% Quarters with correct direction") +
    theme_bw(base_size=11)
  save_plot(p_dir, "04_directional_accuracy", width=9, height=5)
}

# ── P6: Feature importance (top 10 per category) ─────────
for (cc in ok_cats) {
  im <- disag_results[[cc]]$imps
  if (is.null(im) || nrow(im)==0) next

  # Average importance across time snapshots
  im_avg <- im[, .(Gain=mean(Gain, na.rm=TRUE)), by=Feature]
  im_avg <- im_avg[order(-Gain)][1:min(10L, nrow(im_avg))]
  im_avg[, Feature := factor(Feature, levels=Feature[order(Gain)])]
  im_avg[, pct := Gain/sum(Gain)*100]
  im_avg[, ftype := fcase(
    grepl("_lag[0-9]|_rmean|_rsd", Feature),      "Lag/Rolling",
    grepl("regime|time_idx|qtrs_from|cycle|inv",
          Feature),                                "Trend/Cycle",
    grepl(MAQ_PATTERN, Feature, ignore.case=TRUE), "M&A / Liquidity",
    grepl("fedfunds|gs10|gs2|unrate|gdp|cpi|
           mortgage|hy_spread|payroll|deposit|
           loan|umich|yield|housing", Feature),    "Macro",
    default = "Other CU Metrics"
  )]

  p <- ggplot(im_avg, aes(x=Feature, y=Gain, fill=ftype)) +
    geom_col() +
    geom_text(aes(label=sprintf("%.1f%%",pct)), hjust=-0.1, size=3.2) +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0,0.22))) +
    scale_fill_manual(
      values=c("Lag/Rolling"="#2171b5","Trend/Cycle"="#238b45",
               "M&A / Liquidity"="#9467bd",
               "Macro"="#d94801","Other CU Metrics"="#756bb1")) +
    labs(title=sprintf("Top 10 Predictors (avg Gain) | %s", cc),
         x=NULL, y="Avg Gain", fill=NULL) +
    theme_bw(base_size=11) + theme(legend.position="bottom")
  save_plot(p, sprintf("05_importance_%s", cc))
}

# ── P7: M&A / Liquidity variable analysis ────────────────
message("\n    M&A / Liquidity variable analysis...")

if (length(MAQ_VARS) > 0 && nrow(all_imps) > 0) {

  # ── 7a: Gain importance across all categories ────────────
  maq_imp <- all_imps[Feature %in% MAQ_VARS]

  if (nrow(maq_imp) > 0) {
    maq_avg <- maq_imp[, .(mean_gain = mean(Gain, na.rm=TRUE),
                            n_windows = .N),
                        by = .(categories, Feature)]

    # Heatmap: avg Gain per MAQ var x category
    p <- ggplot(maq_avg, aes(x=Feature, y=categories, fill=mean_gain)) +
      geom_tile(colour="white", linewidth=0.5) +
      geom_text(aes(label=sprintf("%.4f", mean_gain)), size=2.8) +
      scale_fill_gradient(low="white", high="#9467bd", name="Avg Gain") +
      labs(title="M&A / Liquidity Variable Importance by Category",
           subtitle=sprintf(
             "Average XGBoost Gain across rolling-window models  (window=%dQ)",
             WINDOW_Q),
           x=NULL, y=NULL) +
      theme_bw(base_size=11) +
      theme(axis.text.x=element_text(angle=45, hjust=1, size=8))
    save_plot(p, "06_maq_importance_heatmap", width=11, height=6)

    # Overall ranked bar: avg Gain collapsed across all categories
    maq_total <- maq_imp[, .(mean_gain = mean(Gain, na.rm=TRUE),
                              n_appear  = .N), by=Feature]
    maq_total <- maq_total[order(-mean_gain)]
    maq_total[, Feature := factor(Feature, levels=Feature[order(mean_gain)])]

    p2 <- ggplot(maq_total, aes(x=Feature, y=mean_gain)) +
      geom_col(fill="#9467bd", alpha=0.85) +
      geom_text(aes(label=sprintf("n=%d", n_appear)),
                hjust=-0.15, size=3.2) +
      coord_flip() +
      scale_y_continuous(expand=expansion(mult=c(0,0.3))) +
      labs(title="M&A / Liquidity Variables — Overall Average Gain",
           subtitle="Collapsed across all categories & rolling windows  (n = # windows selected)",
           x=NULL, y="Average XGBoost Gain") +
      theme_bw(base_size=11)
    save_plot(p2, "06_maq_importance_overall", width=9, height=5)

    message(sprintf("    MAQ vars with non-zero Gain: %d",
                    length(unique(maq_imp$Feature))))
    message("    Top MAQ vars by avg Gain:")
    print(head(maq_total[, .(Feature, mean_gain, n_appear)], 10))

  } else {
    message("    MAQ vars found in FEATS but never appeared in importance")
    message("    (XGBoost did not select them — low predictive value)")
  }

  # ── 7b: Top-10 presence per category ────────────────────
  # For each category, check which MAQ vars appeared in the
  # top-10 features in ANY rolling-window snapshot
  message("\n    Top-10 presence of M&A/Liquidity vars per category:")
  message(sprintf("    %-20s  %-6s  %s", "Category", "In10?", "Variables in top 10"))
  message(strrep("-", 70))

  top10_presence <- list()

  for (cc in ok_cats) {
    im_cc <- disag_results[[cc]]$imps
    if (is.null(im_cc) || nrow(im_cc) == 0) {
      message(sprintf("    %-20s  %-6s  %s", cc, "N/A", "(no importance data)"))
      next
    }

    # Top 10 features per snapshot (date), then take union across snapshots
    top10_per_snap <- im_cc[, .SD[order(-Gain)][1:min(10L,.N)], by=date]
    maq_in_top10   <- intersect(unique(top10_per_snap$Feature), MAQ_VARS)

    # How many snapshots did each MAQ var make the top 10?
    if (length(maq_in_top10) > 0) {
      snap_counts <- top10_per_snap[Feature %in% maq_in_top10,
                                    .(n_snaps=.N, pct_snaps=.N/length(unique(im_cc$date))*100),
                                    by=Feature]
      snap_counts <- snap_counts[order(-n_snaps)]
      detail <- paste(sprintf("%s(%d/%d snaps)",
                              snap_counts$Feature,
                              snap_counts$n_snaps,
                              length(unique(im_cc$date))),
                      collapse="; ")
      message(sprintf("    %-20s  YES    %s", cc, detail))
    } else {
      message(sprintf("    %-20s  NO     (never in top 10)", cc))
    }

    top10_presence[[cc]] <- data.table(
      categories    = cc,
      maq_in_top10  = length(maq_in_top10) > 0,
      maq_vars_top10 = if(length(maq_in_top10)>0)
        paste(maq_in_top10, collapse="; ") else NA_character_,
      n_maq_top10   = length(maq_in_top10)
    )
  }

  # Save top-10 presence table
  if (length(top10_presence) > 0) {
    top10_pres_dt <- rbindlist(top10_presence, fill=TRUE)
    fwrite(top10_pres_dt, paste0("xgb_maq_top10_presence", sfx, ".csv"))

    # Plot: which categories have MAQ vars in top 10
    p3 <- ggplot(top10_pres_dt,
                 aes(x=reorder(categories, n_maq_top10),
                     y=n_maq_top10, fill=maq_in_top10)) +
      geom_col() +
      geom_text(aes(label=ifelse(n_maq_top10>0,
                                 as.character(n_maq_top10), "")),
                hjust=-0.2, size=3.5) +
      coord_flip() +
      scale_y_continuous(expand=expansion(mult=c(0,0.3))) +
      scale_fill_manual(
        values=c("TRUE"="#9467bd","FALSE"="#cccccc"),
        labels=c("TRUE"="Yes","FALSE"="No"),
        name="MAQ in top 10?") +
      labs(title="M&A / Liquidity Variables in Top-10 Features",
           subtitle="Count of distinct M&A/liquidity vars that appeared in top 10 (any rolling window)",
           x=NULL, y="# Distinct M&A/Liquidity vars in top 10") +
      theme_bw(base_size=11) + theme(legend.position="bottom")
    save_plot(p3, "06_maq_top10_presence", width=9, height=5)
  }

} else {
  message("    NOTE: No M&A/liquidity variable patterns matched in FEATS.")
  message("    Update MAQ_PATTERN at top of script with your actual variable names.")
  message(sprintf("    Searching for 'liq|merger|acqui|consol|charter' in features: %s",
                  paste(grep("liq|merger|acqui|consol|charter",
                             FEATS, value=TRUE, ignore.case=TRUE),
                        collapse=", ")))

  # Fallback: print top-20 features overall so user can identify naming
  if (nrow(all_imps) > 0) {
    imp_summary <- all_imps[, .(mean_gain=mean(Gain, na.rm=TRUE)), by=Feature]
    message("    Top 20 features (use these names to update MAQ_PATTERN):")
    print(head(imp_summary[order(-mean_gain)], 20))
  }
}

# ── P8: Feature type breakdown pie/bar ───────────────────
if (nrow(all_imps) > 0) {
  all_imps[, ftype := fcase(
    grepl("_lag[0-9]|_rmean|_rsd", Feature),      "Lag/Rolling",
    grepl("regime|time_idx|qtrs_from|cycle|inv",
          Feature),                                "Trend/Cycle",
    grepl(MAQ_PATTERN, Feature, ignore.case=TRUE), "M&A / Liquidity",
    grepl("fedfunds|gs10|gs2|unrate|gdp|cpi|
           mortgage|hy_spread|payroll|deposit|
           loan|umich|yield|housing", Feature),    "Macro",
    default = "Other CU Metrics"
  )]

  type_share <- all_imps[, .(total_gain=sum(Gain, na.rm=TRUE)), by=.(categories, ftype)]
  type_share[, share := total_gain/sum(total_gain)*100, by=categories]

  p <- ggplot(type_share,
              aes(x=categories, y=share, fill=ftype)) +
    geom_col(position="stack") +
    coord_flip() +
    scale_fill_manual(
      values=c("Lag/Rolling"="#2171b5","Trend/Cycle"="#238b45",
               "M&A / Liquidity"="#9467bd",
               "Macro"="#d94801","Other CU Metrics"="#aec7e8")) +
    scale_x_discrete(limits=rev) +
    labs(title="Feature Type Share of Importance (Gain)",
         x=NULL, y="% of total Gain", fill="Feature type") +
    theme_bw(base_size=11) + theme(legend.position="bottom")
  save_plot(p, "07_feature_type_breakdown", width=10, height=6)
}

# ── P9: Timing ────────────────────────────────────────────
if (nrow(disag_metrics) > 0) {
  disag_metrics[, lbl := ifelse(
    total_seconds>=60,
    sprintf("%dm%02ds", floor(total_seconds/60), round(total_seconds%%60)),
    sprintf("%.0fs", total_seconds))]
  p <- ggplot(disag_metrics,
              aes(x=reorder(categories, total_seconds),
                  y=total_seconds/60)) +
    geom_col(fill="#4292c6") +
    geom_text(aes(label=lbl), hjust=-0.1, size=3.2) +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0,0.25))) +
    labs(title="Runtime per Category", x=NULL, y="Minutes") +
    theme_bw(base_size=11)
  save_plot(p, "08_runtime", width=8, height=5)
}

# ════════════════════════════════════════════════════════
# 8. FINAL SUMMARY
# ════════════════════════════════════════════════════════
tot   <- as.numeric((proc.time()-t0_script)["elapsed"])
n_ok  <- length(ok_cats)
n_err <- length(cats) - n_ok

message("\n=======================================================")
message(sprintf("DONE [%s]  %dh %02dm  |  Disagg: %d/%d OK",
                if(DEBUG_MODE)"DEBUG" else "PROD",
                floor(tot/3600), floor((tot%%3600)/60),
                n_ok, length(cats)))

if (!is.null(comp_metrics)) {
  message("\n── FINAL COMPARISON ──")
  print(comp_metrics[, .(approach, count_rmse, count_mae, skill_vs_naive)])
  winner <- comp_metrics[which.min(count_rmse), approach]
  message(sprintf("    Best approach: %s", winner))
}

message(sprintf("\nPlots saved to: %s/ (%d PDF files)",
                PLOT_DIR,
                length(list.files(PLOT_DIR, pattern="\\.pdf$"))))

if (n_err > 0) {
  message("FAILED categories:")
  for (cc in setdiff(cats, ok_cats))
    message(sprintf("  %s: %s", cc, disag_results[[cc]]$err))
}
if (DEBUG_MODE)
  message("\n*** Set DEBUG_MODE <- FALSE for full production run ***")
message("=======================================================")

# Final notification
best_line <- ""
if (!is.null(comp_metrics)) {
  winner <- comp_metrics[which.min(count_rmse), approach]
  best_rmse <- comp_metrics[which.min(count_rmse), count_rmse]
  best_line <- sprintf("\nWinner: %s (RMSE=%.0f)", winner, best_rmse)
}
notify(sprintf("FICU Count %s Done", if(DEBUG_MODE)"[DEBUG]" else ""),
       sprintf("%d/%d cats OK | %dh%02dm%s\nPlots: %d PDF saved",
               n_ok, length(cats),
               floor(tot/3600), floor((tot%%3600)/60),
               best_line,
               length(list.files(PLOT_DIR, pattern="\\.pdf$"))),
       tags=if(n_err==0)"tada" else "warning")

############################################################
# END
############################################################
