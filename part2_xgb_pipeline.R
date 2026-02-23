############################################################
# PART 2 — ENHANCED XGBOOST PIPELINE
# Input:  qtrly_enriched.rds  (produced by Part 1)
# Target: yoy_ficu_count
# Groups: categories (7 CU asset-size buckets)
# Period: 2002Q1–2025Q3 | Train ≤ 2020Q4 | Test > 2020Q4
#
# Improvements over baseline:
#  • AR lags, rolling stats, trend/cycle features (from Part 1)
#  • Naive random-walk benchmark for context
#  • Directional accuracy metric
#  • Diebold-Mariano test vs naive
#  • Skill score (RMSE relative to naive)
#  • Pooled model (all categories) as additional comparison
############################################################

# ── 0) Packages ───────────────────────────────────────────
suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(xgboost)
  library(foreach)
  library(doParallel)
  library(ggplot2)
})
set.seed(42)

# ── 1) Config ─────────────────────────────────────────────
TARGET       <- "yoy_ficu_count"
DATE_COL     <- "date"
GROUP_COL    <- "categories"
TRAIN_END    <- zoo::as.yearqtr("2020 Q4")
CORR_CUT     <- 0.97       # relaxed — regularization handles multicollinearity

INITIAL_QTRS <- 40         # 10 years for first CV window
ASSESS_QTRS  <- 4          # forecast 4 qtrs per fold
SKIP_QTRS    <- 2          # advance 2 qtrs per fold
MAX_CV_FOLDS <- 12

NROUNDS_MAX  <- 2000
EARLY_STOP   <- 40
N_SEARCH_ITER <- 60        # random grid search iterations

N_CORES <- max(1L, parallel::detectCores() - 1L)

# ── Date utility ─────────────────────────────────────────
# qtrly.rds dates are quarter-start Dates: 2001-01-01, 2001-04-01, etc.
date_to_yearqtr <- function(x) {
  d   <- as.Date(x)
  yr  <- as.integer(format(d, "%Y"))
  mo  <- as.integer(format(d, "%m"))
  qtr <- (mo - 1L) %/% 3L + 1L
  zoo::as.yearqtr(paste0(yr, " Q", qtr))
}

# ── 2) Load enriched data ──────────────────────────────────
message("Loading qtrly_enriched.rds...")
qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)

if (!inherits(qtrly[[DATE_COL]], "yearqtr"))
  qtrly[, (DATE_COL) := date_to_yearqtr(get(DATE_COL))]

# Restrict to model range (2002Q1 onward — need pre-2002 for lags)
qtrly <- qtrly[date >= zoo::as.yearqtr("2002 Q1")]
setorderv(qtrly, c(GROUP_COL, DATE_COL))

cat_list <- split(qtrly, by = GROUP_COL, keep.by = TRUE)
cats     <- sort(names(cat_list))
message(sprintf("Categories: %s", paste(cats, collapse = ", ")))

# ── 3) Feature prep helpers ────────────────────────────────

drop_nzv <- function(mat, freq_cut = 95/5, unique_cut = 10) {
  bad <- vapply(seq_len(ncol(mat)), function(j) {
    x <- mat[!is.na(mat[, j]), j]
    if (length(x) < 5) return(TRUE)
    pct_u <- length(unique(x)) / length(x) * 100
    if (pct_u <= unique_cut) return(TRUE)
    tbl <- sort(tabulate(match(x, unique(x))), decreasing = TRUE)
    length(tbl) < 2 || (tbl[1] / max(tbl[2], 1)) >= freq_cut
  }, logical(1))
  colnames(mat)[!bad]
}

drop_highcorr <- function(mat, cutoff = CORR_CUT) {
  if (ncol(mat) < 2) return(colnames(mat))
  cm <- suppressWarnings(cor(mat, use = "pairwise.complete.obs"))
  cm[is.na(cm)] <- 0; diag(cm) <- 0
  keep <- rep(TRUE, ncol(cm))
  for (i in seq_len(ncol(cm) - 1)) {
    if (!keep[i]) next
    for (j in (i + 1):ncol(cm))
      if (keep[j] && abs(cm[i, j]) > cutoff) keep[j] <- FALSE
  }
  colnames(cm)[keep]
}

prep_xy <- function(dt_cat) {
  dt_cat <- dt_cat[!is.na(get(TARGET))]
  num_cols <- names(dt_cat)[vapply(dt_cat, is.numeric, logical(1))]
  x_cols   <- setdiff(num_cols, c(TARGET, DATE_COL, GROUP_COL,
                                   "quarter_num"))  # kept as numeric, not dropped

  # Exclude raw level versions when we already have YoY — avoids trivial leakage
  # (e.g. keep gdp_yoy, not raw real_gdp levels that encode trend)
  raw_macro_levels <- c("real_gdp","cpi","pce_deflator","nonfarm_pay",
                         "deposits","bank_loans","indpro","houst")
  x_cols <- setdiff(x_cols, raw_macro_levels)

  if (length(x_cols) == 0) return(list(dt = dt_cat, x_cols = character(0)))

  # Impute NAs with column median (XGBoost can handle NAs but median is safer for lags at series start)
  mat_full <- as.matrix(dt_cat[, x_cols, with = FALSE])
  for (j in seq_len(ncol(mat_full))) {
    na_idx <- is.na(mat_full[, j])
    if (any(na_idx)) {
      med <- median(mat_full[!na_idx, j], na.rm = TRUE)
      mat_full[na_idx, j] <- ifelse(is.finite(med), med, 0)
    }
  }
  dt_imp <- copy(dt_cat)
  dt_imp[, (x_cols) := as.data.table(mat_full)]

  x_cols <- drop_nzv(mat_full)
  if (length(x_cols) >= 2)
    x_cols <- drop_highcorr(mat_full[, x_cols, drop = FALSE])

  list(dt = dt_imp, x_cols = x_cols)
}

# ── 4) Rolling-origin CV folds ─────────────────────────────
make_folds <- function(n, initial = INITIAL_QTRS, assess = ASSESS_QTRS,
                       skip = SKIP_QTRS, max_folds = MAX_CV_FOLDS) {
  folds <- list(); t <- initial
  while ((t + assess) <= n && length(folds) < max_folds) {
    folds[[length(folds) + 1L]] <- (t + 1L):(t + assess)
    t <- t + skip
  }
  folds
}

# ── 5) Random param sampler ────────────────────────────────
sample_params <- function(n = N_SEARCH_ITER) {
  data.frame(
    eta              = sample(c(0.01, 0.02, 0.05, 0.08, 0.1), n, replace = TRUE),
    max_depth        = sample(2:5,  n, replace = TRUE),
    min_child_weight = sample(c(3, 5, 10, 15, 20), n, replace = TRUE),
    subsample        = sample(c(0.6, 0.7, 0.8, 0.9), n, replace = TRUE),
    colsample_bytree = sample(c(0.5, 0.6, 0.8, 1.0), n, replace = TRUE),
    gamma            = sample(c(0, 0.1, 0.5, 1.0), n, replace = TRUE),
    lambda           = sample(c(1, 3, 5, 10), n, replace = TRUE),
    alpha            = sample(c(0, 0.5, 1.0), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# ── 6) Score one param set via TS-CV ──────────────────────
score_params <- function(dmat, fold_list, p, nrounds_max, early_stop) {
  params <- list(
    booster = "gbtree", objective = "reg:squarederror",
    eval_metric = "rmse", nthread = 1L,
    eta = p$eta, max_depth = p$max_depth,
    min_child_weight = p$min_child_weight,
    subsample = p$subsample, colsample_bytree = p$colsample_bytree,
    gamma = p$gamma, lambda = p$lambda, alpha = p$alpha
  )
  cv <- xgb.cv(params = params, data = dmat, nrounds = nrounds_max,
                folds = fold_list, metrics = "rmse",
                early_stopping_rounds = early_stop,
                verbose = 0, showsd = FALSE)
  list(rmse = min(cv$evaluation_log$test_rmse_mean),
       nrounds = cv$best_iteration)
}

# ── 7) Naive benchmark (random walk on target) ─────────────
# Naive forecast: predict next quarter = last known value
naive_forecast <- function(y_train, n_test) {
  # Step-1 ahead: repeat last training value
  # For multi-step, propagate
  last_val <- tail(y_train, 1L)
  rep(last_val, n_test)
}

# ── 8) Diebold-Mariano test (one-sided: XGB better than naive?) ──
dm_test <- function(e1, e2) {
  # e1 = XGB errors, e2 = naive errors (both squared loss differentials)
  d <- e2^2 - e1^2     # positive d => XGB better
  n <- length(d)
  if (n < 4) return(list(stat = NA, p_value = NA))
  dbar  <- mean(d)
  # Newey-West variance with lag = h-1 (h=4 quarters)
  h <- 4L
  gamma0 <- var(d)
  gammas <- sapply(1:(h - 1), function(k) mean((d[(k+1):n] - dbar) * (d[1:(n-k)] - dbar)))
  nw_var <- gamma0 + 2 * sum((1 - seq_len(h - 1) / h) * gammas)
  nw_var <- max(nw_var, 1e-12)
  stat   <- dbar / sqrt(nw_var / n)
  p_val  <- pt(stat, df = n - 1, lower.tail = FALSE)
  list(stat = round(stat, 3), p_value = round(p_val, 4))
}

# ── 9) Full pipeline for one category ─────────────────────
run_category <- function(dt_cat, cat_value) {

  prep   <- prep_xy(dt_cat)
  dt     <- prep$dt
  x_cols <- prep$x_cols

  if (length(x_cols) == 0)
    return(list(category = cat_value, error = "No usable predictors"))

  tr_idx <- which(dt[[DATE_COL]] <= TRAIN_END)
  te_idx <- which(dt[[DATE_COL]] >  TRAIN_END)
  dt_tr  <- dt[tr_idx]
  dt_te  <- dt[te_idx]

  n_tr <- nrow(dt_tr)
  if (n_tr < (INITIAL_QTRS + ASSESS_QTRS + 2L))
    return(list(category = cat_value,
                error = paste("Insufficient train rows:", n_tr)))

  fold_list <- make_folds(n_tr)

  X_tr   <- as.matrix(dt_tr[, x_cols, with = FALSE])
  y_tr   <- dt_tr[[TARGET]]
  dmat   <- xgb.DMatrix(data = X_tr, label = y_tr)

  # --- Grid search ---
  grid <- sample_params(N_SEARCH_ITER)
  best_rmse <- Inf; best_params <- NULL; best_nrounds <- NROUNDS_MAX

  for (i in seq_len(nrow(grid))) {
    sc <- tryCatch(
      score_params(dmat, fold_list, grid[i, ], NROUNDS_MAX, EARLY_STOP),
      error = function(e) list(rmse = Inf, nrounds = NROUNDS_MAX)
    )
    if (sc$rmse < best_rmse) {
      best_rmse    <- sc$rmse
      best_nrounds <- sc$nrounds
      best_params  <- c(
        list(booster = "gbtree", objective = "reg:squarederror",
             eval_metric = "rmse", nthread = 1L),
        as.list(grid[i, ])
      )
    }
  }

  # --- Final model ---
  model   <- xgb.train(params = best_params, data = dmat,
                        nrounds = best_nrounds, verbose = 0)
  pred_tr <- predict(model, X_tr)
  pred_te <- if (nrow(dt_te) > 0)
    predict(model, as.matrix(dt_te[, x_cols, with = FALSE]))
  else numeric(0)

  # --- Naive benchmark ---
  naive_te <- naive_forecast(y_tr, nrow(dt_te))

  # --- Residuals ---
  y_te      <- if (nrow(dt_te) > 0) dt_te[[TARGET]] else numeric(0)
  resid_tr  <- y_tr - pred_tr
  resid_te  <- y_te - pred_te

  # --- Metrics ---
  rmse_fn  <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
  mae_fn   <- function(a, p) mean(abs(a - p), na.rm = TRUE)
  mape_fn  <- function(a, p) mean(abs((a - p) / (abs(a) + 1e-8)) * 100, na.rm = TRUE)
  dir_acc  <- function(a, p) {
    da <- diff(a); dp <- diff(p)
    mean(sign(da) == sign(dp), na.rm = TRUE) * 100
  }

  naive_rmse_te   <- if (length(y_te) > 0) rmse_fn(y_te, naive_te) else NA_real_
  xgb_rmse_te     <- if (length(y_te) > 0) rmse_fn(y_te, pred_te) else NA_real_
  skill_score     <- if (!is.na(naive_rmse_te) && naive_rmse_te > 0)
    (1 - xgb_rmse_te / naive_rmse_te) * 100 else NA_real_

  dm <- if (length(y_te) >= 4) dm_test(resid_te, y_te - naive_te) else list(stat = NA, p_value = NA)

  metrics <- data.frame(
    categories    = cat_value,
    best_cv_rmse  = best_rmse,
    best_nrounds  = best_nrounds,
    n_features    = length(x_cols),
    n_cv_folds    = length(fold_list),
    train_rmse    = rmse_fn(y_tr, pred_tr),
    train_mae     = mae_fn(y_tr, pred_tr),
    test_rmse     = xgb_rmse_te,
    test_mae      = if (length(y_te) > 0) mae_fn(y_te, pred_te) else NA_real_,
    test_mape     = if (length(y_te) > 0) mape_fn(y_te, pred_te) else NA_real_,
    test_dir_acc  = if (length(y_te) > 1) dir_acc(y_te, pred_te) else NA_real_,
    naive_rmse    = naive_rmse_te,
    skill_score   = skill_score,
    dm_stat       = dm$stat,
    dm_pvalue     = dm$p_value
  )

  # --- Plot data ---
  plot_df <- rbind(
    data.frame(categories = cat_value, date = dt_tr[[DATE_COL]], set = "Train",
               actual = y_tr, predicted = pred_tr,
               naive = NA_real_, residual = resid_tr),
    data.frame(categories = cat_value, date = dt_te[[DATE_COL]], set = "Test",
               actual = y_te, predicted = pred_te,
               naive = naive_te, residual = resid_te)
  )

  imp   <- xgb.importance(feature_names = x_cols, model = model)
  top10 <- head(imp, 10L)

  list(
    category     = cat_value,
    error        = NULL,
    x_cols       = x_cols,
    best_cv_rmse = best_rmse,
    best_nrounds = best_nrounds,
    best_params  = best_params,
    model        = model,
    importance   = imp,
    top10        = top10,
    plot_df      = plot_df,
    metrics      = metrics
  )
}

# ── 10) Parallel run across categories ─────────────────────
message(sprintf("Launching %d categories across %d cores...", length(cats), N_CORES))

# All objects a worker needs must be explicitly exported.
# Workers are blank R sessions — they cannot see the main session's globals.
worker_exports <- c(
  # data
  "cat_list",
  # functions
  "run_category", "prep_xy", "drop_nzv", "drop_highcorr",
  "make_folds", "sample_params", "score_params",
  "naive_forecast", "dm_test",
  # config constants
  "TARGET", "DATE_COL", "GROUP_COL", "TRAIN_END", "CORR_CUT",
  "INITIAL_QTRS", "ASSESS_QTRS", "SKIP_QTRS", "MAX_CV_FOLDS",
  "NROUNDS_MAX", "EARLY_STOP", "N_SEARCH_ITER"
)

cl <- parallel::makeCluster(N_CORES)
doParallel::registerDoParallel(cl)

results <- foreach::foreach(
  cc        = cats,
  .export   = worker_exports,
  .packages = c("data.table", "zoo", "xgboost")
) %dopar% {
  # Per-worker tryCatch: one bad category won't crash the whole job
  tryCatch(
    run_category(cat_list[[cc]], cc),
    error = function(e)
      list(category = cc, error = conditionMessage(e),
           top10 = NULL, plot_df = NULL, metrics = NULL)
  )
}

parallel::stopCluster(cl)
names(results) <- cats

# ── 11) Pooled model (all categories, category as feature) ─
message("Fitting pooled model (all categories)...")
# Useful to compare: does pooling help low-data categories?

pooled_res <- tryCatch({
  qtrly_pool  <- rbindlist(cat_list)
  qtrly_pool[, cat_idx := as.integer(factor(get(GROUP_COL)))]  # numeric encoding

  prep_pool   <- prep_xy(qtrly_pool)
  dt_pool     <- prep_pool$dt
  x_pool      <- prep_pool$x_cols
  # Ensure cat_idx is included
  if (!"cat_idx" %in% x_pool) x_pool <- c(x_pool, "cat_idx")

  tr_pool     <- dt_pool[get(DATE_COL) <= TRAIN_END]
  te_pool     <- dt_pool[get(DATE_COL) >  TRAIN_END]

  dmat_pool   <- xgb.DMatrix(
    data  = as.matrix(tr_pool[, x_pool, with = FALSE]),
    label = tr_pool[[TARGET]]
  )
  fold_pool   <- make_folds(nrow(tr_pool), initial = INITIAL_QTRS * length(cats))

  # Single param set for pooled (use median best params from individual models)
  pool_params <- list(
    booster = "gbtree", objective = "reg:squarederror",
    eval_metric = "rmse", nthread = N_CORES,
    eta = 0.05, max_depth = 4, min_child_weight = 10,
    subsample = 0.8, colsample_bytree = 0.7,
    gamma = 0.1, lambda = 5, alpha = 0.5
  )

  pool_cv <- xgb.cv(
    params = pool_params, data = dmat_pool,
    nrounds = NROUNDS_MAX,
    folds = if (length(fold_pool) > 0) fold_pool else make_folds(nrow(tr_pool)),
    metrics = "rmse", early_stopping_rounds = EARLY_STOP,
    verbose = 0, showsd = FALSE
  )

  pool_model <- xgb.train(
    params = pool_params, data = dmat_pool,
    nrounds = pool_cv$best_iteration, verbose = 0
  )

  pred_pool_te <- predict(pool_model, as.matrix(te_pool[, x_pool, with = FALSE]))

  list(
    model     = pool_model,
    x_cols    = x_pool,
    pred_te   = data.table(
      categories = te_pool[[GROUP_COL]],
      date       = te_pool[[DATE_COL]],
      actual     = te_pool[[TARGET]],
      predicted  = pred_pool_te
    ),
    test_rmse = sqrt(mean((te_pool[[TARGET]] - pred_pool_te)^2, na.rm = TRUE))
  )
}, error = function(e) {
  message("Pooled model failed: ", e$message); NULL
})

# ── 12) Collect outputs ─────────────────────────────────────
metrics_dt <- rbindlist(
  lapply(results, function(r)
    if (!is.null(r$error)) data.table(categories = r$category, error = r$error)
    else cbind(as.data.table(r$metrics), error = NA_character_)
  ), fill = TRUE
)

top10_dt <- rbindlist(
  lapply(results, function(r)
    if (!is.null(r$error) || is.null(r$top10)) NULL
    else data.table(categories = r$category, r$top10)
  ), fill = TRUE
)

plot_all <- rbindlist(
  lapply(results, function(r)
    if (!is.null(r$error) || is.null(r$plot_df)) NULL
    else as.data.table(r$plot_df)
  ), fill = TRUE
)

saveRDS(results,   "xgb_results_enriched.rds")
fwrite(metrics_dt, "xgb_metrics_enriched.csv")
fwrite(top10_dt,   "xgb_top10_enriched.csv")

message("\n── Per-Category Metrics ─────────────────────────────")
print(metrics_dt[, .(categories, test_rmse, naive_rmse,
                      skill_score, test_dir_acc, dm_pvalue,
                      n_features, best_cv_rmse)])

if (!is.null(pooled_res))
  message(sprintf("\nPooled model test RMSE (all categories): %.4f", pooled_res$test_rmse))

# ── 13) Plot helpers ────────────────────────────────────────

# 13a) Actual vs Predicted + Naive benchmark
plot_actual_vs_pred <- function(cat_value) {
  df <- plot_all[categories == cat_value]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, date_d := as.Date(date)]

  x_min_test <- min(df[set == "Test", date_d], na.rm = TRUE)
  x_max      <- max(df$date_d)
  split_date <- as.Date(zoo::as.yearqtr("2020 Q4"))

  ggplot(df, aes(x = date_d)) +
    annotate("rect", xmin = x_min_test, xmax = x_max + 30,
             ymin = -Inf, ymax = Inf, fill = "#deebf7", alpha = 0.5) +
    geom_line(aes(y = actual,    colour = "Actual"),    linewidth = 0.9) +
    geom_line(aes(y = predicted, colour = "XGBoost"),   linewidth = 0.75, linetype = "dashed") +
    geom_line(data = df[set == "Test"],
              aes(y = naive, colour = "Naive RW"),
              linewidth = 0.6, linetype = "dotted") +
    geom_vline(xintercept = split_date, linetype = "dotted", colour = "grey40") +
    scale_colour_manual(
      values = c("Actual" = "#1f77b4", "XGBoost" = "#d62728", "Naive RW" = "#2ca02c")
    ) +
    labs(
      title    = paste0("YoY FICU Count | ", cat_value),
      subtitle = paste0("Shaded = test period  |  Skill score = ",
                        round(metrics_dt[categories == cat_value, skill_score], 1), "%",
                        "  |  Dir. acc = ",
                        round(metrics_dt[categories == cat_value, test_dir_acc], 1), "%"),
      x = NULL, y = "YoY % Change", colour = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
}

# 13b) Test residuals with ±1 SD bands
plot_residuals_test <- function(cat_value) {
  df  <- plot_all[categories == cat_value & set == "Test"]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, date_d := as.Date(date)]
  sd_r <- sd(df$residual, na.rm = TRUE)

  ggplot(df, aes(x = date_d, y = residual)) +
    geom_hline(yintercept = 0,     linetype = "solid",  colour = "grey50") +
    geom_hline(yintercept =  sd_r, linetype = "dashed", colour = "#fdae61") +
    geom_hline(yintercept = -sd_r, linetype = "dashed", colour = "#fdae61") +
    geom_col(fill = "#d62728", alpha = 0.7) +
    labs(
      title    = paste0("Test Residuals | ", cat_value),
      subtitle = "Dashed lines = ±1 SD",
      x = NULL, y = "Actual – Predicted"
    ) +
    theme_bw(base_size = 11)
}

# 13c) Top-10 feature importance by Gain
plot_top10_importance <- function(cat_value) {
  df <- top10_dt[categories == cat_value]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, Feature := factor(Feature, levels = Feature[order(Gain)])]
  df[, pct := Gain / sum(Gain) * 100]

  # Tag feature type for colour coding
  df[, feat_type := fcase(
    grepl("_lag|_rmean|_rsd",  Feature), "Lag / Rolling",
    grepl("regime|time_idx|quarter|qtrs_from|cycle|inv", Feature), "Trend / Cycle",
    grepl(paste(c("fedfunds","gs10","unrate","gdp","cpi","mortgage",
                   "hy_spread","payroll","deposit","loan","umich","yield"), collapse="|"),
          Feature), "Macro",
    default = "Internal"
  )]

  ggplot(df, aes(x = Feature, y = Gain, fill = feat_type)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    scale_fill_manual(
      values = c("Lag / Rolling" = "#2171b5",
                 "Trend / Cycle" = "#238b45",
                 "Macro"         = "#d94801",
                 "Internal"      = "#756bb1")
    ) +
    labs(
      title = paste0("Top 10 Drivers by Gain | ", cat_value),
      x = NULL, y = "Gain", fill = "Feature type"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
}

# 13d) Cross-category scorecard: XGB vs Naive RMSE + skill score
plot_scorecard <- function() {
  dt <- metrics_dt[!is.na(test_rmse)]
  dt_long <- melt(dt[, .(categories, test_rmse, naive_rmse)],
                  id.vars = "categories",
                  variable.name = "model", value.name = "RMSE")
  dt_long[, model := ifelse(model == "test_rmse", "XGBoost", "Naive RW")]

  p1 <- ggplot(dt_long, aes(x = reorder(categories, RMSE), y = RMSE, fill = model)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("XGBoost" = "#2171b5", "Naive RW" = "#74c476")) +
    coord_flip() +
    labs(title = "Test RMSE: XGBoost vs Naive", x = NULL, fill = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")

  p2 <- ggplot(dt[!is.na(skill_score)],
               aes(x = reorder(categories, skill_score), y = skill_score,
                   fill = skill_score > 0)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_manual(values = c("TRUE" = "#2171b5", "FALSE" = "#d62728"),
                      labels = c("TRUE" = "Better than naive", "FALSE" = "Worse than naive"),
                      name = NULL) +
    coord_flip() +
    labs(title = "Skill Score (% RMSE improvement over Naive)",
         x = NULL, y = "Skill Score (%)") +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")

  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

# ── 14) Render plots ───────────────────────────────────────
success_cats <- cats[vapply(results, function(r) is.null(r$error), logical(1))]

if (length(success_cats) > 0) {
  if (requireNamespace("gridExtra", quietly = TRUE)) print(plot_scorecard())

  for (cc in success_cats) {
    print(plot_actual_vs_pred(cc))
    print(plot_residuals_test(cc))
    print(plot_top10_importance(cc))
  }
}

############################################################
# END
############################################################
