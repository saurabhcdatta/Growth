############################################################
# PART 2 — ENHANCED XGBOOST PIPELINE
# Input:  qtrly_enriched.rds  (produced by Part 1)
# Target: yoy_ficu_count
# Groups: categories (7 CU asset-size buckets)
# Period: 2002Q1–2025Q3 | Train <= 2020Q4 | Test > 2020Q4
#
# Features:
#  - AR lags, rolling stats, trend/cycle indicators (from Part 1)
#  - Naive random-walk benchmark
#  - Directional accuracy metric
#  - Diebold-Mariano test vs naive
#  - Skill score (RMSE relative to naive)
#  - Pooled model (all categories) as comparison
#  - Full execution timing (hours, minutes, seconds)
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

# ── Timing utility ────────────────────────────────────────
# tic("label") ... code ... toc()
# Prints wall-clock start/end and elapsed h/m/s
tic <- function(msg = "") {
  assign(".tic_time", proc.time(),  envir = .GlobalEnv)
  assign(".tic_msg",  msg,          envir = .GlobalEnv)
  message(sprintf("\n[START] %s", msg))
  message(sprintf("        %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  message(strrep("-", 55))
}

toc <- function() {
  elapsed <- as.numeric((proc.time() - get(".tic_time", envir = .GlobalEnv))["elapsed"])
  hrs  <- floor(elapsed / 3600)
  mins <- floor((elapsed %% 3600) / 60)
  secs <- round(elapsed %% 60, 1)
  msg  <- get(".tic_msg", envir = .GlobalEnv)
  message(strrep("-", 55))
  message(sprintf("[DONE]  %s", msg))
  message(sprintf("        %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  message(sprintf("        Elapsed: %02dh %02dm %04.1fs", hrs, mins, secs))
  invisible(elapsed)
}

# Inline timer — one-liner for quick sub-step reporting
toc_inline <- function(t0, label) {
  elapsed <- as.numeric((proc.time() - t0)["elapsed"])
  hrs  <- floor(elapsed / 3600)
  mins <- floor((elapsed %% 3600) / 60)
  secs <- round(elapsed %% 60, 1)
  message(sprintf("  %-45s %02dh %02dm %04.1fs", label, hrs, mins, secs))
  invisible(elapsed)
}

# ── Script-wide start ─────────────────────────────────────
SCRIPT_START <- proc.time()
message(strrep("=", 55))
message("PART 2 — XGBoost CU Forecast Pipeline")
message(sprintf("Started: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message(strrep("=", 55))

# ── 1) Config ─────────────────────────────────────────────
TARGET        <- "yoy_ficu_count"
DATE_COL      <- "date"
GROUP_COL     <- "categories"
TRAIN_END     <- zoo::as.yearqtr("2020 Q4")
CORR_CUT      <- 0.97

INITIAL_QTRS  <- 40
ASSESS_QTRS   <- 4
SKIP_QTRS     <- 2
MAX_CV_FOLDS  <- 12

NROUNDS_MAX   <- 2000
EARLY_STOP    <- 40
N_SEARCH_ITER <- 60

N_CORES <- max(1L, parallel::detectCores() - 1L)
message(sprintf("Config: %d cores | %d search iters | %d max CV folds",
                N_CORES, N_SEARCH_ITER, MAX_CV_FOLDS))

# ── Date utility ──────────────────────────────────────────
# qtrly.rds dates are quarter-start Dates: 2001-01-01, 2001-04-01, etc.
date_to_yearqtr <- function(x) {
  d   <- as.Date(x)
  yr  <- as.integer(format(d, "%Y"))
  mo  <- as.integer(format(d, "%m"))
  qtr <- (mo - 1L) %/% 3L + 1L
  zoo::as.yearqtr(paste0(yr, " Q", qtr))
}

# ── 2) Load enriched data ─────────────────────────────────
tic("Loading & preparing data")
t0_load <- proc.time()

qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)

if (!inherits(qtrly[[DATE_COL]], "yearqtr"))
  qtrly[, (DATE_COL) := date_to_yearqtr(get(DATE_COL))]

qtrly <- qtrly[date >= zoo::as.yearqtr("2002 Q1")]
setorderv(qtrly, c(GROUP_COL, DATE_COL))

# Convert GROUP_COL to character to guarantee list names = actual labels
# (data.table split() names by factor level integer when column is factor)
qtrly[, (GROUP_COL) := as.character(get(GROUP_COL))]

# Base R split — always names list elements by actual group value
cat_list <- split(qtrly, f = qtrly[[GROUP_COL]])
cats     <- sort(names(cat_list))

# Sanity check — print so you can verify names are real labels not numbers
message(sprintf("cat_list names: %s", paste(cats, collapse = " | ")))

toc_inline(t0_load, sprintf(
  "Data loaded: %d rows x %d cols | %d categories",
  nrow(qtrly), ncol(qtrly), length(cats)
))
message(sprintf("  Categories: %s", paste(cats, collapse = ", ")))
toc()

# ── 3) Feature prep helpers ───────────────────────────────

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
  # Guard: TARGET column must exist
  if (!TARGET %in% names(dt_cat))
    return(list(dt = dt_cat, x_cols = character(0)))
  dt_cat   <- dt_cat[!is.na(get(TARGET))]
  num_cols <- names(dt_cat)[vapply(dt_cat, is.numeric, logical(1))]
  x_cols   <- setdiff(num_cols, c(TARGET, DATE_COL, GROUP_COL, "quarter_num"))

  # Drop raw macro levels — keep YoY transforms to avoid trend leakage
  raw_macro_levels <- c("real_gdp", "cpi", "pce_deflator", "nonfarm_pay",
                         "deposits", "bank_loans", "indpro", "houst")
  x_cols <- setdiff(x_cols, raw_macro_levels)

  if (length(x_cols) == 0) return(list(dt = dt_cat, x_cols = character(0)))

  # Impute NAs with column median
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

# ── 4) Rolling-origin CV folds ────────────────────────────
make_folds <- function(n, initial = INITIAL_QTRS, assess = ASSESS_QTRS,
                       skip = SKIP_QTRS, max_folds = MAX_CV_FOLDS) {
  folds <- list(); t <- initial
  while ((t + assess) <= n && length(folds) < max_folds) {
    folds[[length(folds) + 1L]] <- (t + 1L):(t + assess)
    t <- t + skip
  }
  folds
}

# ── 5) Random param sampler ───────────────────────────────
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

# ── 6) Score one param set via TS-CV ─────────────────────
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
  list(rmse    = min(cv$evaluation_log$test_rmse_mean),
       nrounds = cv$best_iteration)
}

# ── 7) Naive benchmark ────────────────────────────────────
naive_forecast <- function(y_train, n_test) {
  rep(tail(y_train, 1L), n_test)
}

# ── 8) Diebold-Mariano test ───────────────────────────────
dm_test <- function(e1, e2) {
  d <- e2^2 - e1^2   # positive => XGB better
  n <- length(d)
  if (n < 4) return(list(stat = NA, p_value = NA))
  dbar   <- mean(d)
  h      <- 4L
  gamma0 <- var(d)
  gammas <- sapply(1:(h - 1), function(k)
    mean((d[(k+1):n] - dbar) * (d[1:(n-k)] - dbar)))
  nw_var <- max(gamma0 + 2 * sum((1 - seq_len(h-1)/h) * gammas), 1e-12)
  stat   <- dbar / sqrt(nw_var / n)
  p_val  <- pt(stat, df = n - 1, lower.tail = FALSE)
  list(stat = round(stat, 3), p_value = round(p_val, 4))
}

# ── 9) Full pipeline for one category ────────────────────
run_category <- function(dt_cat, cat_value) {

  t_cat <- proc.time()   # category-level timer

  # Defensive: ensure GROUP_COL is character in this worker's copy
  if (GROUP_COL %in% names(dt_cat))
    dt_cat[, (GROUP_COL) := as.character(get(GROUP_COL))]

  # Guard against empty input (NULL cat_list slot)
  if (is.null(dt_cat) || nrow(dt_cat) == 0)
    return(list(category = cat_value, error = "Empty data slice received by worker"))

  prep   <- prep_xy(dt_cat)
  dt     <- prep$dt
  x_cols <- prep$x_cols

  if (length(x_cols) == 0)
    return(list(category = cat_value,
                error = "No usable predictors after NZV/corr filter"))

  tr_idx <- which(dt[[DATE_COL]] <= TRAIN_END)
  te_idx <- which(dt[[DATE_COL]] >  TRAIN_END)
  dt_tr  <- dt[tr_idx]
  dt_te  <- dt[te_idx]

  n_tr <- nrow(dt_tr)
  if (n_tr < (INITIAL_QTRS + ASSESS_QTRS + 2L))
    return(list(category = cat_value,
                error = paste("Insufficient train rows:", n_tr)))

  fold_list <- make_folds(n_tr)
  if (length(fold_list) == 0)
    return(list(category = cat_value, error = "No CV folds generated"))

  X_tr  <- as.matrix(dt_tr[, x_cols, with = FALSE])
  y_tr  <- dt_tr[[TARGET]]
  dmat  <- xgb.DMatrix(data = X_tr, label = y_tr)

  # ---- Grid search (timed) --------------------------------
  t_gs  <- proc.time()
  grid  <- sample_params(N_SEARCH_ITER)
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
  gs_secs <- as.numeric((proc.time() - t_gs)["elapsed"])

  # Guard: fall back to safe defaults if all CV iterations failed
  if (is.null(best_params)) {
    best_params <- list(
      booster = "gbtree", objective = "reg:squarederror",
      eval_metric = "rmse", nthread = 1L,
      eta = 0.05, max_depth = 3, min_child_weight = 5,
      subsample = 0.8, colsample_bytree = 0.8,
      gamma = 0, lambda = 5, alpha = 0
    )
    best_nrounds <- 200L
  }

  # ---- Final model ----------------------------------------
  model   <- xgb.train(params = best_params, data = dmat,
                        nrounds = best_nrounds, verbose = 0)
  pred_tr <- predict(model, X_tr)
  pred_te <- if (nrow(dt_te) > 0)
    predict(model, as.matrix(dt_te[, x_cols, with = FALSE]))
  else numeric(0)

  naive_te <- naive_forecast(y_tr, nrow(dt_te))

  y_te     <- if (nrow(dt_te) > 0) dt_te[[TARGET]] else numeric(0)
  resid_tr <- y_tr - pred_tr
  resid_te <- y_te - pred_te

  # ---- Metrics --------------------------------------------
  rmse_fn <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
  mae_fn  <- function(a, p) mean(abs(a - p), na.rm = TRUE)
  mape_fn <- function(a, p) mean(abs((a - p) / (abs(a) + 1e-8)) * 100, na.rm = TRUE)
  dir_acc <- function(a, p) mean(sign(diff(a)) == sign(diff(p)), na.rm = TRUE) * 100

  naive_rmse_te <- if (length(y_te) > 0) rmse_fn(y_te, naive_te) else NA_real_
  xgb_rmse_te   <- if (length(y_te) > 0) rmse_fn(y_te, pred_te)  else NA_real_
  skill_score   <- if (!is.na(naive_rmse_te) && naive_rmse_te > 0)
    (1 - xgb_rmse_te / naive_rmse_te) * 100 else NA_real_

  dm <- if (length(y_te) >= 4) dm_test(resid_te, y_te - naive_te)
        else list(stat = NA, p_value = NA)

  cat_secs <- as.numeric((proc.time() - t_cat)["elapsed"])

  metrics <- data.frame(
    categories    = cat_value,
    best_cv_rmse  = best_rmse,
    best_nrounds  = best_nrounds,
    n_features    = length(x_cols),
    n_cv_folds    = length(fold_list),
    train_rmse    = rmse_fn(y_tr, pred_tr),
    train_mae     = mae_fn(y_tr,  pred_tr),
    test_rmse     = xgb_rmse_te,
    test_mae      = if (length(y_te) > 0) mae_fn(y_te,  pred_te) else NA_real_,
    test_mape     = if (length(y_te) > 0) mape_fn(y_te, pred_te) else NA_real_,
    test_dir_acc  = if (length(y_te) > 1) dir_acc(y_te, pred_te) else NA_real_,
    naive_rmse    = naive_rmse_te,
    skill_score   = skill_score,
    dm_stat       = dm$stat,
    dm_pvalue     = dm$p_value,
    gs_seconds    = round(gs_secs,  1),
    total_seconds = round(cat_secs, 1)
  )

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

# ── 10) Parallel run across categories ───────────────────
worker_exports <- c(
  # data (already character-keyed after fix above)
  "cat_list",
  # functions
  "run_category", "prep_xy", "drop_nzv", "drop_highcorr",
  "make_folds", "sample_params", "score_params",
  "naive_forecast", "dm_test", "date_to_yearqtr",
  # config constants
  "TARGET", "DATE_COL", "GROUP_COL", "TRAIN_END", "CORR_CUT",
  "INITIAL_QTRS", "ASSESS_QTRS", "SKIP_QTRS", "MAX_CV_FOLDS",
  "NROUNDS_MAX", "EARLY_STOP", "N_SEARCH_ITER"
)

tic(sprintf("Parallel XGBoost — %d categories on %d cores",
            length(cats), N_CORES))

cl <- parallel::makeCluster(N_CORES)
doParallel::registerDoParallel(cl)

results <- foreach::foreach(
  cc        = cats,
  .export   = worker_exports,
  .packages = c("data.table", "zoo", "xgboost")
) %dopar% {
  tryCatch(
    run_category(cat_list[[cc]], cc),
    error = function(e)
      list(category = cc, error = conditionMessage(e),
           top10 = NULL, plot_df = NULL, metrics = NULL)
  )
}

parallel::stopCluster(cl)
names(results) <- cats
parallel_time <- toc()

# ── DIAGNOSTIC: per-category status & timing ─────────────
message("\n── Category run status ──────────────────────────────")
message(sprintf("  %-22s %-8s %-10s %-8s %-10s %-14s",
                "Category", "Status", "CV RMSE", "Rounds",
                "Features", "Time"))
message(strrep("-", 76))

for (r in results) {
  if (!is.null(r$error) && !is.na(r$error)) {
    message(sprintf("  %-22s %-8s %s", r$category, "FAILED", r$error))
  } else {
    t_secs <- if (!is.null(r$metrics)) r$metrics$total_seconds else 0
    hrs    <- floor(t_secs / 3600)
    mins   <- floor((t_secs %% 3600) / 60)
    secs   <- round(t_secs %% 60, 1)
    message(sprintf("  %-22s %-8s %-10.4f %-8d %-10d %02dh %02dm %04.1fs",
                    r$category, "OK",
                    ifelse(is.null(r$best_cv_rmse), NA, r$best_cv_rmse),
                    ifelse(is.null(r$best_nrounds), NA, r$best_nrounds),
                    ifelse(is.null(r$x_cols), 0L, length(r$x_cols)),
                    hrs, mins, secs))
  }
}

# ── 11) Pooled model ──────────────────────────────────────
tic("Pooled model (all categories)")

pooled_res <- tryCatch({
  qtrly_pool <- rbindlist(cat_list)
  qtrly_pool[, cat_idx := as.integer(factor(get(GROUP_COL)))]

  prep_pool  <- prep_xy(qtrly_pool)
  dt_pool    <- prep_pool$dt
  x_pool     <- prep_pool$x_cols
  if (!"cat_idx" %in% x_pool) x_pool <- c(x_pool, "cat_idx")

  tr_pool <- dt_pool[get(DATE_COL) <= TRAIN_END]
  te_pool <- dt_pool[get(DATE_COL) >  TRAIN_END]

  dmat_pool <- xgb.DMatrix(
    data  = as.matrix(tr_pool[, x_pool, with = FALSE]),
    label = tr_pool[[TARGET]]
  )

  pool_initial <- min(INITIAL_QTRS * length(cats),
                      as.integer(nrow(tr_pool) * 0.6))
  fold_pool    <- make_folds(nrow(tr_pool), initial = pool_initial)
  if (length(fold_pool) == 0)
    fold_pool <- make_folds(nrow(tr_pool),
                             initial = as.integer(nrow(tr_pool) * 0.5))

  pool_params <- list(
    booster = "gbtree", objective = "reg:squarederror",
    eval_metric = "rmse", nthread = N_CORES,
    eta = 0.05, max_depth = 4, min_child_weight = 10,
    subsample = 0.8, colsample_bytree = 0.7,
    gamma = 0.1, lambda = 5, alpha = 0.5
  )

  pool_cv <- xgb.cv(
    params = pool_params, data = dmat_pool, nrounds = NROUNDS_MAX,
    folds = if (length(fold_pool) > 0) fold_pool
            else make_folds(nrow(tr_pool)),
    metrics = "rmse", early_stopping_rounds = EARLY_STOP,
    verbose = 0, showsd = FALSE
  )

  pool_model <- xgb.train(
    params = pool_params, data = dmat_pool,
    nrounds = pool_cv$best_iteration, verbose = 0
  )

  pred_pool_te <- predict(
    pool_model, as.matrix(te_pool[, x_pool, with = FALSE])
  )

  list(
    model     = pool_model,
    x_cols    = x_pool,
    pred_te   = data.table(
      categories = te_pool[[GROUP_COL]],
      date       = te_pool[[DATE_COL]],
      actual     = te_pool[[TARGET]],
      predicted  = pred_pool_te
    ),
    test_rmse = sqrt(mean(
      (te_pool[[TARGET]] - pred_pool_te)^2, na.rm = TRUE
    ))
  )
}, error = function(e) {
  message("  Pooled model failed: ", e$message); NULL
})

toc()

# ── 12) Collect outputs ───────────────────────────────────
tic("Collecting outputs & saving")

metrics_dt <- rbindlist(
  lapply(results, function(r)
    if (!is.null(r$error) && !is.na(r$error))
      data.table(categories = r$category, error = r$error)
    else
      cbind(as.data.table(r$metrics), error = NA_character_)
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

toc()

# ── 13) Print metrics summary ─────────────────────────────
message("\n── Per-Category Metrics ─────────────────────────────")
metric_cols <- intersect(
  c("categories", "error", "test_rmse", "naive_rmse",
    "skill_score", "test_dir_acc", "dm_pvalue",
    "n_features", "best_cv_rmse", "gs_seconds", "total_seconds"),
  names(metrics_dt)
)
print(metrics_dt[, ..metric_cols])

if (!is.null(pooled_res))
  message(sprintf("\nPooled model test RMSE (all categories): %.4f",
                  pooled_res$test_rmse))

# ── 14) Plot helpers ──────────────────────────────────────

# 14a) Actual vs Predicted + Naive benchmark
plot_actual_vs_pred <- function(cat_value) {
  df <- plot_all[categories == cat_value]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, date_d := as.Date(date)]

  x_min_test <- min(df[set == "Test", date_d], na.rm = TRUE)
  x_max      <- max(df$date_d)
  split_date <- as.Date(zoo::as.yearqtr("2020 Q4"))

  m_row     <- metrics_dt[categories == cat_value]
  skill_lbl <- if (nrow(m_row) > 0 && "skill_score" %in% names(m_row))
    sprintf("Skill score = %.1f%%", m_row$skill_score) else ""
  dir_lbl   <- if (nrow(m_row) > 0 && "test_dir_acc" %in% names(m_row))
    sprintf("Dir. acc = %.1f%%", m_row$test_dir_acc) else ""

  ggplot(df, aes(x = date_d)) +
    annotate("rect", xmin = x_min_test, xmax = x_max + 30,
             ymin = -Inf, ymax = Inf, fill = "#deebf7", alpha = 0.5) +
    geom_line(aes(y = actual,    colour = "Actual"),
              linewidth = 0.9) +
    geom_line(aes(y = predicted, colour = "XGBoost"),
              linewidth = 0.75, linetype = "dashed") +
    geom_line(data = df[set == "Test"],
              aes(y = naive, colour = "Naive RW"),
              linewidth = 0.6, linetype = "dotted") +
    geom_vline(xintercept = split_date,
               linetype = "dotted", colour = "grey40") +
    scale_colour_manual(values = c("Actual"   = "#1f77b4",
                                   "XGBoost"  = "#d62728",
                                   "Naive RW" = "#2ca02c")) +
    labs(title    = paste0("YoY FICU Count | ", cat_value),
         subtitle = paste0("Shaded = test period  |  ",
                           skill_lbl, "  |  ", dir_lbl),
         x = NULL, y = "YoY % Change", colour = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
}

# 14b) Test residuals with +/- 1 SD bands
plot_residuals_test <- function(cat_value) {
  df <- plot_all[categories == cat_value & set == "Test"]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, date_d := as.Date(date)]
  sd_r <- sd(df$residual, na.rm = TRUE)

  ggplot(df, aes(x = date_d, y = residual)) +
    geom_hline(yintercept =  0,    linetype = "solid",  colour = "grey50") +
    geom_hline(yintercept =  sd_r, linetype = "dashed", colour = "#fdae61") +
    geom_hline(yintercept = -sd_r, linetype = "dashed", colour = "#fdae61") +
    geom_col(fill = "#d62728", alpha = 0.7) +
    labs(title    = paste0("Test Residuals | ", cat_value),
         subtitle = "Dashed lines = +/- 1 SD",
         x = NULL, y = "Actual - Predicted") +
    theme_bw(base_size = 11)
}

# 14c) Top-10 feature importance (colour-coded by type)
plot_top10_importance <- function(cat_value) {
  df <- top10_dt[categories == cat_value]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, Feature := factor(Feature, levels = Feature[order(Gain)])]
  df[, pct := Gain / sum(Gain) * 100]
  df[, feat_type := fcase(
    grepl("_lag|_rmean|_rsd", Feature),                             "Lag / Rolling",
    grepl("regime|time_idx|quarter|qtrs_from|cycle|inv", Feature),  "Trend / Cycle",
    grepl(paste(c("fedfunds","gs10","unrate","gdp","cpi","mortgage",
                  "hy_spread","payroll","deposit","loan","umich",
                  "yield"), collapse = "|"), Feature),              "Macro",
    default = "Internal"
  )]

  ggplot(df, aes(x = Feature, y = Gain, fill = feat_type)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    scale_fill_manual(
      values = c("Lag / Rolling" = "#2171b5",
                 "Trend / Cycle" = "#238b45",
                 "Macro"         = "#d94801",
                 "Internal"      = "#756bb1")
    ) +
    labs(title = paste0("Top 10 Drivers by Gain | ", cat_value),
         x = NULL, y = "Gain", fill = "Feature type") +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
}

# 14d) Cross-category scorecard: RMSE + skill score
plot_scorecard <- function() {
  dt <- metrics_dt[!is.na(test_rmse)]
  if (nrow(dt) == 0) {
    message("No successful categories — skipping scorecard plot.")
    return(invisible(NULL))
  }
  dt_long <- melt(dt[, .(categories, test_rmse, naive_rmse)],
                  id.vars = "categories",
                  variable.name = "model", value.name = "RMSE")
  dt_long[, model := ifelse(model == "test_rmse", "XGBoost", "Naive RW")]

  p1 <- ggplot(dt_long,
               aes(x = reorder(categories, RMSE), y = RMSE, fill = model)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("XGBoost"  = "#2171b5",
                                  "Naive RW" = "#74c476")) +
    coord_flip() +
    labs(title = "Test RMSE: XGBoost vs Naive", x = NULL, fill = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")

  p2 <- ggplot(dt[!is.na(skill_score)],
               aes(x = reorder(categories, skill_score),
                   y = skill_score, fill = skill_score > 0)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_manual(
      values = c("TRUE"  = "#2171b5", "FALSE" = "#d62728"),
      labels = c("TRUE"  = "Better than naive",
                 "FALSE" = "Worse than naive"),
      name = NULL
    ) +
    coord_flip() +
    labs(title = "Skill Score (% RMSE improvement over Naive)",
         x = NULL, y = "Skill Score (%)") +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")

  if (requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  } else {
    print(p1); print(p2)
  }
}

# 14e) Execution time per category bar chart
plot_timing <- function() {
  if (!"total_seconds" %in% names(metrics_dt)) return(invisible(NULL))
  dt <- metrics_dt[!is.na(total_seconds)]
  if (nrow(dt) == 0) return(invisible(NULL))

  dt[, time_label := {
    h <- floor(total_seconds / 3600)
    m <- floor((total_seconds %% 3600) / 60)
    s <- round(total_seconds %% 60, 0)
    ifelse(h > 0,
           sprintf("%dh %02dm %02ds", h, m, s),
           ifelse(m > 0,
                  sprintf("%dm %02ds", m, s),
                  sprintf("%ds", s)))
  }]

  ggplot(dt, aes(x = reorder(categories, total_seconds),
                 y = total_seconds / 60)) +
    geom_col(fill = "#4292c6") +
    geom_text(aes(label = time_label), hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = "Execution Time per Category",
         x = NULL, y = "Minutes") +
    theme_bw(base_size = 11)
}

# ── 15) Render all plots ──────────────────────────────────
tic("Rendering plots")

success_cats <- cats[vapply(results, function(r)
  is.null(r$error) || is.na(r$error), logical(1))]

if (length(success_cats) > 0) {
  print(plot_scorecard())
  print(plot_timing())
  for (cc in success_cats) {
    print(plot_actual_vs_pred(cc))
    print(plot_residuals_test(cc))
    print(plot_top10_importance(cc))
  }
} else {
  message("No successful categories — no plots rendered.")
  message("Check the diagnostic status table above for error details.")
}

toc()

# ── 16) Total script runtime ──────────────────────────────
total_secs <- as.numeric((proc.time() - SCRIPT_START)["elapsed"])
total_hrs  <- floor(total_secs / 3600)
total_mins <- floor((total_secs %% 3600) / 60)
total_s    <- round(total_secs %% 60, 1)

message(strrep("=", 55))
message("PART 2 COMPLETE")
message(sprintf("Finished: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message(sprintf("Total runtime: %02dh %02dm %04.1fs",
                total_hrs, total_mins, total_s))
message(strrep("=", 55))

############################################################
# END
############################################################
