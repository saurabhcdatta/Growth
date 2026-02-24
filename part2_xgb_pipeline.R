############################################################
# PART 2 — XGBOOST PIPELINE
# Dependent variable : ficu_count  (actual credit union count)
#
# Modelling strategy : YoY % change → back-transform to counts
#   1. Compute yoy_pct = (ficu_count - lag4_ficu_count) / lag4 * 100
#   2. XGBoost models yoy_pct  (stationary, ~N(0,σ))
#   3. Back-transform: pred_count = lag4 * (1 + pred_yoy_pct/100)
#   4. All metrics & plots shown in BOTH spaces:
#        • YoY % (model space)
#        • Actual ficu_count (interpretable space)
#
# CONFIRMED FROM DIAGNOSTICS:
#   - categories : numeric double 1-7  → remapped to text labels
#   - date       : yearqtr  2000Q1–2025Q3
#   - ficu_count : raw CU count per category-quarter
#   - 713 rows x 773 cols | 518 engineered candidate features
#   - Smallest bucket (cat 1): 75 train / 19 test rows
############################################################

DEBUG_MODE <- TRUE    # ← set FALSE for full production run

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(xgboost)
  library(foreach)
  library(doParallel)
  library(ggplot2)
  library(httr)
})
set.seed(42)

# ── ntfy push notifications ───────────────────────────────
# 1. Install "ntfy" app on your phone (free, iOS/Android)
# 2. Subscribe to a topic of your choice in the app
# 3. Set the same topic name below
NTFY_TOPIC   <- "your-unique-topic-name"   # ← CHANGE THIS
NTFY_ENABLED <- TRUE

notify <- function(title, msg, priority = "default", tags = NULL) {
  if (!NTFY_ENABLED) return(invisible(NULL))
  tryCatch({
    h <- list(Title = title, Priority = priority)
    if (!is.null(tags)) h$Tags <- paste(tags, collapse = ",")
    httr::POST(paste0("https://ntfy.sh/", NTFY_TOPIC),
               body = msg, encode = "raw",
               do.call(httr::add_headers, h))
  }, error = function(e) message("[ntfy] ", e$message))
  invisible(NULL)
}

# ── Timing helpers ────────────────────────────────────────
.tic_t <- NULL; .tic_m <- ""
tic <- function(msg = "") {
  .tic_t <<- proc.time(); .tic_m <<- msg
  message(sprintf("\n[START] %s  (%s)", msg, format(Sys.time(), "%H:%M:%S")))
  message(strrep("-", 60))
}
toc <- function() {
  e <- as.numeric((proc.time() - .tic_t)["elapsed"])
  message(strrep("-", 60))
  message(sprintf("[DONE]  %s  elapsed: %s", .tic_m, hms(e)))
  invisible(e)
}
hms <- function(e)
  sprintf("%02dh %02dm %04.1fs",
          floor(e / 3600), floor((e %% 3600) / 60), e %% 60)

# ── Speed config ──────────────────────────────────────────
if (DEBUG_MODE) {
  INIT_Q <- 24; ASSESS_Q <- 4; SKIP_Q <- 8; MAX_F <- 3
  NR_MAX <- 100; ESTOP <- 10; N_ITER <- 6
} else {
  INIT_Q <- 40; ASSESS_Q <- 4; SKIP_Q <- 2; MAX_F <- 12
  NR_MAX <- 2000; ESTOP <- 40; N_ITER <- 60
}

# Column name constants
RAW_COUNT   <- "ficu_count"        # raw count — what we ultimately forecast
TARGET      <- "yoy_pct"           # what XGBoost actually models (computed below)
LAG4_COL    <- "ficu_count_lag4"   # prior-year count used for back-transform
DATE_COL    <- "date"
GRP         <- "cat_label"         # character category label (built below)
TRAIN_END   <- zoo::as.yearqtr("2020 Q4")
CORR_CUT    <- 0.97
N_CORES     <- max(1L, parallel::detectCores() - 1L)

SCRIPT_T0 <- proc.time()
message(strrep("=", 60))
message(sprintf("PART 2  ficu_count forecast via YoY%%  [%s]  %s",
                if (DEBUG_MODE) "DEBUG" else "PRODUCTION",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message(sprintf("Cores:%d  Iters:%d  Folds:%d  nrounds_max:%d",
                N_CORES, N_ITER, MAX_F, NR_MAX))
message(strrep("=", 60))

notify("CU Count Forecast Started",
       sprintf("[%s] ficu_count via YoY%% — %s",
               if (DEBUG_MODE) "DEBUG" else "PROD",
               format(Sys.time(), "%H:%M")),
       tags = "rocket")

# ════════════════════════════════════════════════════════
# STEP 1: LOAD, FIX CATEGORIES, BUILD TARGET
# ════════════════════════════════════════════════════════
tic("Load & build target")

qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)
message(sprintf("  Loaded: %d rows x %d cols", nrow(qtrly), ncol(qtrly)))

# Confirm required columns exist
stopifnot(
  "ficu_count missing" = RAW_COUNT %in% names(qtrly),
  "date missing"       = DATE_COL  %in% names(qtrly),
  "categories missing" = "categories" %in% names(qtrly)
)

# date is already yearqtr (confirmed by diagnostic)
stopifnot(inherits(qtrly[[DATE_COL]], "yearqtr"))

# ── Remap numeric categories 1-7 → text labels ───────────
# Confirmed: numeric double 1–7
# Labels ordered from smallest to largest asset bucket
CAT_LABELS <- c(
  "1" = "1_Less_1M",
  "2" = "2_1M_10M",
  "3" = "3_10M_50M",
  "4" = "4_50M_100M",
  "5" = "5_100M_500M",
  "6" = "6_500M_1B",
  "7" = "7_Over_1B"
)
qtrly[, (GRP) := CAT_LABELS[as.character(categories)]]

# Catch any unmapped codes
n_na <- sum(is.na(qtrly[[GRP]]))
if (n_na > 0) {
  qtrly[is.na(get(GRP)), (GRP) := paste0("cat_", as.character(categories))]
  message(sprintf("  WARNING: %d rows had unmapped category codes", n_na))
}
message(sprintf("  Categories: %s",
                paste(sort(unique(qtrly[[GRP]])), collapse = " | ")))

# ── Sort before lag computation ───────────────────────────
setorderv(qtrly, c(GRP, DATE_COL))

# ── Build YoY % target & 4-quarter lag (within each category) ──
# lag4 = ficu_count exactly 4 quarters ago (same category)
# yoy_pct = (count - lag4) / lag4 * 100
qtrly[, (LAG4_COL) := shift(get(RAW_COUNT), n = 4L, type = "lag"),
      by = GRP]

qtrly[, (TARGET) := fifelse(
  !is.na(get(LAG4_COL)) & get(LAG4_COL) > 0,
  (get(RAW_COUNT) - get(LAG4_COL)) / get(LAG4_COL) * 100,
  NA_real_
), by = GRP]

# Diagnostic check on target
tgt_ok  <- sum(!is.na(qtrly[[TARGET]]))
tgt_rng <- range(qtrly[[TARGET]], na.rm = TRUE)
message(sprintf("  yoy_pct: %d non-NA  range [%.2f%%, %.2f%%]",
                tgt_ok, tgt_rng[1], tgt_rng[2]))

# Safety: winsorise if extreme outliers exist
p01 <- quantile(qtrly[[TARGET]], 0.01, na.rm = TRUE)
p99 <- quantile(qtrly[[TARGET]], 0.99, na.rm = TRUE)
n_win <- sum(!is.na(qtrly[[TARGET]]) &
               (qtrly[[TARGET]] < p01 | qtrly[[TARGET]] > p99))
if (n_win > 0) {
  qtrly[, (TARGET) := pmax(pmin(get(TARGET), p99), p01)]
  message(sprintf("  Winsorised %d extreme values to [%.2f%%, %.2f%%]",
                  n_win, p01, p99))
}

# ── Candidate feature columns (inclusion-pattern approach) ──
# Only use engineered / stationary columns — never raw levels
FEAT_PATTERN <- paste(
  "^yoy_", "^qoq_",         # YoY and QoQ transforms of all CU metrics
  "_lag[0-9]",              # AR lags
  "_rmean[0-9]",            # rolling means
  "_rsd[0-9]",              # rolling SDs
  "^regime_",               # economic regime dummies (GFC, ZIRP, COVID, hike)
  "^time_idx$",             # linear time trend
  "^qtrs_from_",            # quarters elapsed since events
  "^fedfunds_cycle$",       # cyclical component of fed funds
  "^yield_curve_inv$",      # inverted yield curve binary flag
  # Stationary macro rates — safe levels (already I(0))
  "^fedfunds$", "^gs10$", "^gs2$",
  "^mortgage30$", "^hy_spread$", "^unrate$",
  "^yield_curve$", "^housing_starts$",
  sep = "|"
)

all_num <- names(qtrly)[vapply(qtrly, is.numeric, logical(1))]
FEATS   <- grep(FEAT_PATTERN, all_num, value = TRUE, perl = TRUE)
# Exclude the target and its components to prevent leakage
FEATS   <- setdiff(FEATS, c(TARGET, RAW_COUNT, LAG4_COL,
                             "yoy_ficu_count", DATE_COL,
                             GRP, "categories"))

message(sprintf("  Candidate features: %d  (yoy:%d qoq:%d lags:%d rmean:%d)",
                length(FEATS),
                sum(startsWith(FEATS, "yoy_")),
                sum(startsWith(FEATS, "qoq_")),
                sum(grepl("_lag[0-9]", FEATS)),
                sum(grepl("_rmean",    FEATS))))
stopifnot("No candidate features found" = length(FEATS) > 0)

# ── Filter to model range & split by category ─────────────
qtrly <- qtrly[get(DATE_COL) >= zoo::as.yearqtr("2002 Q1")]
setorderv(qtrly, c(GRP, DATE_COL))

# base R split — always names by actual string value
cat_list <- split(qtrly, f = qtrly[[GRP]])
cats     <- sort(names(cat_list))

message(sprintf("\n  Per-category row counts (train_end = %s):",
                as.character(TRAIN_END)))
for (cc in cats) {
  d   <- cat_list[[cc]]
  ntr <- sum(d[[DATE_COL]] <= TRAIN_END)
  nte <- sum(d[[DATE_COL]] >  TRAIN_END)
  nok <- sum(!is.na(d[[TARGET]]))
  message(sprintf("    %-18s  total:%d  train:%d  test:%d  target_ok:%d",
                  cc, nrow(d), ntr, nte, nok))
}
toc()

# ════════════════════════════════════════════════════════
# STEP 2: SELF-CONTAINED WORKER FUNCTION
# Models YoY% and returns results in BOTH spaces:
#   • pct space  : yoy_pct predictions vs actuals
#   • count space: back-transformed ficu_count predictions
# ════════════════════════════════════════════════════════

xgb_cat <- function(dt, cat_name, feats, target, raw_count,
                     lag4_col, date_col, train_end, corr_cut,
                     init_q, assess_q, skip_q, max_f,
                     nr_max, estop, n_iter) {
  t0 <- proc.time()

  # ── Basic guards ─────────────────────────────────────────
  if (is.null(dt) || nrow(dt) == 0)
    return(list(cat = cat_name, err = "empty input"))

  for (req in c(target, raw_count, lag4_col, date_col)) {
    if (!req %in% names(dt))
      return(list(cat = cat_name,
                  err = sprintf("required column '%s' missing", req)))
  }

  # Keep only rows with a valid target
  dt <- dt[!is.na(dt[[target]]), ]
  if (nrow(dt) == 0)
    return(list(cat = cat_name, err = "all target rows are NA"))

  # ── Feature selection ─────────────────────────────────────
  x <- intersect(feats, names(dt))
  x <- x[vapply(x, function(cn) is.numeric(dt[[cn]]), logical(1))]
  if (length(x) == 0)
    return(list(cat = cat_name, err = "no numeric feature columns"))

  # ── Impute NAs with column median ────────────────────────
  mat <- as.matrix(dt[, x, with = FALSE])
  colnames(mat) <- x
  for (j in seq_len(ncol(mat))) {
    nas <- is.na(mat[, j])
    if (any(nas)) {
      m <- median(mat[!nas, j], na.rm = TRUE)
      mat[nas, j] <- if (is.finite(m)) m else 0
    }
  }

  # ── Near-zero-variance filter ─────────────────────────────
  ok_nzv <- vapply(seq_len(ncol(mat)), function(j) {
    u <- length(unique(mat[, j]))
    u >= 3 && (u / nrow(mat)) > 0.03
  }, logical(1))
  mat <- mat[, ok_nzv, drop = FALSE]; x <- colnames(mat)
  if (ncol(mat) == 0)
    return(list(cat = cat_name, err = "0 features after NZV filter"))

  # ── High-correlation filter ───────────────────────────────
  if (ncol(mat) >= 2) {
    cm <- suppressWarnings(cor(mat, use = "pairwise.complete.obs"))
    cm[is.na(cm)] <- 0; diag(cm) <- 0
    keep <- rep(TRUE, ncol(cm))
    for (i in seq_len(ncol(cm) - 1)) {
      if (!keep[i]) next
      for (j in (i + 1):ncol(cm))
        if (keep[j] && abs(cm[i, j]) > corr_cut) keep[j] <- FALSE
    }
    mat <- mat[, keep, drop = FALSE]; x <- colnames(mat)
  }
  if (ncol(mat) == 0)
    return(list(cat = cat_name, err = "0 features after corr filter"))

  # ── Train / test masks ────────────────────────────────────
  tr <- dt[[date_col]] <= train_end
  te <- dt[[date_col]] >  train_end
  ntr <- sum(tr); nte <- sum(te)

  if (ntr < (init_q + assess_q + 2L))
    return(list(cat = cat_name,
                err = sprintf("train rows %d < needed %d",
                              ntr, init_q + assess_q + 2L)))

  X_tr <- mat[tr, , drop = FALSE]; y_tr <- dt[[target]][tr]
  X_te <- mat[te, , drop = FALSE]; y_te <- dt[[target]][te]

  # Grab count-space values for back-transform
  count_tr  <- dt[[raw_count]][tr]
  count_te  <- dt[[raw_count]][te]
  lag4_tr   <- dt[[lag4_col]][tr]
  lag4_te   <- dt[[lag4_col]][te]

  # ── Rolling-origin CV folds ───────────────────────────────
  folds <- list(); tf <- init_q
  while ((tf + assess_q) <= ntr && length(folds) < max_f) {
    folds[[length(folds) + 1L]] <- (tf + 1L):(tf + assess_q)
    tf <- tf + skip_q
  }
  if (length(folds) == 0)
    return(list(cat = cat_name,
                err = sprintf("no CV folds (ntr=%d init=%d)", ntr, init_q)))

  # ── Random grid search ────────────────────────────────────
  set.seed(42L)
  grid <- data.frame(
    eta              = sample(c(0.01, 0.05, 0.10),  n_iter, replace = TRUE),
    max_depth        = sample(2:4,                   n_iter, replace = TRUE),
    min_child_weight = sample(c(3L, 5L, 10L),       n_iter, replace = TRUE),
    subsample        = sample(c(0.7, 0.8, 0.9),      n_iter, replace = TRUE),
    colsample_bytree = sample(c(0.6, 0.8, 1.0),      n_iter, replace = TRUE),
    gamma            = sample(c(0, 0.1, 0.5),        n_iter, replace = TRUE),
    lambda           = sample(c(1, 5, 10),           n_iter, replace = TRUE),
    alpha            = sample(c(0, 0.5, 1),          n_iter, replace = TRUE),
    stringsAsFactors = FALSE
  )

  dmat      <- xgb.DMatrix(data = X_tr, label = y_tr)
  best_rmse <- Inf; best_nr <- 50L; best_p <- NULL

  for (i in seq_len(nrow(grid))) {
    p <- list(
      booster = "gbtree", objective = "reg:squarederror",
      eval_metric = "rmse", nthread = 1L,
      eta = grid$eta[i], max_depth = grid$max_depth[i],
      min_child_weight = grid$min_child_weight[i],
      subsample = grid$subsample[i],
      colsample_bytree = grid$colsample_bytree[i],
      gamma = grid$gamma[i], lambda = grid$lambda[i],
      alpha = grid$alpha[i]
    )
    sc <- tryCatch({
      cv <- xgb.cv(params = p, data = dmat, nrounds = nr_max,
                   folds = folds, early_stopping_rounds = estop,
                   verbose = 0, showsd = FALSE)
      list(rmse = min(cv$evaluation_log$test_rmse_mean, na.rm = TRUE),
           nr   = max(cv$best_iteration, 5L))
    }, error = function(e) list(rmse = Inf, nr = 50L))

    if (is.finite(sc$rmse) && sc$rmse < best_rmse) {
      best_rmse <- sc$rmse; best_nr <- sc$nr; best_p <- p
    }
  }

  # Fallback params if every CV iteration errored
  if (is.null(best_p)) {
    best_p    <- list(booster = "gbtree", objective = "reg:squarederror",
                      eval_metric = "rmse", nthread = 1L,
                      eta = 0.05, max_depth = 3, min_child_weight = 5,
                      subsample = 0.8, colsample_bytree = 0.8,
                      gamma = 0, lambda = 5, alpha = 0)
    best_nr   <- 50L
    best_rmse <- NA_real_
  }

  # ── Final model — predict in YoY% space ──────────────────
  mdl        <- xgb.train(params = best_p, data = dmat,
                            nrounds = best_nr, verbose = 0)
  pred_pct_tr <- predict(mdl, X_tr)
  pred_pct_te <- if (nte > 0) predict(mdl, X_te) else numeric(0)

  # Naive benchmark in YoY% space: last training YoY%
  naive_pct <- rep(tail(y_tr, 1L), nte)

  # ── Back-transform to ficu_count space ───────────────────
  # pred_count = lag4 * (1 + pred_yoy_pct / 100)
  # This reconstructs absolute count from the % prediction
  bt_count <- function(lag4, pct)
    ifelse(!is.na(lag4) & lag4 > 0,
           round(lag4 * (1 + pct / 100)),
           NA_real_)

  pred_count_tr <- bt_count(lag4_tr, pred_pct_tr)
  pred_count_te <- bt_count(lag4_te, pred_pct_te)
  naive_count   <- bt_count(lag4_te, naive_pct)

  res_pct_tr <- y_tr      - pred_pct_tr
  res_pct_te <- y_te      - pred_pct_te
  res_cnt_te <- count_te  - pred_count_te

  # ── Metrics ───────────────────────────────────────────────
  rmse_fn <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
  mae_fn  <- function(a, p) mean(abs(a - p), na.rm = TRUE)

  # YoY % space metrics
  naive_pct_rmse <- rmse_fn(y_te, naive_pct)
  xgb_pct_rmse   <- rmse_fn(y_te, pred_pct_te)
  skill_pct      <- if (is.finite(naive_pct_rmse) && naive_pct_rmse > 0)
    (1 - xgb_pct_rmse / naive_pct_rmse) * 100 else NA_real_

  # Count space metrics
  naive_cnt_rmse <- rmse_fn(count_te, naive_count)
  xgb_cnt_rmse   <- rmse_fn(count_te, pred_count_te)
  xgb_cnt_mae    <- mae_fn(count_te,  pred_count_te)
  skill_count    <- if (is.finite(naive_cnt_rmse) && naive_cnt_rmse > 0)
    (1 - xgb_cnt_rmse / naive_cnt_rmse) * 100 else NA_real_

  # Directional accuracy on counts (was the direction of change correct?)
  dir_acc <- if (length(count_te) > 1)
    mean(sign(diff(count_te)) == sign(diff(pred_count_te)),
         na.rm = TRUE) * 100
  else NA_real_

  # Diebold-Mariano test (in count space — the space that matters)
  dm_s <- dm_p <- NA_real_
  if (length(res_cnt_te) >= 4) {
    tryCatch({
      d   <- (count_te - naive_count)^2 - res_cnt_te^2
      n   <- length(d); db <- mean(d, na.rm = TRUE)
      g0  <- var(d, na.rm = TRUE)
      gs  <- sapply(1:3, function(k)
        mean((d[(k+1):n] - db) * (d[1:(n-k)] - db), na.rm = TRUE))
      nwv <- max(g0 + 2 * sum((1 - 1:3 / 4) * gs), 1e-12)
      st  <- db / sqrt(nwv / n)
      dm_s <- round(st, 3)
      dm_p <- round(pt(st, df = n - 1, lower.tail = FALSE), 4)
    }, error = function(e) NULL)
  }

  tot_s <- as.numeric((proc.time() - t0)["elapsed"])

  date_tr <- dt[[date_col]][tr]
  date_te <- dt[[date_col]][te]

  # ── Plot data: both spaces ────────────────────────────────
  plot_df <- rbind(
    # YoY % space
    data.frame(
      cat = cat_name, date = date_tr, set = "Train", space = "pct",
      actual = y_tr,     predicted = pred_pct_tr,
      naive  = NA_real_, residual  = res_pct_tr
    ),
    data.frame(
      cat = cat_name, date = date_te, set = "Test", space = "pct",
      actual = y_te,     predicted = pred_pct_te,
      naive  = naive_pct, residual = res_pct_te
    ),
    # Count space
    data.frame(
      cat = cat_name, date = date_tr, set = "Train", space = "count",
      actual = count_tr, predicted = pred_count_tr,
      naive  = NA_real_, residual  = count_tr - pred_count_tr
    ),
    data.frame(
      cat = cat_name, date = date_te, set = "Test", space = "count",
      actual = count_te, predicted = pred_count_te,
      naive  = naive_count, residual = res_cnt_te
    )
  )

  imp   <- xgb.importance(feature_names = x, model = mdl)
  top10 <- head(imp, 10L)

  metrics <- data.frame(
    categories      = cat_name,
    n_train         = ntr,
    n_test          = nte,
    n_features      = length(x),
    n_folds         = length(folds),
    best_cv_rmse    = best_rmse,
    best_nrounds    = best_nr,
    # YoY % space
    train_pct_rmse  = rmse_fn(y_tr, pred_pct_tr),
    test_pct_rmse   = xgb_pct_rmse,
    naive_pct_rmse  = naive_pct_rmse,
    skill_pct       = skill_pct,
    # Count space (the number people care about)
    test_count_rmse = xgb_cnt_rmse,
    test_count_mae  = xgb_cnt_mae,
    naive_count_rmse= naive_cnt_rmse,
    skill_count     = skill_count,
    dir_acc         = dir_acc,
    dm_stat         = dm_s,
    dm_pvalue       = dm_p,
    total_seconds   = round(tot_s, 1)
  )

  list(cat = cat_name, err = NULL, model = mdl,
       x_cols = x, metrics = metrics, plot_df = plot_df,
       top10 = top10,
       best_cv_rmse = best_rmse, best_nrounds = best_nr)
}

# ════════════════════════════════════════════════════════
# STEP 3: PARALLEL RUN
# ════════════════════════════════════════════════════════

# Bundle everything into W — workers receive it via .export
W <- list(
  feats     = FEATS,
  target    = TARGET,
  raw_count = RAW_COUNT,
  lag4_col  = LAG4_COL,
  date_col  = DATE_COL,
  train_end = TRAIN_END,
  corr_cut  = CORR_CUT,
  init_q    = INIT_Q,  assess_q = ASSESS_Q,
  skip_q    = SKIP_Q,  max_f    = MAX_F,
  nr_max    = NR_MAX,  estop    = ESTOP,
  n_iter    = N_ITER
)

tic(sprintf("Parallel XGBoost — %d cats x %d iters [%s]",
            length(cats), N_ITER, if (DEBUG_MODE) "DEBUG" else "PROD"))
notify("Grid Search Started",
       sprintf("[%s] %d cats x %d iters on %d cores",
               if (DEBUG_MODE) "DEBUG" else "PROD",
               length(cats), N_ITER, N_CORES),
       tags = "hourglass_flowing_sand")

cl <- parallel::makeCluster(N_CORES)
doParallel::registerDoParallel(cl)

results <- foreach::foreach(
  cc        = cats,
  .export   = c("cat_list", "xgb_cat", "W"),
  .packages = c("data.table", "zoo", "xgboost")
) %dopar% {
  tryCatch(
    do.call(xgb_cat, c(list(dt = cat_list[[cc]], cat_name = cc), W)),
    error = function(e)
      list(cat = cc, err = conditionMessage(e),
           model = NULL, metrics = NULL,
           plot_df = NULL, top10 = NULL)
  )
}

parallel::stopCluster(cl)
names(results) <- cats
par_time <- toc()

# ── Status table ──────────────────────────────────────────
n_ok  <- sum(vapply(results, function(r) is.null(r$err), logical(1)))
n_err <- length(results) - n_ok

message(sprintf(
  "\n  %-20s %-7s %-9s %-8s %-10s %-10s %s",
  "Category", "Status", "CV RMSE", "Feats",
  "Skill%Cnt", "DirAcc%", "Time"))
message(strrep("-", 80))
for (r in results) {
  if (!is.null(r$err)) {
    message(sprintf("  %-20s FAILED  %s", r$cat, r$err))
  } else {
    m <- r$metrics
    message(sprintf(
      "  %-20s OK      %-9s %-8d %-10s %-10s %s",
      r$cat,
      if (is.finite(r$best_cv_rmse)) sprintf("%.4f", r$best_cv_rmse) else "NA",
      m$n_features,
      if (!is.na(m$skill_count)) sprintf("%.1f%%", m$skill_count) else "NA",
      if (!is.na(m$dir_acc))    sprintf("%.1f%%", m$dir_acc)    else "NA",
      sprintf("%dm%04.1fs",
              floor(m$total_seconds / 60), m$total_seconds %% 60)
    ))
  }
}

if (n_err == 0) {
  notify("Grid Search Done",
         sprintf("All %d OK in %s", n_ok, hms(par_time)),
         tags = "white_check_mark")
} else {
  bad <- paste(sapply(
    results[sapply(results, function(r) !is.null(r$err))],
    function(r) r$cat), collapse = ", ")
  notify("Grid Search (failures)",
         sprintf("%d/%d OK. Failed: %s", n_ok, length(results), bad),
         priority = "high", tags = "warning")
}

# ════════════════════════════════════════════════════════
# STEP 4: POOLED MODEL
# ════════════════════════════════════════════════════════
tic("Pooled model")

pooled <- tryCatch({
  pool <- rbindlist(cat_list)
  pool[, cat_idx := as.integer(factor(get(GRP)))]

  feat_p <- c(intersect(FEATS, names(pool)), "cat_idx")

  mat_p <- as.matrix(pool[, feat_p, with = FALSE])
  for (j in seq_len(ncol(mat_p))) {
    nas <- is.na(mat_p[, j])
    if (any(nas)) {
      m <- median(mat_p[!nas, j], na.rm = TRUE)
      mat_p[nas, j] <- if (is.finite(m)) m else 0
    }
  }
  pool[, (feat_p) := as.data.table(mat_p)]

  trp <- pool[get(DATE_COL) <= TRAIN_END]
  tep <- pool[get(DATE_COL) >  TRAIN_END]

  # CV folds for pooled
  pi <- min(INIT_Q * length(cats), as.integer(nrow(trp) * 0.6))
  fp <- list(); tf <- pi
  while ((tf + ASSESS_Q) <= nrow(trp) && length(fp) < MAX_F) {
    fp[[length(fp) + 1L]] <- (tf + 1L):(tf + ASSESS_Q)
    tf <- tf + SKIP_Q
  }
  if (length(fp) == 0) {
    pi2 <- as.integer(nrow(trp) * 0.5); tf2 <- pi2
    while ((tf2 + ASSESS_Q) <= nrow(trp) && length(fp) < MAX_F) {
      fp[[length(fp) + 1L]] <- (tf2 + 1L):(tf2 + ASSESS_Q)
      tf2 <- tf2 + SKIP_Q
    }
  }

  dm_pool <- xgb.DMatrix(
    data  = as.matrix(trp[, feat_p, with = FALSE]),
    label = trp[[TARGET]]
  )
  pp <- list(booster = "gbtree", objective = "reg:squarederror",
             eval_metric = "rmse", nthread = N_CORES,
             eta = 0.05, max_depth = 4, min_child_weight = 10,
             subsample = 0.8, colsample_bytree = 0.7,
             gamma = 0.1, lambda = 5, alpha = 0.5)

  pcv <- xgb.cv(params = pp, data = dm_pool, nrounds = NR_MAX,
                folds = fp, early_stopping_rounds = ESTOP,
                verbose = 0, showsd = FALSE)

  pm  <- xgb.train(params = pp, data = dm_pool,
                   nrounds = max(pcv$best_iteration, 5L), verbose = 0)

  pred_pct_pool <- predict(pm, as.matrix(tep[, feat_p, with = FALSE]))

  # Back-transform pooled predictions to counts
  pred_cnt_pool <- ifelse(
    !is.na(tep[[LAG4_COL]]) & tep[[LAG4_COL]] > 0,
    round(tep[[LAG4_COL]] * (1 + pred_pct_pool / 100)),
    NA_real_
  )

  cnt_rmse_pool <- sqrt(mean(
    (tep[[RAW_COUNT]] - pred_cnt_pool)^2, na.rm = TRUE))
  pct_rmse_pool <- sqrt(mean(
    (tep[[TARGET]] - pred_pct_pool)^2, na.rm = TRUE))

  list(
    model          = pm,
    x_cols         = feat_p,
    pct_rmse       = pct_rmse_pool,
    count_rmse     = cnt_rmse_pool,
    pred_te = data.table(
      cat        = tep[[GRP]],
      date       = tep[[DATE_COL]],
      actual_pct = tep[[TARGET]],
      pred_pct   = pred_pct_pool,
      actual_cnt = tep[[RAW_COUNT]],
      pred_cnt   = pred_cnt_pool
    )
  )
}, error = function(e) {
  message("  Pooled failed: ", e$message)
  notify("Pooled Failed", e$message, priority = "high", tags = "x")
  NULL
})

toc()

# ════════════════════════════════════════════════════════
# STEP 5: COLLECT & SAVE
# ════════════════════════════════════════════════════════
tic("Collect & save")

sfx <- if (DEBUG_MODE) "_debug" else "_enriched"

metrics_dt <- rbindlist(lapply(results, function(r)
  if (!is.null(r$err))
    data.table(categories = r$cat, error = r$err)
  else
    cbind(as.data.table(r$metrics), error = NA_character_)
), fill = TRUE)

top10_dt <- rbindlist(lapply(results, function(r)
  if (!is.null(r$err) || is.null(r$top10)) NULL
  else data.table(categories = r$cat, r$top10)
), fill = TRUE)

plot_all <- rbindlist(lapply(results, function(r)
  if (!is.null(r$err) || is.null(r$plot_df)) NULL
  else as.data.table(r$plot_df)
), fill = TRUE)

saveRDS(results,   paste0("xgb_results",  sfx, ".rds"))
fwrite(metrics_dt, paste0("xgb_metrics",  sfx, ".csv"))
fwrite(top10_dt,   paste0("xgb_top10",    sfx, ".csv"))

toc()

# Print metrics summary
message("\n── Metrics Summary ──────────────────────────────────")
ok_cols <- intersect(
  c("categories", "error", "n_train", "n_test", "n_features",
    "test_count_rmse", "test_count_mae", "naive_count_rmse",
    "skill_count", "dir_acc", "dm_pvalue",
    "test_pct_rmse", "skill_pct", "total_seconds"),
  names(metrics_dt))
print(metrics_dt[, ..ok_cols])

if (!is.null(pooled))
  message(sprintf(
    "\nPooled model — YoY%% RMSE: %.4f  |  Count RMSE: %.1f CUs",
    pooled$pct_rmse, pooled$count_rmse))

# ════════════════════════════════════════════════════════
# STEP 6: PLOTS (dual-space — both % and counts)
# ════════════════════════════════════════════════════════
tic("Render plots")

ok_cats <- cats[vapply(results, function(r) is.null(r$err), logical(1))]

# ── Plot 1: Actual vs Predicted in COUNT space ────────────
plot_count <- function(cv) {
  df <- plot_all[cat == cv & space == "count"]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, dd := as.Date(date)]
  m   <- metrics_dt[categories == cv]
  sc  <- if (!is.na(m$skill_count))
    sprintf("Count Skill=%.1f%%", m$skill_count) else ""
  da  <- if (!is.na(m$dir_acc))
    sprintf("Dir=%.1f%%", m$dir_acc) else ""
  xmt <- min(df[set == "Test", dd], na.rm = TRUE)
  xmx <- max(df$dd) + 30

  ggplot(df, aes(x = dd)) +
    annotate("rect", xmin = xmt, xmax = xmx, ymin = -Inf, ymax = Inf,
             fill = "#deebf7", alpha = 0.5) +
    geom_line(aes(y = actual,    colour = "Actual"),   linewidth = 0.9) +
    geom_line(aes(y = predicted, colour = "XGBoost"),  linewidth = 0.75,
              linetype = "dashed") +
    geom_line(data = df[set == "Test"],
              aes(y = naive, colour = "Naive RW"),     linewidth = 0.6,
              linetype = "dotted") +
    geom_vline(xintercept = as.Date(zoo::as.yearqtr("2020 Q4")),
               linetype = "dotted", colour = "grey40") +
    scale_colour_manual(
      values = c("Actual" = "#1f77b4", "XGBoost" = "#d62728",
                 "Naive RW" = "#2ca02c")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title    = paste0("FICU Count Forecast | ", cv),
         subtitle = paste0("Shaded = test  |  ", sc, "  |  ", da),
         x = NULL, y = "Number of Credit Unions", colour = NULL) +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")
}

# ── Plot 2: Actual vs Predicted in YoY% space ────────────
plot_pct <- function(cv) {
  df <- plot_all[cat == cv & space == "pct"]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, dd := as.Date(date)]
  m  <- metrics_dt[categories == cv]
  sp <- if (!is.na(m$skill_pct))
    sprintf("Pct Skill=%.1f%%", m$skill_pct) else ""
  xmt <- min(df[set == "Test", dd], na.rm = TRUE)
  xmx <- max(df$dd) + 30

  ggplot(df, aes(x = dd)) +
    annotate("rect", xmin = xmt, xmax = xmx, ymin = -Inf, ymax = Inf,
             fill = "#fff3cd", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_line(aes(y = actual,    colour = "Actual"),   linewidth = 0.9) +
    geom_line(aes(y = predicted, colour = "XGBoost"),  linewidth = 0.75,
              linetype = "dashed") +
    geom_line(data = df[set == "Test"],
              aes(y = naive, colour = "Naive RW"),     linewidth = 0.6,
              linetype = "dotted") +
    geom_vline(xintercept = as.Date(zoo::as.yearqtr("2020 Q4")),
               linetype = "dotted", colour = "grey40") +
    scale_colour_manual(
      values = c("Actual" = "#1f77b4", "XGBoost" = "#d62728",
                 "Naive RW" = "#2ca02c")) +
    labs(title    = paste0("YoY % Change (model space) | ", cv),
         subtitle = paste0("Shaded = test  |  ", sp,
                           "  |  Blue=shaded test  Yellow=pct space"),
         x = NULL, y = "YoY % Change in FICU Count", colour = NULL) +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")
}

# ── Plot 3: Test count residuals ──────────────────────────
plot_res_count <- function(cv) {
  df <- plot_all[cat == cv & space == "count" & set == "Test"]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, dd := as.Date(date)]
  sd_r <- sd(df$residual, na.rm = TRUE)
  ggplot(df, aes(x = dd, y = residual)) +
    geom_hline(yintercept =  0,    colour = "grey50") +
    geom_hline(yintercept =  sd_r, linetype = "dashed", colour = "#fdae61") +
    geom_hline(yintercept = -sd_r, linetype = "dashed", colour = "#fdae61") +
    geom_col(aes(fill = residual > 0), alpha = 0.8) +
    scale_fill_manual(values = c("TRUE" = "#2171b5", "FALSE" = "#d62728"),
                      guide = "none") +
    labs(title    = paste0("Count Residuals (Actual - Predicted) | ", cv),
         subtitle = "Blue = under-predicted  |  Red = over-predicted  |  +/-1 SD dashed",
         x = NULL, y = "Count Error (CUs)") +
    theme_bw(base_size = 11)
}

# ── Plot 4: Feature importance ────────────────────────────
plot_imp <- function(cv) {
  df <- top10_dt[categories == cv]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, Feature := factor(Feature, levels = Feature[order(Gain)])]
  df[, pct := Gain / sum(Gain) * 100]
  df[, ftype := fcase(
    grepl("_lag[0-9]|_rmean|_rsd",      Feature), "Lag/Rolling",
    grepl("regime|time_idx|qtrs_from|
           cycle|inv",                  Feature), "Trend/Cycle",
    grepl("fedfunds|gs10|gs2|unrate|
           gdp|cpi|mortgage|hy_spread|
           payroll|deposit|loan|umich|
           yield|housing",              Feature), "Macro",
    default = "Other"
  )]
  ggplot(df, aes(x = Feature, y = Gain, fill = ftype)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    scale_fill_manual(
      values = c("Lag/Rolling" = "#2171b5", "Trend/Cycle" = "#238b45",
                 "Macro" = "#d94801", "Other" = "#756bb1")) +
    labs(title = paste0("Top 10 Predictors (Gain) | ", cv),
         x = NULL, y = "Gain", fill = NULL) +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")
}

# ── Plot 5: Cross-category scorecard ─────────────────────
plot_scorecard <- function() {
  dt <- metrics_dt[!is.na(test_count_rmse)]
  if (nrow(dt) == 0) return(invisible(NULL))

  # RMSE in count space
  dtl <- melt(dt[, .(categories, test_count_rmse, naive_count_rmse)],
              id.vars = "categories",
              variable.name = "model", value.name = "RMSE")
  dtl[, model := ifelse(model == "test_count_rmse", "XGBoost", "Naive RW")]

  p1 <- ggplot(dtl, aes(x = reorder(categories, RMSE),
                         y = RMSE, fill = model)) +
    geom_col(position = "dodge") + coord_flip() +
    scale_fill_manual(
      values = c("XGBoost" = "#2171b5", "Naive RW" = "#74c476")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Count RMSE: XGBoost vs Naive RW",
         x = NULL, y = "RMSE (# CUs)", fill = NULL) +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")

  # Skill score in count space
  p2 <- ggplot(dt[!is.na(skill_count)],
               aes(x = reorder(categories, skill_count),
                   y = skill_count, fill = skill_count > 0)) +
    geom_col() + geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_manual(
      values = c("TRUE" = "#2171b5", "FALSE" = "#d62728"),
      labels = c("TRUE" = "Beats naive", "FALSE" = "Worse"),
      name   = NULL) +
    coord_flip() +
    labs(title = "Count Skill Score vs Naive RW",
         x = NULL, y = "% RMSE improvement") +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")

  if (requireNamespace("gridExtra", quietly = TRUE))
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  else { print(p1); print(p2) }
}

# ── Plot 6: Directional accuracy ─────────────────────────
plot_dir_acc <- function() {
  dt <- metrics_dt[!is.na(dir_acc)]
  if (nrow(dt) == 0) return(invisible(NULL))
  ggplot(dt, aes(x = reorder(categories, dir_acc),
                 y = dir_acc, fill = dir_acc >= 50)) +
    geom_col() +
    geom_hline(yintercept = 50, linetype = "dashed", colour = "grey40") +
    geom_text(aes(label = sprintf("%.1f%%", dir_acc)),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 110),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(
      values = c("TRUE" = "#2171b5", "FALSE" = "#d62728"),
      labels = c("TRUE" = ">50% (better than coin flip)", "FALSE" = "<50%"),
      name   = NULL) +
    annotate("text", x = 0.6, y = 52, label = "50% baseline",
             colour = "grey40", size = 3.2, hjust = 0) +
    labs(title = "Directional Accuracy of Count Forecast (Test Period)",
         subtitle = "Was the quarter-over-quarter direction correct?",
         x = NULL, y = "% Correct Direction") +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")
}

# ── Plot 7: Runtime ───────────────────────────────────────
plot_timing <- function() {
  dt <- metrics_dt[!is.na(total_seconds)]
  if (nrow(dt) == 0) return(invisible(NULL))
  dt[, lbl := ifelse(
    total_seconds >= 60,
    sprintf("%dm %02ds", floor(total_seconds/60), round(total_seconds%%60)),
    sprintf("%.0fs", total_seconds))]
  ggplot(dt, aes(x = reorder(categories, total_seconds),
                 y = total_seconds / 60)) +
    geom_col(fill = "#4292c6") +
    geom_text(aes(label = lbl), hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = "Runtime per Category", x = NULL, y = "Minutes") +
    theme_bw(base_size = 11)
}

# ── Render all plots ──────────────────────────────────────
if (requireNamespace("scales", quietly = TRUE)) {
  # scales used for comma formatting in count plots
} else {
  message("  Note: install 'scales' package for comma-formatted count axes")
}

if (length(ok_cats) > 0) {
  # Summary scorecards first
  print(plot_scorecard())
  print(plot_dir_acc())
  print(plot_timing())
  # Per-category plots
  for (cc in ok_cats) {
    print(plot_count(cc))       # count space — the key deliverable
    print(plot_pct(cc))         # yoy% space — model diagnostic
    print(plot_res_count(cc))   # count residuals
    print(plot_imp(cc))         # feature importance
  }
} else {
  message("No successful categories — no plots rendered.")
}

toc()

# ════════════════════════════════════════════════════════
# FINAL SUMMARY
# ════════════════════════════════════════════════════════
tot <- as.numeric((proc.time() - SCRIPT_T0)["elapsed"])
message(strrep("=", 60))
message(sprintf("DONE [%s]  %s  |  %d/%d OK",
                if (DEBUG_MODE) "DEBUG" else "PROD",
                hms(tot), n_ok, length(results)))

if (n_ok > 0 && "skill_count" %in% names(metrics_dt)) {
  message("\n── Count Forecast Performance ───────────────────────")
  for (cc in ok_cats) {
    m <- metrics_dt[categories == cc]
    message(sprintf("  %-20s  CountRMSE=%-8.1f  Skill=%-7s  Dir=%-6s  DM_p=%s",
                    cc,
                    ifelse(!is.na(m$test_count_rmse), m$test_count_rmse, NA),
                    ifelse(!is.na(m$skill_count),
                           sprintf("%.1f%%", m$skill_count), "NA"),
                    ifelse(!is.na(m$dir_acc),
                           sprintf("%.1f%%", m$dir_acc), "NA"),
                    ifelse(!is.na(m$dm_pvalue),
                           sprintf("%.3f", m$dm_pvalue), "NA")))
  }
}
if (!is.null(pooled))
  message(sprintf("\nPooled — Count RMSE: %.1f CUs  |  YoY%% RMSE: %.4f",
                  pooled$count_rmse, pooled$pct_rmse))

if (DEBUG_MODE)
  message("\n*** Set DEBUG_MODE <- FALSE for full production run ***")
message(strrep("=", 60))

# Final ntfy
best_line <- ""
if (n_ok > 0 && "skill_count" %in% names(metrics_dt) &&
    any(!is.na(metrics_dt$skill_count))) {
  bc <- metrics_dt[!is.na(skill_count)][which.max(skill_count), categories]
  bs <- metrics_dt[!is.na(skill_count)][which.max(skill_count), skill_count]
  best_line <- sprintf("\nBest: %s (count skill %.1f%%)", bc, bs)
}

notify(
  title    = sprintf("FICU Count Forecast %s Done",
                     if (DEBUG_MODE) "[DEBUG]" else ""),
  msg      = sprintf("%d/%d OK | %s%s", n_ok, length(results),
                     hms(tot), best_line),
  priority = if (n_err > 0) "high" else "default",
  tags     = if (n_err == 0) "tada" else "warning"
)

############################################################
# END
############################################################
