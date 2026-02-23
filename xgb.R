############################################################
# XGBOOST PER-CATEGORY PIPELINE (Quarterly panel)
# Target: yoy_ficu_count
# Steps:
#  (1) Extensive grid search w/ time-series CV per category
#  (2) Drop zero / near-zero variance + multicollinearity
#  (3) Parallel across categories
#  (4) Top-10 variables per category
#  (5) Rolling-origin (expanding) CV
#  (6) Train <= 2020Q4, predict 2021Q1–2025Q3, compare
#  (7) Visual graphs
############################################################

# -----------------------------
# 0) Packages
# -----------------------------
pkgs <- c(
  "data.table", "dplyr", "zoo",
  "caret", "xgboost",
  "foreach", "doParallel",
  "ggplot2"
)
invisible(lapply(pkgs, require, character.only = TRUE))

# -----------------------------
# 1) Helper: prep predictors
#    - numeric only
#    - drop NZV
#    - drop highly correlated
# -----------------------------
prep_xy <- function(df_cat,
                    target = "yoy_ficu_count",
                    drop_cols = c("date", "categories"),
                    corr_cut = 0.95) {
  # keep only non-missing target
  df_cat <- df_cat[!is.na(df_cat[[target]]), , drop = FALSE]
  
  # numeric predictors only (exclude target + id cols)
  num_cols <- names(df_cat)[sapply(df_cat, is.numeric)]
  x_cols <- setdiff(num_cols, c(target, drop_cols))
  
  # drop near-zero variance predictors
  if (length(x_cols) > 0) {
    nzv_idx <- caret::nearZeroVar(df_cat[, x_cols, drop = FALSE])
    if (length(nzv_idx) > 0) x_cols <- x_cols[-nzv_idx]
  }
  
  # drop highly correlated predictors
  if (length(x_cols) >= 2) {
    cmat <- cor(df_cat[, x_cols, drop = FALSE], use = "pairwise.complete.obs")
    bad <- caret::findCorrelation(cmat, cutoff = corr_cut, names = TRUE, exact = TRUE)
    x_cols <- setdiff(x_cols, bad)
  }
  
  list(data = df_cat, x_cols = x_cols)
}

# -----------------------------
# 2) Helper: rolling-origin folds (indices)
#    expanding window
# -----------------------------
make_index_folds <- function(n, initial = 40, assess = 4, skip = 1) {
  # Train: [1..t], Test: [t+1..t+assess]
  folds <- list()
  t <- initial
  k <- 1
  while ((t + assess) <= n) {
    folds[[k]] <- (t + 1):(t + assess)
    k <- k + 1
    t <- t + skip
  }
  folds
}

# -----------------------------
# 3) Helper: score one param set via xgb.cv
# -----------------------------
score_params_ts <- function(df_train, x_cols, target,
                            fold_list,
                            params,
                            nrounds_max = 3000,
                            early_stop = 50,
                            verbose = 0) {
  dtrain <- xgb.DMatrix(
    data  = as.matrix(df_train[, x_cols, drop = FALSE]),
    label = df_train[[target]]
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = nrounds_max,
    folds = fold_list,
    metrics = "rmse",
    early_stopping_rounds = early_stop,
    verbose = verbose
  )
  
  list(
    best_rmse = min(cv$evaluation_log$test_rmse_mean),
    best_nrounds = cv$best_iteration
  )
}

# -----------------------------
# 4) Helper: extended grid
#    (adjust size if too slow)
# -----------------------------
make_param_grid <- function() {
  expand.grid(
    eta = c(0.01, 0.03, 0.05, 0.1),
    max_depth = c(2, 3, 4, 6, 8),
    min_child_weight = c(1, 3, 5, 10),
    subsample = c(0.6, 0.8, 1.0),
    colsample_bytree = c(0.5, 0.7, 0.9, 1.0),
    gamma = c(0, 0.5, 1, 2),
    lambda = c(1, 5, 10),
    alpha = c(0, 0.5, 1),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# 5) Main: run per category
# -----------------------------
run_xgb_for_category <- function(qtrly, cat_value,
                                 target = "yoy_ficu_count",
                                 date_col = "date",
                                 train_end = "2020 Q4",
                                 corr_cut = 0.95,
                                 initial = 40, assess = 4, skip = 1,
                                 grid = NULL,
                                 verbose = 0) {
  
  # Subset category + sort by date
  df <- qtrly %>%
    dplyr::filter(categories == cat_value) %>%
    dplyr::arrange(.data[[date_col]]) %>%
    dplyr::ungroup()
  
  # Ensure date is yearqtr for comparisons
  if (!inherits(df[[date_col]], "yearqtr")) {
    # If df$date is Date, this will still work; if it's already yearqtr it's unchanged
    df[[date_col]] <- zoo::as.yearqtr(df[[date_col]])
  }
  
  # Prep predictors
  prep <- prep_xy(df, target = target, drop_cols = c(date_col, "categories"), corr_cut = corr_cut)
  df <- prep$data
  x_cols <- prep$x_cols
  
  # If no predictors survive, exit gracefully
  if (length(x_cols) == 0) {
    return(list(
      category = cat_value,
      error = "No usable predictors after NZV/correlation filtering."
    ))
  }
  
  # Split train/test by date
  train_end_q <- zoo::as.yearqtr(train_end)
  train_idx <- which(df[[date_col]] <= train_end_q)
  test_idx  <- which(df[[date_col]] >  train_end_q)
  
  df_train <- df[train_idx, , drop = FALSE]
  df_test  <- df[test_idx,  , drop = FALSE]
  
  # Need enough rows for folds
  if (nrow(df_train) < (initial + assess + 1)) {
    return(list(
      category = cat_value,
      error = paste0("Not enough training rows for TS CV. n_train=", nrow(df_train))
    ))
  }
  
  fold_list <- make_index_folds(n = nrow(df_train), initial = initial, assess = assess, skip = skip)
  
  # Parameter grid
  if (is.null(grid)) grid <- make_param_grid()
  
  # Grid search
  best_rmse <- Inf
  best_params <- NULL
  best_nrounds <- NULL
  
  for (i in seq_len(nrow(grid))) {
    params <- list(
      booster = "gbtree",
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = grid$eta[i],
      max_depth = grid$max_depth[i],
      min_child_weight = grid$min_child_weight[i],
      subsample = grid$subsample[i],
      colsample_bytree = grid$colsample_bytree[i],
      gamma = grid$gamma[i],
      lambda = grid$lambda[i],
      alpha = grid$alpha[i]
    )
    
    sc <- score_params_ts(
      df_train = df_train,
      x_cols = x_cols,
      target = target,
      fold_list = fold_list,
      params = params,
      nrounds_max = 3000,
      early_stop = 50,
      verbose = verbose
    )
    
    if (sc$best_rmse < best_rmse) {
      best_rmse <- sc$best_rmse
      best_params <- params
      best_nrounds <- sc$best_nrounds
    }
  }
  
  # Fit final model on all train
  dtrain <- xgb.DMatrix(as.matrix(df_train[, x_cols, drop = FALSE]), label = df_train[[target]])
  final_model <- xgb.train(
    params = best_params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  # Predict
  pred_train <- predict(final_model, as.matrix(df_train[, x_cols, drop = FALSE]))
  pred_test  <- if (nrow(df_test) > 0) predict(final_model, as.matrix(df_test[, x_cols, drop = FALSE])) else numeric(0)
  
  # Importance + top 10
  imp <- xgb.importance(feature_names = x_cols, model = final_model)
  top10 <- head(imp, 10)
  
  # Build plot DF
  plot_train <- data.frame(
    categories = cat_value,
    date = df_train[[date_col]],
    set = "Train",
    actual = df_train[[target]],
    predicted = pred_train
  )
  plot_test <- data.frame(
    categories = cat_value,
    date = df_test[[date_col]],
    set = "Test",
    actual = if (nrow(df_test) > 0) df_test[[target]] else numeric(0),
    predicted = pred_test
  )
  plot_df <- rbind(plot_train, plot_test)
  
  # Metrics on test
  test_rmse <- if (nrow(df_test) > 0) sqrt(mean((plot_test$actual - plot_test$predicted)^2, na.rm = TRUE)) else NA_real_
  test_mae  <- if (nrow(df_test) > 0) mean(abs(plot_test$actual - plot_test$predicted), na.rm = TRUE) else NA_real_
  
  list(
    category = cat_value,
    x_cols = x_cols,
    best_params = best_params,
    best_cv_rmse = best_rmse,
    best_nrounds = best_nrounds,
    model = final_model,
    importance = imp,
    top10 = top10,
    plot_df = plot_df,
    test_rmse = test_rmse,
    test_mae = test_mae
  )
}

# -----------------------------
# 6) Run in parallel across categories
# -----------------------------
# Make sure qtrly$date is sortable quarterly. If it's Date, OK. If it's yearqtr, OK.
# If it's year+quarter separate, convert before this script.

cats <- sort(unique(qtrly$categories))
grid <- make_param_grid()

n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

results <- foreach::foreach(
  cc = cats,
  .packages = c("dplyr", "zoo", "caret", "xgboost", "data.table"),
  .export = c("prep_xy", "make_index_folds", "score_params_ts",
              "make_param_grid", "run_xgb_for_category")
) %dopar% {
  run_xgb_for_category(
    qtrly = qtrly,
    cat_value = cc,
    target = "yoy_ficu_count",
    date_col = "date",
    train_end = "2020 Q4",
    corr_cut = 0.95,
    initial = 40, assess = 4, skip = 1,
    grid = grid,
    verbose = 0
  )
}

parallel::stopCluster(cl)

# -----------------------------
# 7) Collect outputs
# -----------------------------
# A) Top 10 drivers per category
top10_by_cat <- data.table::rbindlist(
  lapply(results, function(r) {
    if (!is.null(r$error) || is.null(r$top10) || nrow(r$top10) == 0) return(NULL)
    data.frame(categories = r$category, r$top10)
  }),
  fill = TRUE
)

# B) Metrics per category
metrics_by_cat <- data.table::rbindlist(
  lapply(results, function(r) {
    data.frame(
      categories = r$category,
      best_cv_rmse = if (!is.null(r$best_cv_rmse)) r$best_cv_rmse else NA_real_,
      best_nrounds = if (!is.null(r$best_nrounds)) r$best_nrounds else NA_real_,
      test_rmse = if (!is.null(r$test_rmse)) r$test_rmse else NA_real_,
      test_mae  = if (!is.null(r$test_mae))  r$test_mae  else NA_real_,
      error = if (!is.null(r$error)) r$error else NA_character_
    )
  }),
  fill = TRUE
)

# C) Combined actual vs predicted
plot_all <- data.table::rbindlist(
  lapply(results, function(r) {
    if (!is.null(r$error) || is.null(r$plot_df)) return(NULL)
    r$plot_df
  }),
  fill = TRUE
)

# -----------------------------
# 8) Plots
# -----------------------------
plot_actual_vs_pred <- function(plot_df, cat_value) {
  df <- plot_df %>% dplyr::filter(categories == cat_value)
  ggplot2::ggplot(df, ggplot2::aes(x = as.Date(date), y = actual)) +
    ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes(y = predicted), linetype = "dashed") +
    ggplot2::facet_wrap(~ set, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Actual vs Predicted (", cat_value, ")"),
      x = "Date",
      y = "yoy_ficu_count"
    )
}

plot_top10_importance <- function(top10_df, cat_value) {
  df <- top10_df %>%
    dplyr::filter(categories == cat_value) %>%
    dplyr::arrange(Gain) %>%
    dplyr::slice_tail(n = 10)
  
  ggplot2::ggplot(df, ggplot2::aes(x = reorder(Feature, Gain), y = Gain)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0("Top 10 Drivers by Gain (", cat_value, ")"),
      x = NULL, y = "Gain"
    )
}

# Example usage:
# 1) See metrics
print(metrics_by_cat)

# 2) See top 10 drivers table
print(top10_by_cat)

# 3) Plot one category (replace with your category value)
# cat_example <- cats[1]
# print(plot_actual_vs_pred(plot_all, cat_example))
# print(plot_top10_importance(top10_by_cat, cat_example))

############################################################
# END
############################################################