############################################################
# PART 2 — XGBOOST  ficu_count forecast
#
# Strategy : model YoY% change, back-transform to counts
# Runs     : SEQUENTIAL (no parallel) — simpler & debuggable
# Runtime  : ~5 min debug / ~30-60 min production
#
# Categories confirmed:
#   1=Less than 10M  2=10M-50M   3=50M-100M  4=100M-500M
#   5=500M-1B        6=1B-10B    7=10B+
############################################################

DEBUG_MODE <- TRUE   # FALSE for full run after debug works

library(data.table); library(zoo); library(xgboost)
library(ggplot2);    library(httr)
set.seed(42)

# ── ntfy ──────────────────────────────────────────────────
NTFY_TOPIC   <- "your-unique-topic-name"  # ← change this
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
  INIT_Q <- 20; ASSESS_Q <- 4; SKIP_Q <- 8; MAX_F <- 3
  NR_MAX <- 100; ESTOP <- 10; N_ITER <- 5
} else {
  INIT_Q <- 40; ASSESS_Q <- 4; SKIP_Q <- 2; MAX_F <- 12
  NR_MAX <- 2000; ESTOP <- 40; N_ITER <- 60
}
TRAIN_END <- zoo::as.yearqtr("2020 Q4")
CORR_CUT  <- 0.97

t0_script <- proc.time()
message("=======================================================")
message(sprintf("PART 2  [%s]  %s",
                if (DEBUG_MODE) "DEBUG" else "PROD",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("=======================================================")
notify("CU Forecast Started",
       sprintf("[%s] %s", if(DEBUG_MODE)"DEBUG" else "PROD",
               format(Sys.time(), "%H:%M")), tags = "rocket")

# ════════════════════════════════════════════════════════
# 1. LOAD DATA
# ════════════════════════════════════════════════════════
message("\n[1] Loading data...")
qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)
message(sprintf("    %d rows x %d cols", nrow(qtrly), ncol(qtrly)))

# Verify required columns
stopifnot(
  "ficu_count missing" = "ficu_count" %in% names(qtrly),
  "date missing"       = "date"       %in% names(qtrly),
  "categories missing" = "categories" %in% names(qtrly)
)
message(sprintf("    ficu_count range: %d to %d",
                min(qtrly$ficu_count, na.rm=TRUE),
                max(qtrly$ficu_count, na.rm=TRUE)))
message(sprintf("    categories unique: %s",
                paste(sort(unique(qtrly$categories)), collapse=" ")))

# ════════════════════════════════════════════════════════
# 2. FIX CATEGORIES
# ════════════════════════════════════════════════════════
message("\n[2] Fixing categories...")
CAT_MAP <- c("1"="1_Less_10M", "2"="2_10M_50M",  "3"="3_50M_100M",
             "4"="4_100M_500M","5"="5_500M_1B",  "6"="6_1B_10B",
             "7"="7_10B_Plus")
qtrly[, cat_label := CAT_MAP[as.character(categories)]]
stopifnot("category mapping failed" = sum(is.na(qtrly$cat_label)) == 0)
message(sprintf("    Labels: %s",
                paste(sort(unique(qtrly$cat_label)), collapse=" | ")))

# ════════════════════════════════════════════════════════
# 3. BUILD TARGET: YoY% of ficu_count
# ════════════════════════════════════════════════════════
message("\n[3] Building YoY% target...")
setorderv(qtrly, c("cat_label", "date"))

# 4-quarter lag within each category
qtrly[, lag4_count := shift(ficu_count, 4L, type="lag"), by = cat_label]

# YoY percentage change
qtrly[, yoy_pct := fifelse(
  !is.na(lag4_count) & lag4_count > 0,
  (ficu_count - lag4_count) / lag4_count * 100,
  NA_real_
)]

r <- range(qtrly$yoy_pct, na.rm=TRUE)
message(sprintf("    yoy_pct: %d non-NA, range [%.2f%%, %.2f%%]",
                sum(!is.na(qtrly$yoy_pct)), r[1], r[2]))

# Winsorise at 1st/99th if extreme
p01 <- quantile(qtrly$yoy_pct, 0.01, na.rm=TRUE)
p99 <- quantile(qtrly$yoy_pct, 0.99, na.rm=TRUE)
n_win <- sum(!is.na(qtrly$yoy_pct) &
             (qtrly$yoy_pct < p01 | qtrly$yoy_pct > p99))
if (n_win > 0) {
  qtrly[, yoy_pct := pmax(pmin(yoy_pct, p99), p01)]
  message(sprintf("    Winsorised %d rows to [%.2f%%, %.2f%%]", n_win, p01, p99))
}

# ════════════════════════════════════════════════════════
# 4. SELECT FEATURES
# ════════════════════════════════════════════════════════
message("\n[4] Selecting features...")

# Include ONLY engineered/stationary columns
FEAT_PAT <- paste(
  "^yoy_", "^qoq_", "_lag[0-9]", "_rmean[0-9]", "_rsd[0-9]",
  "^regime_", "^time_idx$", "^qtrs_from_",
  "^fedfunds_cycle$", "^yield_curve_inv$",
  "^fedfunds$", "^gs10$", "^gs2$", "^mortgage30$",
  "^hy_spread$", "^unrate$", "^yield_curve$", "^housing_starts$",
  sep = "|"
)
all_num <- names(qtrly)[vapply(qtrly, is.numeric, logical(1))]
FEATS   <- grep(FEAT_PAT, all_num, value=TRUE, perl=TRUE)
# Remove anything that could leak target info
FEATS   <- setdiff(FEATS, c("yoy_pct", "ficu_count", "lag4_count",
                             "yoy_ficu_count", "categories"))
message(sprintf("    %d candidate features", length(FEATS)))
stopifnot("No features found" = length(FEATS) > 0)

# ════════════════════════════════════════════════════════
# 5. SPLIT INTO CATEGORIES
# ════════════════════════════════════════════════════════
qtrly  <- qtrly[date >= zoo::as.yearqtr("2002 Q1")]
cats   <- sort(unique(qtrly$cat_label))
message(sprintf("\n[5] Categories (%d):", length(cats)))
for (cc in cats) {
  d <- qtrly[cat_label == cc]
  message(sprintf("    %-18s  total:%-4d  train:%-4d  test:%-4d  yoy_ok:%-4d",
                  cc, nrow(d),
                  sum(d$date <= TRAIN_END),
                  sum(d$date >  TRAIN_END),
                  sum(!is.na(d$yoy_pct))))
}

# ════════════════════════════════════════════════════════
# 6. SEQUENTIAL XGBoost LOOP
# ════════════════════════════════════════════════════════
message(sprintf("\n[6] Running XGBoost — %d categories x %d iters [%s]",
                length(cats), N_ITER, if(DEBUG_MODE)"DEBUG" else "PROD"))
notify("Grid Search Started",
       sprintf("[%s] %d cats x %d iters (sequential)",
               if(DEBUG_MODE)"DEBUG" else "PROD",
               length(cats), N_ITER),
       tags = "hourglass_flowing_sand")

results    <- list()
all_metrics <- list()
all_plots   <- list()
all_top10   <- list()

for (cc in cats) {

  t_cat <- proc.time()
  message(sprintf("\n  ── %s ──", cc))

  # Subset this category
  dt <- qtrly[cat_label == cc & !is.na(yoy_pct)]
  message(sprintf("     rows: %d", nrow(dt)))

  if (nrow(dt) < (INIT_Q + ASSESS_Q + 5L)) {
    message(sprintf("     SKIP: too few rows (%d)", nrow(dt)))
    results[[cc]] <- list(cat=cc, err=sprintf("too few rows: %d", nrow(dt)))
    next
  }

  # ── Features present in this slice ─────────────────────
  x <- intersect(FEATS, names(dt))
  x <- x[vapply(x, function(cn) is.numeric(dt[[cn]]), logical(1))]
  message(sprintf("     features before filter: %d", length(x)))

  if (length(x) == 0) {
    results[[cc]] <- list(cat=cc, err="no features")
    next
  }

  # ── Impute NAs ──────────────────────────────────────────
  mat <- as.matrix(dt[, x, with=FALSE])
  for (j in seq_len(ncol(mat))) {
    nas <- is.na(mat[,j])
    if (any(nas)) {
      m <- median(mat[!nas,j], na.rm=TRUE)
      mat[nas,j] <- if (is.finite(m)) m else 0
    }
  }

  # ── NZV filter ──────────────────────────────────────────
  ok <- vapply(seq_len(ncol(mat)), function(j) {
    u <- length(unique(mat[,j]))
    u >= 3 && (u / nrow(mat)) > 0.03
  }, logical(1))
  mat <- mat[, ok, drop=FALSE]
  x   <- colnames(mat)
  message(sprintf("     features after NZV: %d", length(x)))

  # ── Correlation filter ───────────────────────────────────
  if (ncol(mat) >= 2) {
    cm <- suppressWarnings(cor(mat, use="pairwise.complete.obs"))
    cm[is.na(cm)] <- 0; diag(cm) <- 0
    keep <- rep(TRUE, ncol(cm))
    for (i in seq_len(ncol(cm)-1)) {
      if (!keep[i]) next
      for (j in (i+1):ncol(cm))
        if (keep[j] && abs(cm[i,j]) > CORR_CUT) keep[j] <- FALSE
    }
    mat <- mat[, keep, drop=FALSE]; x <- colnames(mat)
  }
  message(sprintf("     features after corr: %d", length(x)))

  if (length(x) == 0) {
    results[[cc]] <- list(cat=cc, err="0 features after filtering")
    next
  }

  # ── Train / test split ───────────────────────────────────
  tr <- dt$date <= TRAIN_END
  te <- dt$date >  TRAIN_END
  ntr <- sum(tr); nte <- sum(te)
  message(sprintf("     train: %d  test: %d", ntr, nte))

  if (ntr < (INIT_Q + ASSESS_Q + 2L)) {
    results[[cc]] <- list(cat=cc,
                          err=sprintf("train rows %d < %d needed",
                                      ntr, INIT_Q+ASSESS_Q+2L))
    next
  }

  X_tr <- mat[tr,]; y_tr <- dt$yoy_pct[tr]
  X_te <- mat[te,]; y_te <- dt$yoy_pct[te]
  cnt_tr  <- dt$ficu_count[tr]; cnt_te  <- dt$ficu_count[te]
  lag4_tr <- dt$lag4_count[tr]; lag4_te <- dt$lag4_count[te]

  # ── CV folds ─────────────────────────────────────────────
  folds <- list(); tf <- INIT_Q
  while ((tf + ASSESS_Q) <= ntr && length(folds) < MAX_F) {
    folds[[length(folds)+1L]] <- (tf+1L):(tf+ASSESS_Q)
    tf <- tf + SKIP_Q
  }
  message(sprintf("     CV folds: %d", length(folds)))

  if (length(folds) == 0) {
    results[[cc]] <- list(cat=cc, err="no CV folds generated")
    next
  }

  # ── Grid search ──────────────────────────────────────────
  grid <- data.frame(
    eta=sample(c(0.01,0.05,0.1),N_ITER,replace=TRUE),
    max_depth=sample(2:4,N_ITER,replace=TRUE),
    min_child_weight=sample(c(3,5,10),N_ITER,replace=TRUE),
    subsample=sample(c(0.7,0.8,0.9),N_ITER,replace=TRUE),
    colsample_bytree=sample(c(0.6,0.8,1.0),N_ITER,replace=TRUE),
    gamma=sample(c(0,0.1,0.5),N_ITER,replace=TRUE),
    lambda=sample(c(1,5,10),N_ITER,replace=TRUE),
    alpha=sample(c(0,0.5,1),N_ITER,replace=TRUE),
    stringsAsFactors=FALSE
  )

  dmat <- xgb.DMatrix(data=X_tr, label=y_tr)
  best_rmse <- Inf; best_nr <- 50L; best_p <- NULL

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
      cv <- xgb.cv(params=p, data=dmat, nrounds=NR_MAX,
                   folds=folds, early_stopping_rounds=ESTOP,
                   verbose=0, showsd=FALSE)
      list(rmse=min(cv$evaluation_log$test_rmse_mean, na.rm=TRUE),
           nr=max(cv$best_iteration, 5L))
    }, error=function(e) list(rmse=Inf, nr=50L))

    if (is.finite(sc$rmse) && sc$rmse < best_rmse) {
      best_rmse <- sc$rmse; best_nr <- sc$nr; best_p <- p
    }
  }

  # Fallback if all CV iters crashed
  if (is.null(best_p)) {
    message("     WARNING: all CV iters failed — using fallback params")
    best_p <- list(booster="gbtree", objective="reg:squarederror",
                   eval_metric="rmse", nthread=1L,
                   eta=0.05, max_depth=3, min_child_weight=5,
                   subsample=0.8, colsample_bytree=0.8,
                   gamma=0, lambda=5, alpha=0)
    best_nr <- 50L; best_rmse <- NA_real_
  }
  message(sprintf("     best CV RMSE: %s  rounds: %d",
                  if(is.finite(best_rmse)) sprintf("%.4f",best_rmse) else "NA",
                  best_nr))

  # ── Final model ──────────────────────────────────────────
  mdl        <- xgb.train(params=best_p, data=dmat,
                            nrounds=best_nr, verbose=0)
  pred_pct_tr <- predict(mdl, X_tr)
  pred_pct_te <- if (nte > 0) predict(mdl, X_te) else numeric(0)
  naive_pct   <- rep(tail(y_tr, 1L), nte)

  # ── Back-transform to count space ────────────────────────
  bt <- function(lag4, pct)
    ifelse(!is.na(lag4) & lag4 > 0,
           round(lag4 * (1 + pct/100)), NA_real_)

  pred_cnt_tr <- bt(lag4_tr, pred_pct_tr)
  pred_cnt_te <- bt(lag4_te, pred_pct_te)
  naive_cnt   <- bt(lag4_te, naive_pct)

  # ── Metrics ───────────────────────────────────────────────
  rmse_fn <- function(a,p) sqrt(mean((a-p)^2, na.rm=TRUE))
  mae_fn  <- function(a,p) mean(abs(a-p), na.rm=TRUE)

  naive_cnt_rmse <- rmse_fn(cnt_te, naive_cnt)
  xgb_cnt_rmse   <- rmse_fn(cnt_te, pred_cnt_te)
  xgb_cnt_mae    <- mae_fn(cnt_te,  pred_cnt_te)
  skill_cnt      <- if(is.finite(naive_cnt_rmse) && naive_cnt_rmse > 0)
    (1 - xgb_cnt_rmse/naive_cnt_rmse)*100 else NA_real_

  naive_pct_rmse <- rmse_fn(y_te, naive_pct)
  xgb_pct_rmse   <- rmse_fn(y_te, pred_pct_te)
  skill_pct      <- if(is.finite(naive_pct_rmse) && naive_pct_rmse > 0)
    (1 - xgb_pct_rmse/naive_pct_rmse)*100 else NA_real_

  dir_acc <- if(length(cnt_te) > 1)
    mean(sign(diff(cnt_te)) == sign(diff(pred_cnt_te)), na.rm=TRUE)*100
  else NA_real_

  message(sprintf("     count RMSE: %.1f  skill: %s  dir: %s",
                  xgb_cnt_rmse,
                  if(!is.na(skill_cnt))  sprintf("%.1f%%",skill_cnt)  else "NA",
                  if(!is.na(dir_acc)) sprintf("%.1f%%",dir_acc) else "NA"))

  # ── Store results ─────────────────────────────────────────
  cat_secs <- as.numeric((proc.time()-t_cat)["elapsed"])

  all_metrics[[cc]] <- data.table(
    categories      = cc,
    n_train         = ntr, n_test = nte,
    n_features      = length(x), n_folds = length(folds),
    best_cv_rmse    = best_rmse, best_nrounds = best_nr,
    train_pct_rmse  = rmse_fn(y_tr, pred_pct_tr),
    test_pct_rmse   = xgb_pct_rmse,
    naive_pct_rmse  = naive_pct_rmse,
    skill_pct       = skill_pct,
    test_count_rmse = xgb_cnt_rmse,
    test_count_mae  = xgb_cnt_mae,
    naive_count_rmse= naive_cnt_rmse,
    skill_count     = skill_cnt,
    dir_acc         = dir_acc,
    total_seconds   = round(cat_secs, 1)
  )

  all_plots[[cc]] <- rbind(
    data.table(cat=cc, date=dt$date[tr], set="Train", space="count",
               actual=cnt_tr, predicted=pred_cnt_tr,
               naive=NA_real_,  residual=cnt_tr-pred_cnt_tr),
    data.table(cat=cc, date=dt$date[te], set="Test", space="count",
               actual=cnt_te, predicted=pred_cnt_te,
               naive=naive_cnt, residual=cnt_te-pred_cnt_te),
    data.table(cat=cc, date=dt$date[tr], set="Train", space="pct",
               actual=y_tr, predicted=pred_pct_tr,
               naive=NA_real_,  residual=y_tr-pred_pct_tr),
    data.table(cat=cc, date=dt$date[te], set="Test", space="pct",
               actual=y_te, predicted=pred_pct_te,
               naive=naive_pct, residual=y_te-pred_pct_te)
  )

  imp <- xgb.importance(feature_names=x, model=mdl)
  all_top10[[cc]] <- data.table(categories=cc, head(imp, 10L))

  results[[cc]] <- list(cat=cc, err=NULL, model=mdl,
                         x_cols=x, metrics=all_metrics[[cc]])
  message(sprintf("     done in %.0fs", cat_secs))
}

# ════════════════════════════════════════════════════════
# 7. COMBINE & SAVE
# ════════════════════════════════════════════════════════
message("\n[7] Saving outputs...")

metrics_dt <- rbindlist(all_metrics, fill=TRUE)
plot_all   <- rbindlist(all_plots,   fill=TRUE)
top10_dt   <- rbindlist(all_top10,   fill=TRUE)

sfx <- if(DEBUG_MODE) "_debug" else "_enriched"
saveRDS(results,   paste0("xgb_results",  sfx, ".rds"))
fwrite(metrics_dt, paste0("xgb_metrics",  sfx, ".csv"))
fwrite(top10_dt,   paste0("xgb_top10",    sfx, ".csv"))

message("\n── Metrics ──────────────────────────────────────────")
print(metrics_dt[, .(categories, n_train, n_test, n_features,
                     test_count_rmse, naive_count_rmse,
                     skill_count, dir_acc,
                     test_pct_rmse, skill_pct,
                     total_seconds)])

# ════════════════════════════════════════════════════════
# 8. PLOTS
# ════════════════════════════════════════════════════════
ok_cats <- cats[sapply(results, function(r) is.null(r$err))]
message(sprintf("\n[8] Plotting %d/%d successful categories...",
                length(ok_cats), length(cats)))

# Count actual vs predicted
plot_count <- function(cv) {
  df <- plot_all[cat == cv & space == "count"]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, dd := as.Date(date)]
  m  <- metrics_dt[categories == cv]
  sc <- if(!is.na(m$skill_count)) sprintf("Skill=%.1f%%", m$skill_count) else ""
  da <- if(!is.na(m$dir_acc))     sprintf("Dir=%.1f%%", m$dir_acc)       else ""
  xmt <- min(df[set=="Test", dd], na.rm=TRUE)
  ggplot(df, aes(x=dd)) +
    annotate("rect", xmin=xmt, xmax=max(df$dd)+30,
             ymin=-Inf, ymax=Inf, fill="#deebf7", alpha=0.5) +
    geom_line(aes(y=actual,    colour="Actual"),   linewidth=0.9) +
    geom_line(aes(y=predicted, colour="XGBoost"),  linewidth=0.75, linetype="dashed") +
    geom_line(data=df[set=="Test"],
              aes(y=naive, colour="Naive RW"), linewidth=0.6, linetype="dotted") +
    geom_vline(xintercept=as.Date(zoo::as.yearqtr("2020 Q4")),
               linetype="dotted", colour="grey40") +
    scale_colour_manual(
      values=c("Actual"="#1f77b4","XGBoost"="#d62728","Naive RW"="#2ca02c")) +
    labs(title=paste0("FICU Count | ", cv),
         subtitle=paste0("Blue shaded = test  |  ", sc, "  |  ", da),
         x=NULL, y="Number of Credit Unions", colour=NULL) +
    theme_bw(base_size=11) + theme(legend.position="bottom")
}

# YoY% actual vs predicted
plot_pct <- function(cv) {
  df <- plot_all[cat == cv & space == "pct"]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, dd := as.Date(date)]
  m  <- metrics_dt[categories == cv]
  sp <- if(!is.na(m$skill_pct)) sprintf("PctSkill=%.1f%%", m$skill_pct) else ""
  xmt <- min(df[set=="Test", dd], na.rm=TRUE)
  ggplot(df, aes(x=dd)) +
    annotate("rect", xmin=xmt, xmax=max(df$dd)+30,
             ymin=-Inf, ymax=Inf, fill="#fff3cd", alpha=0.6) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
    geom_line(aes(y=actual,    colour="Actual"),   linewidth=0.9) +
    geom_line(aes(y=predicted, colour="XGBoost"),  linewidth=0.75, linetype="dashed") +
    geom_line(data=df[set=="Test"],
              aes(y=naive, colour="Naive RW"), linewidth=0.6, linetype="dotted") +
    scale_colour_manual(
      values=c("Actual"="#1f77b4","XGBoost"="#d62728","Naive RW"="#2ca02c")) +
    labs(title=paste0("YoY% (model space) | ", cv),
         subtitle=sp, x=NULL, y="YoY % Change", colour=NULL) +
    theme_bw(base_size=11) + theme(legend.position="bottom")
}

# Count residuals
plot_resid <- function(cv) {
  df <- plot_all[cat == cv & space == "count" & set == "Test"]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, dd := as.Date(date)]
  sd_r <- sd(df$residual, na.rm=TRUE)
  ggplot(df, aes(x=dd, y=residual)) +
    geom_hline(yintercept=0, colour="grey50") +
    geom_hline(yintercept= sd_r, linetype="dashed", colour="#fdae61") +
    geom_hline(yintercept=-sd_r, linetype="dashed", colour="#fdae61") +
    geom_col(aes(fill=residual>0), alpha=0.8) +
    scale_fill_manual(values=c("TRUE"="#2171b5","FALSE"="#d62728"), guide="none") +
    labs(title=paste0("Count Residuals | ", cv),
         subtitle="Blue=under  Red=over  Dashed=+/-1SD",
         x=NULL, y="Actual - Predicted (CUs)") +
    theme_bw(base_size=11)
}

# Feature importance
plot_imp <- function(cv) {
  df <- top10_dt[categories == cv]
  if (nrow(df) == 0) return(invisible(NULL))
  df[, Feature := factor(Feature, levels=Feature[order(Gain)])]
  df[, pct := Gain/sum(Gain)*100]
  df[, ftype := fcase(
    grepl("_lag[0-9]|_rmean|_rsd", Feature), "Lag/Rolling",
    grepl("regime|time_idx|qtrs_from|cycle|inv", Feature), "Trend/Cycle",
    grepl("fedfunds|gs10|gs2|unrate|gdp|cpi|mortgage|hy_spread|
           payroll|deposit|loan|umich|yield|housing", Feature), "Macro",
    default = "Other")]
  ggplot(df, aes(x=Feature, y=Gain, fill=ftype)) +
    geom_col() +
    geom_text(aes(label=sprintf("%.1f%%",pct)), hjust=-0.1, size=3.2) +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0,0.22))) +
    scale_fill_manual(values=c("Lag/Rolling"="#2171b5","Trend/Cycle"="#238b45",
                               "Macro"="#d94801","Other"="#756bb1")) +
    labs(title=paste0("Top 10 Predictors | ", cv),
         x=NULL, y="Gain", fill=NULL) +
    theme_bw(base_size=11) + theme(legend.position="bottom")
}

# Scorecard
plot_scorecard <- function() {
  dt <- metrics_dt[!is.na(test_count_rmse)]
  if (nrow(dt) == 0) return(invisible(NULL))
  p1 <- ggplot(dt, aes(x=reorder(categories, skill_count),
                        y=skill_count, fill=skill_count>0)) +
    geom_col() + geom_hline(yintercept=0, linetype="dashed") +
    coord_flip() +
    scale_fill_manual(values=c("TRUE"="#2171b5","FALSE"="#d62728"),
                      labels=c("TRUE"="Beats naive","FALSE"="Worse"), name=NULL) +
    labs(title="Count Skill Score vs Naive RW",
         x=NULL, y="% RMSE improvement") +
    theme_bw(base_size=11) + theme(legend.position="bottom")
  p2 <- ggplot(dt[!is.na(dir_acc)],
               aes(x=reorder(categories, dir_acc), y=dir_acc,
                   fill=dir_acc>=50)) +
    geom_col() + geom_hline(yintercept=50, linetype="dashed", colour="grey40") +
    coord_flip() +
    scale_fill_manual(values=c("TRUE"="#2171b5","FALSE"="#d62728"), guide="none") +
    labs(title="Directional Accuracy (test period)",
         x=NULL, y="% Correct Direction") +
    theme_bw(base_size=11)
  if (requireNamespace("gridExtra", quietly=TRUE))
    gridExtra::grid.arrange(p1, p2, ncol=2)
  else { print(p1); print(p2) }
}

if (length(ok_cats) > 0) {
  print(plot_scorecard())
  for (cc in ok_cats) {
    print(plot_count(cc))
    print(plot_pct(cc))
    print(plot_resid(cc))
    print(plot_imp(cc))
  }
} else {
  message("  No successful categories — check errors above")
}

# ════════════════════════════════════════════════════════
# 9. FINAL SUMMARY
# ════════════════════════════════════════════════════════
tot <- as.numeric((proc.time()-t0_script)["elapsed"])
n_ok  <- length(ok_cats)
n_err <- length(cats) - n_ok

message("\n=======================================================")
message(sprintf("DONE [%s]  %dh %02dm  |  %d/%d OK",
                if(DEBUG_MODE)"DEBUG" else "PROD",
                floor(tot/3600), floor((tot%%3600)/60),
                n_ok, length(cats)))

# Failed categories
if (n_err > 0) {
  message("FAILED:")
  for (cc in cats[sapply(results, function(r) !is.null(r$err))])
    message(sprintf("  %s: %s", cc, results[[cc]]$err))
}
if (DEBUG_MODE)
  message("*** Set DEBUG_MODE <- FALSE for full production run ***")
message("=======================================================")

best_line <- ""
if (n_ok > 0 && nrow(metrics_dt) > 0 && "skill_count" %in% names(metrics_dt)) {
  ok_m <- metrics_dt[!is.na(skill_count)]
  if (nrow(ok_m) > 0) {
    bc <- ok_m[which.max(skill_count), categories]
    bs <- ok_m[which.max(skill_count), skill_count]
    best_line <- sprintf("\nBest: %s (skill %.1f%%)", bc, bs)
  }
}
notify(sprintf("FICU Count %s Done", if(DEBUG_MODE)"[DEBUG]" else ""),
       sprintf("%d/%d OK | %dh%02dm%s",
               n_ok, length(cats),
               floor(tot/3600), floor((tot%%3600)/60),
               best_line),
       tags = if(n_err==0) "tada" else "warning")

############################################################
# END
############################################################
