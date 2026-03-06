############################################################
# PART 4 v1.0 — ENSEMBLE MACRO VARIABLE IMPORTANCE
#
# Purpose  : Identify which macroeconomic variables matter
#            most for credit union growth using an ensemble
#            of machine learning models.
#
# Targets  : All 5 dependent variables from Parts 3a/3b/3c:
#            - yoy_fcu_pct          (FCU count growth)
#            - yoy_fiscu_pct        (FISCU count growth)
#            - yoy_fcu_assets_pct   (FCU asset growth)
#            - yoy_fiscu_assets_pct (FISCU asset growth)
#            - yoy_corp_cu_assets_pct (Corporate CU asset growth)
#
# Ensemble : 4 models per target × category:
#            1. Ridge Regression   (glmnet, alpha=0)
#            2. LASSO Regression   (glmnet, alpha=1)
#            3. Elastic Net        (glmnet, alpha=0.5)
#            4. Random Forest      (ranger)
#
# Output   : Variable importance rankings, aggregated across
#            models, categories, and targets.
#            Publication-quality charts for executives.
#
# Parallel : Windows-safe via parallel::parLapply + makeCluster
############################################################

# ════════════════════════════════════════════════════════════
# 0. PACKAGES
# ════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(glmnet)
  library(ranger)
  library(ggplot2)
  library(scales)
  library(parallel)
  library(tictoc)
  library(readxl)
})

# Check required packages
for (pkg in c("data.table","zoo","glmnet","ranger","ggplot2","parallel")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package '%s' required. Install with: install.packages('%s')", pkg, pkg))
}

set.seed(42)
options(scipen = 999)

# ════════════════════════════════════════════════════════════
# 1. CONFIG
# ════════════════════════════════════════════════════════════
DATA_DIR   <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
RESULT_DIR <- "results_4_ensemble"
PLOT_DIR   <- "plots_4_ensemble"

N_CORES <- max(1L, detectCores() - 1L)

setwd(DATA_DIR)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)

message("============================================================")
message("  PART 4: Ensemble Macro Variable Importance")
message("============================================================")
message(sprintf("  Cores available: %d (using %d)", detectCores(), N_CORES))

# ════════════════════════════════════════════════════════════
# 2. LOAD AND PREPARE DATA
# ════════════════════════════════════════════════════════════
message("\n[1] Loading data...")

# ── 2a. Load main panel (for 3a/3b targets) ─────────────
if (!file.exists("modeling_panel_v5.rds"))
  stop("modeling_panel_v5.rds not found. Run Parts 1+2 first.")

panel <- readRDS("modeling_panel_v5.rds")
setDT(panel)

# Strip haven labels
if (requireNamespace("haven", quietly = TRUE)) {
  panel <- as.data.table(haven::zap_labels(panel))
}
for (cn in names(panel)) {
  if (!is.null(attr(panel[[cn]], "label")))  attr(panel[[cn]], "label")  <- NULL
  if (!is.null(attr(panel[[cn]], "labels"))) attr(panel[[cn]], "labels") <- NULL
}

# Category labels
CAT_MAP <- c("1"="1_Less_10M","2"="2_10M_50M","3"="3_50M_100M",
             "4"="4_100M_500M","5"="5_500M_1B","6"="6_1B_10B",
             "7"="7_10B_Plus")
if (!"cat_label" %in% names(panel))
  panel[, cat_label := CAT_MAP[as.character(categories)]]

message(sprintf("  Main panel: %s rows × %s cols",
                format(nrow(panel), big.mark=","),
                format(ncol(panel), big.mark=",")))

# ── 2b. Load corporate CU data (for 3c target) ──────────
CORP_XLSX <- file.path(DATA_DIR, "Corporate_Credit_Unions.xlsx")
has_corp <- file.exists(CORP_XLSX)

if (has_corp) {
  corp_raw <- as.data.table(read_excel(CORP_XLSX))
  date_col  <- if ("date" %in% names(corp_raw)) "date" else names(corp_raw)[1L]
  asset_col <- if ("ccu_total_assets" %in% names(corp_raw)) "ccu_total_assets" else {
    ac <- grep("total.*asset|ccu.*asset", names(corp_raw), ignore.case=TRUE, value=TRUE)
    if (length(ac) > 0) ac[1L] else names(corp_raw)[2L]
  }

  # Parse dates
  raw_dates <- corp_raw[[date_col]]
  if (inherits(raw_dates, c("POSIXct","POSIXlt","Date"))) {
    corp_raw[, date_raw := as.Date(raw_dates)]
  } else {
    raw_chr <- trimws(as.character(raw_dates))
    if (all(grepl("^[0-9]+\\.?[0-9]*$", head(raw_chr[!is.na(raw_chr)], 10)))) {
      corp_raw[, date_raw := as.Date(as.numeric(raw_chr), origin = "1899-12-30")]
    } else {
      corp_raw[, date_raw := as.Date(raw_chr, format = "%m/%d/%Y")]
    }
  }
  corp_raw[, corp_cu_assets := as.numeric(get(asset_col))]
  corp_raw <- corp_raw[!is.na(date_raw) & !is.na(corp_cu_assets)]

  # Monthly to quarterly
  corp_raw[, month_num := as.integer(format(date_raw, "%m"))]
  corp_qtr <- corp_raw[month_num %in% c(3L, 6L, 9L, 12L)]
  setDT(corp_qtr)
  corp_qtr[, date := zoo::as.yearqtr(date_raw)]
  corp_qtr <- corp_qtr[, .(corp_cu_assets = tail(corp_cu_assets, 1L)), by=date]
  setDT(corp_qtr)
  corp_qtr[, corp_cu_assets_lag4 := data.table::shift(corp_cu_assets, n=4L, type="lag")]
  corp_qtr[, yoy_corp_cu_assets_pct := 100 * (corp_cu_assets - corp_cu_assets_lag4) /
                                         corp_cu_assets_lag4]

  # Merge with macro
  macro_cols <- grep("^macro_", names(panel), value=TRUE)
  macro_dt   <- unique(panel[, c("date", macro_cols), with=FALSE], by="date")
  corp_panel <- merge(corp_qtr, macro_dt, by="date", all.x=TRUE)
  setDT(corp_panel)
  corp_panel[, cat_label := "Corporate"]

  message(sprintf("  Corporate CU panel: %d rows", nrow(corp_panel)))
} else {
  message("  [NOTE] Corporate_Credit_Unions.xlsx not found — skipping 3c targets")
  corp_panel <- NULL
}

# ── 2c. Identify macro features ──────────────────────────
macro_feats <- grep("^macro_", names(panel), value=TRUE)
# Remove any that are constant or near-constant
macro_feats <- macro_feats[vapply(macro_feats, function(v) {
  x <- panel[[v]]
  if (!is.numeric(x)) return(FALSE)
  x <- x[!is.na(x)]
  if (length(x) < 10L) return(FALSE)
  sd(x) > 1e-10
}, logical(1))]

message(sprintf("  Macro features: %d", length(macro_feats)))

# ── 2d. Build task list ──────────────────────────────────
# Each task = one (target, category) combination
tasks <- list()

# 3a targets: counts
for (dv in c("yoy_fcu_pct", "yoy_fiscu_pct")) {
  for (cat in sort(unique(panel$cat_label))) {
    sub <- panel[cat_label == cat & !is.na(get(dv))]
    if (nrow(sub) < 15L) next
    tasks[[length(tasks)+1L]] <- list(
      dv       = dv,
      cat      = cat,
      part     = "3a",
      dv_label = if (dv == "yoy_fcu_pct") "FCU Count Growth" else "FISCU Count Growth"
    )
  }
}

# 3b targets: assets
for (dv in c("yoy_fcu_assets_pct", "yoy_fiscu_assets_pct")) {
  for (cat in sort(unique(panel$cat_label))) {
    sub <- panel[cat_label == cat & !is.na(get(dv))]
    if (nrow(sub) < 15L) next
    tasks[[length(tasks)+1L]] <- list(
      dv       = dv,
      cat      = cat,
      part     = "3b",
      dv_label = if (dv == "yoy_fcu_assets_pct") "FCU Asset Growth" else "FISCU Asset Growth"
    )
  }
}

# 3c target: corporate CU assets
if (!is.null(corp_panel)) {
  sub <- corp_panel[!is.na(yoy_corp_cu_assets_pct)]
  if (nrow(sub) >= 10L) {
    tasks[[length(tasks)+1L]] <- list(
      dv       = "yoy_corp_cu_assets_pct",
      cat      = "Corporate",
      part     = "3c",
      dv_label = "Corp CU Asset Growth"
    )
  }
}

n_tasks <- length(tasks)
message(sprintf("  Task grid: %d model tasks (%d cores)", n_tasks, N_CORES))

# ════════════════════════════════════════════════════════════
# 3. ENSEMBLE WORKER FUNCTION
# ════════════════════════════════════════════════════════════
# This function runs 4 ML models for one (target, category)
# and returns variable importance from each.

run_ensemble_task <- function(task, panel, corp_panel, macro_feats) {

  suppressPackageStartupMessages({
    library(data.table)
    library(glmnet)
    library(ranger)
  })

  dv       <- task$dv
  cat_lbl  <- task$cat
  part     <- task$part
  dv_label <- task$dv_label

  # Select data source
  if (part == "3c") {
    if (is.null(corp_panel)) return(NULL)
    dt <- corp_panel[!is.na(get(dv))]
  } else {
    dt <- panel[cat_label == cat_lbl & !is.na(get(dv))]
  }
  if (nrow(dt) < 10L) return(NULL)

  # Build feature matrix
  avail_feats <- intersect(macro_feats, names(dt))
  avail_feats <- avail_feats[vapply(avail_feats, function(v) {
    x <- dt[[v]]; is.numeric(x) && sum(!is.na(x)) >= nrow(dt) * 0.5
  }, logical(1))]

  if (length(avail_feats) < 3L) return(NULL)

  # Complete cases only
  y_vec <- as.numeric(dt[[dv]])
  X_mat <- as.matrix(dt[, avail_feats, with=FALSE])
  storage.mode(X_mat) <- "double"

  complete <- complete.cases(cbind(y_vec, X_mat))
  y_vec <- y_vec[complete]
  X_mat <- X_mat[complete, , drop=FALSE]

  if (length(y_vec) < 10L || ncol(X_mat) < 3L) return(NULL)

  # Scale features for regularised models
  X_scaled <- scale(X_mat)
  # Replace NaN from constant columns
  X_scaled[is.nan(X_scaled)] <- 0

  n_obs <- length(y_vec)
  results <- list()

  # ── Model 1: Ridge (alpha=0) ───────────────────────────
  tryCatch({
    cv_ridge <- cv.glmnet(X_scaled, y_vec, alpha = 0, nfolds = min(10L, n_obs - 1L))
    coefs    <- as.matrix(coef(cv_ridge, s = "lambda.min"))[-1, , drop=FALSE]
    imp_ridge <- data.table(
      variable   = rownames(coefs),
      importance = abs(as.numeric(coefs)),
      model      = "Ridge"
    )
    # Normalise to 0-100 scale
    imp_ridge[, importance := importance / max(max(importance, na.rm=TRUE), 1e-10) * 100]
    results[["ridge"]] <- imp_ridge
  }, error = function(e) NULL)

  # ── Model 2: LASSO (alpha=1) ───────────────────────────
  tryCatch({
    cv_lasso <- cv.glmnet(X_scaled, y_vec, alpha = 1, nfolds = min(10L, n_obs - 1L))
    coefs    <- as.matrix(coef(cv_lasso, s = "lambda.min"))[-1, , drop=FALSE]
    imp_lasso <- data.table(
      variable   = rownames(coefs),
      importance = abs(as.numeric(coefs)),
      model      = "LASSO"
    )
    imp_lasso[, importance := importance / max(max(importance, na.rm=TRUE), 1e-10) * 100]
    results[["lasso"]] <- imp_lasso
  }, error = function(e) NULL)

  # ── Model 3: Elastic Net (alpha=0.5) ───────────────────
  tryCatch({
    cv_enet <- cv.glmnet(X_scaled, y_vec, alpha = 0.5, nfolds = min(10L, n_obs - 1L))
    coefs   <- as.matrix(coef(cv_enet, s = "lambda.min"))[-1, , drop=FALSE]
    imp_enet <- data.table(
      variable   = rownames(coefs),
      importance = abs(as.numeric(coefs)),
      model      = "Elastic Net"
    )
    imp_enet[, importance := importance / max(max(importance, na.rm=TRUE), 1e-10) * 100]
    results[["enet"]] <- imp_enet
  }, error = function(e) NULL)

  # ── Model 4: Random Forest ─────────────────────────────
  tryCatch({
    rf_df <- data.frame(y = y_vec, X_mat)
    rf_fit <- ranger(y ~ ., data = rf_df, num.trees = 500,
                     importance = "impurity", min.node.size = max(3L, n_obs %/% 10L))
    vimp <- rf_fit$variable.importance
    imp_rf <- data.table(
      variable   = names(vimp),
      importance = as.numeric(vimp),
      model      = "Random Forest"
    )
    imp_rf[, importance := importance / max(max(importance, na.rm=TRUE), 1e-10) * 100]
    results[["rf"]] <- imp_rf
  }, error = function(e) NULL)

  if (length(results) == 0L) return(NULL)

  # Combine all models
  all_imp <- rbindlist(results, fill = TRUE)
  all_imp[, `:=`(dep_var = dv, cat_label = cat_lbl, part = part, dv_label = dv_label)]

  all_imp
}

# ════════════════════════════════════════════════════════════
# 4. RUN ENSEMBLE IN PARALLEL
# ════════════════════════════════════════════════════════════
message("\n[2] Running ensemble models...")
tic("Ensemble")

cl <- makeCluster(N_CORES)

# Export everything workers need
clusterExport(cl, varlist = c(
  "panel", "corp_panel", "macro_feats", "run_ensemble_task"
), envir = environment())

# Run with error handling + sequential fallback
par_results <- tryCatch({
  res <- parLapply(cl, tasks, function(task) {
    tryCatch(
      run_ensemble_task(task, panel, corp_panel, macro_feats),
      error = function(e) NULL
    )
  })
  res
}, error = function(e) {
  message(sprintf("  [PARALLEL ERROR] %s", conditionMessage(e)))
  message("  Falling back to sequential execution...")
  lapply(tasks, function(task) {
    tryCatch(
      run_ensemble_task(task, panel, corp_panel, macro_feats),
      error = function(e) NULL
    )
  })
})

# Always stop cluster — regardless of success or failure above
tryCatch(stopCluster(cl), error = function(e) NULL)
message("  Cluster stopped.")

toc()

# ── Consolidate results ──────────────────────────────────
par_results <- par_results[!vapply(par_results, is.null, logical(1))]
if (length(par_results) == 0L) stop("All ensemble tasks failed. Check data availability.")

ensemble_raw <- rbindlist(par_results, fill = TRUE)
message(sprintf("  Raw importance rows: %s", format(nrow(ensemble_raw), big.mark=",")))
message(sprintf("  Models completed: %d / %d tasks", length(par_results), n_tasks))

# ════════════════════════════════════════════════════════════
# 5. AGGREGATE IMPORTANCE SCORES
# ════════════════════════════════════════════════════════════
message("\n[3] Aggregating variable importance...")

# ── 5a. Clean variable names for readability ─────────────
clean_var_name <- function(v) {
  v <- gsub("^macro_", "", v)
  v <- gsub("_lag[0-9]+$", " (lagged)", v)
  v <- gsub("_rmean[0-9]+$", " (rolling avg)", v)
  v <- gsub("_rsd[0-9]+$", " (rolling vol)", v)
  v <- gsub("_cyc$", " (cyclical)", v)
  v <- gsub("_chg$", " (change)", v)
  v <- gsub("^yoy_", "YoY ", v)
  v <- gsub("^qoq_", "QoQ ", v)
  v <- gsub("_accel$", " (accel)", v)
  v <- gsub("_inv$", " (inverted)", v)
  v <- gsub("_", " ", v)
  # Capitalise first letter
  v <- paste0(toupper(substr(v, 1, 1)), substr(v, 2, nchar(v)))
  v
}

ensemble_raw[, var_clean := clean_var_name(variable), by = variable]

# ── 5b. Map variables to base economic concept ──────────
# Group transforms (lag, yoy, rolling, etc.) back to their base variable
get_base_var <- function(v) {
  v <- gsub("^macro_", "", v)
  v <- gsub("^yoy_", "", v)
  v <- gsub("^qoq_", "", v)
  v <- gsub("_lag[0-9]+$", "", v)
  v <- gsub("_rmean[0-9]+$", "", v)
  v <- gsub("_rsd[0-9]+$", "", v)
  v <- gsub("_cyc$", "", v)
  v <- gsub("_chg$", "", v)
  v <- gsub("_accel$", "", v)
  v
}

ensemble_raw[, base_var := get_base_var(variable)]

# ── 5c. Aggregate: mean importance per base variable ─────
# Across all models, categories, and targets
overall_imp <- ensemble_raw[, .(
  mean_importance = mean(importance, na.rm = TRUE),
  median_importance = median(importance, na.rm = TRUE),
  n_models_nonzero = sum(importance > 0.1, na.rm = TRUE),
  n_total_models = .N
), by = base_var]
overall_imp[, pct_models_active := n_models_nonzero / n_total_models * 100]
setorderv(overall_imp, "mean_importance", order = -1L)

# ── 5d. By model type ───────────────────────────────────
by_model <- ensemble_raw[, .(
  mean_importance = mean(importance, na.rm = TRUE)
), by = .(base_var, model)]
by_model_wide <- dcast(by_model, base_var ~ model, value.var = "mean_importance", fill = 0)

# ── 5e. By target group (counts vs assets vs corporate) ──
ensemble_raw[, target_group := fcase(
  part == "3a", "CU Counts",
  part == "3b", "CU Assets",
  part == "3c", "Corporate CU Assets",
  default = "Other"
)]

by_target <- ensemble_raw[, .(
  mean_importance = mean(importance, na.rm = TRUE)
), by = .(base_var, target_group)]

# ── 5f. Clean base var names for display ─────────────────
nice_base_name <- function(v) {
  lookup <- c(
    fedfunds = "Fed Funds Rate",
    fedfunds_chg = "Fed Funds Change",
    fedfunds_cycle = "Fed Funds Cycle",
    gs3m = "3-Month Treasury",
    gs10 = "10-Year Treasury",
    gs30 = "30-Year Treasury",
    yield_curve = "Yield Curve Slope",
    yield_curve_inv = "Yield Curve Inversion",
    spread_2s10s = "2s10s Spread",
    mortgage30 = "30-Year Mortgage Rate",
    real_rate = "Real Interest Rate",
    baa_spread = "BAA Credit Spread",
    credit_tightness = "Credit Tightening",
    unrate = "Unemployment Rate",
    disp_income = "Disposable Income",
    savings_rate = "Savings Rate",
    gdp_real = "Real GDP",
    cons_confidence = "Consumer Confidence",
    cpi = "CPI (level)",
    core_cpi = "Core CPI (level)",
    cpi_yoy = "CPI (YoY %)",
    core_cpi_yoy = "Core CPI (YoY %)",
    housing_permits = "Housing Permits",
    hpi_fed = "House Price Index",
    consumer_bankrupt = "Consumer Bankruptcies",
    cons_loan_delinq = "Consumer Loan Delinquency",
    fwd_1y1y = "1Y Forward 1Y Rate",
    fwd_1y5y = "1Y Forward 5Y Rate",
    fomc_regime = "FOMC Regime",
    hike_run = "Rate Hike Run"
  )
  ifelse(v %in% names(lookup), lookup[v], gsub("_", " ", v))
}

overall_imp[, display_name := nice_base_name(base_var)]

# Top 20 for charts
top20 <- head(overall_imp, 20)

message(sprintf("  Unique base variables: %d", nrow(overall_imp)))
message(sprintf("  Top variable: %s (avg importance: %.1f)",
                top20$display_name[1], top20$mean_importance[1]))

# ════════════════════════════════════════════════════════════
# 6. SAVE RESULTS
# ════════════════════════════════════════════════════════════
message("\n[4] Saving results...")

fwrite(ensemble_raw, file.path(RESULT_DIR, "ensemble_raw_importance.csv"))
fwrite(overall_imp,  file.path(RESULT_DIR, "ensemble_overall_ranking.csv"))
fwrite(by_model_wide, file.path(RESULT_DIR, "ensemble_by_model.csv"))
fwrite(by_target,    file.path(RESULT_DIR, "ensemble_by_target.csv"))

# Excel export
if (requireNamespace("openxlsx", quietly = TRUE)) {
  tryCatch({
    library(openxlsx)
    wb <- createWorkbook()

    # Sheet 1: Overall ranking
    addWorksheet(wb, "Overall Ranking")
    writeData(wb, "Overall Ranking", x = "Macro Variable Importance — Ensemble Ranking",
              startRow = 1, startCol = 1)
    writeData(wb, "Overall Ranking",
              x = "Average importance score across Ridge, LASSO, Elastic Net, and Random Forest models",
              startRow = 2, startCol = 1)
    out_dt <- overall_imp[, .(Rank = .I, Variable = display_name,
                               `Avg Importance` = round(mean_importance, 1),
                               `Median Importance` = round(median_importance, 1),
                               `% Models Active` = round(pct_models_active, 0))]
    writeData(wb, "Overall Ranking", x = out_dt, startRow = 4,
              headerStyle = createStyle(textDecoration = "bold", fgFill = "#D9E2F3",
                                        border = "TopBottomLeftRight", halign = "center"))
    setColWidths(wb, "Overall Ranking", cols = 1:5, widths = c(6, 30, 16, 18, 16))
    addStyle(wb, "Overall Ranking",
             createStyle(fontSize = 14, textDecoration = "bold"), rows = 1, cols = 1)
    addStyle(wb, "Overall Ranking",
             createStyle(fontSize = 10, textDecoration = "italic", fontColour = "#666666"),
             rows = 2, cols = 1)

    # Sheet 2: By model type
    addWorksheet(wb, "By Model Type")
    writeData(wb, "By Model Type", x = "Importance by ML Model Type", startRow = 1, startCol = 1)
    model_out <- merge(by_model_wide, overall_imp[, .(base_var, display_name)], by = "base_var")
    model_out[, base_var := NULL]
    setcolorder(model_out, c("display_name"))
    setnames(model_out, "display_name", "Variable")
    setorderv(model_out, names(model_out)[2], order = -1L)
    writeData(wb, "By Model Type", x = head(model_out, 25), startRow = 3,
              headerStyle = createStyle(textDecoration = "bold", fgFill = "#E8F5E9",
                                        border = "TopBottomLeftRight"))
    setColWidths(wb, "By Model Type", cols = 1:ncol(model_out),
                 widths = c(28, rep(14, ncol(model_out)-1)))

    xlsx_path <- file.path(RESULT_DIR, "ensemble_macro_importance.xlsx")
    saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    message(sprintf("  Excel saved: %s", xlsx_path))
  }, error = function(e) {
    message(sprintf("  [EXCEL WARN] %s", conditionMessage(e)))
  })
}

message("  CSVs saved to ", RESULT_DIR)

# ════════════════════════════════════════════════════════════
# 7. PUBLICATION-QUALITY CHARTS
# ════════════════════════════════════════════════════════════
message("\n[5] Generating publication charts...")

# ── Theme ────────────────────────────────────────────────
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    text             = element_text(family = "sans"),
    plot.title       = element_text(face = "bold", size = 15, hjust = 0,
                                     margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 10, color = "#666666", hjust = 0,
                                     margin = margin(b = 12)),
    plot.caption     = element_text(size = 8, color = "#999999", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.3),
    axis.text.y      = element_text(size = 10, color = "#333333"),
    axis.text.x      = element_text(size = 9, color = "#666666"),
    axis.title       = element_text(size = 10, color = "#555555"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 10, face = "bold"),
    plot.margin      = margin(20, 25, 15, 15),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ── Color palette ────────────────────────────────────────
pal_navy  <- "#0B1D3A"
pal_teal  <- "#2EC4B6"
pal_coral <- "#E76F51"
pal_amber <- "#E8A838"
pal_sky   <- "#5B9BD5"
pal_green <- "#52B788"
pal_slate <- "#64748B"

model_colors <- c(
  "Ridge"         = pal_sky,
  "LASSO"         = pal_coral,
  "Elastic Net"   = pal_amber,
  "Random Forest" = pal_green
)

target_colors <- c(
  "CU Counts"           = pal_sky,
  "CU Assets"           = pal_teal,
  "Corporate CU Assets" = pal_amber
)

save_pub_plot <- function(p, filename, w = 12, h = 8) {
  path <- file.path(PLOT_DIR, filename)
  tryCatch({
    dev <- tryCatch(
      { grDevices::cairo_pdf(path, width = w, height = h); "cairo" },
      error = function(e) { grDevices::pdf(path, width = w, height = h); "pdf" })
    print(p)
    grDevices::dev.off()
    message(sprintf("  Saved: %s [%s]", filename, dev))
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    message(sprintf("  [WARN] %s failed: %s", filename, conditionMessage(e)))
  })
}

# ── CHART 1: Top 20 Overall Importance (horizontal bar) ──
message("  Chart 1: Overall importance ranking...")
p1_data <- copy(top20)
p1_data[, display_name := factor(display_name, levels = rev(display_name))]

p1 <- ggplot(p1_data, aes(x = mean_importance, y = display_name)) +
  geom_col(fill = pal_navy, width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.0f", mean_importance)),
            hjust = -0.2, size = 3.2, color = "#555555") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Top 20 Macroeconomic Drivers of Credit Union Growth",
    subtitle = "Average importance score across Ridge, LASSO, Elastic Net, and Random Forest models\nacross all CU count, asset, and corporate CU targets",
    x = "Average Importance Score", y = NULL,
    caption  = "Source: NCUA Call Reports & Federal Reserve Economic Data  |  Ensemble of 4 ML models"
  ) +
  theme_pub
save_pub_plot(p1, "E1_top20_overall_importance.pdf", w = 12, h = 9)

# ── CHART 2: Importance by Model Type (grouped bars) ─────
message("  Chart 2: Importance by model type...")
top15_vars <- head(overall_imp$base_var, 15)
p2_data <- ensemble_raw[base_var %in% top15_vars,
  .(mean_imp = mean(importance, na.rm=TRUE)),
  by = .(base_var, model)]
p2_data <- merge(p2_data, overall_imp[, .(base_var, display_name)], by = "base_var")
# Order by overall importance
p2_data[, display_name := factor(display_name,
  levels = rev(overall_imp[base_var %in% top15_vars, display_name]))]

p2 <- ggplot(p2_data, aes(x = mean_imp, y = display_name, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
  scale_fill_manual(values = model_colors, name = "Model") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Macro Variable Importance by ML Model Type",
    subtitle = "Top 15 variables — comparing Ridge, LASSO, Elastic Net, and Random Forest",
    x = "Average Importance Score", y = NULL,
    caption  = "Importance normalised to 0–100 within each model before averaging"
  ) +
  theme_pub +
  theme(legend.position = c(0.85, 0.2),
        legend.background = element_rect(fill = "white", color = "#DDDDDD", linewidth = 0.3))
save_pub_plot(p2, "E2_importance_by_model.pdf", w = 13, h = 9)

# ── CHART 3: Importance by Target Group (heatmap) ────────
message("  Chart 3: Heatmap by target group...")
top15_for_hm <- head(overall_imp$base_var, 15)
p3_data <- by_target[base_var %in% top15_for_hm]
p3_data <- merge(p3_data, overall_imp[, .(base_var, display_name)], by = "base_var")
p3_data[, display_name := factor(display_name,
  levels = rev(overall_imp[base_var %in% top15_for_hm, display_name]))]

p3 <- ggplot(p3_data, aes(x = target_group, y = display_name, fill = mean_importance)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.0f", mean_importance),
                color = ifelse(mean_importance > 35, "high", "low")),
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("high" = "white", "low" = "#333333")) +
  scale_fill_gradient2(low = "#F0F4F8", mid = pal_sky, high = pal_navy,
                       midpoint = 40, name = "Importance",
                       guide = guide_colorbar(barwidth = 12, barheight = 0.6)) +
  labs(
    title    = "Which Macro Variables Matter Most — By Forecast Target",
    subtitle = "Comparing importance across CU count models, asset models, and corporate CU models",
    x = NULL, y = NULL,
    caption  = "Darker = more important  |  Ensemble average across Ridge, LASSO, Elastic Net, Random Forest"
  ) +
  theme_pub +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold"),
        legend.position = "bottom")
save_pub_plot(p3, "E3_heatmap_by_target.pdf", w = 10, h = 9)

# ── CHART 4: Model Agreement (dot plot) ──────────────────
message("  Chart 4: Model agreement dot plot...")
top15_agree <- head(overall_imp$base_var, 15)
p4_data <- ensemble_raw[base_var %in% top15_agree,
  .(importance = mean(importance, na.rm=TRUE)),
  by = .(base_var, model)]
p4_data <- merge(p4_data, overall_imp[, .(base_var, display_name)], by = "base_var")
p4_data[, display_name := factor(display_name,
  levels = rev(overall_imp[base_var %in% top15_agree, display_name]))]

# Ensemble mean for reference line
ens_means <- p4_data[, .(ens_mean = mean(importance, na.rm=TRUE)), by = display_name]

p4 <- ggplot(p4_data, aes(x = importance, y = display_name)) +
  geom_segment(data = ens_means,
               aes(x = 0, xend = ens_mean, y = display_name, yend = display_name),
               color = "#E0E0E0", linewidth = 0.8) +
  geom_point(aes(color = model), size = 3.5, alpha = 0.85) +
  geom_point(data = ens_means, aes(x = ens_mean, y = display_name),
             shape = 124, size = 6, color = pal_navy) +
  scale_color_manual(values = model_colors, name = "Model") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Model Agreement on Macro Variable Importance",
    subtitle = "Each dot = one ML model's score  |  Vertical bar = ensemble average\nWhen dots cluster together, all models agree the variable matters",
    x = "Importance Score", y = NULL,
    caption  = "Tight clustering = high agreement across models  |  Wide spread = model-dependent"
  ) +
  theme_pub +
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(fill = "white", color = "#DDDDDD", linewidth = 0.3))
save_pub_plot(p4, "E4_model_agreement.pdf", w = 12, h = 9)

# ── CHART 5: Economic Theme Grouping ─────────────────────
message("  Chart 5: Importance by economic theme...")

# Map base vars to themes
theme_map <- data.table(
  base_var = c("fedfunds","fedfunds_chg","fedfunds_cycle","gs3m","gs10","gs30",
               "yield_curve","yield_curve_inv","spread_2s10s","mortgage30","real_rate",
               "fwd_1y1y","fwd_1y5y","fomc_regime","hike_run",
               "baa_spread","credit_tightness",
               "unrate","disp_income","savings_rate",
               "gdp_real","cons_confidence",
               "cpi","core_cpi","cpi_yoy","core_cpi_yoy",
               "housing_permits","hpi_fed",
               "consumer_bankrupt","cons_loan_delinq"),
  theme    = c(rep("Interest Rates & Monetary Policy", 11),
               rep("Forward Rates & FOMC", 4),
               rep("Credit Conditions", 2),
               rep("Labour & Income", 3),
               rep("Economic Activity", 2),
               rep("Inflation", 4),
               rep("Housing", 2),
               rep("Consumer Credit Quality", 2))
)

theme_imp <- merge(ensemble_raw, theme_map, by = "base_var", all.x = TRUE)
theme_imp[is.na(theme), theme := "Other"]

theme_agg <- theme_imp[, .(
  mean_importance = mean(importance, na.rm = TRUE),
  n_vars = uniqueN(base_var)
), by = theme]
# Convert to share of total (percentage)
theme_agg[, pct_importance := mean_importance / sum(mean_importance) * 100]
setorderv(theme_agg, "pct_importance", order = -1L)
theme_agg[, theme := factor(theme, levels = rev(theme))]

theme_colors <- c(
  "Interest Rates & Monetary Policy" = pal_navy,
  "Forward Rates & FOMC"             = pal_slate,
  "Credit Conditions"                = pal_coral,
  "Labour & Income"                  = pal_teal,
  "Economic Activity"                = pal_green,
  "Inflation"                        = pal_amber,
  "Housing"                          = pal_sky,
  "Consumer Credit Quality"          = "#9B59B6",
  "Other"                            = "#CCCCCC"
)

p5 <- ggplot(theme_agg, aes(x = pct_importance, y = theme, fill = as.character(theme))) +
  geom_col(width = 0.7, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%  (%d vars)", pct_importance, n_vars)),
            hjust = -0.05, size = 3.3, color = "#444444") +
  scale_fill_manual(values = theme_colors) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Which Economic Themes Drive Credit Union Growth?",
    subtitle = "Share of total ensemble importance — macro variables grouped by economic category",
    x = "Share of Total Importance (%)", y = NULL,
    caption  = "Percentages sum to 100%  |  Number in parentheses = count of individual variables in each theme"
  ) +
  theme_pub
save_pub_plot(p5, "E5_importance_by_theme.pdf", w = 12, h = 7)

# ════════════════════════════════════════════════════════════
# 8. SUMMARY
# ════════════════════════════════════════════════════════════
message("\n============================================================")
message("  ENSEMBLE ANALYSIS COMPLETE")
message("============================================================")
message(sprintf("  Tasks completed : %d / %d", length(par_results), n_tasks))
message(sprintf("  ML models used  : Ridge, LASSO, Elastic Net, Random Forest"))
message(sprintf("  Total imp rows  : %s", format(nrow(ensemble_raw), big.mark=",")))
message(sprintf("  Output folder   : %s", RESULT_DIR))
message(sprintf("  Charts folder   : %s", PLOT_DIR))
message("")
message("  Top 10 Macro Drivers:")
for (i in 1:min(10, nrow(overall_imp))) {
  message(sprintf("    %2d. %-30s  Importance: %.1f  (%d%% models active)",
                  i, overall_imp$display_name[i],
                  overall_imp$mean_importance[i],
                  round(overall_imp$pct_models_active[i])))
}
message("")
message(sprintf("  Charts saved to: %s/", PLOT_DIR))
message("    E1 — Top 20 overall importance (bar chart)")
message("    E2 — Importance by ML model type (grouped bars)")
message("    E3 — Heatmap by forecast target (counts vs assets vs corporate)")
message("    E4 — Model agreement (dot plot)")
message("    E5 — Importance by economic theme (category bars)")
message("============================================================")
