############################################################
# PART 5 v1.0 — ENSEMBLE CALL REPORT VARIABLE IMPORTANCE
#
# Purpose  : Identify which credit union call report
#            variables matter most for CU growth using an
#            ensemble of machine learning models.
#
# Variables: CU-specific variables from Part 1 Data Prep:
#            - Exit dynamics: merger_rate, liquid_rate,
#              acquisition_rate, exit_rate, exit_roll4
#            - Size & structure: ln_assets_tot, fcu_count,
#              fiscu_count, fcu_assets, fiscu_assets
#            - Market share: share_fcu_count, share_fcu_assets, etc.
#            - Growth metrics: net_entry_rate, ld_fcu, ld_fiscu
#            - All YoY/QoQ, lag, rolling, cyclical transforms
#
# Targets  : yoy_fcu_pct, yoy_fiscu_pct (counts)
#            yoy_fcu_assets_pct, yoy_fiscu_assets_pct (assets)
#
# Ensemble : Ridge, LASSO, Elastic Net, Random Forest
# Parallel : Windows-safe via parallel::parLapply
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
})

for (pkg in c("data.table","glmnet","ranger","ggplot2","parallel")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package '%s' required. Install with: install.packages('%s')", pkg, pkg))
}

set.seed(42)
options(scipen = 999)

# ════════════════════════════════════════════════════════════
# 1. CONFIG
# ════════════════════════════════════════════════════════════
DATA_DIR   <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
RESULT_DIR <- "results_5_callreport"
PLOT_DIR   <- "plots_5_callreport"

N_CORES <- max(1L, detectCores() - 1L)

setwd(DATA_DIR)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)

message("============================================================")
message("  PART 5: Ensemble Call Report Variable Importance")
message("============================================================")
message(sprintf("  Cores available: %d (using %d)", detectCores(), N_CORES))

# ════════════════════════════════════════════════════════════
# 2. LOAD DATA
# ════════════════════════════════════════════════════════════
message("\n[1] Loading data...")

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

message(sprintf("  Panel: %s rows x %s cols",
                format(nrow(panel), big.mark=","),
                format(ncol(panel), big.mark=",")))

# ════════════════════════════════════════════════════════════
# 3. IDENTIFY CALL REPORT FEATURES
# ════════════════════════════════════════════════════════════
message("\n[2] Identifying call report features...")

all_cols <- names(panel)
num_cols <- all_cols[vapply(all_cols, function(v) is.numeric(panel[[v]]), logical(1))]

# Targets (excluded from features)
target_vars <- c("yoy_fcu_pct", "yoy_fiscu_pct",
                 "yoy_fcu_assets_pct", "yoy_fiscu_assets_pct")

# Identifiers and metadata (excluded)
id_vars <- c("date", "categories", "cat_label", "q1", "q2", "q3", "q4", "qtr")

# Macro variables (excluded — those are Part 4's domain)
macro_vars <- grep("^macro_", num_cols, value = TRUE)

# ── ENDOGENEITY EXCLUSION ────────────────────────────────
# Drop any variable that is a transformation of the same series
# being predicted. These create circular "drivers" — predicting
# growth from yesterday's growth, market share from itself, etc.
# This block ensures the importance ranking surfaces TRULY
# exogenous structural / operational drivers, not autoregressive
# echoes of the dependent variable.

# Patterns identifying any variant of CU count / asset growth
ENDOGENOUS_PATTERNS <- c(
  "^ld_fcu",             "^ld_fiscu",            # log differences = YoY growth
  "^d_fcu",              "^d_fiscu",             # first differences
  "fcu_count",           "fiscu_count",          # all count derivatives
  "fcu_assets",          "fiscu_assets",         # all asset derivatives
  "assets_tot",                                  # the total asset series itself
  "share_fcu",           "share_fiscu",          # market share = function of count/assets
  "^yoy_fcu",            "^yoy_fiscu",           # all YoY growth variants
  "^qoq_fcu",            "^qoq_fiscu",           # all QoQ growth variants
  "n_active",            "n_total",              # active/total CU counts
  "growth_rate",                                 # any growth-rate derived variable
  "ln_assets"                                    # log of total assets — proxies size which proxies growth
)

is_endogenous <- function(v) {
  vl <- tolower(v)
  any(vapply(ENDOGENOUS_PATTERNS, function(p) grepl(p, vl), logical(1)))
}

endogenous_vars <- num_cols[vapply(num_cols, is_endogenous, logical(1))]

# Everything else that's numeric = call report features
cr_feats <- setdiff(num_cols, c(target_vars, id_vars, macro_vars, endogenous_vars))

# Remove near-constant features
cr_feats <- cr_feats[vapply(cr_feats, function(v) {
  x <- panel[[v]]
  x <- x[!is.na(x)]
  if (length(x) < 10L) return(FALSE)
  sd(x) > 1e-10
}, logical(1))]

message(sprintf("  Call report features: %d", length(cr_feats)))
message(sprintf("  (Excluded: %d macro, %d targets, %d ids, %d endogenous)",
                length(macro_vars), length(target_vars),
                length(id_vars), length(endogenous_vars)))

# Show first 20 endogenous exclusions for verification
if (length(endogenous_vars) > 0) {
  message("  Endogenous variables removed (first 20):")
  for (v in head(sort(endogenous_vars), 20))
    message(sprintf("    - %s", v))
  if (length(endogenous_vars) > 20)
    message(sprintf("    ... and %d more", length(endogenous_vars) - 20))
}

# ── Thematic grouping for call report variables ──────────
# Map each feature to a human-readable theme
classify_cr_var <- function(v) {
  v_lower <- tolower(v)
  if (grepl("merger|liquid|acquis|exit", v_lower))    return("Exit Dynamics")
  if (grepl("net_entry", v_lower))                     return("Net Entry")
  if (grepl("share_", v_lower))                        return("Market Share")
  if (grepl("^ld_", v_lower))                          return("Log Differenced Growth")
  if (grepl("ln_assets", v_lower))                     return("Size (Log Assets)")
  if (grepl("fcu_assets|fiscu_assets|assets_tot", v_lower)) {
    if (grepl("yoy_|qoq_|_lag|_rmean|_rsd|_cyc|_accel", v_lower))
      return("Asset Growth Dynamics")
    return("Asset Levels")
  }
  if (grepl("fcu_count|fiscu_count|n_active|n_total", v_lower)) {
    if (grepl("yoy_|qoq_|_lag|_rmean|_rsd|_cyc|_accel", v_lower))
      return("Count Growth Dynamics")
    return("Count Levels")
  }
  if (grepl("_lag[0-9]", v_lower))                     return("Lagged Variables")
  if (grepl("_rmean|_rsd", v_lower))                   return("Rolling Statistics")
  if (grepl("_cyc$", v_lower))                         return("Cyclical Components")
  if (grepl("_accel$", v_lower))                       return("Acceleration / Momentum")
  return("Other CU Variables")
}

# ════════════════════════════════════════════════════════════
# 4. BUILD TASK LIST
# ════════════════════════════════════════════════════════════
message("\n[3] Building task grid...")

# Targets and their labels
DEP_VARS <- list(
  yoy_fcu_pct          = list(label = "FCU Count Growth",  group = "Counts"),
  yoy_fiscu_pct        = list(label = "FISCU Count Growth", group = "Counts"),
  yoy_fcu_assets_pct   = list(label = "FCU Asset Growth",  group = "Assets"),
  yoy_fiscu_assets_pct = list(label = "FISCU Asset Growth", group = "Assets")
)

tasks <- list()
for (dv in names(DEP_VARS)) {
  # Exclude the target itself and its direct derivatives from features
  # E.g., if target is yoy_fcu_pct, exclude fcu_count, fcu_count_lag4 (direct leakage)
  dv_excl <- c(dv)
  if (grepl("fcu_pct$", dv))           dv_excl <- c(dv_excl, "fcu_count", "fcu_count_lag4")
  if (grepl("fiscu_pct$", dv))         dv_excl <- c(dv_excl, "fiscu_count", "fiscu_count_lag4")
  if (grepl("fcu_assets_pct$", dv))    dv_excl <- c(dv_excl, "fcu_assets", "fcu_assets_lag4")
  if (grepl("fiscu_assets_pct$", dv))  dv_excl <- c(dv_excl, "fiscu_assets", "fiscu_assets_lag4")

  feats_dv <- setdiff(cr_feats, dv_excl)

  for (cat in sort(unique(panel$cat_label))) {
    sub <- panel[cat_label == cat & !is.na(get(dv))]
    if (nrow(sub) < 15L) next
    tasks[[length(tasks) + 1L]] <- list(
      dv       = dv,
      cat      = cat,
      dv_label = DEP_VARS[[dv]]$label,
      dv_group = DEP_VARS[[dv]]$group,
      feats    = feats_dv
    )
  }
}

n_tasks <- length(tasks)
message(sprintf("  Task grid: %d tasks (%d cores)", n_tasks, N_CORES))

# ════════════════════════════════════════════════════════════
# 5. ENSEMBLE WORKER FUNCTION
# ════════════════════════════════════════════════════════════

run_cr_ensemble <- function(task, panel) {

  suppressPackageStartupMessages({
    library(data.table)
    library(glmnet)
    library(ranger)
  })

  dv       <- task$dv
  cat_lbl  <- task$cat
  feats    <- task$feats
  dv_label <- task$dv_label
  dv_group <- task$dv_group

  dt <- panel[cat_label == cat_lbl & !is.na(get(dv))]
  if (nrow(dt) < 10L) return(NULL)

  # Available features in this subset
  avail <- intersect(feats, names(dt))
  avail <- avail[vapply(avail, function(v) {
    x <- dt[[v]]; is.numeric(x) && sum(!is.na(x)) >= nrow(dt) * 0.5
  }, logical(1))]
  if (length(avail) < 3L) return(NULL)

  y_vec <- as.numeric(dt[[dv]])
  X_mat <- as.matrix(dt[, avail, with = FALSE])
  storage.mode(X_mat) <- "double"

  complete <- complete.cases(cbind(y_vec, X_mat))
  y_vec <- y_vec[complete]
  X_mat <- X_mat[complete, , drop = FALSE]
  if (length(y_vec) < 10L || ncol(X_mat) < 3L) return(NULL)

  X_scaled <- scale(X_mat)
  X_scaled[is.nan(X_scaled)] <- 0
  n_obs <- length(y_vec)
  results <- list()

  # Ridge
  tryCatch({
    cv_fit <- cv.glmnet(X_scaled, y_vec, alpha = 0, nfolds = min(10L, n_obs - 1L))
    coefs  <- as.matrix(coef(cv_fit, s = "lambda.min"))[-1, , drop = FALSE]
    imp <- data.table(variable = rownames(coefs), importance = abs(as.numeric(coefs)), model = "Ridge")
    imp[, importance := importance / max(max(importance, na.rm=TRUE), 1e-10) * 100]
    results[["ridge"]] <- imp
  }, error = function(e) NULL)

  # LASSO
  tryCatch({
    cv_fit <- cv.glmnet(X_scaled, y_vec, alpha = 1, nfolds = min(10L, n_obs - 1L))
    coefs  <- as.matrix(coef(cv_fit, s = "lambda.min"))[-1, , drop = FALSE]
    imp <- data.table(variable = rownames(coefs), importance = abs(as.numeric(coefs)), model = "LASSO")
    imp[, importance := importance / max(max(importance, na.rm=TRUE), 1e-10) * 100]
    results[["lasso"]] <- imp
  }, error = function(e) NULL)

  # Elastic Net
  tryCatch({
    cv_fit <- cv.glmnet(X_scaled, y_vec, alpha = 0.5, nfolds = min(10L, n_obs - 1L))
    coefs  <- as.matrix(coef(cv_fit, s = "lambda.min"))[-1, , drop = FALSE]
    imp <- data.table(variable = rownames(coefs), importance = abs(as.numeric(coefs)), model = "Elastic Net")
    imp[, importance := importance / max(max(importance, na.rm=TRUE), 1e-10) * 100]
    results[["enet"]] <- imp
  }, error = function(e) NULL)

  # Random Forest
  tryCatch({
    rf_df  <- data.frame(y = y_vec, X_mat)
    rf_fit <- ranger(y ~ ., data = rf_df, num.trees = 500,
                     importance = "impurity", min.node.size = max(3L, n_obs %/% 10L))
    vimp <- rf_fit$variable.importance
    imp <- data.table(variable = names(vimp), importance = as.numeric(vimp), model = "Random Forest")
    imp[, importance := importance / max(max(importance, na.rm=TRUE), 1e-10) * 100]
    results[["rf"]] <- imp
  }, error = function(e) NULL)

  if (length(results) == 0L) return(NULL)

  all_imp <- rbindlist(results, fill = TRUE)
  all_imp[, `:=`(dep_var = dv, cat_label = cat_lbl,
                 dv_label = dv_label, dv_group = dv_group)]
  all_imp
}

# ════════════════════════════════════════════════════════════
# 6. RUN ENSEMBLE IN PARALLEL
# ════════════════════════════════════════════════════════════
message("\n[4] Running ensemble models...")
tic("CR Ensemble")

cl <- makeCluster(N_CORES)
clusterExport(cl, varlist = c("panel", "run_cr_ensemble"), envir = environment())

par_results <- tryCatch({
  res <- parLapply(cl, tasks, function(task) {
    tryCatch(run_cr_ensemble(task, panel), error = function(e) NULL)
  })
  res
}, error = function(e) {
  message(sprintf("  [PARALLEL ERROR] %s — falling back to sequential", conditionMessage(e)))
  lapply(tasks, function(task) {
    tryCatch(run_cr_ensemble(task, panel), error = function(e) NULL)
  })
})

tryCatch(stopCluster(cl), error = function(e) NULL)
message("  Cluster stopped.")
toc()

par_results <- par_results[!vapply(par_results, is.null, logical(1))]
if (length(par_results) == 0L) stop("All tasks failed. Check data.")

ensemble_raw <- rbindlist(par_results, fill = TRUE)
message(sprintf("  Raw importance rows: %s", format(nrow(ensemble_raw), big.mark = ",")))
message(sprintf("  Models completed: %d / %d tasks", length(par_results), n_tasks))

# ════════════════════════════════════════════════════════════
# 7. AGGREGATE IMPORTANCE
# ════════════════════════════════════════════════════════════
message("\n[5] Aggregating variable importance...")

# Clean display names
clean_cr_name <- function(v) {
  v <- gsub("_lag[0-9]+$", " (lagged)", v)
  v <- gsub("_rmean[0-9]+$", " (rolling avg)", v)
  v <- gsub("_rsd[0-9]+$", " (rolling vol)", v)
  v <- gsub("_cyc$", " (cyclical)", v)
  v <- gsub("_accel$", " (acceleration)", v)
  v <- gsub("^yoy_", "YoY ", v)
  v <- gsub("^qoq_", "QoQ ", v)
  v <- gsub("^share_", "Share: ", v)
  v <- gsub("_", " ", v)
  v <- paste0(toupper(substr(v, 1, 1)), substr(v, 2, nchar(v)))
  v
}

ensemble_raw[, var_clean := clean_cr_name(variable), by = variable]
ensemble_raw[, theme := vapply(variable, classify_cr_var, character(1))]

# Overall ranking
overall_imp <- ensemble_raw[, .(
  mean_importance = mean(importance, na.rm = TRUE),
  median_importance = median(importance, na.rm = TRUE),
  n_nonzero = sum(importance > 0.1, na.rm = TRUE),
  n_total = .N
), by = .(variable, var_clean, theme)]
overall_imp[, pct_active := n_nonzero / n_total * 100]
setorderv(overall_imp, "mean_importance", order = -1L)

top25 <- head(overall_imp, 25)

# By model type
by_model <- ensemble_raw[, .(mean_imp = mean(importance, na.rm=TRUE)),
                          by = .(variable, var_clean, model)]

# By target group (counts vs assets)
ensemble_raw[, target_group := fifelse(dv_group == "Counts", "CU Counts", "CU Assets")]
by_target <- ensemble_raw[, .(mean_imp = mean(importance, na.rm=TRUE)),
                           by = .(variable, var_clean, target_group)]

# By theme
theme_agg <- ensemble_raw[, .(
  mean_importance = mean(importance, na.rm = TRUE),
  n_vars = uniqueN(variable)
), by = theme]
theme_agg[, pct_importance := mean_importance / max(sum(mean_importance), 1e-10) * 100]
setorderv(theme_agg, "pct_importance", order = -1L)

message(sprintf("  Unique variables: %d", nrow(overall_imp)))
message(sprintf("  Top variable: %s (%.1f)", top25$var_clean[1], top25$mean_importance[1]))

# ════════════════════════════════════════════════════════════
# 8. SAVE RESULTS
# ════════════════════════════════════════════════════════════
message("\n[6] Saving results...")

fwrite(ensemble_raw, file.path(RESULT_DIR, "cr_ensemble_raw.csv"))
fwrite(overall_imp,  file.path(RESULT_DIR, "cr_ensemble_ranking.csv"))
fwrite(theme_agg,    file.path(RESULT_DIR, "cr_ensemble_by_theme.csv"))

# Excel
if (requireNamespace("openxlsx", quietly = TRUE)) {
  tryCatch({
    library(openxlsx)
    wb <- createWorkbook()
    addWorksheet(wb, "Overall Ranking")
    writeData(wb, "Overall Ranking",
              x = "Call Report Variable Importance — Ensemble Ranking",
              startRow = 1, startCol = 1)
    out_dt <- overall_imp[, .(Rank = .I, Variable = var_clean, Theme = theme,
                               `Avg Importance` = round(mean_importance, 1),
                               `% Models Active` = round(pct_active, 0))]
    writeData(wb, "Overall Ranking", x = head(out_dt, 40), startRow = 3,
              headerStyle = createStyle(textDecoration = "bold", fgFill = "#D9E2F3",
                                        border = "TopBottomLeftRight", halign = "center"))
    setColWidths(wb, "Overall Ranking", cols = 1:5, widths = c(6, 35, 25, 16, 16))
    addStyle(wb, "Overall Ranking",
             createStyle(fontSize = 14, textDecoration = "bold"), rows = 1, cols = 1)
    saveWorkbook(wb, file.path(RESULT_DIR, "cr_ensemble_importance.xlsx"), overwrite = TRUE)
    message("  Excel saved.")
  }, error = function(e) message(sprintf("  [EXCEL WARN] %s", conditionMessage(e))))
}

# ════════════════════════════════════════════════════════════
# 9. PUBLICATION CHARTS
# ════════════════════════════════════════════════════════════
message("\n[7] Generating publication charts...")

theme_pub <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 15, hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "#666666", hjust = 0, margin = margin(b = 12)),
    plot.caption = element_text(size = 8, color = "#999999", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.3),
    axis.text.y = element_text(size = 9.5, color = "#333333"),
    axis.text.x = element_text(size = 9, color = "#666666"),
    axis.title = element_text(size = 10, color = "#555555"),
    legend.position = "bottom",
    plot.margin = margin(20, 25, 15, 15),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

pal_navy  <- "#0B1D3A"
pal_teal  <- "#2EC4B6"
pal_coral <- "#E76F51"
pal_amber <- "#E8A838"
pal_sky   <- "#5B9BD5"
pal_green <- "#52B788"

model_colors <- c("Ridge" = pal_sky, "LASSO" = pal_coral,
                   "Elastic Net" = pal_amber, "Random Forest" = pal_green)

save_pub <- function(p, filename, w = 12, h = 8) {
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

# ── C1: Top 25 Call Report Variables ─────────────────────
message("  Chart C1: Top 25 call report variables...")
p1_data <- copy(top25)
p1_data[, var_clean := factor(var_clean, levels = rev(var_clean))]

p1 <- ggplot(p1_data, aes(x = mean_importance, y = var_clean)) +
  geom_col(aes(fill = theme), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.0f", mean_importance)),
            hjust = -0.2, size = 3, color = "#555555") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2", name = "Category") +
  labs(
    title = "Top 25 Call Report Drivers of Credit Union Growth",
    subtitle = "Average importance across Ridge, LASSO, Elastic Net, and Random Forest\nacross FCU/FISCU count and asset growth targets",
    x = "Average Importance Score", y = NULL,
    caption = "Source: NCUA Call Report variables from Part 1 feature engineering"
  ) +
  theme_pub +
  theme(legend.position = "right", legend.text = element_text(size = 8))
save_pub(p1, "C1_top25_callreport_importance.pdf", w = 14, h = 10)

# ── C2: Importance by Model Type ─────────────────────────
message("  Chart C2: By model type...")
top15_vars <- head(overall_imp$variable, 15)
p2_data <- by_model[variable %in% top15_vars]
p2_data[, var_clean := factor(var_clean,
  levels = rev(overall_imp[variable %in% top15_vars, var_clean]))]

p2 <- ggplot(p2_data, aes(x = mean_imp, y = var_clean, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
  scale_fill_manual(values = model_colors, name = "Model") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Call Report Variable Importance by ML Model",
    subtitle = "Top 15 variables — comparing all four ensemble models",
    x = "Average Importance Score", y = NULL
  ) +
  theme_pub +
  theme(legend.position = c(0.85, 0.2),
        legend.background = element_rect(fill = "white", color = "#DDD", linewidth = 0.3))
save_pub(p2, "C2_callreport_by_model.pdf", w = 13, h = 9)

# ── C3: Counts vs Assets Heatmap ─────────────────────────
message("  Chart C3: Counts vs assets heatmap...")
top15_hm <- head(overall_imp$variable, 15)
p3_data <- by_target[variable %in% top15_hm]
p3_data[, var_clean := factor(var_clean,
  levels = rev(overall_imp[variable %in% top15_hm, var_clean]))]

p3 <- ggplot(p3_data, aes(x = target_group, y = var_clean, fill = mean_imp)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.0f", mean_imp),
                color = ifelse(mean_imp > 35, "high", "low")),
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("high" = "white", "low" = "#333333")) +
  scale_fill_gradient2(low = "#F0F4F8", mid = pal_sky, high = pal_navy,
                       midpoint = 40, name = "Importance",
                       guide = guide_colorbar(barwidth = 12, barheight = 0.6)) +
  labs(
    title = "Call Report Variables — Count Models vs Asset Models",
    subtitle = "Do count growth and asset growth respond to the same CU-specific drivers?",
    x = NULL, y = NULL,
    caption = "Darker = more important  |  Ensemble average across 4 ML models"
  ) +
  theme_pub +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold"),
        legend.position = "bottom")
save_pub(p3, "C3_callreport_heatmap_target.pdf", w = 10, h = 9)

# ── C4: Importance by Theme (percentage) — Executive Version ──
message("  Chart C4: By theme (percentage)...")

# Reclassify into executive-friendly themes
exec_theme <- function(v) {
  v_lower <- tolower(v)
  if (grepl("merger|liquid|acquis|exit", v_lower))             return("Mergers, Liquidations & Exits")
  if (grepl("net_entry", v_lower))                              return("New Charter Activity")
  if (grepl("share_", v_lower))                                 return("Market Share Position")
  if (grepl("fcu_assets|fiscu_assets|assets_tot", v_lower))     return("Asset Size & Growth")
  if (grepl("fcu_count|fiscu_count|n_active|n_total", v_lower)) return("CU Count & Activity")
  if (grepl("ln_assets|ld_fcu|ld_fiscu", v_lower))             return("Size & Growth (Log Scale)")
  return("Other CU Metrics")
}

ensemble_raw[, exec_cat := vapply(variable, exec_theme, character(1))]

exec_agg <- ensemble_raw[, .(
  mean_importance = mean(importance, na.rm = TRUE),
  n_vars = uniqueN(variable)
), by = exec_cat]
exec_agg[, pct_importance := mean_importance / max(sum(mean_importance), 1e-10) * 100]
setorderv(exec_agg, "pct_importance", order = -1L)
exec_agg[, exec_cat := factor(exec_cat, levels = rev(exec_cat))]

exec_colors <- c(
  "Mergers, Liquidations & Exits" = pal_coral,
  "New Charter Activity"          = "#E67E22",
  "Market Share Position"         = pal_teal,
  "Asset Size & Growth"           = pal_sky,
  "CU Count & Activity"           = pal_green,
  "Size & Growth (Log Scale)"     = pal_navy,
  "Other CU Metrics"              = "#95A5A6"
)

p4 <- ggplot(exec_agg, aes(x = pct_importance, y = exec_cat,
                             fill = as.character(exec_cat))) +
  geom_col(width = 0.65, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%  (%d vars)", pct_importance, n_vars)),
            hjust = -0.05, size = 3.5, color = "#444444") +
  scale_fill_manual(values = exec_colors) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Which Credit Union-Specific Factors Drive Growth?",
    subtitle = "Share of total ensemble importance — call report variables grouped by category",
    x = "Share of Total Importance (%)", y = NULL,
    caption = "Percentages sum to 100%  |  Ensemble of Ridge, LASSO, Elastic Net, and Random Forest models"
  ) +
  theme_pub
save_pub(p4, "C4_callreport_by_theme.pdf", w = 12, h = 8)

# ── C5: Model Agreement Dot Plot ─────────────────────────
message("  Chart C5: Model agreement...")
top15_agree <- head(overall_imp$variable, 15)
p5_data <- ensemble_raw[variable %in% top15_agree,
  .(importance = mean(importance, na.rm = TRUE)), by = .(variable, var_clean, model)]
p5_data[, var_clean := factor(var_clean,
  levels = rev(overall_imp[variable %in% top15_agree, var_clean]))]
ens_means <- p5_data[, .(ens_mean = mean(importance, na.rm = TRUE)), by = var_clean]

p5 <- ggplot(p5_data, aes(x = importance, y = var_clean)) +
  geom_segment(data = ens_means,
               aes(x = 0, xend = ens_mean, y = var_clean, yend = var_clean),
               color = "#E0E0E0", linewidth = 0.8) +
  geom_point(aes(color = model), size = 3.5, alpha = 0.85) +
  geom_point(data = ens_means, aes(x = ens_mean, y = var_clean),
             shape = 124, size = 6, color = pal_navy) +
  scale_color_manual(values = model_colors, name = "Model") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Model Agreement on Call Report Variable Importance",
    subtitle = "Each dot = one ML model  |  Vertical bar = ensemble average",
    x = "Importance Score", y = NULL,
    caption = "Tight clustering = all models agree  |  Wide spread = model-dependent"
  ) +
  theme_pub +
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(fill = "white", color = "#DDD", linewidth = 0.3))
save_pub(p5, "C5_callreport_model_agreement.pdf", w = 12, h = 9)

# ════════════════════════════════════════════════════════════
# 10. SUMMARY
# ════════════════════════════════════════════════════════════
message("\n============================================================")
message("  CALL REPORT ENSEMBLE ANALYSIS COMPLETE")
message("============================================================")
message(sprintf("  Tasks completed : %d / %d", length(par_results), n_tasks))
message(sprintf("  ML models       : Ridge, LASSO, Elastic Net, Random Forest"))
message(sprintf("  Total imp rows  : %s", format(nrow(ensemble_raw), big.mark = ",")))
message("")
message("  Top 10 Call Report Drivers:")
for (i in 1:min(10, nrow(overall_imp))) {
  message(sprintf("    %2d. %-35s  [%s]  Imp: %.1f",
                  i, overall_imp$var_clean[i],
                  overall_imp$theme[i],
                  overall_imp$mean_importance[i]))
}
message("")
message(sprintf("  Charts: %s/", PLOT_DIR))
message("    C1 — Top 25 call report variables (bar chart)")
message("    C2 — Importance by ML model type")
message("    C3 — Count vs asset target heatmap")
message("    C4 — Importance by CU variable theme (%)")
message("    C5 — Model agreement dot plot")
message("============================================================")
