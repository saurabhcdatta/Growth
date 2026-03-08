############################################################
# PART 6 v1.0 — UNSUPERVISED ANALYSIS
#
# Purpose  : Extract deeper structural insights from the
#            CU growth data using unsupervised methods:
#
#   Analysis 1: PCA on Macro Variables
#     → How many independent economic forces drive CU growth?
#     → Which variables load together on each component?
#
#   Analysis 2: Asset-Category Clustering
#     → Which of the 7 NCUA size groups behave similarly?
#     → Should any be combined for forecasting?
#
#   Analysis 3: Growth Regime Detection
#     → When did CU growth shift between distinct regimes?
#     → Can we label periods (normal, crisis, recovery)?
#
#   Analysis 4: Variable Correlation Clustering
#     → Which features co-move and represent the same force?
#
# Runtime : Fast (~1-2 min, no parallel needed)
############################################################

# ════════════════════════════════════════════════════════════
# 0. PACKAGES
# ════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(ggplot2)
  library(scales)
  library(tictoc)
  library(stats)       # prcomp, kmeans, hclust
})

set.seed(42)
options(scipen = 999)

# ════════════════════════════════════════════════════════════
# 1. CONFIG
# ════════════════════════════════════════════════════════════
DATA_DIR   <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
RESULT_DIR <- "results_6_unsupervised"
PLOT_DIR   <- "plots_6_unsupervised"

setwd(DATA_DIR)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)

message("============================================================")
message("  PART 6: Unsupervised Analysis")
message("  PCA · Category Clustering · Regime Detection")
message("============================================================")

# ════════════════════════════════════════════════════════════
# 2. LOAD DATA
# ════════════════════════════════════════════════════════════
message("\n[1] Loading data...")

panel <- readRDS("modeling_panel_v5.rds")
setDT(panel)
if (requireNamespace("haven", quietly = TRUE))
  panel <- as.data.table(haven::zap_labels(panel))
for (cn in names(panel)) {
  if (!is.null(attr(panel[[cn]], "label")))  attr(panel[[cn]], "label")  <- NULL
  if (!is.null(attr(panel[[cn]], "labels"))) attr(panel[[cn]], "labels") <- NULL
}

CAT_MAP <- c("1"="1_Less_10M","2"="2_10M_50M","3"="3_50M_100M",
             "4"="4_100M_500M","5"="5_500M_1B","6"="6_1B_10B",
             "7"="7_10B_Plus")
if (!"cat_label" %in% names(panel))
  panel[, cat_label := CAT_MAP[as.character(categories)]]

message(sprintf("  Panel: %s rows × %s cols",
                format(nrow(panel), big.mark=","), format(ncol(panel), big.mark=",")))

# ── Publication theme ────────────────────────────────────
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 15, hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "#666666", hjust = 0, margin = margin(b = 12)),
    plot.caption = element_text(size = 8, color = "#999999", hjust = 0),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 9),
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
pal_slate <- "#64748B"

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
    message(sprintf("  [WARN] %s: %s", filename, conditionMessage(e)))
  })
}

# ════════════════════════════════════════════════════════════
# 3. ANALYSIS 1: PCA ON MACRO VARIABLES
# ════════════════════════════════════════════════════════════
message("\n[2] Analysis 1: PCA on Macro Variables...")
tic("PCA")

# Get one row per quarter (macro is the same across categories)
macro_cols <- grep("^macro_", names(panel), value = TRUE)
macro_dt <- unique(panel[, c("date", macro_cols), with = FALSE], by = "date")
setorderv(macro_dt, "date")

# Keep only numeric, non-constant, mostly-complete columns
good_cols <- macro_cols[vapply(macro_cols, function(v) {
  x <- macro_dt[[v]]
  is.numeric(x) && sum(!is.na(x)) >= nrow(macro_dt) * 0.7 && sd(x, na.rm=TRUE) > 1e-10
}, logical(1))]

# Complete cases matrix
macro_mat <- as.matrix(macro_dt[, good_cols, with = FALSE])
storage.mode(macro_mat) <- "double"
complete_rows <- complete.cases(macro_mat)
macro_mat <- macro_mat[complete_rows, ]
pca_dates <- macro_dt$date[complete_rows]

message(sprintf("  PCA input: %d quarters × %d macro features", nrow(macro_mat), ncol(macro_mat)))

# Run PCA
pca_fit <- prcomp(macro_mat, center = TRUE, scale. = TRUE)

# Variance explained
var_explained <- summary(pca_fit)$importance
pct_var <- var_explained["Proportion of Variance", ] * 100
cum_var <- var_explained["Cumulative Proportion", ] * 100

# How many components for 80% and 90%
n_80 <- which(cum_var >= 80)[1]
n_90 <- which(cum_var >= 90)[1]
message(sprintf("  Components for 80%% variance: %d", n_80))
message(sprintf("  Components for 90%% variance: %d", n_90))

# ── Map variable to economic theme for PCA loading labels ──
pca_theme <- function(v) {
  v <- tolower(v)
  if (grepl("fedfunds|gs3m|gs10|gs30|yield|spread_2s10s|mortgage|real_rate|fwd_|fomc|hike", v))
    return("Interest Rates")
  if (grepl("baa|credit_tight", v)) return("Credit Conditions")
  if (grepl("unrate|disp_income|savings", v)) return("Labour & Income")
  if (grepl("gdp|cons_confidence", v)) return("Economic Activity")
  if (grepl("cpi|core_cpi|inflation", v)) return("Inflation")
  if (grepl("housing|hpi", v)) return("Housing")
  if (grepl("bankrupt|delinq", v)) return("Credit Quality")
  return("Other")
}

# ── Chart U1: Scree Plot (Variance Explained) ────────────
message("  Chart U1: Scree plot...")
n_show <- min(15, length(pct_var))
scree_dt <- data.table(
  PC = factor(paste0("PC", 1:n_show), levels = paste0("PC", 1:n_show)),
  pct = pct_var[1:n_show],
  cum = cum_var[1:n_show]
)

p_u1 <- ggplot(scree_dt, aes(x = PC)) +
  geom_col(aes(y = pct), fill = pal_navy, width = 0.6, alpha = 0.9) +
  geom_line(aes(y = cum, group = 1), color = pal_coral, linewidth = 1.2) +
  geom_point(aes(y = cum), color = pal_coral, size = 3) +
  geom_hline(yintercept = 80, linetype = "dashed", color = pal_slate, linewidth = 0.5) +
  geom_text(aes(y = pct, label = sprintf("%.1f%%", pct)),
            vjust = -0.5, size = 3, color = "#555555") +
  annotate("text", x = n_show - 1, y = 82, label = "80% threshold",
           size = 3.2, color = pal_slate, fontface = "italic") +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 105),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "How Many Independent Economic Forces Drive CU Growth?",
    subtitle = sprintf("Bars = variance each component explains  |  Line = cumulative  |  %d components reach 80%%", n_80),
    x = "Principal Component", y = "Variance Explained (%)",
    caption = sprintf("PCA on %d macro features  |  %d quarterly observations (2005–2025)", ncol(macro_mat), nrow(macro_mat))
  ) +
  theme_pub +
  theme(panel.grid.major.x = element_blank())
save_pub(p_u1, "U1_pca_scree_plot.pdf", w = 12, h = 7)

# ── Chart U2: Top Loadings per Component ─────────────────
message("  Chart U2: PCA loadings...")
n_pcs <- min(4, ncol(pca_fit$rotation))
load_list <- list()
for (pc in 1:n_pcs) {
  loads <- pca_fit$rotation[, pc]
  top_n <- head(order(abs(loads), decreasing = TRUE), 12)
  load_list[[pc]] <- data.table(
    variable = names(loads)[top_n],
    loading  = loads[top_n],
    PC       = paste0("PC", pc, " (", round(pct_var[pc], 1), "%)"),
    theme    = vapply(names(loads)[top_n], pca_theme, character(1))
  )
}
load_dt <- rbindlist(load_list)

# Clean names
load_dt[, var_clean := gsub("^macro_", "", variable)]
load_dt[, var_clean := gsub("_", " ", var_clean)]
load_dt[, var_clean := paste0(toupper(substr(var_clean, 1, 1)), substr(var_clean, 2, nchar(var_clean)))]

theme_fill <- c(
  "Interest Rates"    = pal_navy,
  "Credit Conditions" = pal_coral,
  "Labour & Income"   = pal_teal,
  "Economic Activity" = pal_green,
  "Inflation"         = pal_amber,
  "Housing"           = pal_sky,
  "Credit Quality"    = "#9B59B6",
  "Other"             = "#95A5A6"
)

p_u2 <- ggplot(load_dt, aes(x = loading, y = reorder(var_clean, abs(loading)), fill = theme)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_vline(xintercept = 0, color = "#999999", linewidth = 0.3) +
  facet_wrap(~PC, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = theme_fill, name = "Economic Theme") +
  labs(
    title = "What Each Principal Component Represents",
    subtitle = "Top 12 variable loadings per component — color shows economic theme\nPositive = moves with component  |  Negative = moves against",
    x = "Loading Strength", y = NULL,
    caption = "Variables with strongest loadings define the component's economic interpretation"
  ) +
  theme_pub +
  theme(strip.text = element_text(face = "bold", size = 11),
        legend.position = "right",
        axis.text.y = element_text(size = 8))
save_pub(p_u2, "U2_pca_loadings.pdf", w = 14, h = 10)

# ── Chart U3: Component Scores Over Time ─────────────────
message("  Chart U3: PC scores timeline...")
scores_dt <- data.table(
  date = pca_dates,
  PC1  = pca_fit$x[, 1],
  PC2  = pca_fit$x[, 2],
  PC3  = if (ncol(pca_fit$x) >= 3) pca_fit$x[, 3] else NA_real_
)
scores_long <- melt(scores_dt, id.vars = "date", variable.name = "PC", value.name = "score")
scores_long <- scores_long[!is.na(score)]
scores_long[, date_d := as.Date(zoo::as.yearqtr(date))]

# Label PCs
pc_labels <- sprintf("PC%d (%.0f%%)", 1:3, pct_var[1:3])
scores_long[, PC_label := factor(PC, levels = c("PC1","PC2","PC3"),
                                  labels = pc_labels[1:3])]

p_u3 <- ggplot(scores_long, aes(x = date_d, y = score, color = PC_label)) +
  geom_hline(yintercept = 0, color = "#DDDDDD", linewidth = 0.5) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted",
             color = "#999999", linewidth = 0.4) +
  annotate("text", x = as.Date("2020-06-01"), y = max(scores_dt$PC1, na.rm=TRUE) * 0.9,
           label = "COVID", size = 3, color = "#999999", fontface = "italic", hjust = 0) +
  scale_color_manual(values = c(pal_navy, pal_coral, pal_teal), name = "Component") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Economic Forces Over Time — Principal Component Scores",
    subtitle = "How the dominant macro dimensions have evolved from 2005 to 2025",
    x = "Quarter", y = "Component Score (standardised)",
    caption = "Large swings = major economic regime shifts  |  COVID, GFC, and rate cycles visible"
  ) +
  theme_pub
save_pub(p_u3, "U3_pca_scores_timeline.pdf", w = 13, h = 7)

toc()

# ════════════════════════════════════════════════════════════
# 4. ANALYSIS 2: ASSET-CATEGORY CLUSTERING
# ════════════════════════════════════════════════════════════
message("\n[3] Analysis 2: Asset-Category Clustering...")
tic("Clustering")

# Build growth profile per category: one row per category with key metrics
cats <- sort(unique(panel$cat_label))
growth_vars <- c("yoy_fcu_pct", "yoy_fiscu_pct",
                 "yoy_fcu_assets_pct", "yoy_fiscu_assets_pct")
growth_vars <- intersect(growth_vars, names(panel))

# Compute summary stats per category
cat_profiles <- panel[, {
  out <- list(cat_label = cat_label[1])
  for (gv in growth_vars) {
    vals <- get(gv)
    vals <- vals[!is.na(vals)]
    out[[paste0(gv, "_mean")]]  <- if (length(vals) > 0) mean(vals) else NA_real_
    out[[paste0(gv, "_sd")]]    <- if (length(vals) > 0) sd(vals)   else NA_real_
    out[[paste0(gv, "_trend")]] <- if (length(vals) > 5) {
      coef(lm(vals ~ seq_along(vals)))[2]
    } else NA_real_
  }
  out
}, by = cat_label]

# Also add exit dynamics
exit_vars <- intersect(c("merger_rate", "liquid_rate", "exit_rate"), names(panel))
for (ev in exit_vars) {
  cat_profiles <- merge(cat_profiles,
    panel[, .(tmp = mean(get(ev), na.rm=TRUE)), by = cat_label],
    by = "cat_label", all.x = TRUE)
  setnames(cat_profiles, "tmp", paste0(ev, "_mean"))
}

# Numeric columns for clustering
clust_cols <- setdiff(names(cat_profiles), "cat_label")
clust_cols <- clust_cols[vapply(clust_cols, function(v) {
  x <- cat_profiles[[v]]; is.numeric(x) && sum(!is.na(x)) >= 5 && sd(x, na.rm=TRUE) > 1e-10
}, logical(1))]

clust_mat <- as.matrix(cat_profiles[, clust_cols, with = FALSE])
rownames(clust_mat) <- cat_profiles$cat_label
clust_mat[is.na(clust_mat)] <- 0
clust_scaled <- scale(clust_mat)

# Hierarchical clustering
hc <- hclust(dist(clust_scaled), method = "ward.D2")

message(sprintf("  Clustering on %d categories × %d features", nrow(clust_mat), ncol(clust_mat)))

# ── Chart U4: Dendrogram ─────────────────────────────────
message("  Chart U4: Category dendrogram...")

# Convert to dendrogram data for ggplot
dend <- as.dendrogram(hc)
# Using base R plot for dendrogram (no ggdendro dependency needed)

# Simple approach: use base R for dendrogram, save via pdf device
pdf_path_u4 <- file.path(PLOT_DIR, "U4_category_dendrogram.pdf")
tryCatch({
  dev <- tryCatch(
    { grDevices::cairo_pdf(pdf_path_u4, width = 10, height = 7); "cairo" },
    error = function(e) { grDevices::pdf(pdf_path_u4, width = 10, height = 7); "pdf" })

  par(mar = c(8, 4, 4, 2), family = "sans")
  plot(hc, hang = -1, labels = cat_profiles$cat_label[hc$order],
       main = "Which Asset-Size Categories Behave Similarly?",
       sub = "", xlab = "", ylab = "Distance (Ward's method)",
       cex = 1.1, cex.main = 1.4, font.main = 2)

  # Add colored boxes for suggested clusters (k=3)
  rect.hclust(hc, k = 3, border = c(pal_coral, pal_teal, pal_navy))
  mtext("Categories inside the same box have similar growth patterns and can potentially be grouped",
        side = 1, line = 6, cex = 0.85, col = "#666666")

  grDevices::dev.off()
  message(sprintf("  Saved: U4_category_dendrogram.pdf [%s]", dev))
}, error = function(e) {
  try(grDevices::dev.off(), silent = TRUE)
  message(sprintf("  [WARN] U4: %s", conditionMessage(e)))
})

# Helper function (not using ggdendro)


# ── Chart U5: Category Growth Profile Heatmap ────────────
message("  Chart U5: Growth profile heatmap...")

# Normalize each metric to 0-1 for heatmap
hm_mat <- clust_scaled
hm_dt <- data.table(cat_label = rownames(hm_mat))
for (j in 1:ncol(hm_mat)) {
  hm_dt[, (colnames(hm_mat)[j]) := hm_mat[, j]]
}
hm_long <- melt(hm_dt, id.vars = "cat_label", variable.name = "metric", value.name = "z_score")

# Clean metric names
hm_long[, metric_clean := gsub("_mean$|_sd$|_trend$", "", metric)]
hm_long[, metric_clean := gsub("yoy_|qoq_", "", metric_clean)]
hm_long[, metric_clean := gsub("_pct", "", metric_clean)]
hm_long[, metric_clean := gsub("_", " ", metric_clean)]
hm_long[, stat := fifelse(grepl("_mean$", metric), "Mean",
                  fifelse(grepl("_sd$", metric), "Volatility",
                  fifelse(grepl("_trend$", metric), "Trend", "Level")))]
hm_long[, metric_label := paste0(metric_clean, " (", stat, ")")]

# Order categories by dendrogram
hm_long[, cat_label := factor(cat_label, levels = cat_profiles$cat_label[hc$order])]

p_u5 <- ggplot(hm_long, aes(x = cat_label, y = metric_label, fill = z_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = pal_coral, mid = "white", high = pal_navy,
                       midpoint = 0, name = "Z-Score",
                       guide = guide_colorbar(barwidth = 12, barheight = 0.6)) +
  labs(
    title = "Growth Profile by Asset-Size Category",
    subtitle = "Standardised metrics — blue = above average, red = below average\nCategories ordered by similarity (dendrogram order)",
    x = NULL, y = NULL,
    caption = "Z-score: 0 = system average  |  Positive = above average for that metric"
  ) +
  theme_pub +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank(),
        legend.position = "bottom")
save_pub(p_u5, "U5_category_profile_heatmap.pdf", w = 12, h = 10)

toc()

# ════════════════════════════════════════════════════════════
# 5. ANALYSIS 3: GROWTH REGIME DETECTION
# ════════════════════════════════════════════════════════════
message("\n[4] Analysis 3: Growth Regime Detection...")
tic("Regimes")

# System-wide average growth per quarter
sys_growth <- panel[, .(
  fcu_growth    = mean(yoy_fcu_pct, na.rm = TRUE),
  fiscu_growth  = mean(yoy_fiscu_pct, na.rm = TRUE),
  fcu_asset_gr  = mean(yoy_fcu_assets_pct, na.rm = TRUE),
  fiscu_asset_gr = mean(yoy_fiscu_assets_pct, na.rm = TRUE)
), by = date]
setorderv(sys_growth, "date")

# Use available growth columns
gr_cols <- intersect(c("fcu_growth","fiscu_growth","fcu_asset_gr","fiscu_asset_gr"),
                     names(sys_growth))
sys_growth <- sys_growth[complete.cases(sys_growth[, gr_cols, with = FALSE])]

# K-means on growth profiles (k = 3 regimes: contraction, normal, expansion)
gr_mat <- scale(as.matrix(sys_growth[, gr_cols, with = FALSE]))

# Try k = 2, 3, 4 — pick best by silhouette
best_k <- 3L
if (nrow(gr_mat) >= 10L) {
  sil_scores <- numeric(3)
  for (ki in 2:4) {
    km <- kmeans(gr_mat, centers = ki, nstart = 25, iter.max = 100)
    ss <- cluster::silhouette(km$cluster, dist(gr_mat))
    sil_scores[ki - 1] <- mean(ss[, 3])
  }
  best_k <- which.max(sil_scores) + 1L
  message(sprintf("  Best k by silhouette: %d (scores: %.3f, %.3f, %.3f)",
                  best_k, sil_scores[1], sil_scores[2], sil_scores[3]))
}

km_fit <- kmeans(gr_mat, centers = best_k, nstart = 25, iter.max = 100)
sys_growth[, regime := km_fit$cluster]

# Label regimes by average growth (highest = Expansion, lowest = Contraction)
regime_means <- sys_growth[, .(avg_growth = mean(fcu_asset_gr, na.rm = TRUE)), by = regime]
setorderv(regime_means, "avg_growth")
regime_labels <- c("Contraction/Stress", "Normal Growth", "Expansion/Recovery")
if (best_k == 2L) regime_labels <- c("Below-Trend", "Above-Trend")
if (best_k == 4L) regime_labels <- c("Deep Stress", "Contraction", "Normal", "Expansion")
regime_means[, label := regime_labels[1:.N]]

sys_growth <- merge(sys_growth, regime_means[, .(regime, regime_label = label)],
                    by = "regime", all.x = TRUE)
sys_growth[, date_d := as.Date(zoo::as.yearqtr(date))]
setorderv(sys_growth, "date")

# ── Chart U6: Regime Timeline ────────────────────────────
message("  Chart U6: Regime timeline...")

regime_colors <- c(
  "Contraction/Stress" = pal_coral,
  "Normal Growth"      = pal_sky,
  "Expansion/Recovery"  = pal_green,
  "Below-Trend"        = pal_coral,
  "Above-Trend"        = pal_green,
  "Deep Stress"        = "#C0392B",
  "Contraction"        = pal_coral,
  "Normal"             = pal_sky,
  "Expansion"          = pal_green
)

p_u6 <- ggplot(sys_growth, aes(x = date_d, y = fcu_asset_gr)) +
  geom_rect(aes(xmin = date_d - 45, xmax = date_d + 45,
                ymin = -Inf, ymax = Inf, fill = regime_label),
            alpha = 0.15, show.legend = TRUE) +
  geom_line(color = pal_navy, linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "#999999", linewidth = 0.3, linetype = "dashed") +
  scale_fill_manual(values = regime_colors, name = "Economic Regime") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Credit Union Growth Regimes Over Time",
    subtitle = sprintf("FCU asset growth (YoY %%) with %d detected regimes — shaded by economic period", best_k),
    x = "Quarter", y = "FCU Asset Growth (YoY %)",
    caption = sprintf("Regimes detected via K-means clustering (k=%d, silhouette-optimised)  |  GFC, COVID, rate hikes visible", best_k)
  ) +
  theme_pub +
  theme(legend.position = "top")
save_pub(p_u6, "U6_growth_regime_timeline.pdf", w = 14, h = 7)

# ── Chart U7: Regime Box Plots ───────────────────────────
message("  Chart U7: Regime distributions...")
regime_long <- melt(sys_growth,
  id.vars = c("date", "regime_label"),
  measure.vars = gr_cols,
  variable.name = "metric", value.name = "growth")

regime_long[, metric_label := fcase(
  metric == "fcu_growth",     "FCU Count",
  metric == "fiscu_growth",   "FISCU Count",
  metric == "fcu_asset_gr",   "FCU Assets",
  metric == "fiscu_asset_gr", "FISCU Assets",
  default = metric
)]

p_u7 <- ggplot(regime_long, aes(x = regime_label, y = growth, fill = regime_label)) +
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.size = 1.5) +
  geom_hline(yintercept = 0, color = "#999999", linewidth = 0.3, linetype = "dashed") +
  facet_wrap(~metric_label, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = regime_colors, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Growth Distribution by Economic Regime",
    subtitle = "How different CU growth metrics behave under each regime\nBox = 25th–75th percentile  |  Line = median  |  Dots = outliers",
    x = NULL, y = "YoY Growth (%)",
    caption = "Contraction regimes show negative or flat growth; expansion regimes show positive momentum"
  ) +
  theme_pub +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        strip.text = element_text(face = "bold", size = 11))
save_pub(p_u7, "U7_regime_distributions.pdf", w = 12, h = 9)

toc()

# ════════════════════════════════════════════════════════════
# 6. ANALYSIS 4: VARIABLE CORRELATION CLUSTERING
# ════════════════════════════════════════════════════════════
message("\n[5] Analysis 4: Variable Correlation Clustering...")
tic("CorrClust")

# Use curated macro + key CU variables
cu_vars <- intersect(c("merger_rate", "liquid_rate", "exit_rate",
                       "yoy_fcu_pct", "yoy_fiscu_pct",
                       "yoy_fcu_assets_pct", "yoy_fiscu_assets_pct"),
                     names(panel))

# System-level averages per quarter
sys_vars <- panel[, lapply(.SD, mean, na.rm = TRUE),
                  .SDcols = c(good_cols, cu_vars), by = date]
setorderv(sys_vars, "date")

# Correlation matrix on complete cases
corr_cols <- intersect(c(good_cols, cu_vars), names(sys_vars))
corr_cols <- corr_cols[vapply(corr_cols, function(v) {
  x <- sys_vars[[v]]; sum(!is.na(x)) >= 15 && sd(x, na.rm=TRUE) > 1e-10
}, logical(1))]

# Limit to top 40 most variable features for readability
col_vars <- vapply(corr_cols, function(v) var(sys_vars[[v]], na.rm=TRUE), numeric(1))
top_corr_cols <- names(sort(col_vars, decreasing = TRUE))[1:min(40, length(corr_cols))]

corr_mat_full <- cor(sys_vars[, top_corr_cols, with = FALSE], use = "pairwise.complete.obs")
corr_mat_full[is.na(corr_mat_full)] <- 0

# Hierarchical clustering on correlation distance
corr_dist <- as.dist(1 - abs(corr_mat_full))
hc_corr <- hclust(corr_dist, method = "complete")

# ── Chart U8: Correlation Dendrogram ─────────────────────
message("  Chart U8: Variable correlation clustering...")

# Clean labels
clean_labels <- gsub("^macro_", "", hc_corr$labels)
clean_labels <- gsub("_", " ", clean_labels)
clean_labels <- paste0(toupper(substr(clean_labels, 1, 1)),
                       substr(clean_labels, 2, nchar(clean_labels)))

pdf_path_u8 <- file.path(PLOT_DIR, "U8_variable_correlation_clusters.pdf")
tryCatch({
  dev <- tryCatch(
    { grDevices::cairo_pdf(pdf_path_u8, width = 14, height = 9); "cairo" },
    error = function(e) { grDevices::pdf(pdf_path_u8, width = 14, height = 9); "pdf" })

  par(mar = c(12, 4, 4, 2), family = "sans")
  hc_plot <- hc_corr
  hc_plot$labels <- clean_labels
  plot(hc_plot, hang = -1,
       main = "Which Variables Move Together?",
       sub = "", xlab = "", ylab = "Correlation Distance",
       cex = 0.75, cex.main = 1.4, font.main = 2)
  rect.hclust(hc_plot, k = 6, border = c(pal_navy, pal_coral, pal_teal,
                                           pal_green, pal_amber, pal_sky))
  mtext("Variables in the same box are highly correlated — they represent the same underlying economic force",
        side = 1, line = 10.5, cex = 0.85, col = "#666666")

  grDevices::dev.off()
  message(sprintf("  Saved: U8_variable_correlation_clusters.pdf [%s]", dev))
}, error = function(e) {
  try(grDevices::dev.off(), silent = TRUE)
  message(sprintf("  [WARN] U8: %s", conditionMessage(e)))
})

toc()

# ════════════════════════════════════════════════════════════
# 7. SAVE RESULTS
# ════════════════════════════════════════════════════════════
message("\n[6] Saving results...")

# PCA results
pca_summary <- data.table(
  Component = paste0("PC", 1:min(10, length(pct_var))),
  Variance_Pct = round(pct_var[1:min(10, length(pct_var))], 2),
  Cumulative_Pct = round(cum_var[1:min(10, length(pct_var))], 2)
)
fwrite(pca_summary, file.path(RESULT_DIR, "pca_variance_explained.csv"))

# Category cluster assignments
cluster_assign <- data.table(
  cat_label = cat_profiles$cat_label,
  cluster_k3 = cutree(hc, k = 3)
)
fwrite(cluster_assign, file.path(RESULT_DIR, "category_clusters.csv"))

# Regime assignments
fwrite(sys_growth[, .(date, regime, regime_label, fcu_asset_gr)],
       file.path(RESULT_DIR, "growth_regimes.csv"))

message("  CSVs saved to ", RESULT_DIR)

# ════════════════════════════════════════════════════════════
# 8. SUMMARY
# ════════════════════════════════════════════════════════════
message("\n============================================================")
message("  UNSUPERVISED ANALYSIS COMPLETE")
message("============================================================")
message(sprintf("  PCA: %d components for 80%% variance (of %d features)", n_80, ncol(macro_mat)))
message(sprintf("  Clustering: 7 categories → %d natural groups", length(unique(cutree(hc, k = 3)))))
message(sprintf("  Regimes: %d detected (k=%d)", best_k, best_k))
message("")
message(sprintf("  Charts saved to: %s/", PLOT_DIR))
message("    U1 — PCA scree plot (how many forces?)")
message("    U2 — PCA loadings (what each force represents)")
message("    U3 — PC scores timeline (forces over time)")
message("    U4 — Category dendrogram (which categories cluster?)")
message("    U5 — Category growth profile heatmap")
message("    U6 — Growth regime timeline")
message("    U7 — Regime distribution box plots")
message("    U8 — Variable correlation clusters")
message("============================================================")
