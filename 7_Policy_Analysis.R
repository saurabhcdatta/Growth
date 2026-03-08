############################################################
# PART 7 v1.0 — POLICY ANALYSIS & SCENARIO ASSESSMENT
#
# Purpose  : Translate the statistical findings from Parts
#            3-6 into actionable policy insights for NCUA
#            decision-makers.
#
# Analyses :
#   7A — Sensitivity Analysis
#        "A 1-SD shock in macro theme X historically
#         corresponds to Y% change in CU growth"
#
#   7B — Leading Indicator Analysis
#        Which macro variables lead CU growth by 1-4
#        quarters? Identifies early warning signals.
#
#   7C — Category Vulnerability Scoring
#        Which asset-size categories are most sensitive
#        to rate hikes, recessions, and credit stress?
#
#   7D — Category Migration Analysis
#        Historical migration patterns: CUs moving from
#        small to large categories over time.
#
#   7E — Consolidation Tipping Point
#        At what size do CUs flip from "merger target"
#        to "likely acquirer"?
#
# Output   : Publication charts + CSV tables
# Runtime  : ~2-3 minutes (no parallel needed)
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
})

set.seed(42)
options(scipen = 999)

# ════════════════════════════════════════════════════════════
# 1. CONFIG
# ════════════════════════════════════════════════════════════
DATA_DIR   <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
RESULT_DIR <- "results_7_policy"
PLOT_DIR   <- "plots_7_policy"

setwd(DATA_DIR)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)

message("============================================================")
message("  PART 7: Policy Analysis & Scenario Assessment")
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

setorderv(panel, c("categories","date"))
cats <- sort(unique(panel$cat_label))

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

pal_navy  <- "#0B1D3A"; pal_teal  <- "#2EC4B6"; pal_coral <- "#E76F51"
pal_amber <- "#E8A838"; pal_sky   <- "#5B9BD5"; pal_green <- "#52B788"
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
# 3. ANALYSIS 7A: SENSITIVITY ANALYSIS
# ════════════════════════════════════════════════════════════
message("\n[2] Analysis 7A: Macro Sensitivity...")
tic("7A Sensitivity")

# Define macro themes with representative variables
macro_themes <- list(
  "Fed Funds Rate"         = "macro_fedfunds",
  "10-Year Treasury"       = "macro_gs10",
  "30-Year Mortgage Rate"  = "macro_mortgage30",
  "Yield Curve Slope"      = "macro_yield_curve",
  "BAA Credit Spread"      = "macro_baa_spread",
  "Unemployment Rate"      = "macro_unrate",
  "Disposable Income"      = "macro_disp_income",
  "Consumer Confidence"    = "macro_cons_confidence",
  "CPI (YoY %)"            = "macro_cpi_yoy",
  "House Price Index"      = "macro_hpi_fed",
  "Consumer Loan Delinq"   = "macro_cons_loan_delinq",
  "Savings Rate"           = "macro_savings_rate"
)

# System-level averages per quarter
targets <- c("yoy_fcu_pct", "yoy_fiscu_pct",
             "yoy_fcu_assets_pct", "yoy_fiscu_assets_pct")
targets <- intersect(targets, names(panel))

sys_avg <- panel[, c(
  lapply(.SD, mean, na.rm = TRUE),
  list(date = date[1])
), .SDcols = c(targets, unlist(macro_themes)),
  by = date]
setorderv(sys_avg, "date")

# Compute sensitivity: correlation × (SD_y / SD_x) = regression coefficient
sens_results <- list()
for (theme_name in names(macro_themes)) {
  macro_var <- macro_themes[[theme_name]]
  if (!macro_var %in% names(sys_avg)) next

  x_vals <- as.numeric(sys_avg[[macro_var]])

  for (tgt in targets) {
    y_vals <- as.numeric(sys_avg[[tgt]])
    ok <- !is.na(x_vals) & !is.na(y_vals)
    if (sum(ok) < 15) next

    x <- x_vals[ok]; y <- y_vals[ok]
    sd_x <- sd(x); sd_y <- sd(y)
    if (sd_x < 1e-10 || sd_y < 1e-10) next

    r <- cor(x, y)
    # Standardised sensitivity: 1-SD change in macro → how many SD change in CU growth
    beta_std <- r  # correlation = standardised regression coefficient in bivariate case
    # Unstandardised: 1-unit change in macro → change in CU growth (percentage points)
    beta_raw <- coef(lm(y ~ x))[2]
    # Impact of 1-SD shock
    impact_1sd <- beta_raw * sd_x

    tgt_label <- gsub("yoy_|_pct", "", tgt)
    tgt_label <- gsub("_", " ", tgt_label)
    tgt_label <- paste0(toupper(substr(tgt_label, 1, 1)), substr(tgt_label, 2, nchar(tgt_label)))

    sens_results[[length(sens_results) + 1L]] <- data.table(
      macro_theme  = theme_name,
      target       = tgt,
      target_label = tgt_label,
      correlation  = round(r, 3),
      beta_std     = round(beta_std, 3),
      impact_1sd   = round(impact_1sd, 2),
      sd_macro     = round(sd_x, 3),
      n_obs        = sum(ok)
    )
  }
}

sens_dt <- rbindlist(sens_results)

# Average impact across targets
sens_avg <- sens_dt[, .(
  avg_impact    = mean(abs(impact_1sd), na.rm = TRUE),
  avg_corr      = mean(abs(correlation), na.rm = TRUE),
  direction     = fifelse(mean(impact_1sd, na.rm = TRUE) > 0, "Positive", "Negative")
), by = macro_theme]
setorderv(sens_avg, "avg_impact", order = -1L)

fwrite(sens_dt,  file.path(RESULT_DIR, "sensitivity_detail.csv"))
fwrite(sens_avg, file.path(RESULT_DIR, "sensitivity_summary.csv"))

# ── Chart P1: Sensitivity Waterfall ──────────────────────
message("  Chart P1: Sensitivity waterfall...")
sens_avg[, macro_theme := factor(macro_theme, levels = rev(macro_theme))]

p_s1 <- ggplot(sens_avg, aes(x = avg_impact, y = macro_theme, fill = direction)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f pp", avg_impact)),
            hjust = -0.1, size = 3.3, color = "#444444") +
  scale_fill_manual(values = c("Positive" = pal_teal, "Negative" = pal_coral),
                    name = "Direction of Effect") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Macro Sensitivity — Impact of a 1-Standard-Deviation Shock",
    subtitle = "Average absolute impact on CU growth (percentage points) across all count and asset targets",
    x = "Impact on CU Growth (percentage points)", y = NULL,
    caption = "1-SD shock: e.g., if Fed Funds typically varies by 1.5%, this shows the impact of a 1.5% move\nPositive = macro increase → CU growth increase  |  Negative = macro increase → CU growth decrease"
  ) +
  theme_pub +
  theme(legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill = "white", color = "#DDD", linewidth = 0.3))
save_pub(p_s1, "P1_sensitivity_waterfall.pdf", w = 12, h = 8)

# ── Chart P2: Sensitivity by Target (heatmap) ───────────
message("  Chart P2: Sensitivity heatmap by target...")
top_themes <- head(sens_avg$macro_theme, 10)
p2_data <- sens_dt[macro_theme %in% as.character(top_themes)]
p2_data[, macro_theme := factor(macro_theme, levels = rev(as.character(top_themes)))]

p_s2 <- ggplot(p2_data, aes(x = target_label, y = macro_theme, fill = impact_1sd)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%+.2f", impact_1sd),
                color = ifelse(abs(impact_1sd) > 0.8, "high", "low")),
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("high" = "white", "low" = "#333333")) +
  scale_fill_gradient2(low = pal_coral, mid = "white", high = pal_teal,
                       midpoint = 0, name = "Impact (pp)",
                       guide = guide_colorbar(barwidth = 12, barheight = 0.6)) +
  labs(
    title = "How Each Macro Factor Affects Different CU Growth Measures",
    subtitle = "Impact of a 1-SD shock (percentage points)  |  Red = negative  |  Green = positive",
    x = NULL, y = NULL,
    caption = "Read across rows: positive = macro increase helps CU growth  |  Negative = hurts"
  ) +
  theme_pub +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1, size = 10, face = "bold"))
save_pub(p_s2, "P2_sensitivity_heatmap.pdf", w = 12, h = 9)

toc()

# ════════════════════════════════════════════════════════════
# 4. ANALYSIS 7B: LEADING INDICATOR ANALYSIS
# ════════════════════════════════════════════════════════════
message("\n[3] Analysis 7B: Leading Indicators...")
tic("7B Leading")

# Cross-correlation at lags 1-8 quarters
lead_results <- list()
for (theme_name in names(macro_themes)) {
  macro_var <- macro_themes[[theme_name]]
  if (!macro_var %in% names(sys_avg)) next
  x_vals <- as.numeric(sys_avg[[macro_var]])

  for (tgt in targets) {
    y_vals <- as.numeric(sys_avg[[tgt]])
    ok <- !is.na(x_vals) & !is.na(y_vals)
    if (sum(ok) < 20) next

    x <- x_vals[ok]; y <- y_vals[ok]

    for (lag_q in 0:8) {
      n_eff <- length(x) - lag_q
      if (n_eff < 15) next
      x_lag <- x[1:(length(x) - lag_q)]
      y_fut <- y[(1 + lag_q):length(y)]
      r <- cor(x_lag, y_fut, use = "complete.obs")

      lead_results[[length(lead_results) + 1L]] <- data.table(
        macro_theme = theme_name,
        target      = tgt,
        lag_quarters = lag_q,
        correlation  = round(r, 4),
        abs_corr     = round(abs(r), 4)
      )
    }
  }
}

lead_dt <- rbindlist(lead_results)

# Find optimal leading lag per (macro, target)
best_lead <- lead_dt[lag_quarters > 0,
  .SD[which.max(abs_corr)],
  by = .(macro_theme, target)]

# Average optimal lag across targets
lead_summary <- best_lead[, .(
  avg_optimal_lag = round(mean(lag_quarters), 1),
  avg_lead_corr   = round(mean(abs_corr), 3),
  max_lead_corr   = round(max(abs_corr), 3)
), by = macro_theme]
setorderv(lead_summary, "avg_lead_corr", order = -1L)

fwrite(lead_dt,      file.path(RESULT_DIR, "leading_indicators_detail.csv"))
fwrite(lead_summary, file.path(RESULT_DIR, "leading_indicators_summary.csv"))

# ── Chart P3: Leading Indicator Lag Profile ──────────────
message("  Chart P3: Leading indicator profiles...")

# For top 6 themes, show correlation across lags
top6_lead <- head(lead_summary$macro_theme, 6)
p3_data <- lead_dt[macro_theme %in% top6_lead,
  .(avg_corr = mean(correlation, na.rm = TRUE)), by = .(macro_theme, lag_quarters)]

p_l1 <- ggplot(p3_data, aes(x = lag_quarters, y = avg_corr, color = macro_theme)) +
  geom_hline(yintercept = 0, color = "#CCCCCC", linewidth = 0.4) +
  geom_line(linewidth = 1, alpha = 0.85) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 0, color = "#999999", linetype = "dotted") +
  scale_color_manual(values = c(pal_navy, pal_coral, pal_teal,
                                 pal_amber, pal_sky, pal_green),
                     name = "Macro Variable") +
  scale_x_continuous(breaks = 0:8,
                     labels = c("Same\nquarter", paste0("+", 1:8, "Q"))) +
  labs(
    title = "Early Warning — How Far Ahead Do Macro Variables Signal CU Growth?",
    subtitle = "Correlation between macro variable today and CU growth X quarters later\nPeak = optimal leading indicator lag",
    x = "Macro variable leads CU growth by...", y = "Correlation",
    caption = "Higher absolute correlation at lag > 0 = better early warning signal"
  ) +
  theme_pub +
  theme(legend.position = "right",
        legend.text = element_text(size = 9))
save_pub(p_l1, "P3_leading_indicator_profiles.pdf", w = 13, h = 8)

# ── Chart P4: Optimal Lead Time Summary ──────────────────
message("  Chart P4: Optimal lead times...")
lead_summary[, macro_theme := factor(macro_theme, levels = rev(macro_theme))]

p_l2 <- ggplot(lead_summary, aes(x = avg_optimal_lag, y = macro_theme)) +
  geom_segment(aes(x = 0, xend = avg_optimal_lag, yend = macro_theme),
               color = "#E0E0E0", linewidth = 1.5) +
  geom_point(aes(size = avg_lead_corr, color = avg_lead_corr), alpha = 0.9) +
  geom_text(aes(label = sprintf("%.0fQ (r=%.2f)", avg_optimal_lag, avg_lead_corr)),
            hjust = -0.15, size = 3.2, color = "#444444") +
  scale_color_gradient(low = pal_amber, high = pal_navy, name = "Correlation\nStrength") +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  scale_x_continuous(breaks = 0:8, expand = expansion(mult = c(0, 0.3)),
                     labels = function(x) paste0(x, "Q")) +
  labs(
    title = "How Far Ahead Can We See CU Growth Changes Coming?",
    subtitle = "Optimal leading lag and correlation strength for each macro variable\nLarger, darker dots = stronger early warning signal",
    x = "Optimal Leading Lag (quarters)", y = NULL,
    caption = "Example: if Fed Funds shows 2Q lead, a rate change today predicts CU growth impact 6 months later"
  ) +
  theme_pub
save_pub(p_l2, "P4_optimal_lead_times.pdf", w = 12, h = 8)

toc()

# ════════════════════════════════════════════════════════════
# 5. ANALYSIS 7C: CATEGORY VULNERABILITY SCORING
# ════════════════════════════════════════════════════════════
message("\n[4] Analysis 7C: Category Vulnerability Scoring...")
tic("7C Vulnerability")

# For each category, compute sensitivity to key stress indicators
stress_vars <- list(
  "Rate Hike"       = "macro_fedfunds",
  "Unemployment"    = "macro_unrate",
  "Credit Stress"   = "macro_baa_spread",
  "Yield Inversion" = "macro_yield_curve"
)

vuln_results <- list()
for (cat in cats) {
  cat_dt <- panel[cat_label == cat]
  setorderv(cat_dt, "date")

  for (stress_name in names(stress_vars)) {
    sv <- stress_vars[[stress_name]]
    if (!sv %in% names(cat_dt)) next

    for (tgt in targets) {
      x <- as.numeric(cat_dt[[sv]])
      y <- as.numeric(cat_dt[[tgt]])
      ok <- !is.na(x) & !is.na(y)
      if (sum(ok) < 15) next

      r <- cor(x[ok], y[ok])
      beta <- coef(lm(y[ok] ~ x[ok]))[2]
      impact <- beta * sd(x[ok])

      vuln_results[[length(vuln_results) + 1L]] <- data.table(
        cat_label    = cat,
        stress_type  = stress_name,
        target       = tgt,
        correlation  = round(r, 3),
        impact_1sd   = round(impact, 2)
      )
    }
  }
}

vuln_dt <- rbindlist(vuln_results)

# Vulnerability score per category = average absolute impact across all stressors
vuln_score <- vuln_dt[, .(
  vulnerability = mean(abs(impact_1sd), na.rm = TRUE),
  worst_stress  = stress_type[which.max(abs(impact_1sd))],
  worst_impact  = max(abs(impact_1sd), na.rm = TRUE)
), by = cat_label]
setorderv(vuln_score, "vulnerability", order = -1L)

fwrite(vuln_dt,    file.path(RESULT_DIR, "category_vulnerability_detail.csv"))
fwrite(vuln_score, file.path(RESULT_DIR, "category_vulnerability_scores.csv"))

# ── Chart P5: Category Vulnerability Heatmap ─────────────
message("  Chart P5: Vulnerability heatmap...")
vuln_hm <- vuln_dt[, .(avg_impact = mean(impact_1sd, na.rm = TRUE)),
                    by = .(cat_label, stress_type)]
vuln_hm[, cat_label := factor(cat_label, levels = rev(cats))]

p_v1 <- ggplot(vuln_hm, aes(x = stress_type, y = cat_label, fill = avg_impact)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%+.2f", avg_impact),
                color = ifelse(abs(avg_impact) > 0.5, "high", "low")),
            size = 3.8, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("high" = "white", "low" = "#333333")) +
  scale_fill_gradient2(low = pal_coral, mid = "#FAFAFA", high = pal_teal,
                       midpoint = 0, name = "Impact (pp)",
                       guide = guide_colorbar(barwidth = 12, barheight = 0.6)) +
  labs(
    title = "Which CU Size Categories Are Most Vulnerable to Economic Stress?",
    subtitle = "Average impact of 1-SD stress shock on CU growth (percentage points)\nRed = stress hurts growth  |  Green = stress helps (safe haven effect)",
    x = NULL, y = NULL,
    caption = "Read across: which stressors hit each size category hardest?"
  ) +
  theme_pub +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
save_pub(p_v1, "P5_vulnerability_heatmap.pdf", w = 11, h = 8)

# ── Chart P6: Overall Vulnerability Ranking ──────────────
message("  Chart P6: Vulnerability ranking...")
vuln_score[, cat_label := factor(cat_label, levels = rev(vuln_score$cat_label))]

p_v2 <- ggplot(vuln_score, aes(x = vulnerability, y = cat_label)) +
  geom_col(fill = pal_navy, width = 0.6, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f pp\n(worst: %s)", vulnerability, worst_stress)),
            hjust = -0.05, size = 3.2, color = "#444444", lineheight = 0.9) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(
    title = "Overall Vulnerability Score by Asset-Size Category",
    subtitle = "Average sensitivity to rate hikes, unemployment, credit stress, and yield curve inversion\nHigher = more sensitive to macro shocks",
    x = "Average Vulnerability (pp per 1-SD shock)", y = NULL,
    caption = "Vulnerability = mean absolute impact across 4 stress scenarios × 4 CU growth targets"
  ) +
  theme_pub +
  theme(panel.grid.major.y = element_blank())
save_pub(p_v2, "P6_vulnerability_ranking.pdf", w = 12, h = 7)

toc()

# ════════════════════════════════════════════════════════════
# 6. ANALYSIS 7D: CATEGORY MIGRATION
# ════════════════════════════════════════════════════════════
message("\n[5] Analysis 7D: Category Migration Analysis...")
tic("7D Migration")

# Count CUs per category per quarter
cat_counts <- panel[, .(
  fcu_n = if ("fcu_count" %in% names(.SD)) sum(fcu_count, na.rm=TRUE) else NA_real_,
  fiscu_n = if ("fiscu_count" %in% names(.SD)) sum(fiscu_count, na.rm=TRUE) else NA_real_
), by = .(date, cat_label)]

# Compute share of total per quarter
cat_counts[, total_fcu := sum(fcu_n, na.rm=TRUE), by = date]
cat_counts[total_fcu > 0, share_fcu := fcu_n / total_fcu * 100]
cat_counts[, date_d := as.Date(zoo::as.yearqtr(date))]

# ── Chart P7: Category Share Over Time ───────────────────
message("  Chart P7: Category migration stacked area...")

cat_colors <- c(
  "1_Less_10M"  = "#E74C3C",
  "2_10M_50M"   = "#E67E22",
  "3_50M_100M"  = "#F1C40F",
  "4_100M_500M" = "#2ECC71",
  "5_500M_1B"   = "#3498DB",
  "6_1B_10B"    = "#2C3E50",
  "7_10B_Plus"  = "#8E44AD"
)

p_m1 <- ggplot(cat_counts[!is.na(share_fcu)],
               aes(x = date_d, y = share_fcu, fill = cat_label)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = cat_colors, name = "Asset Category") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Credit Union Migration — Category Composition Over Time",
    subtitle = "Share of total FCU count by asset-size category\nSmall CUs shrinking, large CUs growing as a share of the system",
    x = NULL, y = "Share of Total FCU Count (%)",
    caption = "Source: NCUA Call Reports  |  Each color band = one asset-size category"
  ) +
  theme_pub +
  theme(legend.position = "right",
        legend.text = element_text(size = 9))
save_pub(p_m1, "P7_category_migration.pdf", w = 14, h = 8)

# ── Chart P8: First vs Last Quarter Comparison ───────────
message("  Chart P8: Migration comparison...")
first_q <- min(cat_counts$date, na.rm = TRUE)
last_q  <- max(cat_counts$date, na.rm = TRUE)

compare_dt <- rbind(
  cat_counts[date == first_q & !is.na(share_fcu), .(cat_label, share = share_fcu, period = as.character(zoo::as.yearqtr(first_q)))],
  cat_counts[date == last_q & !is.na(share_fcu),  .(cat_label, share = share_fcu, period = as.character(zoo::as.yearqtr(last_q)))]
)

p_m2 <- ggplot(compare_dt, aes(x = cat_label, y = share, fill = period)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", share)),
            position = position_dodge(width = 0.75), vjust = -0.3,
            size = 3, color = "#555555") +
  scale_fill_manual(values = c(pal_sky, pal_navy), name = "Period") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Category Shift — Then vs Now",
    subtitle = sprintf("Share of total FCU count: %s vs %s",
                       as.character(zoo::as.yearqtr(first_q)),
                       as.character(zoo::as.yearqtr(last_q))),
    x = "Asset-Size Category", y = "Share of Total FCU Count (%)",
    caption = "Small CU categories losing share → large categories gaining"
  ) +
  theme_pub +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", color = "#DDD", linewidth = 0.3))
save_pub(p_m2, "P8_migration_comparison.pdf", w = 12, h = 8)

toc()

# ════════════════════════════════════════════════════════════
# 7. ANALYSIS 7E: CONSOLIDATION TIPPING POINT
# ════════════════════════════════════════════════════════════
message("\n[6] Analysis 7E: Consolidation Tipping Point...")
tic("7E Tipping")

# Exit rates by category
exit_vars_avail <- intersect(c("merger_rate", "liquid_rate", "exit_rate",
                                "acquisition_rate"), names(panel))

if (length(exit_vars_avail) > 0) {
  exit_by_cat <- panel[, lapply(.SD, mean, na.rm = TRUE),
                       .SDcols = exit_vars_avail, by = cat_label]
  setorderv(exit_by_cat, "cat_label")

  # Asset growth by category
  asset_gr <- panel[, .(
    avg_asset_growth = mean(yoy_fcu_assets_pct, na.rm = TRUE),
    avg_count_growth = mean(yoy_fcu_pct, na.rm = TRUE)
  ), by = cat_label]
  setorderv(asset_gr, "cat_label")

  exit_profile <- merge(exit_by_cat, asset_gr, by = "cat_label")

  fwrite(exit_profile, file.path(RESULT_DIR, "consolidation_profile.csv"))

  # ── Chart P9: Consolidation Profile ──────────────────────
  message("  Chart P9: Consolidation profile...")

  # Melt exit rates for grouped bar
  exit_long <- melt(exit_profile, id.vars = "cat_label",
                    measure.vars = intersect(c("merger_rate","liquid_rate"), names(exit_profile)),
                    variable.name = "exit_type", value.name = "rate")
  exit_long[, exit_type := gsub("_rate", "", exit_type)]
  exit_long[, exit_type := paste0(toupper(substr(exit_type,1,1)), substr(exit_type,2,nchar(exit_type)))]

  p_t1 <- ggplot(exit_long, aes(x = cat_label, y = rate, fill = exit_type)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
    geom_text(aes(label = sprintf("%.2f%%", rate)),
              position = position_dodge(width = 0.7), vjust = -0.3,
              size = 2.8, color = "#555555") +
    scale_fill_manual(values = c("Merger" = pal_coral, "Liquid" = pal_amber),
                      name = "Exit Type") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                       labels = function(x) paste0(x, "%")) +
    labs(
      title = "Consolidation Tipping Point — Exit Rates by Size Category",
      subtitle = "Average quarterly merger and liquidation rates\nSmaller CUs face higher exit pressure → the tipping point is visible",
      x = "Asset-Size Category", y = "Average Quarterly Exit Rate (%)",
      caption = "Higher bars = more CUs in this category exit per quarter\nTipping point: where exit rate drops sharply = size threshold for stability"
    ) +
    theme_pub +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9))
  save_pub(p_t1, "P9_consolidation_tipping_point.pdf", w = 12, h = 8)

  # ── Chart P10: Exit Rate vs Asset Growth Scatter ───────
  message("  Chart P10: Exit vs growth scatter...")

  p_t2 <- ggplot(exit_profile, aes(x = merger_rate, y = avg_asset_growth)) +
    geom_smooth(method = "lm", se = TRUE, color = pal_sky, fill = pal_sky,
                alpha = 0.15, linewidth = 0.8) +
    geom_point(size = 5, color = pal_navy, alpha = 0.9) +
    geom_text(aes(label = cat_label), vjust = -1.2, size = 3.2, color = "#555555") +
    labs(
      title = "Merger Pressure vs Asset Growth — The Consolidation Trade-off",
      subtitle = "Each dot = one asset-size category\nCategories with high merger rates tend to have different asset growth profiles",
      x = "Average Merger Rate (%)", y = "Average FCU Asset Growth (YoY %)",
      caption = "Relationship shows whether consolidation pressure correlates with faster or slower asset growth"
    ) +
    theme_pub
  save_pub(p_t2, "P10_merger_vs_growth.pdf", w = 11, h = 8)

} else {
  message("  [SKIP] No exit rate variables found in panel")
}

toc()

# ════════════════════════════════════════════════════════════
# 8. SAVE SUMMARY & FINAL OUTPUT
# ════════════════════════════════════════════════════════════
message("\n[7] Saving summary...")

# Excel summary
if (requireNamespace("openxlsx", quietly = TRUE)) {
  tryCatch({
    library(openxlsx)
    wb <- createWorkbook()

    # Sheet 1: Sensitivity
    addWorksheet(wb, "Macro Sensitivity")
    writeData(wb, "Macro Sensitivity",
              x = "Impact of 1-SD Macro Shock on CU Growth (pp)", startRow = 1)
    writeData(wb, "Macro Sensitivity", x = sens_avg, startRow = 3,
              headerStyle = createStyle(textDecoration = "bold", fgFill = "#D9E2F3",
                                        border = "TopBottomLeftRight"))
    setColWidths(wb, "Macro Sensitivity", cols = 1:4, widths = c(28, 12, 12, 14))
    addStyle(wb, "Macro Sensitivity",
             createStyle(fontSize = 13, textDecoration = "bold"), rows = 1, cols = 1)

    # Sheet 2: Leading Indicators
    addWorksheet(wb, "Leading Indicators")
    writeData(wb, "Leading Indicators",
              x = "Optimal Early Warning Lags", startRow = 1)
    writeData(wb, "Leading Indicators", x = lead_summary, startRow = 3,
              headerStyle = createStyle(textDecoration = "bold", fgFill = "#E8F5E9",
                                        border = "TopBottomLeftRight"))
    setColWidths(wb, "Leading Indicators", cols = 1:4, widths = c(28, 16, 16, 16))

    # Sheet 3: Vulnerability
    addWorksheet(wb, "Vulnerability Scores")
    writeData(wb, "Vulnerability Scores",
              x = "Category Vulnerability to Macro Stress", startRow = 1)
    writeData(wb, "Vulnerability Scores", x = vuln_score, startRow = 3,
              headerStyle = createStyle(textDecoration = "bold", fgFill = "#FFF3E0",
                                        border = "TopBottomLeftRight"))
    setColWidths(wb, "Vulnerability Scores", cols = 1:4, widths = c(18, 14, 18, 14))

    xlsx_path <- file.path(RESULT_DIR, "policy_analysis_summary.xlsx")
    saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    message(sprintf("  Excel saved: %s", xlsx_path))
  }, error = function(e) message(sprintf("  [EXCEL WARN] %s", conditionMessage(e))))
}

# ════════════════════════════════════════════════════════════
# 9. SUMMARY
# ════════════════════════════════════════════════════════════
message("\n============================================================")
message("  POLICY ANALYSIS COMPLETE")
message("============================================================")
message("")
message("  7A — SENSITIVITY: 1-SD shock impacts")
message(sprintf("    Most impactful: %s (%.2f pp avg)",
                sens_avg$macro_theme[1], sens_avg$avg_impact[1]))
message("")
message("  7B — LEADING INDICATORS: Early warning lags")
message(sprintf("    Best early warning: %s (%s quarter lead, r=%.2f)",
                lead_summary$macro_theme[1],
                lead_summary$avg_optimal_lag[1],
                lead_summary$avg_lead_corr[1]))
message("")
message("  7C — VULNERABILITY: Category stress scores")
message(sprintf("    Most vulnerable: %s (%.2f pp avg sensitivity)",
                vuln_score$cat_label[1], vuln_score$vulnerability[1]))
message("")
message("  7D — MIGRATION: Category composition trends")
message("    Small CUs declining → large CUs gaining share")
message("")
if (length(exit_vars_avail) > 0) {
  message("  7E — TIPPING POINT: Exit rate by size")
  message(sprintf("    Highest merger pressure: %s",
                  exit_profile$cat_label[which.max(exit_profile$merger_rate)]))
}
message("")
message(sprintf("  Charts: %s/ (P1-P10)", PLOT_DIR))
message(sprintf("  Data:   %s/", RESULT_DIR))
message("============================================================")
