############################################################
# SLIDE GRAPHIC 2 — Top Call Report Drivers with Lead Times
#
# Mirror of S1 but for call report variables. Computes lead
# time per call report variable as the lag (0..MAX_LAG quarters)
# at which average correlation with future CU growth is highest.
#
# Inputs : results_5_callreport/cr_ensemble_ranking.csv
#          qtrly_enriched_v3.rds  (panel with all CR vars + targets)
#
# Output : plots_slides/S2_cr_drivers_leadtimes.pdf/png
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

DATA_DIR <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR <- "plots_slides"
setwd(DATA_DIR)
dir.create(PLOT_DIR, showWarnings = FALSE)

MAX_LAG <- 8L  # quarters to test for lead time

# ════════════════════════════════════════════════════════════
# 1. LOAD INPUTS
# ════════════════════════════════════════════════════════════
imp_path   <- "results_5_callreport/cr_ensemble_ranking.csv"
panel_path <- "qtrly_enriched_v3.rds"

if (!file.exists(imp_path))
  stop(sprintf("Run Part 5 (PATCHED) first — missing %s", imp_path))
if (!file.exists(panel_path))
  stop(sprintf("Missing panel file %s — run Part 1 first", panel_path))

imp   <- fread(imp_path)
panel <- as.data.table(readRDS(panel_path))

message(sprintf("Loaded ranking : %d rows | columns: %s",
                nrow(imp), paste(names(imp), collapse=", ")))
message(sprintf("Loaded panel   : %d rows | %d columns",
                nrow(panel), ncol(panel)))

# ════════════════════════════════════════════════════════════
# 2. COMPUTE LEAD TIME PER CALL REPORT VARIABLE
# ════════════════════════════════════════════════════════════
# For each (cr_var, target) pair and each lag 0..MAX_LAG, compute
# correlation between cr_var(t) and target(t + lag). Pick the lag
# at which AVERAGE |cor| across the 4 targets is highest.

target_vars <- c("yoy_fcu_pct", "yoy_fiscu_pct",
                 "yoy_fcu_assets_pct", "yoy_fiscu_assets_pct")

# Aggregate panel to system level (system-wide quarterly series)
# This mirrors what Part 10 does for macro variables
sys <- panel[, lapply(.SD, function(x) {
              if (is.numeric(x)) sum(x, na.rm = TRUE) else x[1]
            }),
            by = date,
            .SDcols = setdiff(names(panel),
                              c("date", "categories", "cat_label",
                                "q1", "q2", "q3", "q4", "qtr"))]
setorder(sys, date)

# But for growth variables we need MEAN not SUM at system level
# Actually growth rates already aggregated at panel level — use the
# category-1 row (or any single category) as proxy. Cleanest: take
# the first non-NA value per date for growth variables.
# Better approach: re-aggregate from panel keeping growth as panel-mean.
sys_means <- panel[, lapply(.SD, function(x) {
                if (is.numeric(x)) mean(x, na.rm = TRUE) else x[1]
              }),
              by = date,
              .SDcols = setdiff(names(panel),
                                c("date", "categories", "cat_label",
                                  "q1", "q2", "q3", "q4", "qtr"))]
setorder(sys_means, date)

# Compute lead time only for variables that appear in importance ranking
imp_vars <- intersect(imp$variable, names(sys_means))
message(sprintf("Computing lead times for %d call report variables...", length(imp_vars)))

compute_best_lead <- function(cr_var) {
  x <- as.numeric(sys_means[[cr_var]])
  best_lag  <- 0L
  best_avg  <- 0
  for (lg in 0:MAX_LAG) {
    rs <- numeric(0)
    for (tv in target_vars) {
      if (!tv %in% names(sys_means)) next
      y <- as.numeric(sys_means[[tv]])
      n <- length(x)
      if (n - lg < 15L) next
      x_now <- x[1:(n - lg)]
      y_fut <- y[(1 + lg):n]
      ok <- !is.na(x_now) & !is.na(y_fut)
      if (sum(ok) < 15L) next
      r <- suppressWarnings(cor(x_now[ok], y_fut[ok]))
      if (!is.na(r)) rs <- c(rs, abs(r))
    }
    if (length(rs) == 0) next
    avg_r <- mean(rs)
    if (avg_r > best_avg) {
      best_avg <- avg_r
      best_lag <- lg
    }
  }
  list(best_lead_q = best_lag, best_abs_corr = best_avg)
}

leads <- rbindlist(lapply(imp_vars, function(v) {
  res <- compute_best_lead(v)
  data.table(variable = v, best_lead_q = res$best_lead_q,
             best_abs_corr = res$best_abs_corr)
}))

# Merge importance with lead time
imp <- merge(imp, leads, by = "variable", all.x = TRUE)
imp[is.na(best_lead_q), best_lead_q := 0L]

# ════════════════════════════════════════════════════════════
# 3. LABEL & THEME CR VARIABLES
# ════════════════════════════════════════════════════════════
# Use Part 5's existing var_clean if present
if (!"var_clean" %in% names(imp)) {
  imp[, var_clean := gsub("_", " ", variable)]
}

# Polish labels for executive readability
prettify_cr <- function(v) {
  v <- gsub("_", " ", v)
  v <- tools::toTitleCase(v)
  v <- gsub("\\bYoy\\b",   "YoY",   v)
  v <- gsub("\\bQoq\\b",   "QoQ",   v)
  v <- gsub("\\bCu\\b",    "CU",    v)
  v <- gsub("\\bRoa\\b",   "ROA",   v)
  v <- gsub("\\bRoe\\b",   "ROE",   v)
  v <- gsub("\\bNcua\\b",  "NCUA",  v)
  v <- gsub("\\bFcu\\b",   "FCU",   v)
  v <- gsub("\\bFiscu\\b", "FISCU", v)
  v <- gsub("\\bHhi\\b",   "HHI",   v)
  v <- gsub("\\bNim\\b",   "NIM",   v)
  v <- gsub("\\bLtv\\b",   "LTV",   v)
  v <- gsub("\\bDq\\b",    "DQ",    v)
  v
}
imp[, label := vapply(var_clean, prettify_cr, character(1))]

# Theme classification — use existing theme if present, else assign
classify_cr_theme <- function(v) {
  vl <- tolower(v)
  if (grepl("delinq|deling|chargeoff|charge_off|nonperf|loss",      vl)) return("Credit Risk")
  if (grepl("net_worth|capital|cap_adeq|leverage_ratio|risk_based", vl)) return("Capital & Solvency")
  if (grepl("loan_to_share|loan_to_asset|investment|securit|liquid",vl)) return("Asset Composition")
  if (grepl("nim|net_interest|spread_inc|interest_inc",             vl)) return("Net Interest Margin")
  if (grepl("non_int_inc|fee_inc|service_charge",                   vl)) return("Non-Interest Income")
  if (grepl("op_exp|efficiency|overhead|expense_ratio",             vl)) return("Operating Efficiency")
  if (grepl("roa|roe|return_on|earnings|profit",                    vl)) return("Profitability")
  if (grepl("members?|membership|fom_",                             vl)) return("Membership")
  if (grepl("merger|liquid|acquis|exit",                            vl)) return("Exit Dynamics")
  if (grepl("net_entry|new_charter",                                vl)) return("New Entry")
  if (grepl("loan|loans",                                           vl)) return("Lending Activity")
  if (grepl("share|deposit",                                        vl)) return("Funding")
  return("Other Operational")
}

if (!"theme" %in% names(imp) || all(is.na(imp$theme))) {
  imp[, theme := vapply(variable, classify_cr_theme, character(1))]
} else {
  # Use Part 5 theme but fall back to our classifier for "Other CU Variables"
  imp[, theme := ifelse(is.na(theme) | theme == "" | theme == "Other CU Variables",
                         vapply(variable, classify_cr_theme, character(1)),
                         theme)]
}

# ════════════════════════════════════════════════════════════
# 4. PICK TOP 10
# ════════════════════════════════════════════════════════════
setorderv(imp, "mean_importance", order = -1L)
top10 <- head(imp, 10)

# Detect importance scale
imp_max_val <- max(top10$mean_importance, na.rm = TRUE)
top10[, imp_pct := if (imp_max_val <= 1) mean_importance * 100 else mean_importance]

# Truncate long labels
top10[, label_short := ifelse(nchar(label) > 48,
                               paste0(substr(label, 1, 46), "…"),
                               label)]
top10[, label_short := factor(label_short, levels = rev(label_short))]
top10[, annotation := sprintf("%.0f%% • +%dQ lead", imp_pct, best_lead_q)]

# Console output
message("\nTop 10 Call Report Drivers:")
for (i in 1:nrow(top10)) {
  message(sprintf("  %2d. %-40s  %5.1f%%  +%dQ lead  (%s)",
                  i,
                  as.character(top10$label_short[i]),
                  top10$imp_pct[i],
                  top10$best_lead_q[i],
                  top10$theme[i]))
}

# ════════════════════════════════════════════════════════════
# 5. BUILD CHART (matches S1 styling)
# ════════════════════════════════════════════════════════════
theme_colors <- c(
  "Credit Risk"           = "#E76F51",
  "Capital & Solvency"    = "#5B9BD5",
  "Asset Composition"     = "#3D6FBF",
  "Net Interest Margin"   = "#52B788",
  "Non-Interest Income"   = "#90C088",
  "Operating Efficiency"  = "#F4A261",
  "Profitability"         = "#E8A838",
  "Membership"            = "#2EC4B6",
  "Lending Activity"      = "#7B68A8",
  "Funding"               = "#C77DFF",
  "Exit Dynamics"         = "#B5651D",
  "New Entry"             = "#6FAE6F",
  "Other Operational"     = "#999999"
)

# Only keep themes that appear (legend tidiness)
present_themes <- unique(top10$theme)
theme_colors_used <- theme_colors[names(theme_colors) %in% present_themes]
# Add fallback for any new themes
unmapped <- setdiff(present_themes, names(theme_colors))
if (length(unmapped) > 0) {
  fallback_palette <- c("#9C6644", "#6A994E", "#386641", "#BC4749", "#A7C957")
  for (i in seq_along(unmapped)) {
    theme_colors_used[unmapped[i]] <- fallback_palette[(i - 1) %% length(fallback_palette) + 1]
  }
}

p <- ggplot(top10, aes(x = imp_pct, y = label_short, fill = theme)) +
  geom_col(width = 0.65, alpha = 0.92) +
  geom_text(aes(label = annotation), hjust = -0.05,
            size = 3.4, fontface = "bold", color = "#333333") +
  scale_fill_manual(values = theme_colors_used, name = "Theme") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.32))) +
  labs(
    title    = "Top Call Report Drivers by Importance and Lead Time",
    subtitle = "How well each CU-specific operational variable explains future CU growth, and how far ahead the signal arrives\nLead time = quarters the call report variable leads CU response",
    x        = "Importance Score (R² × 100)",
    y        = NULL,
    caption  = "Source: Ensemble ML (Ridge, LASSO, Elastic Net, Random Forest) across 14 forecast targets  |  Endogenous variables (lagged growth, market share, count/asset levels) excluded"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#0B1D3A"),
    plot.subtitle = element_text(size = 11, color = "#666666", margin = margin(b = 12)),
    plot.caption  = element_text(size = 8, color = "#999999", hjust = 0),
    axis.text.y   = element_text(face = "bold", size = 10, color = "#333333"),
    axis.text.x   = element_text(size = 9, color = "#666666"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.4),
    legend.position = "bottom",
    legend.text  = element_text(size = 9),
    legend.title = element_text(size = 9, face = "bold"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 25, 15, 15)
  )

# ── Save ─────────────────────────────────────────────────
pdf_path <- file.path(PLOT_DIR, "S2_cr_drivers_leadtimes.pdf")
png_path <- file.path(PLOT_DIR, "S2_cr_drivers_leadtimes.png")

tryCatch({
  grDevices::cairo_pdf(pdf_path, width = 13, height = 7.5)
  print(p); grDevices::dev.off()
}, error = function(e) {
  try(grDevices::dev.off(), silent = TRUE)
  grDevices::pdf(pdf_path, width = 13, height = 7.5)
  print(p); grDevices::dev.off()
})

ggsave(png_path, p, width = 13, height = 7.5, dpi = 200, bg = "white")

message(sprintf("\nSaved: %s", pdf_path))
message(sprintf("Saved: %s", png_path))
