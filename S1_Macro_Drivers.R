############################################################
# SLIDE GRAPHIC 1 — Top Macro Drivers with Lead Times
#
# Inputs : results_4_ensemble/ensemble_overall_ranking.csv
#            (columns: base_var, mean_importance, display_name, ...)
#          results_10_proxy/proxy_all_correlations.csv
#            (columns: cr_var, macro_var, lag, r_squared, ...)
#
# Output : plots_slides/S1_macro_drivers_leadtimes.pdf/png
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

# ── Load Part 4 importance ranking ───────────────────────
imp_path <- "results_4_ensemble/ensemble_overall_ranking.csv"
if (!file.exists(imp_path))
  stop(sprintf("File not found: %s — run Part 4 first.", imp_path))
imp <- fread(imp_path)
message(sprintf("Loaded importance ranking: %d rows", nrow(imp)))
message(sprintf("  Columns: %s", paste(names(imp), collapse=", ")))

# ── Load Part 10 correlation results for lead time ───────
prx_path <- "results_10_proxy/proxy_all_correlations.csv"
if (!file.exists(prx_path))
  stop(sprintf("File not found: %s — run Part 10 first.", prx_path))
prx <- fread(prx_path)
message(sprintf("Loaded proxy correlations: %d rows", nrow(prx)))

# ── Helper: extract base variable from full macro var name ──
# Mirrors Part 4's get_base_var so merge keys match
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

# ── Compute optimal lead per BASE macro variable ─────────
# Add base_var to proxy data, then average r² across all CR targets
# AND across all transformations of the same base variable.
# Find the lag with highest avg_r² for each base variable.
prx[, base_var := get_base_var(macro_var)]

prx_agg <- prx[, .(avg_r2 = mean(r_squared, na.rm = TRUE)),
               by = .(base_var, lag)]
best_lag <- prx_agg[, .SD[which.max(avg_r2)], by = base_var]
setnames(best_lag, "lag", "best_lead_q")

# ── Merge importance with best lag ───────────────────────
top <- merge(imp, best_lag[, .(base_var, best_lead_q)],
             by = "base_var", all.x = TRUE)
top[is.na(best_lead_q), best_lead_q := 0L]

# Top 10 by importance
setorderv(top, "mean_importance", order = -1L)
top10 <- head(top, 10)

# Use display_name from Part 4 if present, else build one
if (!"display_name" %in% names(top10)) {
  top10[, display_name := gsub("_", " ", base_var)]
}

# ── Theme classification ─────────────────────────────────
classify_macro <- function(v) {
  v <- tolower(v)
  if (grepl("baa|credit_tight|bbb|spread", v))   return("Credit Conditions")
  if (grepl("fedfunds|gs[0-9]|yield|mortgage|sofr|prime|fwd|fomc|hike|real_rate", v))
    return("Interest Rates")
  if (grepl("unrate|disp_income|labor|lfpr|nairu|claims|employ|savings", v))
    return("Labour & Income")
  if (grepl("gdp|cons_confidence|indpro|pce|consumption|invest|bankrupt", v))
    return("Economic Activity")
  if (grepl("cpi|ppi|inflation|deflator", v))    return("Inflation")
  if (grepl("housing|hpi|rent|home_price|permits", v)) return("Housing")
  if (grepl("sp500|djia|nasdaq", v))             return("Financial Markets")
  return("Other")
}

top10[, theme := vapply(base_var, classify_macro, character(1))]

# ── Color palette by theme ───────────────────────────────
theme_colors <- c(
  "Credit Conditions"  = "#E76F51",
  "Interest Rates"     = "#5B9BD5",
  "Labour & Income"    = "#2EC4B6",
  "Economic Activity"  = "#52B788",
  "Inflation"          = "#F4A261",
  "Housing"            = "#7B68A8",
  "Financial Markets"  = "#E8A838",
  "Other"              = "#999999"
)

# ── Build chart ──────────────────────────────────────────
# Detect importance scale: 0-1 vs 0-100
imp_max <- max(top10$mean_importance, na.rm = TRUE)
top10[, imp_pct := if (imp_max <= 1) mean_importance * 100 else mean_importance]

top10[, label := sprintf("%.0f%% • +%dQ lead", imp_pct, best_lead_q)]
top10[, display_name := factor(display_name, levels = rev(display_name))]

p <- ggplot(top10, aes(x = imp_pct, y = display_name, fill = theme)) +
  geom_col(width = 0.65, alpha = 0.92) +
  geom_text(aes(label = label), hjust = -0.05,
            size = 3.4, fontface = "bold", color = "#333333") +
  scale_fill_manual(values = theme_colors, name = "Theme") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.32))) +
  labs(
    title    = "Top Macro Drivers by Importance and Lead Time",
    subtitle = "How well each macro variable explains CU growth, and how far ahead the signal arrives\nLead time = quarters the macro variable leads CU response",
    x        = "Importance Score (R² × 100)",
    y        = NULL,
    caption  = "Source: Ensemble ML (Ridge, LASSO, Elastic Net, Random Forest) across 14 forecast targets"
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
pdf_path <- file.path(PLOT_DIR, "S1_macro_drivers_leadtimes.pdf")
png_path <- file.path(PLOT_DIR, "S1_macro_drivers_leadtimes.png")

tryCatch({
  grDevices::cairo_pdf(pdf_path, width = 12, height = 7.5)
  print(p); grDevices::dev.off()
}, error = function(e) {
  try(grDevices::dev.off(), silent = TRUE)
  grDevices::pdf(pdf_path, width = 12, height = 7.5)
  print(p); grDevices::dev.off()
})

ggsave(png_path, p, width = 12, height = 7.5, dpi = 200, bg = "white")

message(sprintf("\nSaved: %s", pdf_path))
message(sprintf("Saved: %s", png_path))

# ── Print top 10 to console ──────────────────────────────
message("\nTop 10 Macro Drivers:")
for (i in 1:nrow(top10)) {
  message(sprintf("  %2d. %-32s  %5.1f%%  +%dQ lead  (%s)",
                  i, as.character(top10$display_name[i]),
                  top10$imp_pct[i],
                  top10$best_lead_q[i], top10$theme[i]))
}
