############################################################
# SLIDE GRAPHIC 1 — Top Macro Drivers with Lead Times
#
# Inputs : results_4_ensemble/ensemble_overall_ranking.csv
#          results_10_proxy/proxy_all_correlations.csv
#
# Output : plots_slides/S1_macro_drivers_leadtimes.pdf
#          plots_slides/S1_macro_drivers_leadtimes.png
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

# ── Load Part 10 correlation results for lead time ───────
prx_path <- "results_10_proxy/proxy_all_correlations.csv"
if (!file.exists(prx_path))
  stop(sprintf("File not found: %s — run Part 10 first.", prx_path))
prx <- fread(prx_path)

# ── Compute optimal lead per macro variable ──────────────
# Average across all CR targets, find lag with highest |correlation|
prx_agg <- prx[, .(avg_r2 = mean(r_squared, na.rm = TRUE)),
               by = .(macro_var, lag)]
best_lag <- prx_agg[, .SD[which.max(avg_r2)], by = macro_var]
setnames(best_lag, "lag", "best_lead_q")

# ── Merge importance with best lag ───────────────────────
top <- merge(imp, best_lag[, .(variable = macro_var, best_lead_q)],
             by = "variable", all.x = TRUE)
top[is.na(best_lead_q), best_lead_q := 0L]

# Take top 10 by importance
setorderv(top, "mean_importance", order = -1L)
top10 <- head(top, 10)

# ── Theme classification ─────────────────────────────────
classify_macro <- function(v) {
  v <- tolower(gsub("^macro_", "", v))
  if (grepl("baa|credit_tight|bbb|spread", v))   return("Credit Conditions")
  if (grepl("fedfunds|gs[0-9]|yield|mortgage|sofr|prime|fwd|fomc", v)) return("Interest Rates")
  if (grepl("unrate|disp_income|labor|lfpr|nairu|claims|employ", v))   return("Labour & Income")
  if (grepl("gdp|cons_confidence|indpro|pce|consumption|invest", v))   return("Economic Activity")
  if (grepl("cpi|ppi|inflation|deflator", v))    return("Inflation")
  if (grepl("housing|hpi|rent|home_price", v))   return("Housing")
  if (grepl("sp500|djia|nasdaq", v))             return("Financial Markets")
  return("Other")
}

top10[, theme := vapply(variable, classify_macro, character(1))]

# Clean display names
clean_name <- function(v) {
  v <- gsub("^macro_", "", v)
  v <- gsub("_", " ", v)
  v <- gsub("\\b([a-z])", "\\U\\1", v, perl = TRUE)
  v
}
top10[, var_display := vapply(variable, clean_name, character(1))]

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
top10[, label := sprintf("%.0f%% • +%dQ lead",
                          mean_importance * 100, best_lead_q)]
top10[, var_display := factor(var_display, levels = rev(var_display))]

# Convert importance to 0-100 scale for display
top10[, imp_pct := mean_importance * 100]

p <- ggplot(top10, aes(x = imp_pct, y = var_display, fill = theme)) +
  geom_col(width = 0.65, alpha = 0.92) +
  geom_text(aes(label = label), hjust = -0.05,
            size = 3.4, fontface = "bold", color = "#333333") +
  scale_fill_manual(values = theme_colors, name = "Theme") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.32))) +
  labs(
    title = "Top Macro Drivers by Importance and Lead Time",
    subtitle = "How well each macro variable explains CU growth, and how far ahead the signal arrives\nLead time = quarters the macro variable leads CU response",
    x = "Importance Score (R² × 100)",
    y = NULL,
    caption = "Source: Ensemble ML (Ridge, LASSO, Elastic Net, Random Forest) across 14 forecast targets"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#0B1D3A"),
    plot.subtitle = element_text(size = 11, color = "#666666", margin = margin(b = 12)),
    plot.caption = element_text(size = 8, color = "#999999", hjust = 0),
    axis.text.y = element_text(face = "bold", size = 10, color = "#333333"),
    axis.text.x = element_text(size = 9, color = "#666666"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.4),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
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

message(sprintf("Saved: %s", pdf_path))
message(sprintf("Saved: %s", png_path))

# ── Print top 10 to console ──────────────────────────────
message("\nTop 10 Macro Drivers:")
for (i in 1:nrow(top10)) {
  message(sprintf("  %2d. %-30s  %5.1f%%  +%dQ lead  (%s)",
                  i, top10$var_display[i], top10$imp_pct[i],
                  top10$best_lead_q[i], top10$theme[i]))
}
