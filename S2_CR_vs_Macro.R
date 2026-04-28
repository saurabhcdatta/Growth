############################################################
# SLIDE GRAPHIC 2 — Call Report vs Macroeconomic Drivers
#
# Side-by-side comparison: top 5 call report drivers (left)
# vs top 5 macro drivers (right). Uses the FRB Variable Data
# Dictionary for proper macro labels.
#
# Inputs : results_4_ensemble/ensemble_overall_ranking.csv
#            (columns: base_var, mean_importance, display_name, ...)
#          results_5_callreport/cr_ensemble_ranking.csv
#            (columns: variable, var_clean, theme, mean_importance, ...)
#          FRB_Variable_Data_Dictionary.xlsx
#
# Output : plots_slides/S2_drivers_cr_vs_macro.pdf/png
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

# Optional: patchwork preferred, fall back to gridExtra
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
if (has_patchwork) library(patchwork) else library(gridExtra)

DATA_DIR  <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR  <- "plots_slides"
DICT_PATH <- "FRB_Variable_Data_Dictionary.xlsx"
setwd(DATA_DIR)
dir.create(PLOT_DIR, showWarnings = FALSE)

# ════════════════════════════════════════════════════════════
# 1. LOAD FRB DATA DICTIONARY (macro labels)
# ════════════════════════════════════════════════════════════
if (!file.exists(DICT_PATH))
  stop(sprintf("Dictionary not found at %s — copy the file to %s",
               DICT_PATH, DATA_DIR))

# VAR_MAP sheet — has FRB Code, Short Name, Description
varmap <- as.data.table(read_excel(DICT_PATH, sheet = "VAR_MAP"))
setnames(varmap, c("frb_code", "short_name", "description", "in_curated", "macro_name"))

# Forward-fill section headers (manual LOCF since nafill is numeric-only)
varmap[, is_section := !is.na(frb_code) & (is.na(short_name) | short_name == "")]
varmap[, section    := fifelse(is_section, frb_code, NA_character_)]
current <- NA_character_
for (i in seq_len(nrow(varmap))) {
  if (!is.na(varmap$section[i])) current <- varmap$section[i]
  else                            varmap$section[i] <- current
}
varmap <- varmap[!is_section & !is.na(short_name) & short_name != ""]

# Derived series sheet
derived <- as.data.table(read_excel(DICT_PATH, sheet = "Derived_Series"))
setnames(derived, c("short_name", "description", "macro_name"))
derived[, frb_code   := NA_character_]
derived[, in_curated := NA_character_]
derived[, section    := "DERIVED"]
derived[, is_section := FALSE]

# Section -> theme mapping
section_to_theme <- function(s) {
  if (is.na(s)) return("Other")
  s <- toupper(s)
  if (grepl("CREDIT QUAL", s))                  return("Credit Conditions")
  if (grepl("CONSUMER CREDIT|DEBT", s))         return("Credit Conditions")
  if (grepl("FORWARD", s))                      return("Interest Rates")
  if (grepl("RATES|MONETARY", s))               return("Interest Rates")
  if (grepl("SPREAD", s))                       return("Spreads")
  if (grepl("LABOR|EMPLOY", s))                 return("Labour & Income")
  if (grepl("HOUSEHOLD FINANCE", s))            return("Labour & Income")
  if (grepl("OUTPUT|ACTIVITY", s))              return("Economic Activity")
  if (grepl("PRICE|INFLATION", s))              return("Inflation")
  if (grepl("HOUSING", s))                      return("Housing")
  if (grepl("FINANCIAL MARKET", s))             return("Financial Markets")
  if (grepl("SENTIMENT", s))                    return("Sentiment")
  if (grepl("DERIVED", s))                      return("Derived")
  return("Other")
}

varmap[, theme := vapply(section, section_to_theme, character(1))]

# Classify derived series via content (since they're all section "DERIVED")
classify_derived <- function(v) {
  v <- tolower(v)
  if (grepl("baa|credit_tight|bbb|delinq|chargeoff|leverage|bankrupt|hy_", v))
    return("Credit Conditions")
  if (grepl("yield_curve|spread", v))           return("Spreads")
  if (grepl("fedfunds|gs[0-9]|treas|mortgage|sofr|prime|fwd|fomc|hike|real_rate|tbill|cmt|mpe", v))
    return("Interest Rates")
  if (grepl("unrate|nairu|lfpr|labor|claims|unemp|ui_benefit", v))
    return("Labour & Income")
  if (grepl("income|savings|wage|profits|disp_|labor_inc|interest_pymts|dividend_inc|net_int", v))
    return("Labour & Income")
  if (grepl("gdp|indpro|pce|consumption|fixed_invest|cap_stock", v))
    return("Economic Activity")
  if (grepl("cpi|ppi|inflation|deflator|infl_target|oil|brent|tax_wedge", v))
    return("Inflation")
  if (grepl("housing|hpi|rent|home_price|permits|cs20|fhfa|foreclos|cre_price", v))
    return("Housing")
  if (grepl("sp500|djia|nasdaq|msci|mktcap", v)) return("Financial Markets")
  if (grepl("revolving|nonrev|total_credit|mortgage_debt", v))
    return("Credit Conditions")
  if (grepl("confidence|sentiment", v))         return("Sentiment")
  return("Other")
}

derived[, theme := vapply(short_name, classify_derived, character(1))]

# Master lookup
short_lookup <- rbindlist(list(
  varmap[,  .(short_name, frb_code, description, theme)],
  derived[, .(short_name, frb_code, description, theme)]
), use.names = TRUE)
setkey(short_lookup, short_name)

frb_lookup <- short_lookup[!is.na(frb_code),
                            .(frb_code, short_name, description, theme)]

message(sprintf("Loaded dictionary: %d short_name entries (%d with FRB code)",
                nrow(short_lookup), nrow(frb_lookup)))

# ════════════════════════════════════════════════════════════
# 2. LABEL RESOLVER FOR MACRO VARIABLES
# ════════════════════════════════════════════════════════════
resolve_macro_label <- function(b) {
  if (is.na(b) || b == "") return(list(desc = NA_character_, theme = "Other"))
  bl <- tolower(b)
  hit <- short_lookup[tolower(short_name) == bl]
  if (nrow(hit) > 0) return(list(desc = hit$description[1], theme = hit$theme[1]))
  hit <- frb_lookup[toupper(frb_code) == toupper(b)]
  if (nrow(hit) > 0) return(list(desc = hit$description[1], theme = hit$theme[1]))
  cleaned <- bl
  cleaned <- gsub("^macro_", "", cleaned)
  cleaned <- gsub("^yoy_", "", cleaned)
  cleaned <- gsub("^qoq_", "", cleaned)
  cleaned <- gsub("_lag[0-9]+$", "", cleaned)
  cleaned <- gsub("_rmean[0-9]+$", "", cleaned)
  cleaned <- gsub("_rsd[0-9]+$", "", cleaned)
  cleaned <- gsub("_cyc$", "", cleaned)
  cleaned <- gsub("_chg$", "", cleaned)
  cleaned <- gsub("_accel$", "", cleaned)
  cleaned <- gsub("_trail[0-9]+$", "", cleaned)
  if (cleaned != bl) {
    hit <- short_lookup[tolower(short_name) == cleaned]
    if (nrow(hit) > 0) return(list(desc = hit$description[1], theme = hit$theme[1]))
  }
  return(list(desc = tools::toTitleCase(gsub("_", " ", b)),
              theme = classify_derived(b)))
}

# ════════════════════════════════════════════════════════════
# 3. LOAD RANKINGS
# ════════════════════════════════════════════════════════════
macro_path <- "results_4_ensemble/ensemble_overall_ranking.csv"
cr_path    <- "results_5_callreport/cr_ensemble_ranking.csv"

if (!file.exists(macro_path)) stop(sprintf("Missing: %s", macro_path))
if (!file.exists(cr_path))    stop(sprintf("Missing: %s", cr_path))

macro_imp <- fread(macro_path)
cr_imp    <- fread(cr_path)

message(sprintf("Macro ranking: %d rows | columns: %s",
                nrow(macro_imp), paste(names(macro_imp), collapse=", ")))
message(sprintf("CR ranking:    %d rows | columns: %s",
                nrow(cr_imp), paste(names(cr_imp), collapse=", ")))

# ════════════════════════════════════════════════════════════
# 4. RESOLVE MACRO LABELS via dictionary
# ════════════════════════════════════════════════════════════
macro_imp[, c("desc","theme") := {
  res <- lapply(base_var, resolve_macro_label)
  list(vapply(res, function(x) x$desc,  character(1)),
       vapply(res, function(x) x$theme, character(1)))
}]

# ════════════════════════════════════════════════════════════
# 5. CALL REPORT LABELS
# ════════════════════════════════════════════════════════════
# Part 5 already produces var_clean (display name) + theme.
# We just lightly polish the labels for chart readability.
if (!"var_clean" %in% names(cr_imp)) {
  cr_imp[, var_clean := gsub("_", " ", variable)]
}

prettify_cr <- function(v) {
  v <- gsub("_", " ", v)
  # Title-case but keep acronyms
  v <- tools::toTitleCase(v)
  v <- gsub("\\bYoy\\b", "YoY", v)
  v <- gsub("\\bCu\\b",  "CU",  v)
  v <- gsub("\\bRoa\\b", "ROA", v)
  v <- gsub("\\bNcua\\b","NCUA",v)
  v <- gsub("\\bFcu\\b", "FCU", v)
  v <- gsub("\\bFiscu\\b","FISCU",v)
  v <- gsub("\\bHhi\\b", "HHI", v)
  v
}

cr_imp[, label := vapply(var_clean, prettify_cr, character(1))]

# ════════════════════════════════════════════════════════════
# 6. PICK TOP 5 FROM EACH AND PREP FOR CHART
# ════════════════════════════════════════════════════════════
setorderv(macro_imp, "mean_importance", order = -1L)
setorderv(cr_imp,    "mean_importance", order = -1L)

top5_macro <- head(macro_imp, 5)
top5_cr    <- head(cr_imp,    5)

# Detect importance scale (0-1 vs 0-100) and convert to %
m_max <- max(top5_macro$mean_importance, na.rm = TRUE)
c_max <- max(top5_cr$mean_importance,    na.rm = TRUE)
top5_macro[, imp_pct := if (m_max <= 1) mean_importance * 100 else mean_importance]
top5_cr[,    imp_pct := if (c_max <= 1) mean_importance * 100 else mean_importance]

# Truncate long labels
trunc_lbl <- function(s, n = 36) {
  ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "…"), s)
}
top5_macro[, var_display := trunc_lbl(desc)]
top5_cr[,    var_display := trunc_lbl(label)]

# Common x-axis max
xmax <- max(c(top5_macro$imp_pct, top5_cr$imp_pct)) * 1.18
xmax <- ceiling(xmax / 5) * 5  # round up to nearest 5

# Print to console
message("\nTop 5 Call Report Drivers:")
for (i in 1:nrow(top5_cr)) {
  message(sprintf("  %d. %-38s  %5.1f%%", i,
                  top5_cr$var_display[i], top5_cr$imp_pct[i]))
}
message("\nTop 5 Macro Drivers:")
for (i in 1:nrow(top5_macro)) {
  message(sprintf("  %d. %-38s  %5.1f%%  (%s)", i,
                  top5_macro$var_display[i], top5_macro$imp_pct[i],
                  top5_macro$theme[i]))
}

# ════════════════════════════════════════════════════════════
# 7. BUILD CHART
# ════════════════════════════════════════════════════════════
CORAL <- "#E76F51"
SKY   <- "#5B9BD5"
NAVY  <- "#0B1D3A"

build_side_plot <- function(dt, fill_color, title_text) {
  ggplot(dt, aes(x = imp_pct, y = var_display)) +
    geom_col(fill = fill_color, width = 0.6, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.0f%%", imp_pct)),
              hjust = -0.1, size = 4, fontface = "bold", color = "#333333") +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       limits = c(0, xmax),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = title_text,
         x = "Importance Score (R² × 100)",
         y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title          = element_text(face = "bold", size = 14, color = "white",
                                          hjust = 0, margin = margin(t = 8, b = 8, l = 12)),
      plot.title.position = "plot",
      axis.text.y         = element_text(face = "bold", size = 10.5, color = "#333333"),
      axis.text.x         = element_text(size = 9, color = "#666666"),
      axis.title.x        = element_text(size = 10, color = "#666666",
                                          margin = margin(t = 5)),
      panel.grid.minor    = element_blank(),
      panel.grid.major.y  = element_blank(),
      panel.grid.major.x  = element_line(color = "#EEEEEE", linewidth = 0.4),
      legend.position     = "none",
      plot.background     = element_rect(fill = fill_color, color = NA),
      panel.background    = element_rect(fill = "white", color = NA),
      plot.margin         = margin(0, 0, 0, 0)
    )
}

top5_cr[,    var_display := factor(var_display, levels = rev(var_display))]
top5_macro[, var_display := factor(var_display, levels = rev(var_display))]

p_cr    <- build_side_plot(top5_cr,    CORAL, "Call Report Variables  (CU-specific)")
p_macro <- build_side_plot(top5_macro, SKY,   "Macroeconomic Variables  (system-wide)")

# ── Combine ──────────────────────────────────────────────
if (has_patchwork) {
  combined <- (p_cr | p_macro) +
    plot_annotation(
      title    = "What Drives CU Growth? — Call Report vs Macroeconomic Variables",
      subtitle = "Top 5 drivers from each variable family ranked by importance score",
      caption  = "Source: Ensemble ML (Ridge, LASSO, Elastic Net, Random Forest)  |  Macro labels from FRB Variable Data Dictionary  |  Call report variables explain more, but macro variables have future values — the rationale for proxy mapping",
      theme = theme(
        plot.title    = element_text(face = "bold", size = 17, color = NAVY,
                                      margin = margin(b = 4)),
        plot.subtitle = element_text(size = 11, color = "#666666",
                                      margin = margin(b = 14)),
        plot.caption  = element_text(size = 8.5, color = "#999999", hjust = 0,
                                      margin = margin(t = 12)),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 25, 15, 20)
      )
    )
} else {
  combined <- gridExtra::arrangeGrob(
    p_cr, p_macro, ncol = 2,
    top = grid::textGrob(
      "What Drives CU Growth? — Call Report vs Macroeconomic Variables",
      gp = grid::gpar(fontsize = 17, fontface = "bold", col = NAVY))
  )
}

# ── Save ─────────────────────────────────────────────────
pdf_path <- file.path(PLOT_DIR, "S2_drivers_cr_vs_macro.pdf")
png_path <- file.path(PLOT_DIR, "S2_drivers_cr_vs_macro.png")

tryCatch({
  grDevices::cairo_pdf(pdf_path, width = 14, height = 7)
  if (has_patchwork) print(combined) else grid::grid.draw(combined)
  grDevices::dev.off()
}, error = function(e) {
  try(grDevices::dev.off(), silent = TRUE)
  grDevices::pdf(pdf_path, width = 14, height = 7)
  if (has_patchwork) print(combined) else grid::grid.draw(combined)
  grDevices::dev.off()
})

ggsave(png_path, combined, width = 14, height = 7, dpi = 200, bg = "white")

message(sprintf("\nSaved: %s", pdf_path))
message(sprintf("Saved: %s", png_path))
