############################################################
# SLIDE GRAPHIC 1 — Top Macro Drivers with Lead Times
#
# Uses the FRB_Variable_Data_Dictionary.xlsx to translate
# raw codes/short-names into human-readable labels and themes.
#
# Inputs : results_4_ensemble/ensemble_overall_ranking.csv
#          results_10_proxy/proxy_all_correlations.csv
#          FRB_Variable_Data_Dictionary.xlsx
#
# Output : plots_slides/S1_macro_drivers_leadtimes.pdf/png
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

DATA_DIR <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR <- "plots_slides"
DICT_PATH <- "FRB_Variable_Data_Dictionary.xlsx"
setwd(DATA_DIR)
dir.create(PLOT_DIR, showWarnings = FALSE)

# ════════════════════════════════════════════════════════════
# 1. LOAD DATA DICTIONARY
# ════════════════════════════════════════════════════════════
if (!file.exists(DICT_PATH)) stop(sprintf(
  "Dictionary not found at %s — copy FRB_Variable_Data_Dictionary.xlsx to %s",
  DICT_PATH, DATA_DIR))

# Sheet 1: VAR_MAP — has FRB Code, Short Name, Description, macro_ Panel Name
# Some rows are section headers (FRB Code present, Short Name empty)
varmap <- as.data.table(read_excel(DICT_PATH, sheet = "VAR_MAP"))
setnames(varmap, c("frb_code", "short_name", "description", "in_curated", "macro_name"))

# ── Forward-fill section names ───────────────────────────
# Rows where Short Name is NA but FRB Code has text are section headers.
# Manual LOCF (nafill is numeric-only in data.table).
varmap[, is_section := !is.na(frb_code) & (is.na(short_name) | short_name == "")]
varmap[, section := fifelse(is_section, frb_code, NA_character_)]
current <- NA_character_
for (i in seq_len(nrow(varmap))) {
  if (!is.na(varmap$section[i])) current <- varmap$section[i]
  else                            varmap$section[i] <- current
}
# Drop section header rows now that we've propagated their info
varmap <- varmap[!is_section & !is.na(short_name) & short_name != ""]

# Sheet 2: Derived_Series — composite indicators (no section column)
derived <- as.data.table(read_excel(DICT_PATH, sheet = "Derived_Series"))
setnames(derived, c("short_name", "description", "macro_name"))
derived[, frb_code := NA_character_]
derived[, in_curated := NA_character_]
derived[, section := "DERIVED"]
derived[, is_section := FALSE]

# Map sections to themes
section_to_theme <- function(s) {
  if (is.na(s)) return("Other")
  s <- toupper(s)
  if (grepl("CREDIT QUAL", s))                       return("Credit Conditions")
  if (grepl("CONSUMER CREDIT|DEBT", s))              return("Credit Conditions")
  if (grepl("FORWARD", s))                           return("Interest Rates")
  if (grepl("RATES|MONETARY", s))                    return("Interest Rates")
  if (grepl("SPREAD", s))                            return("Spreads")
  if (grepl("LABOR|EMPLOY", s))                      return("Labour & Income")
  if (grepl("HOUSEHOLD FINANCE", s))                 return("Labour & Income")
  if (grepl("OUTPUT|ACTIVITY", s))                   return("Economic Activity")
  if (grepl("PRICE|INFLATION", s))                   return("Inflation")
  if (grepl("HOUSING", s))                           return("Housing")
  if (grepl("FINANCIAL MARKET", s))                  return("Financial Markets")
  if (grepl("SENTIMENT", s))                         return("Sentiment")
  if (grepl("DERIVED", s))                           return("Derived")  # placeholder
  return("Other")
}

varmap[, theme := vapply(section, section_to_theme, character(1))]

# Build the master lookup (short_name -> description, theme)
short_lookup <- varmap[, .(short_name, frb_code, description, theme)]

# Add derived rows; classify their theme by content (since section is just "DERIVED")
classify_derived_or_unknown <- function(v) {
  v <- tolower(v)
  if (grepl("baa|credit_tight|bbb|delinq|chargeoff|leverage|bankrupt|hy_", v))
    return("Credit Conditions")
  if (grepl("yield_curve|spread", v))             return("Spreads")
  if (grepl("fedfunds|gs[0-9]|treas|mortgage|sofr|prime|fwd|fomc|hike|real_rate|tbill|cmt|prescriptive|mpe", v))
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
  if (grepl("sp500|djia|nasdaq|msci|mktcap", v))    return("Financial Markets")
  if (grepl("revolving|nonrev|total_credit|mortgage_debt", v))
    return("Credit Conditions")
  if (grepl("confidence|sentiment", v))             return("Sentiment")
  return("Other")
}

derived[, theme := vapply(short_name, classify_derived_or_unknown, character(1))]
short_lookup <- rbindlist(list(
  short_lookup,
  derived[, .(short_name, frb_code, description, theme)]
), use.names = TRUE)

setkey(short_lookup, short_name)

# Lookup: from FRB code -> info
frb_lookup <- short_lookup[!is.na(frb_code), .(frb_code, short_name, description, theme)]

message(sprintf("Loaded dictionary: %d short_name entries (%d with FRB code)",
                nrow(short_lookup), nrow(frb_lookup)))

# ════════════════════════════════════════════════════════════
# 2. LOAD PART 4 IMPORTANCE & PART 10 LAGS
# ════════════════════════════════════════════════════════════
imp_path <- "results_4_ensemble/ensemble_overall_ranking.csv"
prx_path <- "results_10_proxy/proxy_all_correlations.csv"
if (!file.exists(imp_path)) stop(sprintf("File not found: %s", imp_path))
if (!file.exists(prx_path)) stop(sprintf("File not found: %s", prx_path))

imp <- fread(imp_path)
prx <- fread(prx_path)
message(sprintf("Importance: %d rows | columns: %s",
                nrow(imp), paste(names(imp), collapse=", ")))

# ── Show top-10 raw to user for debugging ────────────────
message("\n[DIAGNOSTIC] Top 10 base_var values from importance ranking:")
print(head(imp[order(-mean_importance), .(base_var, mean_importance)], 10))

# ════════════════════════════════════════════════════════════
# 3. RESOLVE LABELS — try multiple strategies
# ════════════════════════════════════════════════════════════
# Strategy 1: base_var matches a known short_name (e.g. "fedfunds")
# Strategy 2: base_var matches an FRB code (e.g. "RFF")
# Strategy 3: base_var still has prefixes — strip and retry
# Strategy 4: fall back to a cleaned-up version of base_var

resolve_label <- function(b) {
  if (is.na(b) || b == "") return(list(desc = NA_character_, theme = "Other"))
  bl <- tolower(b)
  # 1) short_name match (case-insensitive)
  hit <- short_lookup[tolower(short_name) == bl]
  if (nrow(hit) > 0) return(list(desc = hit$description[1], theme = hit$theme[1]))
  # 2) FRB code match (uppercase)
  hit <- frb_lookup[toupper(frb_code) == toupper(b)]
  if (nrow(hit) > 0) return(list(desc = hit$description[1], theme = hit$theme[1]))
  # 3) Strip transformations and try again
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
  # 4) Fallback — pretty-cased version, theme via heuristic
  pretty <- gsub("_", " ", b)
  pretty <- tools::toTitleCase(pretty)
  return(list(desc = pretty, theme = classify_derived_or_unknown(b)))
}

imp[, c("desc","theme") := {
  res <- lapply(base_var, resolve_label)
  list(vapply(res, function(x) x$desc, character(1)),
       vapply(res, function(x) x$theme, character(1)))
}]

# Diagnostic — how many resolved?
n_resolved <- sum(imp$theme != "Other")
n_total <- nrow(imp)
message(sprintf("\nResolved %d / %d variables to known themes (%.0f%%)",
                n_resolved, n_total, 100 * n_resolved / n_total))

unresolved <- imp[theme == "Other", .(base_var, mean_importance)]
if (nrow(unresolved) > 0) {
  message("\nUnresolved (theme = Other) — top 10:")
  print(head(unresolved[order(-mean_importance)], 10))
  message("\nIf these matter, add them to the dictionary or write them in the user-overrides block below.")
}

# ── User overrides for stubborn unresolved codes ─────────
# Add manual mappings here if the dictionary doesn't cover something
USER_OVERRIDES <- data.table(
  base_var = character(0),  # e.g. c("UDRRC", "KPS")
  desc     = character(0),  # e.g. c("Unemployment Duration ...", "Real Capital Stock")
  theme    = character(0)   # e.g. c("Labour & Income",          "Economic Activity")
)
if (nrow(USER_OVERRIDES) > 0) {
  imp[USER_OVERRIDES, on = "base_var", c("desc","theme") := list(i.desc, i.theme)]
}

# ════════════════════════════════════════════════════════════
# 4. COMPUTE OPTIMAL LEAD TIME
# ════════════════════════════════════════════════════════════
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

prx[, base_var := get_base_var(macro_var)]
prx_agg <- prx[, .(avg_r2 = mean(r_squared, na.rm = TRUE)),
               by = .(base_var, lag)]
best_lag <- prx_agg[, .SD[which.max(avg_r2)], by = base_var]
setnames(best_lag, "lag", "best_lead_q")

top <- merge(imp, best_lag[, .(base_var, best_lead_q)],
             by = "base_var", all.x = TRUE)
top[is.na(best_lead_q), best_lead_q := 0L]

# Top 10
setorderv(top, "mean_importance", order = -1L)
top10 <- head(top, 10)

# ════════════════════════════════════════════════════════════
# 5. BUILD CHART
# ════════════════════════════════════════════════════════════
# Detect importance scale
imp_max_val <- max(top10$mean_importance, na.rm = TRUE)
top10[, imp_pct := if (imp_max_val <= 1) mean_importance * 100 else mean_importance]

# Truncate long descriptions only if very long
top10[, label_short := ifelse(nchar(desc) > 48,
                               paste0(substr(desc, 1, 46), "…"),
                               desc)]
top10[, label_short := factor(label_short, levels = rev(label_short))]
top10[, annotation := sprintf("%.0f%% • +%dQ lead", imp_pct, best_lead_q)]

theme_colors <- c(
  "Credit Conditions"  = "#E76F51",
  "Interest Rates"     = "#5B9BD5",
  "Spreads"            = "#3D6FBF",
  "Labour & Income"    = "#2EC4B6",
  "Economic Activity"  = "#52B788",
  "Inflation"          = "#F4A261",
  "Housing"            = "#7B68A8",
  "Financial Markets"  = "#E8A838",
  "Sentiment"          = "#C77DFF",
  "Other"              = "#999999"
)

# Only keep themes that appear in top10 (legend tidiness)
present_themes <- unique(top10$theme)
theme_colors_used <- theme_colors[names(theme_colors) %in% present_themes]

p <- ggplot(top10, aes(x = imp_pct, y = label_short, fill = theme)) +
  geom_col(width = 0.65, alpha = 0.92) +
  geom_text(aes(label = annotation), hjust = -0.05,
            size = 3.4, fontface = "bold", color = "#333333") +
  scale_fill_manual(values = theme_colors_used, name = "Theme") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.32))) +
  labs(
    title    = "Top Macro Drivers by Importance and Lead Time",
    subtitle = "How well each macro variable explains CU growth, and how far ahead the signal arrives\nLead time = quarters the macro variable leads CU response",
    x        = "Importance Score (R² × 100)",
    y        = NULL,
    caption  = "Source: Ensemble ML (Ridge, LASSO, Elastic Net, Random Forest) across 14 forecast targets  |  Labels from FRB Variable Data Dictionary"
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

# ── Print top 10 to console ──────────────────────────────
message("\nTop 10 Macro Drivers (with descriptions and lead times):")
for (i in 1:nrow(top10)) {
  message(sprintf("  %2d. %-38s  %5.1f%%  +%dQ lead  (%s)",
                  i,
                  as.character(top10$desc[i]),
                  top10$imp_pct[i],
                  top10$best_lead_q[i],
                  top10$theme[i]))
}
