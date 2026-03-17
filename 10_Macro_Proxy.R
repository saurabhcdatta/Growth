############################################################
# PART 10 v1.0 — MACRO PROXY ANALYSIS
#
# Purpose  : For each important call report variable,
#            identify which macroeconomic variable(s) best
#            serve as proxies — enabling forward-looking
#            inference using FRB macro forecasts.
#
# Logic    : CU-specific variables (merger rates, asset
#            growth, market share) matter for forecasting
#            (Part 5) but have no future values. Macro
#            variables (rates, unemployment, etc.) DO have
#            FRB projections. If a macro variable tracks
#            a CU variable closely, it can serve as a
#            leading or coincident proxy.
#
# Analyses :
#  10A — Proxy Matching: best macro proxy per CR variable
#  10B — Lead-Lag Structure: optimal timing relationship
#  10C — Proxy Quality Heatmap: R² across all pairs
#  10D — Thematic Mapping: which macro themes proxy
#        which CU-specific dynamics
#  10E — Time Series Overlay: visual validation of
#        top proxy pairs
#
# Output   : Publication charts + Excel proxy mapping
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
RESULT_DIR <- "results_10_proxy"
PLOT_DIR   <- "plots_10_proxy"

setwd(DATA_DIR)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)

# Maximum lag to test (quarters)
MAX_LAG <- 8L

message("============================================================")
message("  PART 10: Macro Proxy Analysis for Call Report Variables")
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

# ── System-level averages per quarter ────────────────────
# Average across all categories for each quarter
all_num <- names(panel)[vapply(names(panel), function(v) is.numeric(panel[[v]]), logical(1))]
sys <- panel[, lapply(.SD, mean, na.rm = TRUE), .SDcols = all_num, by = date]
setorderv(sys, "date")
message(sprintf("  System-level: %d quarters", nrow(sys)))

# ── Identify call report and macro columns ───────────────
macro_cols <- grep("^macro_", names(sys), value = TRUE)
macro_cols <- macro_cols[vapply(macro_cols, function(v) {
  x <- sys[[v]]; sum(!is.na(x)) >= 20 && sd(x, na.rm=TRUE) > 1e-10
}, logical(1))]

# Key call report variables to find proxies for
# These are the CU-specific variables that matter (from Part 5)
# but have no future values
CR_TARGETS <- c(
  # Exit dynamics
  "merger_rate", "liquid_rate", "exit_rate", "exit_roll4",
  # Net entry
  "net_entry_rate", "net_entry_rate_fiscu",
  # Counts
  "fcu_count", "fiscu_count", "n_active",
  # Assets
  "fcu_assets", "fiscu_assets", "assets_tot",
  # Growth
  "yoy_fcu_pct", "yoy_fiscu_pct",
  "yoy_fcu_assets_pct", "yoy_fiscu_assets_pct",
  # Market share transforms
  "share_fcu_count", "share_fiscu_count",
  "share_fcu_assets", "share_fiscu_assets",
  # Log transforms
  "ln_assets_tot", "ld_fcu", "ld_fiscu"
)
CR_TARGETS <- intersect(CR_TARGETS, names(sys))
CR_TARGETS <- CR_TARGETS[vapply(CR_TARGETS, function(v) {
  x <- sys[[v]]; sum(!is.na(x)) >= 20 && sd(x, na.rm=TRUE) > 1e-10
}, logical(1))]

message(sprintf("  Call report targets: %d", length(CR_TARGETS)))
message(sprintf("  Macro candidates: %d", length(macro_cols)))

# ── Theme classifiers ────────────────────────────────────
classify_cr <- function(v) {
  v <- tolower(v)
  if (grepl("merger|liquid|acquis|exit", v))    return("Exit Dynamics")
  if (grepl("net_entry", v))                     return("Net Entry")
  if (grepl("share_", v))                        return("Market Share")
  if (grepl("^ld_", v))                          return("Log Growth")
  if (grepl("ln_assets", v))                     return("Size")
  if (grepl("assets|_assets_", v))               return("Assets")
  if (grepl("count|n_active", v))                return("CU Counts")
  if (grepl("yoy_.*pct", v))                     return("Growth Rates")
  return("Other")
}

classify_macro <- function(v) {
  v <- tolower(gsub("^macro_", "", v))
  if (grepl("fedfunds|gs[0-9]|yield|spread|mortgage|real_rate|fwd_|fomc|hike|sofr|prime", v))
    return("Interest Rates")
  if (grepl("baa|credit_tight|bbb", v)) return("Credit Conditions")
  if (grepl("unrate|disp_income|savings|labor|lfpr|nairu|initial_claims", v)) return("Labour & Income")
  if (grepl("gdp|cons_confidence|indpro|pce|consumption|fixed_invest", v)) return("Economic Activity")
  if (grepl("cpi|core_cpi|ppi|inflation|deflator", v)) return("Inflation")
  if (grepl("housing|hpi|rent|foreclos|home_price", v)) return("Housing")
  if (grepl("bankrupt|delinq|chargeoff|leverage", v)) return("Credit Quality")
  if (grepl("sp500|djia|nasdaq|msci", v)) return("Financial Markets")
  return("Other")
}

# ── Publication theme ────────────────────────────────────
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 15, hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "#666666", hjust = 0, margin = margin(b = 12)),
    plot.caption = element_text(size = 8, color = "#999999", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(20, 25, 15, 15),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

pal_navy <- "#0B1D3A"; pal_teal <- "#2EC4B6"; pal_coral <- "#E76F51"
pal_amber <- "#E8A838"; pal_sky <- "#5B9BD5"; pal_green <- "#52B788"

save_pub <- function(p, filename, w = 12, h = 8) {
  path <- file.path(PLOT_DIR, filename)
  tryCatch({
    dev <- tryCatch(
      { grDevices::cairo_pdf(path, width = w, height = h); "cairo" },
      error = function(e) { grDevices::pdf(path, width = w, height = h); "pdf" })
    print(p); grDevices::dev.off()
    message(sprintf("  Saved: %s", filename))
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    message(sprintf("  [WARN] %s: %s", filename, conditionMessage(e)))
  })
}

# ════════════════════════════════════════════════════════════
# 3. ANALYSIS 10A: PROXY MATCHING
# ════════════════════════════════════════════════════════════
message("\n[2] Analysis 10A: Computing all proxy correlations...")
tic("10A")

# For each (CR variable, macro variable), compute correlation at lags 0 to MAX_LAG
proxy_results <- list()

for (cr_var in CR_TARGETS) {
  y <- as.numeric(sys[[cr_var]])
  cr_theme <- classify_cr(cr_var)

  for (m_var in macro_cols) {
    x <- as.numeric(sys[[m_var]])
    m_theme <- classify_macro(m_var)

    for (lag_q in 0:MAX_LAG) {
      n_eff <- length(x) - lag_q
      if (n_eff < 15) next

      # macro leads CU variable by lag_q quarters
      x_lag <- x[1:(length(x) - lag_q)]
      y_fut <- y[(1 + lag_q):length(y)]
      ok <- !is.na(x_lag) & !is.na(y_fut)
      if (sum(ok) < 15) next

      r <- cor(x_lag[ok], y_fut[ok])
      r2 <- r^2

      proxy_results[[length(proxy_results) + 1L]] <- data.table(
        cr_var    = cr_var,
        cr_theme  = cr_theme,
        macro_var = m_var,
        macro_theme = m_theme,
        lag       = lag_q,
        corr      = round(r, 4),
        r_squared = round(r2, 4),
        abs_corr  = round(abs(r), 4),
        n_obs     = sum(ok)
      )
    }
  }
}

all_proxy <- rbindlist(proxy_results)
message(sprintf("  Total proxy pairs tested: %s", format(nrow(all_proxy), big.mark=",")))

# ── Find best proxy per CR variable (highest R² at any lag) ──
best_proxy <- all_proxy[, .SD[which.max(r_squared)], by = cr_var]
setorderv(best_proxy, "r_squared", order = -1L)

# ── Find best proxy at lag=0 (coincident) ────────────────
best_coincident <- all_proxy[lag == 0, .SD[which.max(r_squared)], by = cr_var]
setorderv(best_coincident, "r_squared", order = -1L)

# ── Find best leading proxy (lag > 0) ────────────────────
best_leading <- all_proxy[lag > 0, .SD[which.max(r_squared)], by = cr_var]
setorderv(best_leading, "r_squared", order = -1L)

fwrite(all_proxy,      file.path(RESULT_DIR, "proxy_all_correlations.csv"))
fwrite(best_proxy,     file.path(RESULT_DIR, "proxy_best_overall.csv"))
fwrite(best_leading,   file.path(RESULT_DIR, "proxy_best_leading.csv"))

toc()

# ── Clean names for display ──────────────────────────────
clean_name <- function(v) {
  v <- gsub("^macro_", "", v)
  v <- gsub("_", " ", v)
  v <- paste0(toupper(substr(v, 1, 1)), substr(v, 2, nchar(v)))
  v
}

best_proxy[, cr_clean := clean_name(cr_var)]
best_proxy[, macro_clean := clean_name(macro_var)]
best_leading[, cr_clean := clean_name(cr_var)]
best_leading[, macro_clean := clean_name(macro_var)]

# ════════════════════════════════════════════════════════════
# 4. CHARTS
# ════════════════════════════════════════════════════════════
message("\n[3] Generating publication charts...")

# ── M1: Best Macro Proxy per CR Variable ─────────────────
message("  Chart M1: Best proxy mapping...")
m1_data <- head(best_proxy, 20)
m1_data[, cr_clean := factor(cr_clean, levels = rev(cr_clean))]

p_m1 <- ggplot(m1_data, aes(x = r_squared, y = cr_clean)) +
  geom_col(aes(fill = macro_theme), width = 0.65, alpha = 0.9) +
  geom_text(aes(label = sprintf("R²=%.2f  %s (lag %dQ)", r_squared, macro_clean, lag)),
            hjust = -0.02, size = 2.6, color = "#444444") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45)), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Set2", name = "Macro Theme") +
  labs(
    title = "Best Macroeconomic Proxy for Each Call Report Variable",
    subtitle = "Which macro variable best explains each CU-specific variable?\nR² = how much variance the macro proxy captures  |  Lag = how far macro leads",
    x = "R² (Proxy Quality)", y = NULL,
    caption = "Higher R² = better proxy  |  Lag 0 = same quarter  |  Lag 2 = macro leads by 6 months"
  ) +
  theme_pub +
  theme(legend.position = "right", legend.text = element_text(size = 8))
save_pub(p_m1, "M1_best_proxy_mapping.pdf", w = 15, h = 10)

# ── M2: Proxy Quality Heatmap (top CR vars × top macro) ──
message("  Chart M2: Proxy quality heatmap...")

# Top 12 CR variables and top 15 macro variables by max R²
top_cr <- head(best_proxy$cr_var, 12)
top_macro_per_cr <- unique(best_proxy[cr_var %in% top_cr, macro_var])
# Also add the globally most important macro vars
macro_importance <- all_proxy[, .(max_r2 = max(r_squared, na.rm=TRUE)), by = macro_var]
setorderv(macro_importance, "max_r2", order = -1L)
top_macro <- unique(c(top_macro_per_cr, head(macro_importance$macro_var, 15)))[1:15]

hm_data <- all_proxy[cr_var %in% top_cr & macro_var %in% top_macro & lag == 0,
                      .(cr_var, macro_var, r_squared, corr)]
# Use best lag instead of just lag=0
hm_best <- all_proxy[cr_var %in% top_cr & macro_var %in% top_macro,
                      .SD[which.max(r_squared)], by = .(cr_var, macro_var)]

hm_best[, cr_clean := clean_name(cr_var)]
hm_best[, macro_clean := clean_name(macro_var)]
hm_best[, cr_clean := factor(cr_clean, levels = rev(clean_name(top_cr)))]

p_m2 <- ggplot(hm_best, aes(x = macro_clean, y = cr_clean, fill = r_squared)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", r_squared),
                color = ifelse(r_squared > 0.3, "high", "low")),
            size = 2.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("high" = "white", "low" = "#555555")) +
  scale_fill_gradient2(low = "#FAFAFA", mid = pal_sky, high = pal_navy,
                       midpoint = 0.3, name = "R²",
                       guide = guide_colorbar(barwidth = 1, barheight = 8)) +
  labs(
    title = "Proxy Quality Matrix — How Well Does Each Macro Variable Track Each CU Variable?",
    subtitle = "R² at optimal lag  |  Darker = stronger proxy relationship",
    x = NULL, y = NULL,
    caption = "Read across rows: which macro variable best proxies each CU-specific measure"
  ) +
  theme_pub +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 55, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        legend.position = "right")
save_pub(p_m2, "M2_proxy_quality_heatmap.pdf", w = 14, h = 9)

# ── M3: Lead-Lag Structure ───────────────────────────────
message("  Chart M3: Lead-lag profiles...")

# For top 8 CR variables, show how R² varies across lags for the best macro proxy
top8_cr <- head(best_proxy$cr_var, 8)
lag_profiles <- list()
for (crv in top8_cr) {
  best_m <- best_proxy[cr_var == crv, macro_var]
  lag_data <- all_proxy[cr_var == crv & macro_var == best_m]
  lag_data[, cr_clean := clean_name(crv)]
  lag_data[, pair_label := sprintf("%s → %s", clean_name(best_m), clean_name(crv))]
  lag_profiles[[length(lag_profiles) + 1L]] <- lag_data
}
lag_dt <- rbindlist(lag_profiles)

p_m3 <- ggplot(lag_dt, aes(x = lag, y = r_squared, color = pair_label)) +
  geom_line(linewidth = 1, alpha = 0.85) +
  geom_point(size = 2.5) +
  facet_wrap(~cr_clean, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = 0:MAX_LAG,
                     labels = c("Same Q", paste0("+", 1:MAX_LAG, "Q"))) +
  scale_color_brewer(palette = "Dark2", name = "Macro → CU Pair") +
  labs(
    title = "Lead-Lag Structure — When Does the Macro Signal Arrive?",
    subtitle = "R² between macro proxy and CU variable at different lead times\nPeak = optimal advance warning period",
    x = "Macro leads CU variable by...", y = "R²",
    caption = "Higher R² at lag > 0 = macro variable provides advance warning of CU variable changes"
  ) +
  theme_pub +
  theme(strip.text = element_text(face = "bold", size = 10),
        legend.position = "none",
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1))
save_pub(p_m3, "M3_leadlag_profiles.pdf", w = 14, h = 12)

# ── M4: Thematic Mapping (which macro themes → which CU themes) ──
message("  Chart M4: Theme-to-theme mapping...")

theme_map <- all_proxy[, .SD[which.max(r_squared)], by = .(cr_var, macro_var)]
theme_agg <- theme_map[, .(
  avg_r2 = mean(r_squared, na.rm = TRUE),
  max_r2 = max(r_squared, na.rm = TRUE),
  n_pairs = .N
), by = .(cr_theme, macro_theme)]
theme_agg <- theme_agg[avg_r2 > 0.01]

p_m4 <- ggplot(theme_agg, aes(x = macro_theme, y = cr_theme, fill = avg_r2)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f\n(%d)", avg_r2, n_pairs),
                color = ifelse(avg_r2 > 0.15, "high", "low")),
            size = 3, fontface = "bold", lineheight = 0.85, show.legend = FALSE) +
  scale_color_manual(values = c("high" = "white", "low" = "#444444")) +
  scale_fill_gradient2(low = "#FAFAFA", mid = pal_amber, high = pal_coral,
                       midpoint = 0.15, name = "Avg R²",
                       guide = guide_colorbar(barwidth = 1.2, barheight = 8)) +
  labs(
    title = "Which Economic Themes Proxy Which CU Dynamics?",
    subtitle = "Average R² between macro theme and CU variable theme at optimal lag\nNumber in parentheses = count of variable pairs tested",
    x = "Macroeconomic Theme", y = "Call Report Theme",
    caption = "Stronger color = macro theme is a better proxy for the CU dynamic\nRead across: what macro forces explain each CU-specific pattern"
  ) +
  theme_pub +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        legend.position = "right")
save_pub(p_m4, "M4_theme_to_theme_mapping.pdf", w = 13, h = 9)

# ── M5: Time Series Overlay — Top 6 Proxy Pairs ─────────
message("  Chart M5: Time series overlays...")

top6 <- head(best_proxy, 6)
overlay_list <- list()

for (i in 1:nrow(top6)) {
  crv <- top6$cr_var[i]
  mv  <- top6$macro_var[i]
  lg  <- top6$lag[i]

  y_vals <- as.numeric(sys[[crv]])
  x_vals <- as.numeric(sys[[mv]])
  dates  <- sys$date

  # Standardise both for overlay
  y_z <- (y_vals - mean(y_vals, na.rm=TRUE)) / max(sd(y_vals, na.rm=TRUE), 1e-10)
  x_z <- (x_vals - mean(x_vals, na.rm=TRUE)) / max(sd(x_vals, na.rm=TRUE), 1e-10)

  n <- length(dates)
  overlay_list[[length(overlay_list) + 1L]] <- rbindlist(list(
    data.table(date = dates, value = y_z, series = clean_name(crv),
               pair = sprintf("%s vs %s (R²=%.2f, lag %dQ)",
                              clean_name(crv), clean_name(mv), top6$r_squared[i], lg)),
    data.table(date = dates, value = x_z, series = clean_name(mv),
               pair = sprintf("%s vs %s (R²=%.2f, lag %dQ)",
                              clean_name(crv), clean_name(mv), top6$r_squared[i], lg))
  ))
}

overlay_dt <- rbindlist(overlay_list)
overlay_dt[, date_d := as.Date(zoo::as.yearqtr(date))]

p_m5 <- ggplot(overlay_dt, aes(x = date_d, y = value, color = series)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "#DDDDDD", linewidth = 0.3) +
  facet_wrap(~pair, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c(rep(pal_navy, 6), rep(pal_coral, 6)),
                     name = "Series") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  labs(
    title = "Visual Validation — Do the Macro Proxies Actually Track the CU Variables?",
    subtitle = "Both series standardised (z-score) for visual comparison\nStronger co-movement = better proxy relationship",
    x = NULL, y = "Standardised Value (z-score)",
    caption = "Blue = CU variable  |  Red = macro proxy  |  Closer tracking = higher R²"
  ) +
  theme_pub +
  theme(strip.text = element_text(face = "bold", size = 9),
        legend.position = "none")
save_pub(p_m5, "M5_proxy_overlay_timeseries.pdf", w = 14, h = 12)

# ── M6: Proxy Direction Summary (positive vs negative) ───
message("  Chart M6: Proxy direction summary...")

dir_data <- best_proxy[, .(
  cr_clean = cr_clean,
  macro_clean = macro_clean,
  cr_theme = cr_theme,
  r_squared = r_squared,
  direction = fifelse(corr > 0, "Positive (+)", "Negative (−)"),
  lag = lag
)]
dir_data <- head(dir_data[order(-r_squared)], 20)
dir_data[, cr_clean := factor(cr_clean, levels = rev(cr_clean))]

p_m6 <- ggplot(dir_data, aes(x = r_squared, y = cr_clean, fill = direction)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = sprintf("%s (lag %dQ)", macro_clean, lag)),
            hjust = -0.02, size = 2.5, color = "#444444") +
  scale_fill_manual(values = c("Positive (+)" = pal_teal, "Negative (−)" = pal_coral),
                    name = "Direction") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45)), limits = c(0, 1)) +
  labs(
    title = "Proxy Direction — Does the Macro Variable Move With or Against the CU Variable?",
    subtitle = "Positive = both rise together  |  Negative = one rises when the other falls\nLabels show best macro proxy and its lead time",
    x = "R² (Proxy Strength)", y = NULL,
    caption = "Example: Negative direction for merger rate + Fed Funds = rate hikes associated with more mergers"
  ) +
  theme_pub +
  theme(legend.position = c(0.85, 0.2),
        legend.background = element_rect(fill = "white", color = "#DDD", linewidth = 0.3))
save_pub(p_m6, "M6_proxy_direction.pdf", w = 14, h = 10)

# ════════════════════════════════════════════════════════════
# 5. EXCEL OUTPUT
# ════════════════════════════════════════════════════════════
message("\n[4] Saving Excel...")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  tryCatch({
    library(openxlsx)
    wb <- createWorkbook()

    # Sheet 1: Best proxy per CR variable
    addWorksheet(wb, "Best Proxy")
    writeData(wb, "Best Proxy",
              x = "Macro Proxy Mapping — Best Match for Each CU Variable", startRow = 1)
    out1 <- best_proxy[, .(
      `CU Variable`    = cr_clean,
      `CU Theme`       = cr_theme,
      `Best Macro Proxy` = macro_clean,
      `Macro Theme`    = macro_theme,
      `R²`             = round(r_squared, 3),
      `Correlation`    = round(corr, 3),
      `Optimal Lag (Q)` = lag,
      `Direction`      = fifelse(corr > 0, "Positive", "Negative")
    )]
    writeData(wb, "Best Proxy", x = out1, startRow = 3,
      headerStyle = createStyle(textDecoration = "bold", fgFill = "#D9E2F3",
                                 border = "TopBottomLeftRight", halign = "center"))
    setColWidths(wb, "Best Proxy", cols = 1:8,
                 widths = c(25, 18, 25, 18, 8, 12, 14, 12))
    addStyle(wb, "Best Proxy",
             createStyle(fontSize = 13, textDecoration = "bold"), rows = 1, cols = 1)

    # Sheet 2: Best leading proxy
    addWorksheet(wb, "Best Leading Proxy")
    writeData(wb, "Best Leading Proxy",
              x = "Leading Indicators — Best Macro Proxy with Advance Warning", startRow = 1)
    out2 <- best_leading[, .(
      `CU Variable`    = cr_clean,
      `Best Leading Macro` = macro_clean,
      `R²`             = round(r_squared, 3),
      `Lead Time (Q)`  = lag,
      `Direction`      = fifelse(corr > 0, "Positive", "Negative")
    )]
    writeData(wb, "Best Leading Proxy", x = head(out2, 25), startRow = 3,
      headerStyle = createStyle(textDecoration = "bold", fgFill = "#E8F5E9",
                                 border = "TopBottomLeftRight"))

    # Sheet 3: Theme mapping
    addWorksheet(wb, "Theme Mapping")
    writeData(wb, "Theme Mapping",
              x = "Macro Theme → CU Theme Proxy Strength", startRow = 1)
    out3 <- theme_agg[order(-avg_r2), .(
      `CU Theme`    = cr_theme,
      `Macro Theme` = macro_theme,
      `Avg R²`      = round(avg_r2, 3),
      `Max R²`      = round(max_r2, 3),
      `Pairs Tested` = n_pairs
    )]
    writeData(wb, "Theme Mapping", x = out3, startRow = 3,
      headerStyle = createStyle(textDecoration = "bold", fgFill = "#FFF3E0",
                                 border = "TopBottomLeftRight"))

    xlsx_path <- file.path(RESULT_DIR, "macro_proxy_analysis.xlsx")
    saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    message(sprintf("  Excel saved: %s", xlsx_path))
  }, error = function(e) message(sprintf("  [EXCEL WARN] %s", conditionMessage(e))))
}

# ════════════════════════════════════════════════════════════
# 6. SUMMARY
# ════════════════════════════════════════════════════════════
message("\n============================================================")
message("  MACRO PROXY ANALYSIS COMPLETE")
message("============================================================")
message(sprintf("  CR variables analysed: %d", length(CR_TARGETS)))
message(sprintf("  Macro candidates:      %d", length(macro_cols)))
message(sprintf("  Total pairs × lags:    %s", format(nrow(all_proxy), big.mark=",")))
message("")
message("  Top 10 Proxy Matches:")
for (i in 1:min(10, nrow(best_proxy))) {
  message(sprintf("    %-25s → %-25s  R²=%.3f  lag=%dQ  %s",
                  best_proxy$cr_clean[i], best_proxy$macro_clean[i],
                  best_proxy$r_squared[i], best_proxy$lag[i],
                  ifelse(best_proxy$corr[i] > 0, "(+)", "(-)")))
}
message("")
message(sprintf("  Charts: %s/", PLOT_DIR))
message("    M1 — Best proxy per CR variable (bar chart)")
message("    M2 — Proxy quality heatmap (R² matrix)")
message("    M3 — Lead-lag profiles (when does signal arrive)")
message("    M4 — Theme-to-theme mapping (macro → CU themes)")
message("    M5 — Time series overlay (visual validation)")
message("    M6 — Proxy direction (positive vs negative)")
message(sprintf("  Data: %s/", RESULT_DIR))
message("============================================================")
