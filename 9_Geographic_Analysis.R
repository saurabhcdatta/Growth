############################################################
# PART 9 v1.0 — GEOGRAPHIC ANALYSIS
#
# Purpose  : Analyse how CU asset category distributions
#            shift across states and regions over 1, 3, and
#            5-year forecast horizons.
#
# Requires : Part 8 output (fc_fcu, fc_fiscu) OR re-reads
#            from results_8_disagg/ CSV files.
#
# Analyses :
#   9A — State Mobility Scorecard
#        Rank states by % of CUs projected to move up
#
#   9B — Top/Bottom State Comparison
#        Which states are growing vs consolidating?
#
#   9C — Regional Transition Matrices
#        FROM → TO heatmaps by NCUA region
#
#   9D — State-Level Composition Shift
#        Small CU share now vs 5 years for each state
#
# Output   : Publication charts + Excel scorecard
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
RESULT_DIR <- "results_9_geographic"
PLOT_DIR   <- "plots_9_geographic"

setwd(DATA_DIR)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)

ASSET_LABELS <- c("1_Less_10M", "2_10M_50M", "3_50M_100M",
                  "4_100M_500M", "5_500M_1B", "6_1B_10B", "7_10B_Plus")

message("============================================================")
message("  PART 9: Geographic Analysis — State & Regional Migration")
message("============================================================")

# ════════════════════════════════════════════════════════════
# 2. LOAD PART 8 RESULTS
# ════════════════════════════════════════════════════════════
message("\n[1] Loading Part 8 forecast data...")

# Try to use in-memory objects from Part 8, otherwise read CSVs
if (exists("fc_fcu") && exists("fc_fiscu")) {
  message("  Using in-memory fc_fcu / fc_fiscu from Part 8")
} else {
  csv_fcu   <- file.path("results_8_disagg", "fcu_forecasts.csv")
  csv_fiscu <- file.path("results_8_disagg", "fiscu_forecasts.csv")
  if (file.exists(csv_fcu) && file.exists(csv_fiscu)) {
    fc_fcu   <- fread(csv_fcu)
    fc_fiscu <- fread(csv_fiscu)
    # Reconstruct column names from CSV headers
    if ("Join Number" %in% names(fc_fcu)) {
      nm_map <- c("Join Number"="join_number", "CU Name"="cu_name",
                   "Region"="region", "State"="reporting_state",
                   "Current Assets ($)"="assets_now",
                   "Asset Category (Now)"="bucket_now", "Cat # Now"="cat_now",
                   "Projected Assets 1Yr"="assets_1yr", "Category 1Yr"="bucket_1yr",
                   "Cat # 1Yr"="cat_1yr",
                   "Projected Assets 3Yr"="assets_3yr", "Category 3Yr"="bucket_3yr",
                   "Cat # 3Yr"="cat_3yr",
                   "Projected Assets 5Yr"="assets_5yr", "Category 5Yr"="bucket_5yr",
                   "Cat # 5Yr"="cat_5yr",
                   "ARIMA Model"="arima_order", "Obs (Quarters)"="n_quarters")
      for (old_nm in names(nm_map)) {
        if (old_nm %in% names(fc_fcu))   setnames(fc_fcu, old_nm, nm_map[old_nm])
        if (old_nm %in% names(fc_fiscu)) setnames(fc_fiscu, old_nm, nm_map[old_nm])
      }
    }
    message(sprintf("  Loaded CSVs: FCU=%s, FISCU=%s",
                    format(nrow(fc_fcu), big.mark=","),
                    format(nrow(fc_fiscu), big.mark=",")))
  } else {
    stop("Part 8 results not found. Run Part 8 first, or ensure results_8_disagg/ exists.")
  }
}

# Combine for analyses that don't need FCU/FISCU split
fc_all <- rbindlist(list(
  copy(fc_fcu)[, cu_type_label := "FCU"],
  copy(fc_fiscu)[, cu_type_label := "FISCU"]
), fill = TRUE)

# Clean state names
fc_all[, state := trimws(as.character(reporting_state))]
fc_all <- fc_all[!is.na(state) & state != "" & state != "NA"]

message(sprintf("  Total CUs with state info: %s across %d states",
                format(nrow(fc_all), big.mark=","), uniqueN(fc_all$state)))

# ── Bucket number helper ─────────────────────────────────
bucket_num <- function(b) as.integer(substr(as.character(b), 1, 1))

fc_all[, bnum_now := bucket_num(bucket_now)]
fc_all[, bnum_1yr := bucket_num(bucket_1yr)]
fc_all[, bnum_3yr := bucket_num(bucket_3yr)]
fc_all[, bnum_5yr := bucket_num(bucket_5yr)]

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
# 3. ANALYSIS 9A: STATE MOBILITY SCORECARD
# ════════════════════════════════════════════════════════════
message("\n[2] Analysis 9A: State Mobility Scorecard...")
tic("9A")

state_score <- fc_all[, .(
  n_cus           = .N,
  pct_up_1yr      = sum(bnum_1yr > bnum_now, na.rm=TRUE) / .N * 100,
  pct_same_1yr    = sum(bnum_1yr == bnum_now, na.rm=TRUE) / .N * 100,
  pct_down_1yr    = sum(bnum_1yr < bnum_now, na.rm=TRUE) / .N * 100,
  pct_up_3yr      = sum(bnum_3yr > bnum_now, na.rm=TRUE) / .N * 100,
  pct_same_3yr    = sum(bnum_3yr == bnum_now, na.rm=TRUE) / .N * 100,
  pct_down_3yr    = sum(bnum_3yr < bnum_now, na.rm=TRUE) / .N * 100,
  pct_up_5yr      = sum(bnum_5yr > bnum_now, na.rm=TRUE) / .N * 100,
  pct_same_5yr    = sum(bnum_5yr == bnum_now, na.rm=TRUE) / .N * 100,
  pct_down_5yr    = sum(bnum_5yr < bnum_now, na.rm=TRUE) / .N * 100,
  avg_cat_now     = mean(bnum_now, na.rm=TRUE),
  avg_cat_5yr     = mean(bnum_5yr, na.rm=TRUE),
  avg_asset_now   = mean(assets_now, na.rm=TRUE)
), by = state]

# Net mobility score: pct_up minus pct_down (5yr horizon)
state_score[, net_mobility_5yr := pct_up_5yr - pct_down_5yr]
state_score[, avg_cat_shift := avg_cat_5yr - avg_cat_now]
setorderv(state_score, "net_mobility_5yr", order = -1L)

# Filter to states with at least 10 CUs for reliability
state_score_filt <- state_score[n_cus >= 10]

fwrite(state_score, file.path(RESULT_DIR, "state_mobility_scorecard.csv"))

# ── Chart G1: Top 15 & Bottom 15 States by Net Mobility ──
message("  Chart G1: State mobility ranking...")

top15 <- head(state_score_filt, 15)
bot15 <- tail(state_score_filt, 15)
rank_dt <- rbindlist(list(
  top15[, .(state, net_mobility_5yr, n_cus, group = "Top 15 — Highest Upward Mobility")],
  bot15[, .(state, net_mobility_5yr, n_cus, group = "Bottom 15 — Most Consolidation")]
))
rank_dt[, state := factor(state, levels = rev(unique(state)))]
rank_dt[, group := factor(group, levels = c("Top 15 — Highest Upward Mobility",
                                              "Bottom 15 — Most Consolidation"))]

p_g1 <- ggplot(rank_dt, aes(x = net_mobility_5yr, y = state,
                              fill = fifelse(net_mobility_5yr >= 0, "Positive", "Negative"))) +
  geom_col(width = 0.65, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%+.1f%% (%d CUs)", net_mobility_5yr, n_cus)),
            hjust = fifelse(rank_dt$net_mobility_5yr >= 0, -0.05, 1.05),
            size = 2.8, color = "#444444") +
  geom_vline(xintercept = 0, color = "#999999", linewidth = 0.4) +
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Positive" = pal_green, "Negative" = pal_coral)) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "State CU Mobility — Which States Are Growing vs Consolidating?",
    subtitle = "Net mobility = % of CUs moving up a category minus % moving down (5-year horizon)\nPositive = more CUs growing into larger categories  |  Negative = more shrinking/consolidating",
    x = "Net Mobility (% up − % down)", y = NULL,
    caption = sprintf("States with ≥ 10 CUs shown  |  Based on %s individual ARIMA forecasts",
                      format(nrow(fc_all), big.mark=","))
  ) +
  theme_pub +
  theme(strip.text = element_text(face = "bold", size = 11),
        axis.text.y = element_text(size = 9))
save_pub(p_g1, "G1_state_mobility_ranking.pdf", w = 13, h = 12)

toc()

# ════════════════════════════════════════════════════════════
# 4. ANALYSIS 9B: TOP/BOTTOM STATE COMPARISON
# ════════════════════════════════════════════════════════════
message("\n[3] Analysis 9B: State Comparison Charts...")
tic("9B")

# ── Chart G2: Migration Direction by State (top 20 by CU count) ──
message("  Chart G2: Migration direction by state...")
top20_states <- head(state_score[order(-n_cus)], 20)$state

mig_by_state <- fc_all[state %in% top20_states, .(
  up_n    = sum(bnum_5yr > bnum_now, na.rm=TRUE),
  same_n  = sum(bnum_5yr == bnum_now, na.rm=TRUE),
  down_n  = sum(bnum_5yr < bnum_now, na.rm=TRUE),
  n_cus   = .N
), by = state]
mig_by_state[, up_pct   := up_n / n_cus * 100]
mig_by_state[, same_pct := same_n / n_cus * 100]
mig_by_state[, down_pct := down_n / n_cus * 100]

mig_long <- rbindlist(list(
  mig_by_state[, .(state, n_cus, direction = "Moved Up",      pct = up_pct,   count = up_n)],
  mig_by_state[, .(state, n_cus, direction = "Same Category", pct = same_pct, count = same_n)],
  mig_by_state[, .(state, n_cus, direction = "Moved Down",    pct = down_pct, count = down_n)]
))
mig_long[, state := factor(state, levels = rev(mig_by_state[order(-n_cus), state]))]
mig_long[, direction := factor(direction, levels = c("Moved Down", "Same Category", "Moved Up"))]
# Label: show count and % inside bar (only if segment is wide enough)
mig_long[, label := fifelse(pct >= 5, sprintf("%d (%.0f%%)", count, pct), "")]

dir_colors <- c("Moved Up" = pal_green, "Same Category" = pal_sky, "Moved Down" = pal_coral)

p_g2 <- ggplot(mig_long, aes(x = pct, y = state, fill = direction)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 2.6, color = "white", fontface = "bold") +
  # Add total CU count at end of bar
  geom_text(data = mig_by_state,
            aes(x = 102, y = state, label = sprintf("n=%d", n_cus)),
            inherit.aes = FALSE, size = 2.5, color = "#888888", hjust = 0) +
  scale_fill_manual(values = dir_colors, name = "5-Year Projection") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "CU Category Migration by State — Top 20 States by CU Count",
    subtitle = "What percentage of each state's CUs are projected to move up, stay, or move down\nin asset category over the next 5 years  |  Labels show count (% of state total)",
    x = "Share of CUs (%)", y = NULL,
    caption = "States sorted by total CU count (largest at top)  |  Labels hidden if segment < 5%"
  ) +
  theme_pub
save_pub(p_g2, "G2_state_migration_direction.pdf", w = 14, h = 10)

# ── Chart G3: Average Category Shift (dot plot) ──────────
message("  Chart G3: Average category shift...")
shift_dt <- state_score_filt[, .(state, avg_cat_now, avg_cat_5yr, n_cus, avg_cat_shift)]
shift_dt <- shift_dt[order(-n_cus)][1:min(25, .N)]
shift_dt[, state := factor(state, levels = rev(state))]

p_g3 <- ggplot(shift_dt) +
  geom_segment(aes(x = avg_cat_now, xend = avg_cat_5yr, y = state, yend = state),
               color = "#CCCCCC", linewidth = 1.5) +
  geom_point(aes(x = avg_cat_now, y = state), color = pal_sky, size = 4) +
  geom_point(aes(x = avg_cat_5yr, y = state), color = pal_navy, size = 4) +
  geom_text(aes(x = avg_cat_5yr, y = state, label = sprintf("%+.2f", avg_cat_shift)),
            hjust = -0.3, size = 2.8, color = "#555555") +
  scale_x_continuous(breaks = 1:7, labels = ASSET_LABELS,
                     limits = c(0.5, 7.5)) +
  labs(
    title = "Average Asset Category Shift by State — Now to 5 Years",
    subtitle = "Light dot = average category now  |  Dark dot = average category in 5 years\nRightward shift = CUs growing into larger categories",
    x = "Average Asset Category", y = NULL,
    caption = "Top 25 states by CU count  |  Number shows net category shift (+0.15 = slight upward drift)"
  ) +
  theme_pub +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
save_pub(p_g3, "G3_state_avg_category_shift.pdf", w = 13, h = 10)

toc()

# ════════════════════════════════════════════════════════════
# 5. ANALYSIS 9C: REGIONAL TRANSITION MATRICES
# ════════════════════════════════════════════════════════════
message("\n[4] Analysis 9C: Regional Transition Matrices...")
tic("9C")

# Use NCUA region if available, otherwise group states into regions
if (all(is.na(fc_all$region)) || uniqueN(fc_all$region[!is.na(fc_all$region)]) < 2) {
  # Create approximate NCUA regions from state codes
  region_map <- c(
    # Region 1: Northeast
    CT="Region 1", DE="Region 1", DC="Region 1", ME="Region 1", MD="Region 1",
    MA="Region 1", NH="Region 1", NJ="Region 1", NY="Region 1", PA="Region 1",
    RI="Region 1", VT="Region 1", VA="Region 1", WV="Region 1",
    # Region 2: Southeast
    AL="Region 2", FL="Region 2", GA="Region 2", KY="Region 2", MS="Region 2",
    NC="Region 2", PR="Region 2", SC="Region 2", TN="Region 2", VI="Region 2",
    # Region 3: Midwest
    IA="Region 3", IL="Region 3", IN="Region 3", KS="Region 3", MI="Region 3",
    MN="Region 3", MO="Region 3", ND="Region 3", NE="Region 3", OH="Region 3",
    SD="Region 3", WI="Region 3",
    # Region 4: Southwest
    AR="Region 4", CO="Region 4", LA="Region 4", NM="Region 4", OK="Region 4",
    TX="Region 4", WY="Region 4",
    # Region 5: West
    AK="Region 5", AS="Region 5", AZ="Region 5", CA="Region 5", GU="Region 5",
    HI="Region 5", ID="Region 5", MT="Region 5", NV="Region 5", OR="Region 5",
    UT="Region 5", WA="Region 5"
  )
  fc_all[, region_label := region_map[toupper(state)]]
  fc_all[is.na(region_label), region_label := "Other"]
  message("  Built regions from state codes")
} else {
  fc_all[, region_label := paste0("Region ", region)]
  message("  Using NCUA region column")
}

regions <- sort(unique(fc_all$region_label[fc_all$region_label != "Other"]))
message(sprintf("  Regions: %s", paste(regions, collapse = ", ")))

# ── Chart G4: Regional Transition Matrices ───────────────
message("  Chart G4: Regional transition matrices...")

for (hz_info in list(
  list(col = "bucket_1yr", label = "1 Year", short = "1yr"),
  list(col = "bucket_3yr", label = "3 Years", short = "3yr"),
  list(col = "bucket_5yr", label = "5 Years", short = "5yr"))) {

  for (reg in regions) {
    reg_dt <- fc_all[region_label == reg & !is.na(bucket_now) & !is.na(get(hz_info$col))]
    if (nrow(reg_dt) < 10) next

    flow <- reg_dt[, .N, by = .(from = bucket_now, to = get(hz_info$col))]
    setnames(flow, "to", "to_cat")
    flow[, from_total := sum(N), by = from]
    flow[, pct := N / from_total * 100]
    flow[, from := factor(from, levels = rev(ASSET_LABELS))]
    flow[, to_cat := factor(to_cat, levels = ASSET_LABELS)]

    reg_clean <- gsub(" ", "", reg)

    p_r <- ggplot(flow, aes(x = to_cat, y = from, fill = pct)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = sprintf("%d\n(%.0f%%)", N, pct),
                    color = ifelse(pct > 40, "high", "low")),
                size = 2.8, fontface = "bold", lineheight = 0.85, show.legend = FALSE) +
      scale_color_manual(values = c("high" = "white", "low" = "#333333")) +
      scale_fill_gradient2(low = "#FAFAFA", mid = pal_sky, high = pal_navy,
                           midpoint = 25, name = "% of\nSource",
                           guide = guide_colorbar(barwidth = 1, barheight = 6)) +
      labs(
        title = sprintf("%s — CU Transition Matrix (%s Out)", reg, hz_info$label),
        subtitle = sprintf("Rows = current category  |  Columns = projected  |  %d CUs", nrow(reg_dt)),
        x = sprintf("Projected Category (%s)", hz_info$label),
        y = "Current Category",
        caption = "Each cell: CU count (% of source row)"
      ) +
      theme_pub +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.text.y = element_text(size = 8))
    save_pub(p_r, sprintf("G4_%s_transition_%s.pdf", reg_clean, hz_info$short), w = 10, h = 8)
  }
}

toc()

# ════════════════════════════════════════════════════════════
# 6. ANALYSIS 9D: STATE SMALL CU SHARE SHIFT
# ════════════════════════════════════════════════════════════
message("\n[5] Analysis 9D: Small CU Share Shift...")
tic("9D")

# % of CUs in categories 1-2 (under $50M) — now vs 5yr
small_share <- fc_all[, .(
  pct_small_now = sum(bnum_now <= 2, na.rm=TRUE) / .N * 100,
  pct_small_5yr = sum(bnum_5yr <= 2, na.rm=TRUE) / .N * 100,
  n_cus = .N
), by = state]
small_share[, shift := pct_small_5yr - pct_small_now]
small_share <- small_share[n_cus >= 10]
setorderv(small_share, "shift")

# ── Chart G5: Small CU Share Shift ───────────────────────
message("  Chart G5: Small CU share shift...")
top_shift <- rbindlist(list(
  head(small_share, 15)[, grp := "Biggest Decline in Small CUs"],
  tail(small_share, 15)[, grp := "Smallest Change / Increase"]
))
top_shift[, state := factor(state, levels = rev(unique(state)))]

p_g5 <- ggplot(top_shift, aes(x = shift, y = state,
                                fill = fifelse(shift <= 0, "Declining", "Increasing"))) +
  geom_col(width = 0.6, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%+.1f pp", shift)),
            hjust = fifelse(top_shift$shift <= 0, 1.1, -0.1),
            size = 2.8, color = "#444444") +
  geom_vline(xintercept = 0, color = "#999999", linewidth = 0.4) +
  facet_wrap(~grp, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Declining" = pal_coral, "Increasing" = pal_teal)) +
  scale_x_continuous(labels = function(x) paste0(x, " pp")) +
  labs(
    title = "Change in Small CU Share by State (Under $50M)",
    subtitle = "Projected change in % of CUs in categories 1-2 over 5 years\nNegative = small CUs shrinking as a share (growing into larger categories)",
    x = "Change in Small CU Share (percentage points)", y = NULL,
    caption = "States with ≥ 10 CUs  |  More negative = faster consolidation of small CUs"
  ) +
  theme_pub +
  theme(strip.text = element_text(face = "bold", size = 11))
save_pub(p_g5, "G5_small_cu_share_shift.pdf", w = 12, h = 12)

toc()

# ════════════════════════════════════════════════════════════
# 7. EXCEL SCORECARD
# ════════════════════════════════════════════════════════════
message("\n[6] Saving results...")

fwrite(state_score, file.path(RESULT_DIR, "state_scorecard_full.csv"))
fwrite(small_share, file.path(RESULT_DIR, "state_small_cu_shift.csv"))

if (requireNamespace("openxlsx", quietly = TRUE)) {
  tryCatch({
    library(openxlsx)
    wb <- createWorkbook()

    # Sheet 1: State Scorecard
    addWorksheet(wb, "State Scorecard")
    writeData(wb, "State Scorecard",
              x = "State CU Mobility Scorecard — 5-Year Forecast Horizon", startRow = 1)
    sc_out <- state_score[order(-net_mobility_5yr), .(
      State = state, `Total CUs` = n_cus,
      `% Up 1Yr` = round(pct_up_1yr, 1), `% Same 1Yr` = round(pct_same_1yr, 1),
      `% Down 1Yr` = round(pct_down_1yr, 1),
      `% Up 5Yr` = round(pct_up_5yr, 1), `% Same 5Yr` = round(pct_same_5yr, 1),
      `% Down 5Yr` = round(pct_down_5yr, 1),
      `Net Mobility 5Yr` = round(net_mobility_5yr, 1),
      `Avg Cat Now` = round(avg_cat_now, 2), `Avg Cat 5Yr` = round(avg_cat_5yr, 2),
      `Cat Shift` = round(avg_cat_shift, 2)
    )]
    writeData(wb, "State Scorecard", x = sc_out, startRow = 3,
      headerStyle = createStyle(textDecoration = "bold", fgFill = "#D9E2F3",
                                 border = "TopBottomLeftRight", halign = "center"))
    setColWidths(wb, "State Scorecard", cols = 1:ncol(sc_out),
                 widths = c(8, 10, rep(10, ncol(sc_out) - 2)))
    addStyle(wb, "State Scorecard",
             createStyle(fontSize = 13, textDecoration = "bold"), rows = 1, cols = 1)

    # Sheet 2: Small CU Shift
    addWorksheet(wb, "Small CU Shift")
    writeData(wb, "Small CU Shift",
              x = "Change in Small CU (Under $50M) Share by State", startRow = 1)
    ss_out <- small_share[order(shift), .(
      State = state, `Total CUs` = n_cus,
      `% Small Now` = round(pct_small_now, 1),
      `% Small 5Yr` = round(pct_small_5yr, 1),
      `Shift (pp)` = round(shift, 1)
    )]
    writeData(wb, "Small CU Shift", x = ss_out, startRow = 3,
      headerStyle = createStyle(textDecoration = "bold", fgFill = "#E8F5E9",
                                 border = "TopBottomLeftRight"))
    setColWidths(wb, "Small CU Shift", cols = 1:5, widths = c(8, 10, 12, 12, 10))

    xlsx_path <- file.path(RESULT_DIR, "geographic_analysis.xlsx")
    saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    message(sprintf("  Excel saved: %s", xlsx_path))
  }, error = function(e) message(sprintf("  [EXCEL WARN] %s", conditionMessage(e))))
}

# ════════════════════════════════════════════════════════════
# 8. SUMMARY
# ════════════════════════════════════════════════════════════
message("\n============================================================")
message("  GEOGRAPHIC ANALYSIS COMPLETE")
message("============================================================")
message(sprintf("  CUs analysed: %s across %d states",
                format(nrow(fc_all), big.mark=","), uniqueN(fc_all$state)))
message(sprintf("  Top mobility state (5yr):   %s (%+.1f%%)",
                state_score_filt$state[1], state_score_filt$net_mobility_5yr[1]))
message(sprintf("  Bottom mobility state:      %s (%+.1f%%)",
                tail(state_score_filt, 1)$state, tail(state_score_filt, 1)$net_mobility_5yr))
message("")
message(sprintf("  Charts: %s/", PLOT_DIR))
message("    G1 — State mobility ranking (top/bottom 15)")
message("    G2 — Migration direction by state (top 20)")
message("    G3 — Average category shift (dot plot)")
message("    G4 — Regional transition matrices (per region × horizon)")
message("    G5 — Small CU share shift by state")
message(sprintf("  Data: %s/", RESULT_DIR))
message("============================================================")
