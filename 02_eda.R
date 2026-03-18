# =============================================================================
# OIL PRICE SHOCK × CREDIT UNION RESEARCH
# Script 02 — Exploratory Data Analysis
# =============================================================================
# Input  : Data/panel_base.rds   (from 01_data_prep_v2.R)
#          Data/macro_base.rds
# Output : Figures/                directory of PNG charts
#
# Charts produced:
#  01  Oil price history + cycle annotation          (PBRENT 2005-2025)
#  02  PBRENT vs aggregate CU outcomes — time series overlay
#  03  Cross-correlation: PBRENT leads CU outcomes by 1-8 quarters
#  04  Direct vs indirect: oil-state vs non-oil CU outcomes over time
#  05  Deposit channel: cert_share & loan_to_share vs fomc_regime
#  06  Asset tier response: dq_rate and netintmrg by tier across cycles
#  07  Structural break: pre/post 2015Q1 (shale era)
#  08  Spillover: non-oil CU response by spillover exposure tercile
#  09  Heatmap: oil price cycle episodes × CU outcome variables
#  10  Missingness overview
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(stringr)
})

msg <- function(...) cat(sprintf(...), "\n")
hdr <- function(s)   cat("\n---", s, "---\n")

cat("=================================================================\n")
cat(" OIL SHOCK × CU  |  SCRIPT 02: EDA\n")
cat("=================================================================\n")

# =============================================================================
# CONFIG
# =============================================================================
dir.create("Figures", showWarnings = FALSE)

# ── Publication theme ─────────────────────────────────────────────────────────
theme_pub <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
  theme(
    text              = element_text(family = "sans", colour = "#1a1a1a"),
    plot.title        = element_text(size = base_size + 2, face = "bold",
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(size = base_size - 0.5, colour = "#555555",
                                     margin = margin(b = 8)),
    plot.caption      = element_text(size = base_size - 2, colour = "#888888",
                                     hjust = 0),
    axis.title        = element_text(size = base_size - 0.5, colour = "#333333"),
    axis.text         = element_text(size = base_size - 1.5, colour = "#444444"),
    panel.grid.major  = element_line(colour = "#e8e8e8", linewidth = 0.3),
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(colour = "#cccccc", fill = NA,
                                     linewidth = 0.4),
    strip.text        = element_text(size = base_size - 1, face = "bold",
                                     colour = "#333333"),
    strip.background  = element_rect(fill = "#f5f5f5", colour = "#cccccc"),
    legend.position   = "bottom",
    legend.text       = element_text(size = base_size - 1.5),
    legend.title      = element_text(size = base_size - 1, face = "bold"),
    plot.margin       = margin(10, 12, 8, 10)
  )
}

# Colour palettes
COL_OIL    <- "#b5470a"          # Brent oil — burnt orange
COL_DIRECT <- "#1a3a5c"          # oil-state CUs — navy
COL_INDIR  <- "#2d7a4a"          # non-oil CUs — forest green
COL_SPILL  <- "#7a3080"          # spillover — purple
COL_NEG    <- "#c0392b"          # stress / negative — red
COL_POS    <- "#27ae60"          # positive — green
TIER_COLS  <- c("T1_under10M"  = "#4a90d9",
                "T2_10to100M"  = "#e67e22",
                "T3_100Mto1B"  = "#8e44ad",
                "T4_over1B"    = "#16a085")

# Key oil price episode shading bands
EPISODES <- data.frame(
  label = c("GFC","Shale\nBust","COVID\nCrash","Post-COVID\nSurge"),
  xmin  = as.Date(c("2008-07-01","2014-07-01","2020-01-01","2021-01-01")),
  xmax  = as.Date(c("2009-06-30","2016-06-30","2020-06-30","2022-06-30")),
  fill  = c("#fde8e8","#e8f0fd","#fde8e8","#e8fde8")
)

save_plot <- function(p, filename, w = 10, h = 6.5, dpi = 300) {
  path <- file.path("Figures", filename)
  ggsave(path, plot = p, width = w, height = h, dpi = dpi,
         bg = "white")
  msg("  Saved: %s", path)
}

# =============================================================================
# LOAD DATA
# =============================================================================
hdr("Loading data")

panel <- readRDS("Data/panel_base.rds")
macro <- readRDS("Data/macro_base.rds")
setDT(panel); setDT(macro)

# Calendar date for plotting
Q_MONTH <- c("1"=1L,"2"=4L,"3"=7L,"4"=10L)
panel[, cal_date := as.Date(paste(year,
                                   Q_MONTH[as.character(quarter)],
                                   "01", sep="-"))]
macro[, cal_date := as.Date(paste(year,
                                   Q_MONTH[as.character(quarter)],
                                   "01", sep="-"))]

# Macro spine — unique quarters
mac_spine <- unique(macro[, .(cal_date, yyyyqq,
                               pbrent   = macro_base_pbrent,
                               yoy_oil  = macro_base_yoy_oil,
                               lurc     = macro_base_lurc,
                               pcpi     = macro_base_pcpi,
                               rmtg     = macro_base_rmtg,
                               fedfunds = macro_base_rff,
                               fomc_regime = macro_base_fomc_regime)])[order(cal_date)]

msg("  panel: %s rows × %s cols | %s CUs | quarters %sQ%s–%sQ%s",
    format(nrow(panel),big.mark=","), ncol(panel),
    format(uniqueN(panel$join_number),big.mark=","),
    min(panel$year), panel[which.min(yyyyqq), quarter],
    max(panel$year), panel[which.max(yyyyqq), quarter])

# =============================================================================
# HELPER: aggregate CU outcomes to quarter level
# =============================================================================
agg_quarter <- function(dt, vars, by_vars = "yyyyqq") {
  dt[, c(list(cal_date = first(cal_date), year = first(year),
              quarter  = first(quarter)),
         lapply(.SD, function(x) mean(x, na.rm=TRUE))),
     by = by_vars, .SDcols = intersect(vars, names(dt))
  ][order(get(by_vars[1]))]
}

agg_group <- function(dt, vars, group_col, by_vars = c("yyyyqq", group_col)) {
  dt[!is.na(get(group_col)),
     c(list(cal_date = first(cal_date), year = first(year),
             quarter  = first(quarter)),
        lapply(.SD, function(x) mean(x, na.rm=TRUE))),
     by = by_vars, .SDcols = intersect(vars, names(dt))
  ][order(yyyyqq)]
}

# Available outcome vars
cu_outcomes <- intersect(c("dq_rate","chg_tot_lns_ratio","netintmrg",
                             "pcanetworth","networth","roa","costfds",
                             "insured_share_growth","cert_share",
                             "loan_to_share","nim_spread"),
                          names(panel))

msg("  CU outcomes available: %s", paste(cu_outcomes, collapse=", "))

# Episode rectangles helper — returns flat list for ggplot layer addition
ep_rects <- function(episodes = EPISODES) {
  rects <- mapply(function(xmn, xmx, fl) {
    annotate("rect", xmin=xmn, xmax=xmx,
             ymin=-Inf, ymax=Inf, fill=fl, alpha=0.35)
  }, episodes$xmin, episodes$xmax, episodes$fill, SIMPLIFY=FALSE)

  texts <- mapply(function(xmn, xmx, lb) {
    annotate("text", x=xmn + (xmx-xmn)/2,
             y=Inf, label=lb, vjust=1.3, size=2.5,
             colour="#888888", fontface="italic")
  }, episodes$xmin, episodes$xmax, episodes$label, SIMPLIFY=FALSE)

  # Flatten to single list so ggplot + list() works correctly
  c(rects, texts)
}

# =============================================================================
# CHART 01 — PBRENT Oil Price History + Cycle Annotation
# =============================================================================
hdr("Chart 01: Oil price history")

mac_hist <- mac_spine[!is.na(pbrent) & cal_date >= as.Date("2005-01-01")]

p_oil_level <- ggplot(mac_hist, aes(x = cal_date, y = pbrent)) +
  ep_rects() +
  geom_line(colour = COL_OIL, linewidth = 0.9) +
  geom_hline(yintercept = mean(mac_hist$pbrent, na.rm=TRUE),
             linetype = "dashed", colour = "#888888", linewidth = 0.4) +
  annotate("text", x = min(mac_hist$cal_date) + 60,
           y = mean(mac_hist$pbrent, na.rm=TRUE) + 4,
           label = "Period average", size = 2.8, colour = "#888888") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "/bbl")) +
  labs(title    = "Brent Crude Oil Price (PBRENT) — 2005Q1 to 2025Q4",
       subtitle = "Shaded: GFC 2008–09 | Shale bust 2014–16 | COVID 2020 | Post-COVID surge 2021–22",
       x = NULL, y = "$/barrel",
       caption  = "Source: FRB CCAR 2026 Baseline (macro_base_pbrent)") +
  theme_pub()

p_oil_yoy <- ggplot(mac_hist[!is.na(yoy_oil)], aes(x = cal_date, y = yoy_oil,
                                                      fill = yoy_oil >= 0)) +
  geom_col(width = 70, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = COL_POS, "FALSE" = COL_NEG)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "YoY % Change in Brent Oil Price",
       x = NULL, y = "YoY %",
       caption = "Source: FRB CCAR 2026 Baseline") +
  theme_pub()

p01 <- p_oil_level / p_oil_yoy +
  plot_annotation(
    title   = "FIGURE 01 — Brent Crude Oil Price: Level & Annual Change",
    theme   = theme(plot.title = element_text(face="bold", size=12))
  )

save_plot(p01, "01_oil_price_history.png", w=11, h=8)

# =============================================================================
# CHART 02 — PBRENT vs Aggregate CU Outcomes (time-series overlay)
# =============================================================================
hdr("Chart 02: PBRENT vs CU outcomes")

agg <- agg_quarter(panel, cu_outcomes)
agg <- merge(agg, mac_spine[, .(yyyyqq, pbrent, yoy_oil)],
             by="yyyyqq", all.x=TRUE)

make_dual_axis <- function(outcome_var, outcome_label, y_fmt = waiver()) {
  if (!outcome_var %in% names(agg)) return(NULL)

  d <- agg[!is.na(get(outcome_var)) & !is.na(pbrent)]
  ov_max <- max(abs(d[[outcome_var]]), na.rm=TRUE)
  pb_max <- max(abs(d$pbrent),         na.rm=TRUE)
  oil_scale <- if (is.finite(ov_max) && is.finite(pb_max) && pb_max > 0)
                 ov_max / pb_max else 1

  ggplot(d, aes(x = cal_date)) +
    ep_rects() +
    geom_line(aes(y = pbrent * oil_scale, colour = "PBRENT (scaled)"),
              linewidth = 0.6, linetype = "dashed") +
    geom_line(aes(y = get(outcome_var), colour = outcome_label),
              linewidth = 0.85) +
    scale_colour_manual(values = c("PBRENT (scaled)" = COL_OIL,
                                    setNames(COL_DIRECT, outcome_label)),
                        name = NULL) +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    scale_y_continuous(labels = y_fmt) +
    labs(title   = outcome_label,
         x = NULL, y = outcome_label) +
    theme_pub() +
    theme(legend.position = "none")
}

panels_02 <- list(
  make_dual_axis("dq_rate",            "Delinquency Rate (%)",    percent_format(scale=1)),
  make_dual_axis("netintmrg",          "Net Interest Margin (%)", number_format(accuracy=0.1)),
  make_dual_axis("costfds",            "Cost of Funds (%)",       number_format(accuracy=0.01)),
  make_dual_axis("insured_share_growth","Insured Share Growth (YoY%)", number_format(accuracy=0.1)),
  make_dual_axis("cert_share",         "Certificate Share of Deposits", percent_format(scale=100)),
  make_dual_axis("loan_to_share",      "Loan-to-Share Ratio",    number_format(accuracy=0.01))
)
panels_02 <- Filter(Negate(is.null), panels_02)

if (length(panels_02) >= 2) {
  p02 <- wrap_plots(panels_02, ncol = 2) +
    plot_annotation(
      title    = "FIGURE 02 — PBRENT vs Aggregate CU Outcomes (2005–2025)",
      subtitle = "PBRENT dashed orange line (scaled to outcome axis) | Shaded: key oil episodes",
      caption  = "Source: NCUA Form 5300 Call Report; FRB CCAR 2026 Baseline",
      theme    = theme(plot.title    = element_text(face="bold", size=12),
                       plot.subtitle = element_text(size=9, colour="#555"))
    )
  save_plot(p02, "02_pbrent_vs_cu_outcomes.png", w=13, h=10)
}

# =============================================================================
# CHART 03 — Cross-Correlation: PBRENT leads CU outcomes
# =============================================================================
hdr("Chart 03: Cross-correlations")

agg_cc <- merge(agg_quarter(panel, cu_outcomes),
                mac_spine[, .(yyyyqq, yoy_oil)],
                by = "yyyyqq", all.x = TRUE)

cc_results <- rbindlist(lapply(cu_outcomes, function(v) {
  if (!v %in% names(agg_cc)) return(NULL)
  x  <- agg_cc$yoy_oil
  y  <- agg_cc[[v]]
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 20) return(NULL)
  lags <- -4:8  # negative = oil leads by that many quarters
  cors <- sapply(lags, function(k) {
    if (k >= 0) cor(x[ok][1:(sum(ok)-k)], y[ok][(k+1):sum(ok)],
                    use="complete.obs")
    else        cor(x[ok][(-k+1):sum(ok)], y[ok][1:(sum(ok)+k)],
                    use="complete.obs")
  })
  data.table(outcome=v, lag=lags, correlation=cors)
}))

out_labels <- c(
  dq_rate            = "Delinquency Rate",
  chg_tot_lns_ratio  = "Net Charge-Off Ratio",
  netintmrg          = "Net Interest Margin",
  costfds            = "Cost of Funds",
  insured_share_growth = "Insured Share Growth",
  cert_share         = "Certificate Share",
  loan_to_share      = "Loan-to-Share Ratio",
  roa                = "Return on Assets"
)
cc_results[, outcome_label := out_labels[outcome]]
cc_results[is.na(outcome_label), outcome_label := outcome]

p03 <- ggplot(cc_results[!is.na(correlation)],
              aes(x = lag, y = correlation, colour = outcome_label)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "#888") +
  geom_vline(xintercept = 0, linewidth = 0.4, linetype="dashed",
             colour = COL_OIL) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.8) +
  annotate("text", x=0.1, y=Inf, label="← Oil leads  |  Oil lags →",
           vjust=1.5, hjust=0, size=2.8, colour="#888") +
  scale_colour_brewer(palette="Dark2", name="CU Outcome") +
  scale_x_continuous(breaks=-4:8,
                     labels=c(paste0("Lag\n",4:1),"0",
                               paste0("Lead\n",1:8))) +
  labs(title    = "FIGURE 03 — Cross-Correlation: PBRENT YoY vs CU Outcomes",
       subtitle = "Positive lag = oil change precedes CU outcome by that many quarters",
       x = "Quarter lag (positive = PBRENT leads)",
       y = "Pearson Correlation",
       caption = "Source: NCUA Form 5300; FRB CCAR 2026 Baseline") +
  theme_pub() +
  theme(legend.position = "right",
        legend.key.height = unit(0.5,"cm"))

save_plot(p03, "03_cross_correlation.png", w=11, h=6.5)

# =============================================================================
# CHART 04 — Direct vs Indirect: Oil-state vs Non-oil CU outcomes
# =============================================================================
hdr("Chart 04: Direct vs indirect effect")

if ("cu_group" %in% names(panel)) {
  agg_grp <- agg_group(panel,
                        intersect(cu_outcomes, names(panel)),
                        "cu_group")
  agg_grp <- merge(agg_grp,
                   mac_spine[, .(yyyyqq, pbrent, yoy_oil)],
                   by="yyyyqq", all.x=TRUE)

  grp_cols <- c("Direct"     = COL_DIRECT,
                "Indirect"   = COL_INDIR,
                "Negligible" = "#aaaaaa")

  make_group_plot <- function(v, lab, y_fmt=waiver()) {
    if (!v %in% names(agg_grp)) return(NULL)
    ggplot(agg_grp[!is.na(get(v)) & cu_group != "Negligible"],
           aes(x=cal_date, y=get(v), colour=cu_group)) +
      ep_rects() +
      geom_line(linewidth=0.8) +
      scale_colour_manual(values=grp_cols, name="CU group") +
      scale_x_date(date_breaks="3 years", date_labels="%Y") +
      scale_y_continuous(labels=y_fmt) +
      labs(title=lab, x=NULL, y=lab) +
      theme_pub() +
      theme(legend.position="none")
  }

  p04_panels <- list(
    make_group_plot("dq_rate",             "Delinquency Rate (%)"),
    make_group_plot("netintmrg",           "Net Interest Margin (%)"),
    make_group_plot("insured_share_growth","Insured Share Growth (YoY%)"),
    make_group_plot("costfds",             "Cost of Funds (%)"),
    make_group_plot("cert_share",          "Certificate Share",
                    percent_format(scale=100)),
    make_group_plot("loan_to_share",       "Loan-to-Share Ratio")
  )
  p04_panels <- Filter(Negate(is.null), p04_panels)

  # Shared legend
  leg_plot <- ggplot(data.frame(x=1, y=1,
                                 g=c("Direct","Indirect","Negligible")),
                      aes(x,y,colour=g)) +
    geom_line() +
    scale_colour_manual(values=grp_cols, name="CU Group") +
    theme_pub() + theme(legend.position="bottom")
  shared_leg <- cowplot::get_legend(leg_plot)

  p04 <- wrap_plots(p04_panels, ncol=2) +
    plot_annotation(
      title    = "FIGURE 04 — Direct vs Indirect Effect: Oil-State vs Non-Oil CUs",
      subtitle = "Direct = state mining emp share >= 2%  |  Indirect = non-oil CUs with spillover linkage",
      caption  = "Source: NCUA Form 5300; BLS QCEW oil exposure classification",
      theme    = theme(plot.title    = element_text(face="bold", size=12),
                       plot.subtitle = element_text(size=9, colour="#555"))
    )
  save_plot(p04, "04_direct_vs_indirect.png", w=13, h=10)
}

# =============================================================================
# CHART 05 — Deposit Channel: cert_share & loan_to_share vs FOMC regime
# =============================================================================
hdr("Chart 05: Deposit channel")

if (all(c("cert_share","loan_to_share","fomc_x_brent") %in% names(panel))) {

  dep_agg <- agg_quarter(panel, c("cert_share","loan_to_share",
                                   "insured_share_growth","costfds"))
  dep_agg <- merge(dep_agg,
                   mac_spine[, .(yyyyqq, pbrent, fedfunds, fomc_regime)],
                   by="yyyyqq", all.x=TRUE)

  # Regime shading
  dep_agg[, regime_label := fcase(
    fomc_regime ==  1L, "Hiking",
    fomc_regime == -1L, "Cutting",
    default           = "Hold"
  )]

  p_cert <- ggplot(dep_agg[!is.na(cert_share)],
                   aes(x=cal_date, y=cert_share)) +
    ep_rects() +
    geom_line(colour=COL_DIRECT, linewidth=0.85) +
    geom_line(aes(y=fedfunds/100, colour="Fed Funds Rate (RHS)"),
              linetype="dashed", linewidth=0.6) +
    scale_colour_manual(values=c("Fed Funds Rate (RHS)"=COL_OIL), name=NULL) +
    scale_y_continuous(labels=percent_format(scale=100),
                       sec.axis=sec_axis(~.*100, name="Fed Funds Rate (%)")) +
    scale_x_date(date_breaks="2 years", date_labels="%Y") +
    labs(title="Certificate Share of Total Deposits vs Fed Funds Rate",
         subtitle="Rising rates → members shift from demand deposits to certificates (◆ deposit migration)",
         x=NULL, y="Certificate Share") +
    theme_pub()

  p_lts <- ggplot(dep_agg[!is.na(loan_to_share)],
                  aes(x=cal_date, y=loan_to_share)) +
    ep_rects() +
    geom_line(colour=COL_INDIR, linewidth=0.85) +
    geom_hline(yintercept=0.8, linetype="dashed",
               colour=COL_NEG, linewidth=0.4) +
    annotate("text", x=max(dep_agg$cal_date, na.rm=TRUE),
             y=0.81, label="80% liquidity threshold",
             hjust=1, size=2.8, colour=COL_NEG) +
    scale_x_date(date_breaks="2 years", date_labels="%Y") +
    scale_y_continuous(labels=number_format(accuracy=0.01)) +
    labs(title="Loan-to-Share Ratio",
         subtitle="Oil-state CUs: PBRENT↑ → deposit surge → ratio compression",
         x=NULL, y="Loan / Total Shares") +
    theme_pub()

  p_cof <- ggplot(dep_agg[!is.na(costfds)],
                  aes(x=cal_date, y=costfds)) +
    ep_rects() +
    geom_line(colour=COL_SPILL, linewidth=0.85) +
    scale_x_date(date_breaks="2 years", date_labels="%Y") +
    scale_y_continuous(labels=number_format(accuracy=0.01)) +
    labs(title="Cost of Funds (costfds, %)",
         subtitle="CoF squeeze: certificate surge raises funding costs even as deposit volumes grow",
         x=NULL, y="Cost of Funds (%)") +
    theme_pub()

  p_isg <- ggplot(dep_agg[!is.na(insured_share_growth)],
                  aes(x=cal_date, y=insured_share_growth,
                      fill=insured_share_growth>=0)) +
    geom_col(width=70, show.legend=FALSE) +
    scale_fill_manual(values=c("TRUE"=COL_POS,"FALSE"=COL_NEG)) +
    scale_x_date(date_breaks="2 years", date_labels="%Y") +
    scale_y_continuous(labels=number_format(accuracy=0.1)) +
    labs(title="Insured Share Growth (YoY %)",
         subtitle="Oil-state income windfall → deposit inflows; inflation erosion → drawdown",
         x=NULL, y="YoY Growth (%)") +
    theme_pub()

  p05 <- (p_cert + p_lts) / (p_cof + p_isg) +
    plot_annotation(
      title   = "FIGURE 05 — Deposit Channel Dynamics",
      caption = "Source: NCUA Form 5300 Call Report; FRB CCAR 2026 Baseline",
      theme   = theme(plot.title=element_text(face="bold",size=12))
    )
  save_plot(p05, "05_deposit_channel.png", w=13, h=10)
}

# =============================================================================
# CHART 06 — Asset Tier Response: dq_rate & netintmrg by tier
# =============================================================================
hdr("Chart 06: Asset tier response")

if ("asset_tier" %in% names(panel)) {
  tier_agg <- agg_group(panel,
                         intersect(c("dq_rate","netintmrg","costfds",
                                     "cert_share","insured_share_growth"),
                                   names(panel)),
                         "asset_tier")
  tier_agg <- merge(tier_agg,
                    mac_spine[,.(yyyyqq,pbrent,yoy_oil)],
                    by="yyyyqq", all.x=TRUE)

  make_tier_plot <- function(v, lab, y_fmt=waiver()) {
    if (!v %in% names(tier_agg)) return(NULL)
    ggplot(tier_agg[!is.na(get(v))],
           aes(x=cal_date, y=get(v), colour=asset_tier)) +
      ep_rects() +
      geom_line(linewidth=0.75) +
      scale_colour_manual(values=TIER_COLS, name="Asset Tier") +
      scale_x_date(date_breaks="3 years", date_labels="%Y") +
      scale_y_continuous(labels=y_fmt) +
      labs(title=lab, x=NULL, y=lab) +
      theme_pub() +
      theme(legend.position="right")
  }

  t_panels <- list(
    make_tier_plot("dq_rate",  "Delinquency Rate (%)"),
    make_tier_plot("netintmrg","Net Interest Margin (%)"),
    make_tier_plot("costfds",  "Cost of Funds (%)"),
    make_tier_plot("cert_share","Certificate Share",
                   percent_format(scale=100))
  )
  t_panels <- Filter(Negate(is.null), t_panels)

  p06 <- wrap_plots(t_panels, ncol=2) +
    plot_annotation(
      title    = "FIGURE 06 — CU Outcomes by Asset Tier (2005–2025)",
      subtitle = "T1 < $10M | T2 $10-100M | T3 $100M-$1B | T4 > $1B",
      caption  = "Source: NCUA Form 5300 Call Report",
      theme    = theme(plot.title=element_text(face="bold",size=12),
                       plot.subtitle=element_text(size=9,colour="#555"))
    )
  save_plot(p06, "06_asset_tier_response.png", w=13, h=10)
}

# =============================================================================
# CHART 07 — Structural Break: pre vs post 2015Q1 (shale era)
# =============================================================================
hdr("Chart 07: Structural break 2015Q1")

agg_sb <- merge(agg_quarter(panel, cu_outcomes),
                mac_spine[,.(yyyyqq,yoy_oil)],
                by="yyyyqq", all.x=TRUE)
agg_sb[, era := fifelse(yyyyqq < 201501L, "Pre-Shale\n(2005–2014)",
                                           "Post-Shale\n(2015–2025)")]

sb_long <- melt(agg_sb[!is.na(yoy_oil)],
                id.vars     = c("yyyyqq","cal_date","era","yoy_oil"),
                measure.vars= intersect(cu_outcomes, names(agg_sb)),
                variable.name="outcome", value.name="value")
sb_long[, outcome_label := out_labels[as.character(outcome)]]
sb_long[is.na(outcome_label), outcome_label := as.character(outcome)]

# Scatter: yoy_oil vs each outcome, coloured by era
plot_sb_outcomes <- intersect(c("dq_rate","netintmrg","costfds",
                                 "insured_share_growth"), cu_outcomes)

sb_plots <- lapply(plot_sb_outcomes, function(v) {
  d <- sb_long[outcome==v & !is.na(value) & !is.na(yoy_oil)]
  if (nrow(d) < 10) return(NULL)
  ggplot(d, aes(x=yoy_oil, y=value, colour=era)) +
    geom_point(alpha=0.5, size=1.2) +
    geom_smooth(method="lm", se=TRUE, linewidth=0.8) +
    scale_colour_manual(values=c("Pre-Shale\n(2005–2014)"="#1a3a5c",
                                  "Post-Shale\n(2015–2025)"="#b5470a"),
                        name="Era") +
    labs(title  = unique(d$outcome_label),
         x      = "PBRENT YoY %",
         y      = unique(d$outcome_label)) +
    theme_pub() +
    theme(legend.position="right")
})
sb_plots <- Filter(Negate(is.null), sb_plots)

if (length(sb_plots) >= 2) {
  p07 <- wrap_plots(sb_plots, ncol=2) +
    plot_annotation(
      title    = "FIGURE 07 — Structural Break: Oil-CU Relationship Pre vs Post Shale Era (2015Q1)",
      subtitle = "Slope change between eras tests the shale revolution structural break hypothesis",
      caption  = "Source: NCUA Form 5300; FRB CCAR 2026 Baseline",
      theme    = theme(plot.title=element_text(face="bold",size=12),
                       plot.subtitle=element_text(size=9,colour="#555"))
    )
  save_plot(p07, "07_structural_break.png", w=13, h=9)
}

# =============================================================================
# CHART 08 — Spillover: non-oil CU response by spillover tercile
# =============================================================================
hdr("Chart 08: Spillover exposure terciles")

if ("spillover_exposure" %in% names(panel)) {
  non_oil <- panel[oil_exposure_bin == 0 & !is.na(spillover_exposure)]

  if (nrow(non_oil) > 100) {
    # Assign terciles
    breaks <- quantile(non_oil$spillover_exposure,
                       probs=c(0,1/3,2/3,1), na.rm=TRUE)
    non_oil[, spill_tercile := cut(spillover_exposure, breaks=breaks,
                                    labels=c("Low","Medium","High"),
                                    include.lowest=TRUE)]

    spill_agg <- agg_group(non_oil,
                            intersect(c("dq_rate","netintmrg","insured_share_growth",
                                        "costfds"), names(non_oil)),
                            "spill_tercile")
    spill_agg <- merge(spill_agg,
                       mac_spine[,.(yyyyqq,yoy_oil)],
                       by="yyyyqq", all.x=TRUE)

    SPILL_COLS <- c("Low"="#a8d8a8","Medium"="#4a9a6a","High"="#1a5a3a")

    sp_panels <- lapply(
      intersect(c("dq_rate","netintmrg"), names(spill_agg)),
      function(v) {
        lab <- out_labels[v] %||% v
        ggplot(spill_agg[!is.na(get(v)) & !is.na(spill_tercile)],
               aes(x=cal_date, y=get(v), colour=spill_tercile)) +
          ep_rects() +
          geom_line(linewidth=0.8) +
          scale_colour_manual(values=SPILL_COLS,
                               name="Spillover\nTercile") +
          scale_x_date(date_breaks="3 years", date_labels="%Y") +
          labs(title=lab, x=NULL, y=lab) +
          theme_pub()
      })

    if (length(sp_panels) >= 1) {
      p08 <- wrap_plots(sp_panels, ncol=2) +
        plot_annotation(
          title    = "FIGURE 08 — Indirect Channel: Non-Oil CU Response by Spillover Exposure Tercile",
          subtitle = "High spillover = non-oil state with strong economic linkage to oil states",
          caption  = "Source: NCUA Form 5300; BLS QCEW adjacency-weighted spillover index",
          theme    = theme(plot.title=element_text(face="bold",size=12),
                           plot.subtitle=element_text(size=9,colour="#555"))
        )
      save_plot(p08, "08_spillover_terciles.png", w=13, h=6.5)
    }
  }
}

# =============================================================================
# CHART 09 — Episode Heatmap: oil cycle × CU outcome
# =============================================================================
hdr("Chart 09: Episode heatmap")

# Define oil price episodes
ep_def <- data.table(
  episode = c("Pre-GFC\n2005-07","GFC\n2008-09","Recovery\n2010-13",
              "Shale Bust\n2014-16","Rebound\n2017-19","COVID\n2020",
              "Surge\n2021-22","Post-Surge\n2023-25"),
  yr_from = c(2005, 2008, 2010, 2014, 2017, 2020, 2021, 2023),
  yr_to   = c(2007, 2009, 2013, 2016, 2019, 2020, 2022, 2025),
  q_from  = c(1, 1, 1, 1, 1, 1, 1, 1),
  q_to    = c(4, 4, 4, 4, 4, 4, 4, 4)
)
ep_def[, yyyyqq_from := yr_from * 100L + q_from]
ep_def[, yyyyqq_to   := yr_to   * 100L + q_to]

agg_ep <- agg_quarter(panel, cu_outcomes)
agg_ep <- merge(agg_ep, mac_spine[,.(yyyyqq,yoy_oil)], by="yyyyqq", all.x=TRUE)

# Compute mean per episode
heat_list <- lapply(1:nrow(ep_def), function(i) {
  ep  <- ep_def[i]
  sub <- agg_ep[yyyyqq >= ep$yyyyqq_from & yyyyqq <= ep$yyyyqq_to]
  if (nrow(sub) == 0) return(NULL)
  means <- sapply(cu_outcomes, function(v) mean(sub[[v]], na.rm=TRUE))
  as.data.table(c(list(episode=ep$episode), as.list(means)))
})
heat_dt <- rbindlist(heat_list, fill=TRUE)

# Normalise each outcome 0-1 for heatmap colouring
heat_long <- melt(heat_dt, id.vars="episode",
                  variable.name="outcome", value.name="value")
heat_long[, outcome_label := out_labels[as.character(outcome)]]
heat_long[is.na(outcome_label), outcome_label := as.character(outcome)]
heat_long[, norm_val := {
  mn <- min(value, na.rm=TRUE); mx <- max(value, na.rm=TRUE)
  if (mx > mn) (value - mn)/(mx - mn) else 0.5
}, by=outcome]
heat_long[, episode := factor(episode, levels=ep_def$episode)]

# For outcomes where high = stress, flip the colour scale
stress_outcomes <- c("dq_rate","chg_tot_lns_ratio","costfds")
heat_long[outcome %in% stress_outcomes, norm_val := 1 - norm_val]

p09 <- ggplot(heat_long[!is.na(norm_val)],
              aes(x=episode, y=outcome_label, fill=norm_val)) +
  geom_tile(colour="white", linewidth=0.5) +
  geom_text(aes(label=round(value,2)), size=2.5, colour="#1a1a1a") +
  scale_fill_gradient2(low="#2166ac", mid="#f7f7f7", high="#d73027",
                       midpoint=0.5, name="Relative\nlevel\n(green=good)") +
  scale_x_discrete(position="top") +
  labs(title   = "FIGURE 09 — CU Outcome Heatmap by Oil Price Episode",
       subtitle = "Red = elevated stress / tight conditions | Blue = favourable | Raw values shown",
       caption  = "Source: NCUA Form 5300; FRB CCAR 2026 Baseline",
       x=NULL, y=NULL) +
  theme_pub() +
  theme(axis.text.x    = element_text(size=8.5, angle=0),
        axis.text.y    = element_text(size=8.5),
        legend.position= "right",
        panel.grid     = element_blank())

save_plot(p09, "09_episode_heatmap.png", w=13, h=6)

# =============================================================================
# CHART 10 — Missingness Overview
# =============================================================================
hdr("Chart 10: Missingness")

check_miss <- c(
  "dq_rate","chg_tot_lns_ratio","netintmrg","pcanetworth",
  "networth","costfds","roa","insured_tot","dep_shrcert","acct_018",
  "insured_share_growth","cert_share","loan_to_share","nim_spread",
  "macro_base_pbrent","macro_base_lurc","macro_base_pcpi",
  "macro_base_rmtg","macro_base_phpi","macro_base_uypsav",
  "macro_base_yoy_oil","macro_base_yield_curve","macro_base_fomc_regime",
  "oil_exposure_cont","oil_exposure_bin","spillover_exposure",
  "oil_x_brent","fomc_x_brent","oil_bartik_iv"
)
fv  <- intersect(check_miss, names(panel))
pct <- sapply(panel[, ..fv],
              function(x) round(mean(is.na(x))*100, 1))
miss_tbl <- data.table(
  variable    = names(pct),
  pct_missing = pct,
  category    = fcase(
    names(pct) %like% "macro_", "CCAR Macro",
    names(pct) %like% "oil_|spill|brent|fomc_x|bartik", "Exposure/Interaction",
    default = "CU Outcome"
  )
)[order(-pct_missing)]

p10 <- ggplot(miss_tbl, aes(x=pct_missing,
                              y=reorder(variable, -pct_missing),
                              fill=category)) +
  geom_col(width=0.7) +
  geom_vline(xintercept=c(5,10,20), linetype="dashed",
             colour="#bbbbbb", linewidth=0.3) +
  scale_fill_manual(
    values=c("CU Outcome"="#1a3a5c","CCAR Macro"="#b5470a",
             "Exposure/Interaction"="#2d7a4a"),
    name="Category") +
  scale_x_continuous(labels=percent_format(scale=1),
                     limits=c(0, max(miss_tbl$pct_missing)*1.1)) +
  labs(title   = "FIGURE 10 — Variable Missingness Overview (panel_base)",
       subtitle = "Variables with >10% missing require investigation before modelling",
       caption  = "Source: NCUA Form 5300 + FRB CCAR 2026 Baseline merged panel",
       x="% Missing", y=NULL) +
  theme_pub() +
  theme(legend.position="right",
        panel.grid.major.y=element_blank())

save_plot(p10, "10_missingness.png", w=10, h=7)

# =============================================================================
# SUMMARY TABLE — key descriptive statistics by cu_group
# =============================================================================
hdr("Descriptive statistics")

desc_vars <- intersect(c("dq_rate","chg_tot_lns_ratio","netintmrg",
                          "pcanetworth","costfds","roa",
                          "insured_share_growth","cert_share",
                          "loan_to_share"), names(panel))

group_col <- if ("cu_group" %in% names(panel)) "cu_group" else "oil_exposure_bin"

desc_tbl <- panel[!is.na(get(group_col)),
  lapply(.SD, function(x)
    sprintf("%.3f (%.3f)", mean(x,na.rm=TRUE), sd(x,na.rm=TRUE))),
  by = group_col,
  .SDcols = desc_vars]

cat("\n  Descriptive statistics by CU group (mean (sd)):\n")
print(t(desc_tbl), quote=FALSE)

# =============================================================================
# COMPLETE
# =============================================================================
cat("\n=================================================================\n")
cat(" SCRIPT 02 COMPLETE — FIGURES SAVED TO Figures/\n")
cat("=================================================================\n")
figs <- list.files("Figures", pattern="\\.png$", full.names=FALSE)
for (f in sort(figs)) cat(sprintf("  %s\n", f))
cat("=================================================================\n")
