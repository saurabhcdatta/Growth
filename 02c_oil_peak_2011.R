# =============================================================================
# OIL PRICE SHOCK × CREDIT UNION RESEARCH
# Script 02c — Event Study: 2011 Oil Price Peak
# =============================================================================
# Context: WTI/Brent crude hit cycle peak ~$113-127/bbl in Apr-May 2011
#          Driven by: Arab Spring (Libya supply disruption) + global demand
#          recovery post-GFC. This is a clean supply shock — exogenous to
#          US credit union fundamentals — ideal natural experiment.
#
# Event window:
#   Pre-peak  : 2009Q1 – 2010Q4  (post-GFC recovery, rising oil)
#   Peak      : 2011Q1 – 2011Q4  (sustained high oil, $100-127/bbl)
#   Post-peak : 2012Q1 – 2013Q4  (gradual decline before shale bust)
#
# Charts produced:
#   2c01  Oil price context: 2009-2013 with event window annotation
#   2c02  CU outcomes through the episode (indexed to 2010Q4 = 100)
#   2c03  Direct vs indirect CU response at the peak
#   2c04  Before / During / After mean comparison (bar chart)
#   2c05  Lag response: how many quarters after the peak do outcomes respond
#   2c06  Macro backdrop: FOMC regime, unemployment, CPI during episode
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
cat(" SCRIPT 02c: EVENT STUDY — 2011 OIL PRICE PEAK\n")
cat("=================================================================\n")

dir.create("Figures", showWarnings = FALSE)

# =============================================================================
# THEME & COLOURS
# =============================================================================
theme_pub <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
  theme(
    plot.title        = element_text(size=base_size+2, face="bold",
                                      margin=margin(b=4)),
    plot.subtitle     = element_text(size=base_size-0.5, colour="#555555",
                                      margin=margin(b=8)),
    plot.caption      = element_text(size=base_size-2, colour="#888888",
                                      hjust=0),
    axis.title        = element_text(size=base_size-0.5, colour="#333333"),
    axis.text         = element_text(size=base_size-1.5, colour="#444444"),
    panel.grid.major  = element_line(colour="#e8e8e8", linewidth=0.3),
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(colour="#cccccc", fill=NA, linewidth=0.4),
    strip.text        = element_text(size=base_size-1, face="bold"),
    strip.background  = element_rect(fill="#f5f5f5", colour="#cccccc"),
    legend.position   = "bottom",
    legend.text       = element_text(size=base_size-1.5),
    legend.title      = element_text(size=base_size-1, face="bold"),
    plot.margin       = margin(10,12,8,10)
  )
}

COL_OIL    <- "#b5470a"
COL_DIRECT <- "#1a3a5c"
COL_INDIR  <- "#2d7a4a"
COL_PEAK   <- "#c0392b"
COL_PRE    <- "#2980b9"
COL_POST   <- "#8e44ad"

save_plot <- function(p, filename, w=11, h=7, dpi=300) {
  path <- file.path("Figures", filename)
  ggsave(path, plot=p, width=w, height=h, dpi=dpi, bg="white")
  msg("  Saved: %s", path)
}

# =============================================================================
# LOAD DATA
# =============================================================================
hdr("Loading data")

panel <- readRDS("Data/panel_base.rds")
macro <- readRDS("Data/macro_base.rds")
setDT(panel); setDT(macro)

Q_MONTH <- c("1"=1L,"2"=4L,"3"=7L,"4"=10L)
panel[, cal_date := as.Date(paste(year, Q_MONTH[as.character(quarter)],
                                   "01", sep="-"))]
macro[, cal_date := as.Date(paste(year, Q_MONTH[as.character(quarter)],
                                   "01", sep="-"))]

mac_spine <- unique(macro[, .(
  cal_date, yyyyqq,
  pbrent      = macro_base_pbrent,
  yoy_oil     = macro_base_yoy_oil,
  lurc        = macro_base_lurc,
  pcpi        = macro_base_pcpi,
  rmtg        = macro_base_rmtg,
  fomc_regime = macro_base_fomc_regime,
  real_rate   = macro_base_real_rate,
  yield_curve = macro_base_yield_curve
)])[order(cal_date)]

# =============================================================================
# EVENT WINDOW DEFINITION
# =============================================================================
# Phase 0 — GFC trough:      2009Q1 – 2009Q4  (baseline reference)
# Phase 1 — Pre-peak rise:   2010Q1 – 2010Q4  (oil recovering, <$90)
# Phase 2 — Peak episode:    2011Q1 – 2011Q4  (oil $90-127, Arab Spring)
# Phase 3 — Post-peak:       2012Q1 – 2013Q4  (oil declining, $80-110)
# Phase 4 — Shale bust onset:2014Q1 – 2014Q4  (comparison: next shock)

PHASES <- data.table(
  phase     = c("GFC Trough\n2009","Pre-Peak\n2010",
                "2011 Peak\n(Arab Spring)","Post-Peak\n2012-13",
                "Shale Bust\nOnset 2014"),
  label_short = c("GFC","Pre","Peak","Post","Bust"),
  yyyyqq_from = c(200901L, 201001L, 201101L, 201201L, 201401L),
  yyyyqq_to   = c(200904L, 201004L, 201104L, 201304L, 201404L),
  fill_col    = c("#fde8e8","#e8f4fd","#fff0e0","#e8fde8","#f5e8fd")
)

STUDY_FROM <- 200901L
STUDY_TO   <- 201404L
PEAK_FROM  <- as.Date("2011-01-01")
PEAK_TO    <- as.Date("2011-12-31")
BASE_QTR   <- 201004L  # index reference: 2010Q4

cu_outcomes <- intersect(
  c("dq_rate","chg_tot_lns_ratio","netintmrg","pcanetworth",
    "costfds","roa","insured_share_growth","cert_share","loan_to_share"),
  names(panel))

msg("  Study window: %d to %d | Outcomes: %s",
    STUDY_FROM, STUDY_TO, paste(cu_outcomes, collapse=", "))

# Subset panel and macro to study window
panel_ev <- panel[yyyyqq >= STUDY_FROM & yyyyqq <= STUDY_TO]
mac_ev   <- mac_spine[yyyyqq >= STUDY_FROM & yyyyqq <= STUDY_TO]

# Assign phase labels
assign_phase <- function(dt) {
  dt[, phase := fcase(
    yyyyqq >= 200901L & yyyyqq <= 200904L, "GFC Trough\n2009",
    yyyyqq >= 201001L & yyyyqq <= 201004L, "Pre-Peak\n2010",
    yyyyqq >= 201101L & yyyyqq <= 201104L, "2011 Peak\n(Arab Spring)",
    yyyyqq >= 201201L & yyyyqq <= 201304L, "Post-Peak\n2012-13",
    yyyyqq >= 201401L & yyyyqq <= 201404L, "Shale Bust\nOnset 2014",
    default = NA_character_
  )]
  dt[, phase := factor(phase, levels=PHASES$phase)]
}
assign_phase(panel_ev)
assign_phase(mac_ev)

# =============================================================================
# CHART 2c01 — Oil Price Context: 2009-2014 with Phase Annotations
# =============================================================================
hdr("Chart 2c01: Oil price context")

# Full episode chart
p2c01_main <- ggplot(mac_ev[!is.na(pbrent)], aes(x=cal_date, y=pbrent)) +
  # Phase shading
  mapply(function(fr, to, fl) {
    annotate("rect",
             xmin  = as.Date(paste0(substr(as.character(fr),1,4),"-",
                                    c("1"="01","2"="04","3"="07","4"="10")[
                                      substr(as.character(fr),7,7)],"-01")),
             xmax  = as.Date(paste0(substr(as.character(to),1,4),"-",
                                    c("1"="01","2"="04","3"="07","4"="10")[
                                      substr(as.character(to),7,7)],"-01")) + 90,
             ymin=-Inf, ymax=Inf, fill=fl, alpha=0.4)
  }, PHASES$yyyyqq_from, PHASES$yyyyqq_to, PHASES$fill_col, SIMPLIFY=FALSE) +
  # Peak annotation band
  annotate("rect", xmin=PEAK_FROM, xmax=PEAK_TO,
           ymin=100, ymax=Inf, fill="#ff6b35", alpha=0.15) +
  geom_line(colour=COL_OIL, linewidth=1.1) +
  geom_point(colour=COL_OIL, size=2.2) +
  # Peak label
  annotate("text", x=as.Date("2011-06-01"), y=130,
           label="Peak: ~$127/bbl\nApr 2011\n(Arab Spring)", 
           size=3, colour=COL_PEAK, fontface="bold", hjust=0.5) +
  annotate("segment", x=as.Date("2011-04-01"), xend=as.Date("2011-04-01"),
           y=127, yend=122, arrow=arrow(length=unit(0.2,"cm")),
           colour=COL_PEAK, linewidth=0.6) +
  # $100 threshold line
  geom_hline(yintercept=100, linetype="dashed",
             colour="#888888", linewidth=0.4) +
  annotate("text", x=as.Date("2009-03-01"), y=102,
           label="$100/bbl", size=2.8, colour="#888888") +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="/bbl")) +
  labs(title="Brent Crude Oil — 2009 to 2014",
       subtitle="Orange shading = 2011 peak episode | Phases: GFC Trough → Pre-Peak → 2011 Peak → Post-Peak → Shale Bust Onset",
       x=NULL, y="$/barrel") +
  theme_pub()

# YoY change bars
p2c01_yoy <- ggplot(mac_ev[!is.na(yoy_oil)],
                     aes(x=cal_date, y=yoy_oil, fill=yoy_oil>=0)) +
  geom_col(width=70, show.legend=FALSE) +
  scale_fill_manual(values=c("TRUE"="#27ae60","FALSE"="#c0392b")) +
  geom_hline(yintercept=0, linewidth=0.3) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(labels=number_format(accuracy=1, suffix="%")) +
  labs(title="YoY % Change in Brent Oil", x=NULL, y="YoY %") +
  theme_pub()

p2c01 <- p2c01_main / p2c01_yoy +
  plot_annotation(
    title   = "FIGURE 2c01 — The 2011 Oil Price Episode: Context & Magnitude",
    caption = "Source: FRB CCAR 2026 Baseline (PBRENT); Arab Spring onset Feb 2011",
    theme   = theme(plot.title=element_text(face="bold", size=12))
  )
save_plot(p2c01, "2c01_oil_2011_context.png", w=11, h=8)

# =============================================================================
# CHART 2c02 — CU Outcomes Indexed to Pre-Peak (2010Q4 = 100)
# =============================================================================
hdr("Chart 2c02: CU outcomes indexed to 2010Q4")

# Aggregate to quarter level
agg_ev <- panel_ev[, c(
  list(cal_date=first(cal_date), phase=first(phase)),
  lapply(.SD, function(x) mean(x, na.rm=TRUE))),
  by=yyyyqq, .SDcols=cu_outcomes][order(yyyyqq)]

agg_ev <- merge(agg_ev, mac_ev[,.(yyyyqq,pbrent,yoy_oil)],
                by="yyyyqq", all.x=TRUE)

# Index each outcome: value / value_at_base * 100
base_vals <- agg_ev[yyyyqq == BASE_QTR]

plot_outcomes <- intersect(
  c("dq_rate","netintmrg","costfds","insured_share_growth",
    "cert_share","loan_to_share"), cu_outcomes)

indexed_list <- lapply(plot_outcomes, function(v) {
  if (!v %in% names(agg_ev)) return(NULL)
  base_val <- base_vals[[v]]
  if (is.null(base_val) || is.na(base_val) || base_val == 0) return(NULL)
  d <- agg_ev[!is.na(get(v)), .(yyyyqq, cal_date, phase,
                                  value=get(v),
                                  indexed=get(v)/base_val*100,
                                  outcome=v)]
  d
})
indexed_dt <- rbindlist(Filter(Negate(is.null), indexed_list))

out_labels <- c(
  dq_rate             = "Delinquency Rate",
  netintmrg           = "Net Interest Margin",
  costfds             = "Cost of Funds",
  insured_share_growth= "Insured Share Growth",
  cert_share          = "Certificate Share",
  loan_to_share       = "Loan-to-Share Ratio"
)
indexed_dt[, outcome_label := out_labels[outcome]]
indexed_dt[is.na(outcome_label), outcome_label := outcome]

p2c02 <- ggplot(indexed_dt, aes(x=cal_date, y=indexed, colour=outcome_label)) +
  # Peak shading
  annotate("rect", xmin=PEAK_FROM, xmax=PEAK_TO,
           ymin=-Inf, ymax=Inf, fill="#fff0e0", alpha=0.6) +
  annotate("text", x=as.Date("2011-06-15"), y=Inf,
           label="2011 Peak", vjust=1.4, size=2.8,
           colour=COL_PEAK, fontface="italic") +
  geom_hline(yintercept=100, linetype="dashed",
             colour="#888888", linewidth=0.4) +
  annotate("text", x=as.Date("2009-03-01"), y=101.5,
           label="Base = 2010Q4", size=2.6, colour="#888888") +
  geom_line(linewidth=0.85) +
  geom_point(size=1.8) +
  scale_colour_brewer(palette="Dark2", name="CU Outcome") +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(labels=number_format(accuracy=1, suffix="")) +
  labs(title    = "FIGURE 2c02 — CU Outcomes Indexed to 2010Q4 = 100",
       subtitle  = "Values above 100 = higher than pre-peak base | Oil peak shaded orange",
       x=NULL, y="Index (2010Q4 = 100)",
       caption  = "Source: NCUA Form 5300; FRB CCAR 2026 Baseline") +
  theme_pub() +
  theme(legend.position="right")

save_plot(p2c02, "2c02_cu_outcomes_indexed.png", w=12, h=7)

# =============================================================================
# CHART 2c03 — Direct vs Indirect Response at Peak
# =============================================================================
hdr("Chart 2c03: Direct vs indirect at peak")

# Build grouping
panel_ev2 <- copy(panel_ev)

# Use reporting_state for oil-state classification
sc_col <- intersect(c("reporting_state","state_code","state"), names(panel_ev2))[1]
OIL_STATES_2011 <- c("TX","ND","LA","AK","WY","OK","NM","CO","WV","PA","MT","KS")

if (!is.na(sc_col)) {
  panel_ev2[, cu_grp := fifelse(
    toupper(get(sc_col)) %in% OIL_STATES_2011,
    "Oil-State CUs\n(Direct Channel)",
    "Non-Oil CUs\n(Indirect Channel)"
  )]
  msg("  Groups: %s Direct | %s Indirect",
      format(panel_ev2[cu_grp %like% "Direct", .N], big.mark=","),
      format(panel_ev2[cu_grp %like% "Indirect", .N], big.mark=","))
} else {
  panel_ev2[, cu_grp := "All CUs"]
}

GRP_COLS <- c("Oil-State CUs\n(Direct Channel)"  = COL_DIRECT,
               "Non-Oil CUs\n(Indirect Channel)"  = COL_INDIR,
               "All CUs"                           = "#555555")

plot_vars_03 <- intersect(c("dq_rate","netintmrg","costfds",
                              "insured_share_growth"), cu_outcomes)

agg_grp03 <- panel_ev2[!is.na(cu_grp),
  c(list(cal_date=first(cal_date), phase=first(phase)),
    lapply(.SD, function(x) mean(x, na.rm=TRUE))),
  by=.(yyyyqq, cu_grp),
  .SDcols=plot_vars_03
][order(yyyyqq)]

make_grp_plot <- function(v, lab) {
  if (!v %in% names(agg_grp03)) return(NULL)
  d <- agg_grp03[!is.na(get(v))]
  ggplot(d, aes(x=cal_date, y=get(v), colour=cu_grp)) +
    annotate("rect", xmin=PEAK_FROM, xmax=PEAK_TO,
             ymin=-Inf, ymax=Inf, fill="#fff0e0", alpha=0.5) +
    geom_line(linewidth=0.85) +
    geom_point(size=1.8) +
    scale_colour_manual(values=GRP_COLS, name=NULL) +
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    labs(title=lab, x=NULL, y=lab) +
    theme_pub() +
    theme(legend.position="bottom",
          legend.text=element_text(size=7.5))
}

p03_panels <- lapply(setNames(plot_vars_03,
                               out_labels[plot_vars_03]),
                      function(v) make_grp_plot(v, out_labels[v]))
p03_panels <- Filter(Negate(is.null), p03_panels)

if (length(p03_panels) >= 2) {
  p2c03 <- wrap_plots(p03_panels, ncol=2, guides="collect") &
    theme(legend.position="bottom")
  p2c03 <- p2c03 +
    plot_annotation(
      title    = "FIGURE 2c03 — Direct vs Indirect Channel: 2011 Oil Peak Episode",
      subtitle = "Oil-state CUs feel direct income/credit effects | Non-oil CUs feel macro spillover",
      caption  = "Source: NCUA Form 5300; Oil-state = TX,ND,LA,AK,WY,OK,NM,CO,WV,PA,MT,KS",
      theme    = theme(plot.title=element_text(face="bold", size=12),
                       plot.subtitle=element_text(size=9,colour="#555"))
    )
  save_plot(p2c03, "2c03_direct_vs_indirect_2011.png", w=13, h=9)
}

# =============================================================================
# CHART 2c04 — Before / During / After Mean Comparison
# =============================================================================
hdr("Chart 2c04: Before/During/After means")

panel_ev3 <- copy(panel_ev)
panel_ev3[, bda := fcase(
  yyyyqq >= 201001L & yyyyqq <= 201004L, "Before\n(2010)",
  yyyyqq >= 201101L & yyyyqq <= 201104L, "During\n(2011 Peak)",
  yyyyqq >= 201201L & yyyyqq <= 201304L, "After\n(2012-13)",
  default = NA_character_
)]
panel_ev3[, bda := factor(bda, levels=c("Before\n(2010)",
                                          "During\n(2011 Peak)",
                                          "After\n(2012-13)"))]

bda_agg <- panel_ev3[!is.na(bda),
  lapply(.SD, function(x) mean(x, na.rm=TRUE)),
  by=bda, .SDcols=plot_vars_03
]

bda_long <- melt(bda_agg, id.vars="bda",
                  variable.name="outcome", value.name="mean_val")
bda_long[, outcome_label := out_labels[as.character(outcome)]]
bda_long[is.na(outcome_label), outcome_label := as.character(outcome)]

# Normalise within each outcome for colour scale
bda_long[, norm_val := {
  mn <- min(mean_val, na.rm=TRUE)
  mx <- max(mean_val, na.rm=TRUE)
  if (mx > mn) (mean_val - mn)/(mx-mn) else 0.5
}, by=outcome]

BDA_COLS <- c("Before\n(2010)"       = COL_PRE,
               "During\n(2011 Peak)" = COL_PEAK,
               "After\n(2012-13)"    = COL_POST)

p2c04 <- ggplot(bda_long, aes(x=bda, y=mean_val, fill=bda)) +
  geom_col(width=0.65, show.legend=FALSE) +
  geom_text(aes(label=round(mean_val, 3)),
            vjust=-0.4, size=2.8, fontface="bold") +
  scale_fill_manual(values=BDA_COLS) +
  facet_wrap(~outcome_label, scales="free_y", ncol=2) +
  labs(title    = "FIGURE 2c04 — CU Outcome Means: Before / During / After 2011 Oil Peak",
       subtitle  = "Simple mean comparison across the three phases of the episode",
       x=NULL, y="Mean Value",
       caption  = "Source: NCUA Form 5300 Call Report") +
  theme_pub() +
  theme(strip.text=element_text(size=9, face="bold"),
        axis.text.x=element_text(size=8))

save_plot(p2c04, "2c04_before_during_after.png", w=13, h=9)

# =============================================================================
# CHART 2c05 — Lag Response Analysis
# =============================================================================
hdr("Chart 2c05: Lag response analysis")

# Cross-correlate: PBRENT YoY vs each CU outcome at lags 0-8
# Focus on the 2009-2014 window only (clean episode isolation)

cc_ev <- agg_ev[!is.na(yoy_oil)]

lag_results <- rbindlist(lapply(plot_vars_03, function(v) {
  if (!v %in% names(cc_ev)) return(NULL)
  x <- cc_ev$yoy_oil
  y <- cc_ev[[v]]
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 8) return(NULL)

  lags <- 0:8
  cors <- sapply(lags, function(k) {
    n <- sum(ok)
    if (k == 0) cor(x[ok], y[ok], use="complete.obs")
    else if (k < n) cor(x[ok][1:(n-k)], y[ok][(k+1):n], use="complete.obs")
    else NA_real_
  })
  data.table(outcome=v, lag=lags, correlation=cors)
}))

lag_results[, outcome_label := out_labels[outcome]]
lag_results[is.na(outcome_label), outcome_label := outcome]

# Find peak lag per outcome
peak_lags <- lag_results[!is.na(correlation),
                           .(peak_lag = lag[which.max(abs(correlation))],
                             peak_cor = correlation[which.max(abs(correlation))]),
                           by=outcome_label]

p2c05 <- ggplot(lag_results[!is.na(correlation)],
                 aes(x=lag, y=correlation, colour=outcome_label)) +
  geom_hline(yintercept=0, linewidth=0.3, colour="#888") +
  geom_vline(xintercept=0, linetype="dashed",
             colour=COL_PEAK, linewidth=0.4) +
  geom_line(linewidth=0.85) +
  geom_point(size=2.5) +
  # Peak lag markers
  geom_point(data=lag_results[lag_results[, .I[which.max(abs(correlation))],
                                           by=outcome]$V1],
             aes(x=lag, y=correlation),
             shape=21, size=4, fill="white", stroke=1.5) +
  scale_colour_brewer(palette="Dark2", name="CU Outcome") +
  scale_x_continuous(breaks=0:8,
                     labels=paste0(0:8,"Q")) +
  labs(title    = "FIGURE 2c05 — Lag Response: How Many Quarters After 2011 Peak Do Outcomes Respond?",
       subtitle  = "Open circles = quarter of peak correlation | x=0: same quarter as oil shock",
       x="Lag (quarters after PBRENT shock)",
       y="Pearson Correlation",
       caption  = "Source: NCUA Form 5300; computed on 2009Q1-2014Q4 episode window only") +
  theme_pub() +
  theme(legend.position="right")

save_plot(p2c05, "2c05_lag_response_2011.png", w=12, h=7)

msg("  Peak lag by outcome:")
print(peak_lags, row.names=FALSE)

# =============================================================================
# CHART 2c06 — Macro Backdrop During Episode
# =============================================================================
hdr("Chart 2c06: Macro backdrop")

# Panel 1: PBRENT + Fed Funds Rate (dual axis)
p_rates <- ggplot(mac_ev[!is.na(pbrent)], aes(x=cal_date)) +
  annotate("rect", xmin=PEAK_FROM, xmax=PEAK_TO,
           ymin=-Inf, ymax=Inf, fill="#fff0e0", alpha=0.5) +
  geom_line(aes(y=pbrent, colour="PBRENT ($/bbl)"), linewidth=0.9) +
  scale_colour_manual(values=c("PBRENT ($/bbl)"=COL_OIL), name=NULL) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  labs(title="Brent Oil Price", x=NULL, y="$/bbl") +
  theme_pub()

# Panel 2: Unemployment
p_unemp <- ggplot(mac_ev[!is.na(lurc)], aes(x=cal_date, y=lurc)) +
  annotate("rect", xmin=PEAK_FROM, xmax=PEAK_TO,
           ymin=-Inf, ymax=Inf, fill="#fff0e0", alpha=0.5) +
  geom_line(colour="#e74c3c", linewidth=0.9) +
  geom_hline(yintercept=mac_ev[yyyyqq==201004L, lurc],
             linetype="dashed", colour="#888", linewidth=0.4) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(labels=number_format(accuracy=0.1, suffix="%")) +
  labs(title="Unemployment Rate (LURC)", x=NULL, y="%") +
  theme_pub()

# Panel 3: CPI
p_cpi <- ggplot(mac_ev[!is.na(pcpi)], aes(x=cal_date, y=pcpi)) +
  annotate("rect", xmin=PEAK_FROM, xmax=PEAK_TO,
           ymin=-Inf, ymax=Inf, fill="#fff0e0", alpha=0.5) +
  geom_line(colour="#e67e22", linewidth=0.9) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(labels=number_format(accuracy=0.1)) +
  labs(title="Consumer Price Index (PCPI)", x=NULL, y="Index") +
  theme_pub()

# Panel 4: FOMC regime bar
p_fomc <- ggplot(mac_ev[!is.na(fomc_regime)],
                  aes(x=cal_date, y=fomc_regime,
                      fill=factor(fomc_regime))) +
  annotate("rect", xmin=PEAK_FROM, xmax=PEAK_TO,
           ymin=-Inf, ymax=Inf, fill="#fff0e0", alpha=0.5) +
  geom_col(width=70, show.legend=FALSE) +
  geom_hline(yintercept=0, linewidth=0.3) +
  scale_fill_manual(values=c("-1"="#c0392b","0"="#95a5a6","1"="#27ae60")) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(breaks=c(-1,0,1),
                     labels=c("Cutting","Hold","Hiking")) +
  labs(title="FOMC Regime (+1 Hike / 0 Hold / -1 Cut)",
       subtitle="2011 peak occurred during ZIRP/hold — Fed could not offset oil shock",
       x=NULL, y=NULL) +
  theme_pub()

p2c06 <- (p_rates + p_unemp) / (p_cpi + p_fomc) +
  plot_annotation(
    title    = "FIGURE 2c06 — Macro Backdrop: The 2011 Oil Peak in Context",
    subtitle = paste("Key: High unemployment (post-GFC) + ZIRP (Fed on hold) + oil surge =",
                     "stagflation risk | CUs caught between member stress and margin pressure"),
    caption  = "Source: FRB CCAR 2026 Baseline macro scenario",
    theme    = theme(plot.title=element_text(face="bold", size=12),
                     plot.subtitle=element_text(size=8.5, colour="#555"))
  )
save_plot(p2c06, "2c06_macro_backdrop_2011.png", w=13, h=9)

# =============================================================================
# SUMMARY TABLE — Episode Statistics
# =============================================================================
hdr("Episode summary statistics")

ep_stats <- panel_ev[!is.na(phase),
  lapply(.SD, function(x) round(mean(x, na.rm=TRUE), 4)),
  by=phase, .SDcols=intersect(plot_vars_03, names(panel_ev))
][order(phase)]

cat("\n  CU Outcome Means by Phase:\n")
print(ep_stats, row.names=FALSE)

# PBRENT stats by phase
pb_stats <- mac_ev[!is.na(phase) & !is.na(pbrent),
                    .(pbrent_mean=round(mean(pbrent),1),
                      pbrent_max =round(max(pbrent),1),
                      pbrent_min =round(min(pbrent),1)),
                    by=phase][order(phase)]

cat("\n  PBRENT by Phase ($/bbl):\n")
print(pb_stats, row.names=FALSE)

# =============================================================================
# COMPLETE
# =============================================================================
cat("\n=================================================================\n")
cat(" SCRIPT 02c COMPLETE — EVENT STUDY: 2011 OIL PEAK\n")
cat("=================================================================\n")
figs <- list.files("Figures", pattern="2c0", full.names=FALSE)
for (f in sort(figs)) cat(sprintf("  %s\n", f))
cat("\n  Key finding to investigate:\n")
cat("  Compare NIM / CoF / delinquency levels DURING vs BEFORE peak\n")
cat("  Look for lag: do CU outcomes respond 1-4 quarters after PBRENT peak?\n")
cat("  Check: did oil-state CUs see deposit INFLOWS (buffer effect) in 2011?\n")
cat("=================================================================\n")
