# =============================================================================
# OIL PRICE SHOCK × CREDIT UNION RESEARCH
# Script 02c — Event Study: 2008 Record Peak & 2011 Secondary Peak
# =============================================================================
# From Yahoo Finance (CL=F WTI):
#   ALL-TIME RECORD : ~$147/bbl  2008Q3  (demand surge + supply constraints)
#   Secondary peak  : ~$120/bbl  2011Q2  (post-GFC recovery + Arab Spring)
#
# Key difference between episodes:
#   2008: Fed cutting 5.25%→0.25%, GFC credit shock overlaps oil shock
#   2011: ZIRP already in place (0.25%), cleaner identification
#         Rate channel muted → isolates real income / deposit channel
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
cat(" SCRIPT 02c: OIL PEAK EVENT STUDY (2008 + 2011)\n")
cat("=================================================================\n")

dir.create("Figures", showWarnings = FALSE)

Q_MONTH <- c("1"=1L, "2"=4L, "3"=7L, "4"=10L)

qqdate <- function(yyyyqq) {
  yr  <- as.integer(floor(yyyyqq / 100))
  qtr <- as.integer(yyyyqq %% 100)
  as.Date(paste(yr, Q_MONTH[as.character(qtr)], "01", sep="-"))
}

theme_pub <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
  theme(
    plot.title       = element_text(size=base_size+2, face="bold", margin=margin(b=4)),
    plot.subtitle    = element_text(size=base_size-0.5, colour="#555", margin=margin(b=8)),
    plot.caption     = element_text(size=base_size-2, colour="#888", hjust=0),
    axis.title       = element_text(size=base_size-0.5, colour="#333"),
    axis.text        = element_text(size=base_size-1.5, colour="#444"),
    panel.grid.major = element_line(colour="#e8e8e8", linewidth=0.3),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour="#ccc", fill=NA, linewidth=0.4),
    strip.text       = element_text(size=base_size-1, face="bold"),
    strip.background = element_rect(fill="#f5f5f5", colour="#ccc"),
    legend.position  = "bottom",
    legend.text      = element_text(size=base_size-1.5),
    plot.margin      = margin(10,12,8,10)
  )
}

COL_OIL  <- "#b5470a"
COL_2008 <- "#c0392b"
COL_2011 <- "#2980b9"
COL_ZIRP <- "#8e44ad"
COL_DIR  <- "#1a3a5c"
COL_IND  <- "#2d7a4a"
COL_POS  <- "#27ae60"
COL_NEG  <- "#c0392b"

save_plot <- function(p, fn, w=12, h=7.5, dpi=300) {
  path <- file.path("Figures", fn)
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

panel[, cal_date := as.Date(paste(year, Q_MONTH[as.character(quarter)], "01", sep="-"))]
macro[, cal_date := as.Date(paste(year, Q_MONTH[as.character(quarter)], "01", sep="-"))]

mac <- unique(macro[, .(
  cal_date, yyyyqq,
  pbrent  = macro_base_pbrent,
  yoy_oil = macro_base_yoy_oil,
  rff     = macro_base_rff,
  lurc    = macro_base_lurc,
  pcpi    = macro_base_pcpi,
  yc      = macro_base_yield_curve,
  fomc    = macro_base_fomc_regime
)])[order(cal_date)]

cu_vars <- intersect(
  c("dq_rate","chg_tot_lns_ratio","netintmrg","costfds","roa",
    "pcanetworth","insured_share_growth","cert_share","loan_to_share"),
  names(panel))

state_col <- intersect(c("reporting_state","state_code","state"), names(panel))[1]
OIL_ST    <- c("TX","ND","LA","AK","WY","OK","NM","CO","WV","PA","MT")
if (!is.na(state_col))
  panel[, oil_group := fifelse(toupper(get(state_col)) %in% OIL_ST,
                                "Oil-State", "Non-Oil")]

msg("  Panel: %s CU-qtrs | state col: %s",
    format(nrow(panel), big.mark=","), state_col)
if ("oil_group" %in% names(panel))
  msg("  Oil-state: %s | Non-oil: %s",
      format(panel[oil_group=="Oil-State",.N], big.mark=","),
      format(panel[oil_group=="Non-Oil",.N],   big.mark=","))

# =============================================================================
# EPISODE DEFINITIONS  (all dates as proper Date objects — no substr parsing)
# =============================================================================

EP08 <- list(
  name      = "2008 Record Peak",
  peak_qtr  = 200803L,
  peak_date = qqdate(200803L),
  win_from  = qqdate(200601L),
  win_to    = qqdate(201004L),
  col       = COL_2008,
  phases = data.table(
    label = c("Pre-Rise", "Rise", "Record Peak", "GFC Crash", "Recovery"),
    xmin  = c(qqdate(200601L), qqdate(200701L), qqdate(200803L),
               qqdate(200804L), qqdate(200901L)),
    xmax  = c(qqdate(200700L+4L), qqdate(200802L), qqdate(200803L),
               qqdate(200901L), qqdate(201004L)),
    fill  = c("#e8f0fd","#fdf0e8","#fde8e8","#f0e8fd","#e8fde8")
  )
)

EP11 <- list(
  name      = "2011 Secondary Peak",
  peak_qtr  = 201102L,
  peak_date = qqdate(201102L),
  win_from  = qqdate(200901L),
  win_to    = qqdate(201404L),
  col       = COL_2011,
  phases = data.table(
    label = c("Post-GFC Trough", "Recovery Rise", "2011 Peak",
               "Post-Peak Decline", "New Normal"),
    xmin  = c(qqdate(200901L), qqdate(200902L), qqdate(201101L),
               qqdate(201103L), qqdate(201301L)),
    xmax  = c(qqdate(200904L), qqdate(201101L), qqdate(201102L),
               qqdate(201204L), qqdate(201404L)),
    fill  = c("#e8f0fd","#fdf0e8","#fde8e8","#e8fde8","#f5f0fa")
  )
)

# Fix EP08 phase xmax for "Pre-Rise" (avoid qqdate of invalid 200704)
EP08$phases[label == "Pre-Rise", xmax := qqdate(200704L)]
EP08$phases[label == "Rise",     xmax := qqdate(200802L)]

# Helper: flat list of rect + text annotate layers — uses pre-built Date cols
ep_rects <- function(ep) {
  rects <- mapply(function(xn, xx, fl)
    annotate("rect", xmin=xn, xmax=xx, ymin=-Inf, ymax=Inf,
             fill=fl, alpha=0.35),
    ep$phases$xmin, ep$phases$xmax, ep$phases$fill,
    SIMPLIFY=FALSE)
  texts <- mapply(function(xn, xx, lb)
    annotate("text", x=xn + (xx-xn)/2, y=Inf, label=lb,
             vjust=1.4, size=2.2, colour="#888888", fontface="italic"),
    ep$phases$xmin, ep$phases$xmax, ep$phases$label,
    SIMPLIFY=FALSE)
  vline <- list(geom_vline(xintercept=ep$peak_date,
                            colour=ep$col, linetype="dashed", linewidth=0.8))
  c(rects, texts, vline)
}

# =============================================================================
# CHART 2c-01 — Full Oil Price History: Both Peaks in Context
# =============================================================================
hdr("Chart 2c-01: Both peaks in context")

mac_full <- mac[!is.na(pbrent) &
                  cal_date >= as.Date("2005-01-01") &
                  cal_date <= as.Date("2015-12-31")]

pb_2008 <- mac_full[yyyyqq == 200803L, pbrent]
pb_2011 <- mac_full[yyyyqq == 201102L, pbrent]

p2c01 <- ggplot(mac_full, aes(x=cal_date, y=pbrent)) +
  annotate("rect", xmin=EP08$win_from, xmax=EP08$win_to,
           ymin=-Inf, ymax=Inf, fill="#fde8e8", alpha=0.2) +
  annotate("rect", xmin=EP11$win_from, xmax=EP11$win_to,
           ymin=-Inf, ymax=Inf, fill="#e8f0fd", alpha=0.2) +
  annotate("rect",
           xmin=as.Date("2008-12-01"), xmax=as.Date("2015-12-31"),
           ymin=-Inf, ymax=5, fill=COL_ZIRP, alpha=0.2) +
  annotate("text", x=as.Date("2011-01-01"), y=3,
           label="ZIRP Era (Fed Funds <= 0.25%)",
           size=2.5, colour=COL_ZIRP, fontface="italic") +
  geom_line(colour=COL_OIL, linewidth=1.0) +
  geom_point(data=mac_full[yyyyqq %in% c(200803L, 201102L)],
             colour=c(COL_2008, COL_2011), size=5, shape=18) +
  annotate("label",
           x=EP08$peak_date, y=pb_2008,
           label=sprintf("ALL-TIME RECORD\n$%.0f/bbl\n2008Q3", pb_2008),
           size=2.8, colour=COL_2008, fontface="bold",
           fill="white", label.size=0.3, vjust=-0.2) +
  annotate("label",
           x=EP11$peak_date, y=pb_2011,
           label=sprintf("Secondary Peak\n$%.0f/bbl\n2011Q2", pb_2011),
           size=2.8, colour=COL_2011, fontface="bold",
           fill="white", label.size=0.3, vjust=-0.2) +
  annotate("segment",
           x=as.Date("2008-09-15"), xend=as.Date("2008-09-15"),
           y=30, yend=pb_2008 * 0.65,
           colour="#aaa", linewidth=0.4, linetype="dotted") +
  annotate("text", x=as.Date("2008-09-20"), y=pb_2008 * 0.67,
           label="Lehman\nCollapse", size=2.3, colour="#888", hjust=0) +
  annotate("segment",
           x=as.Date("2011-02-15"), xend=as.Date("2011-02-15"),
           y=30, yend=pb_2011 * 0.55,
           colour="#aaa", linewidth=0.4, linetype="dotted") +
  annotate("text", x=as.Date("2011-02-20"), y=pb_2011 * 0.57,
           label="Arab\nSpring", size=2.3, colour="#888", hjust=0) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(labels=dollar_format(prefix="$", suffix="/bbl")) +
  labs(title    = "FIGURE 2c-01 — WTI/Brent Crude Oil: Both Pre-Shale Peaks (2005-2015)",
       subtitle = "Red = 2008 record episode | Blue = 2011 secondary episode | Purple band = ZIRP era",
       x=NULL, y="$/barrel",
       caption  = "Source: Yahoo Finance (CL=F); FRB CCAR 2026 Baseline (PBRENT)") +
  theme_pub()

save_plot(p2c01, "2c01_both_peaks_context.png", w=13, h=7)

# =============================================================================
# CHART 2c-02 — CU Outcomes: 2008 Record Peak Episode
# =============================================================================
hdr("Chart 2c-02: CU outcomes — 2008 episode")

agg_08 <- panel[cal_date >= EP08$win_from & cal_date <= EP08$win_to,
  c(list(cal_date=first(cal_date)),
    lapply(.SD, mean, na.rm=TRUE)),
  by=yyyyqq, .SDcols=cu_vars][order(yyyyqq)]
agg_08 <- merge(agg_08, mac[,.(yyyyqq,pbrent,rff)], by="yyyyqq", all.x=TRUE)

make_ep <- function(agg_dt, ep, v, lab, y_fmt=waiver()) {
  if (!v %in% names(agg_dt)) return(NULL)
  d <- agg_dt[!is.na(get(v))]
  if (nrow(d) == 0) return(NULL)
  os <- max(abs(d[[v]]), na.rm=TRUE) / max(abs(d$pbrent), na.rm=TRUE)
  if (!is.finite(os) || os == 0) os <- 1
  ggplot(d, aes(x=cal_date)) +
    ep_rects(ep) +
    geom_line(aes(y=pbrent*os), colour=COL_OIL,
              linetype="dashed", linewidth=0.5, alpha=0.6) +
    geom_line(aes(y=get(v)), colour=ep$col, linewidth=0.9) +
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    scale_y_continuous(labels=y_fmt) +
    labs(title=lab, x=NULL, y=lab) +
    theme_pub() + theme(legend.position="none")
}

p2c02 <- wrap_plots(Filter(Negate(is.null), list(
  make_ep(agg_08, EP08, "dq_rate",             "Delinquency Rate (%)"),
  make_ep(agg_08, EP08, "netintmrg",           "Net Interest Margin (%)"),
  make_ep(agg_08, EP08, "costfds",             "Cost of Funds (%)"),
  make_ep(agg_08, EP08, "insured_share_growth","Insured Share Growth (YoY%)")
)), ncol=2) +
  plot_annotation(
    title   = "FIGURE 2c-02 — CU Outcomes: 2008 ALL-TIME Oil Price Record",
    subtitle= "Fed cutting 5.25%->0.25% | GFC credit shock overlaps | Dashed = PBRENT scaled",
    caption = "Source: NCUA Form 5300; FRB CCAR 2026 Baseline",
    theme   = theme(plot.title=element_text(face="bold",size=12),
                    plot.subtitle=element_text(size=9,colour="#555"))
  )
save_plot(p2c02, "2c02_cu_outcomes_2008_peak.png", w=12, h=9)

# =============================================================================
# CHART 2c-03 — CU Outcomes: 2011 Secondary Peak Episode
# =============================================================================
hdr("Chart 2c-03: CU outcomes — 2011 episode")

agg_11 <- panel[cal_date >= EP11$win_from & cal_date <= EP11$win_to,
  c(list(cal_date=first(cal_date)),
    lapply(.SD, mean, na.rm=TRUE)),
  by=yyyyqq, .SDcols=cu_vars][order(yyyyqq)]
agg_11 <- merge(agg_11, mac[,.(yyyyqq,pbrent,rff)], by="yyyyqq", all.x=TRUE)

p2c03 <- wrap_plots(Filter(Negate(is.null), list(
  make_ep(agg_11, EP11, "dq_rate",             "Delinquency Rate (%)"),
  make_ep(agg_11, EP11, "netintmrg",           "Net Interest Margin (%)"),
  make_ep(agg_11, EP11, "costfds",             "Cost of Funds (%)"),
  make_ep(agg_11, EP11, "insured_share_growth","Insured Share Growth (YoY%)")
)), ncol=2) +
  plot_annotation(
    title   = "FIGURE 2c-03 — CU Outcomes: 2011 Secondary Oil Price Peak",
    subtitle= "ZIRP (0.25%) throughout | Arab Spring supply disruption | Dashed = PBRENT scaled",
    caption = "Source: NCUA Form 5300; FRB CCAR 2026 Baseline",
    theme   = theme(plot.title=element_text(face="bold",size=12),
                    plot.subtitle=element_text(size=9,colour="#555"))
  )
save_plot(p2c03, "2c03_cu_outcomes_2011_peak.png", w=12, h=9)

# =============================================================================
# CHART 2c-04 — Side-by-Side: 2008 vs 2011 Normalised to Peak = 100
# =============================================================================
hdr("Chart 2c-04: Normalised 2008 vs 2011 comparison")

norm_ep <- function(panel_dt, ep, vars) {
  win_from_q <- panel_dt[cal_date >= ep$win_from &
                           cal_date <= ep$win_to, min(yyyyqq)]
  win_to_q   <- panel_dt[cal_date >= ep$win_from &
                           cal_date <= ep$win_to, max(yyyyqq)]

  agg <- panel_dt[yyyyqq >= win_from_q & yyyyqq <= win_to_q,
    c(list(cal_date=first(cal_date)),
      lapply(.SD, mean, na.rm=TRUE)),
    by=yyyyqq, .SDcols=vars][order(yyyyqq)]

  peak_means <- agg[yyyyqq == ep$peak_qtr,
                     lapply(.SD, mean, na.rm=TRUE),
                     .SDcols=vars]

  agg[, q_rel := {
    yr  <- as.integer(floor(yyyyqq/100))
    qtr <- as.integer(yyyyqq %% 100)
    pk_yr  <- as.integer(floor(ep$peak_qtr/100))
    pk_qtr <- as.integer(ep$peak_qtr %% 100)
    (yr - pk_yr)*4L + (qtr - pk_qtr)
  }]

  for (v in vars) {
    pk <- as.numeric(peak_means[[v]])
    if (!is.na(pk) && abs(pk) > 0.001)
      agg[, paste0(v,"_idx") := get(v) / pk * 100]
  }
  agg[, episode := ep$name]
  agg
}

plot_vars <- intersect(c("dq_rate","netintmrg","costfds","insured_share_growth"),
                        cu_vars)
n08  <- norm_ep(panel, EP08, plot_vars)
n11  <- norm_ep(panel, EP11, plot_vars)
nall <- rbindlist(list(n08, n11), fill=TRUE)

idx_vars <- paste0(plot_vars, "_idx")
idx_vars <- intersect(idx_vars, names(nall))

var_labs <- c(
  dq_rate_idx              = "Delinquency Rate",
  netintmrg_idx            = "Net Interest Margin",
  costfds_idx              = "Cost of Funds",
  insured_share_growth_idx = "Insured Share Growth"
)

nlong <- melt(nall[q_rel >= -6L & q_rel <= 10L],
              id.vars      = c("q_rel","episode"),
              measure.vars = idx_vars,
              variable.name= "outcome",
              value.name   = "idx_val")
nlong[, outcome_label := var_labs[as.character(outcome)]]
nlong[is.na(outcome_label), outcome_label := as.character(outcome)]

EP_COLS <- c("2008 Record Peak"    = COL_2008,
             "2011 Secondary Peak" = COL_2011)
EP_LT   <- c("2008 Record Peak"    = "solid",
             "2011 Secondary Peak" = "dashed")

p2c04 <- ggplot(nlong[!is.na(idx_val) & !is.na(outcome_label)],
                aes(x=q_rel, y=idx_val,
                    colour=episode, linetype=episode)) +
  geom_hline(yintercept=100, linewidth=0.35, colour="#aaa") +
  geom_vline(xintercept=0,   linewidth=0.6,  colour="#666") +
  geom_line(linewidth=0.85) +
  geom_point(size=1.8) +
  scale_colour_manual(values=EP_COLS, name="Episode") +
  scale_linetype_manual(values=EP_LT,  name="Episode") +
  scale_x_continuous(breaks=seq(-6,10,2),
                     labels=function(x) paste0(ifelse(x<0,"","+"),x,"Q")) +
  facet_wrap(~outcome_label, scales="free_y", ncol=2) +
  labs(title    = "FIGURE 2c-04 — 2008 vs 2011: Normalised CU Response (Peak Quarter = 100)",
       subtitle = "Red solid = 2008 record | Blue dashed = 2011 secondary | 0 = peak quarter",
       caption  = "Source: NCUA Form 5300",
       x        = "Quarters Relative to Peak",
       y        = "Index (100 = peak quarter value)") +
  theme_pub() +
  theme(strip.text=element_text(size=9,face="bold"))

save_plot(p2c04, "2c04_2008_vs_2011_normalised.png", w=13, h=9)

# =============================================================================
# CHART 2c-05 — Direct vs Indirect: Oil-State vs Non-Oil at 2011 Peak
# =============================================================================
hdr("Chart 2c-05: Direct vs indirect — 2011")

if ("oil_group" %in% names(panel) &&
    panel[oil_group=="Oil-State", .N] > 0) {

  agg_g11 <- panel[cal_date >= EP11$win_from & cal_date <= EP11$win_to,
    c(list(cal_date=first(cal_date)),
      lapply(.SD, mean, na.rm=TRUE)),
    by=.(yyyyqq, oil_group),
    .SDcols=cu_vars][order(yyyyqq)]
  agg_g11 <- merge(agg_g11, mac[,.(yyyyqq,pbrent)],
                    by="yyyyqq", all.x=TRUE)

  GRP_COLS <- c("Oil-State"=COL_DIR, "Non-Oil"=COL_IND)

  make_g <- function(v, lab, y_fmt=waiver()) {
    if (!v %in% names(agg_g11)) return(NULL)
    d <- agg_g11[!is.na(get(v)) & !is.na(oil_group)]
    if (nrow(d)==0 || uniqueN(d$oil_group)<2) return(NULL)
    ggplot(d, aes(x=cal_date, y=get(v), colour=oil_group)) +
      ep_rects(EP11) +
      geom_line(linewidth=0.9) +
      scale_colour_manual(values=GRP_COLS, name=NULL) +
      scale_x_date(date_breaks="1 year", date_labels="%Y") +
      scale_y_continuous(labels=y_fmt) +
      labs(title=lab, x=NULL, y=lab) +
      theme_pub() + theme(legend.position="bottom")
  }

  g_panels <- Filter(Negate(is.null), list(
    make_g("dq_rate",             "Delinquency Rate (%)"),
    make_g("netintmrg",           "Net Interest Margin (%)"),
    make_g("insured_share_growth","Insured Share Growth (YoY%)"),
    make_g("costfds",             "Cost of Funds (%)")
  ))

  if (length(g_panels) >= 2) {
    p2c05 <- wrap_plots(g_panels, ncol=2, guides="collect") &
      theme(legend.position="bottom")
    p2c05 <- p2c05 +
      plot_annotation(
        title   = "FIGURE 2c-05 — Direct vs Indirect: Oil-State vs Non-Oil CUs at 2011 Peak",
        subtitle= "Buffer test: oil-state CUs should show deposit inflows vs non-oil outflows",
        caption = "Source: NCUA Form 5300; classification via reporting_state",
        theme   = theme(plot.title=element_text(face="bold",size=12),
                        plot.subtitle=element_text(size=9,colour="#555"))
      )
    save_plot(p2c05, "2c05_direct_indirect_2011.png", w=12, h=9)
  }
} else {
  msg("  Skipping 2c-05: no oil-state CUs found in panel")
}

# =============================================================================
# CHART 2c-06 — Macro Backdrop: Why 2008 and 2011 Produced Different Outcomes
# =============================================================================
hdr("Chart 2c-06: Macro backdrop")

mac_comp <- mac[cal_date >= as.Date("2006-01-01") &
                  cal_date <= as.Date("2014-12-31") &
                  !is.na(pbrent)]

peak_pts <- mac_comp[yyyyqq %in% c(200803L, 201102L)]

make_mac <- function(v, title, subtitle, y_fmt=waiver(), col="#333") {
  if (!v %in% names(mac_comp)) return(NULL)
  ggplot(mac_comp[!is.na(get(v))], aes(x=cal_date, y=get(v))) +
    geom_line(colour=col, linewidth=0.9) +
    geom_point(data=peak_pts[!is.na(get(v))],
               aes(colour=factor(yyyyqq)), size=4, shape=18,
               show.legend=FALSE) +
    scale_colour_manual(values=c("200803"=COL_2008,"201102"=COL_2011)) +
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    scale_y_continuous(labels=y_fmt) +
    labs(title=title, subtitle=subtitle, x=NULL, y=NULL) +
    theme_pub()
}

p2c06 <- (
  make_mac("pbrent","Brent Oil ($/bbl)",
           "Red diamond = 2008Q3 | Blue diamond = 2011Q2",
           dollar_format(prefix="$",suffix="/bbl"), COL_OIL) +
  make_mac("rff","Fed Funds Rate (%)",
           "2008: cutting 5.25%->0.25% | 2011: already at ZIRP floor",
           number_format(accuracy=0.01,suffix="%"), COL_ZIRP)
) / (
  make_mac("lurc","Unemployment Rate (%)",
           "2008: rising sharply (GFC) | 2011: elevated but stabilising",
           number_format(accuracy=0.1,suffix="%"), COL_NEG) +
  make_mac("yc","Yield Curve (10Y-3M)",
           "2008: flattening -> NIM headwind | 2011: steep -> NIM tailwind",
           number_format(accuracy=0.01), COL_DIR)
) +
  plot_annotation(
    title    = "FIGURE 2c-06 — Macro Backdrop: Why 2008 and 2011 Produced Different CU Outcomes",
    subtitle = "ZIRP + steep yield curve in 2011 = rate channel muted + NIM tailwind",
    caption  = "Source: FRB CCAR 2026 Baseline macro scenario",
    theme    = theme(plot.title=element_text(face="bold",size=12),
                     plot.subtitle=element_text(size=9,colour="#555"))
  )
save_plot(p2c06, "2c06_macro_backdrop_comparison.png", w=13, h=10)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================
hdr("Summary statistics at each peak")

for (ep in list(EP08, EP11)) {
  cat(sprintf("\n  %s (yyyyqq=%d):\n", ep$name, ep$peak_qtr))
  cat(sprintf("    PBRENT      : $%.1f/bbl\n", mac[yyyyqq==ep$peak_qtr, pbrent]))
  cat(sprintf("    Fed Funds   : %.2f%%\n",    mac[yyyyqq==ep$peak_qtr, rff]))
  cat(sprintf("    Unemployment: %.1f%%\n",    mac[yyyyqq==ep$peak_qtr, lurc]))
  ep_m <- panel[yyyyqq==ep$peak_qtr,
                 lapply(.SD, mean, na.rm=TRUE),
                 .SDcols=intersect(plot_vars, names(panel))]
  for (v in names(ep_m))
    cat(sprintf("    %-30s: %.3f\n", v, ep_m[[v]]))
}

cat("\n=================================================================\n")
cat(" SCRIPT 02c COMPLETE\n")
cat("=================================================================\n")
cat("  2c01_both_peaks_context.png\n")
cat("  2c02_cu_outcomes_2008_peak.png\n")
cat("  2c03_cu_outcomes_2011_peak.png\n")
cat("  2c04_2008_vs_2011_normalised.png\n")
cat("  2c05_direct_indirect_2011.png\n")
cat("  2c06_macro_backdrop_comparison.png\n")
cat("=================================================================\n")
