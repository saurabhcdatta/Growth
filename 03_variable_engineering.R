# =============================================================================
# OIL PRICE SHOCK × CREDIT UNION RESEARCH
# Script 03 — Variable Engineering & Stationarity
# =============================================================================
# Inputs  : Data/panel_base.rds, Data/macro_base.rds
# Outputs : Data/panel_model.rds   — model-ready panel with all engineered vars
#           Figures/03_*.png       — stationarity & lag selection charts
#
# Sections:
#   1.  Load & audit existing variables from Phase 1
#   2.  Structural & regime dummies
#   3.  Additional CU-level variable engineering
#   4.  Macro variable transforms & credit tightness composite
#   5.  Stationarity testing (ADF + KPSS) on all model variables
#   6.  Lag selection (AIC/BIC grid) for VARX specification
#   7.  Chow test — structural break at 2015Q1
#   8.  Winsorisation review & final bounds
#   9.  Variable registry & save
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(tseries)      # adf.test, kpss.test
  library(strucchange)  # Chow test, breakpoints
  library(vars)         # VAR lag selection
  library(scales)
  library(stringr)
})

msg <- function(...) cat(sprintf(...), "\n")
hdr <- function(s)   cat("\n---", s, "---\n")

# Null-coalescing helper
`%||%` <- function(a,b) if(!is.null(a) && length(a)>0 && !is.na(a[1])) a else b

cat("=================================================================\n")
cat(" OIL SHOCK × CU  |  SCRIPT 03: VARIABLE ENGINEERING\n")
cat("=================================================================\n")

dir.create("Figures", showWarnings = FALSE)

Q_MONTH <- c("1"=1L,"2"=4L,"3"=7L,"4"=10L)

theme_pub <- function(base_size=10) {
  theme_minimal(base_size=base_size) +
  theme(
    plot.title       = element_text(size=base_size+2, face="bold", margin=margin(b=4)),
    plot.subtitle    = element_text(size=base_size-0.5, colour="#555", margin=margin(b=8)),
    plot.caption     = element_text(size=base_size-2, colour="#888", hjust=0),
    axis.title       = element_text(size=base_size-0.5),
    axis.text        = element_text(size=base_size-1.5),
    panel.grid.major = element_line(colour="#e8e8e8", linewidth=0.3),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour="#ccc", fill=NA, linewidth=0.4),
    strip.text       = element_text(size=base_size-1, face="bold"),
    strip.background = element_rect(fill="#f5f5f5"),
    legend.position  = "bottom",
    plot.margin      = margin(10,12,8,10)
  )
}

save_plot <- function(p, fn, w=12, h=7, dpi=300) {
  path <- file.path("Figures", fn)
  ggsave(path, p, width=w, height=h, dpi=dpi, bg="white")
  msg("  Saved: %s", path)
}

winsor <- function(x, p=0.01) {
  lo <- quantile(x, p,   na.rm=TRUE)
  hi <- quantile(x, 1-p, na.rm=TRUE)
  pmin(pmax(x, lo), hi)
}

# =============================================================================
# 1. LOAD & AUDIT
# =============================================================================
hdr("SECTION 1: Load & Audit")

panel <- readRDS("Data/panel_base.rds")
macro <- readRDS("Data/macro_base.rds")
setDT(panel); setDT(macro)

panel[, cal_date := as.Date(paste(year, Q_MONTH[as.character(quarter)],
                                   "01", sep="-"))]
macro[, cal_date := as.Date(paste(year, Q_MONTH[as.character(quarter)],
                                   "01", sep="-"))]

msg("  Panel: %s rows × %s cols | %s CUs | %s quarters",
    format(nrow(panel),big.mark=","), ncol(panel),
    format(uniqueN(panel$join_number),big.mark=","),
    uniqueN(panel$yyyyqq))

# Confirm key vars from Phase 1
phase1_vars <- c(
  "netintmrg","networth","pcanetworth","costfds","roa",
  "insured_share_growth","cert_share","loan_to_share","nim_spread",
  "cert_growth_yoy","dep_growth_yoy",
  "oil_exposure_cont","oil_exposure_bin","oil_bartik_iv",
  "spillover_exposure","cu_group",
  "macro_base_pbrent","macro_base_yoy_oil","macro_base_lurc",
  "macro_base_pcpi","macro_base_rmtg","macro_base_yield_curve",
  "macro_base_fomc_regime","macro_base_real_rate","macro_base_hike_run",
  "oil_x_brent","spillover_x_brent","fomc_x_brent"
)
found_p1   <- intersect(phase1_vars, names(panel))
missing_p1 <- setdiff(phase1_vars, names(panel))
msg("  Phase 1 vars confirmed: %d/%d", length(found_p1), length(phase1_vars))
if (length(missing_p1) > 0)
  msg("  Missing from Phase 1: %s", paste(missing_p1, collapse=", "))

# =============================================================================
# 2. STRUCTURAL & REGIME DUMMIES
# =============================================================================
hdr("SECTION 2: Structural & Regime Dummies")

# ── 2.1 Era dummies ───────────────────────────────────────────────────────────
panel[, `:=`(
  # Primary structural break (from EDA evidence)
  post_shale    = as.integer(yyyyqq >= 201501L),

  # GFC episode (messy identification — oil + credit shock)
  gfc_dummy     = as.integer(yyyyqq >= 200803L & yyyyqq <= 200904L),

  # ZIRP era (rate channel muted)
  zirp_era      = as.integer(yyyyqq >= 200901L & yyyyqq <= 201504L),

  # COVID shock (massive confound — must control)
  covid_dummy   = as.integer(yyyyqq >= 202001L & yyyyqq <= 202104L),

  # Post-COVID rate hike cycle (2022-2023)
  hike_cycle    = as.integer(yyyyqq >= 202201L & yyyyqq <= 202304L),

  # Pre-shale oil boom (2010-2014)
  shale_boom    = as.integer(yyyyqq >= 201001L & yyyyqq <= 201404L),

  # Arab spring episode
  arab_spring   = as.integer(yyyyqq >= 201101L & yyyyqq <= 201202L)
)]

msg("  ✓ Era dummies: post_shale, gfc_dummy, zirp_era, covid_dummy,")
msg("                 hike_cycle, shale_boom, arab_spring")

# ── 2.2 Interaction dummies ───────────────────────────────────────────────────
panel[, `:=`(
  # ZIRP × oil shock (rate channel muted)
  zirp_x_oil    = zirp_era * macro_base_yoy_oil,

  # Post-shale × oil shock (structural break interaction)
  post_x_oil    = post_shale * macro_base_yoy_oil,

  # Post-shale × oil × direct (triple interaction — headline finding)
  post_x_oil_x_direct = post_shale * macro_base_yoy_oil *
                          fifelse(!is.na(oil_exposure_bin), oil_exposure_bin, 0L),

  # GFC contamination flag
  clean_sample  = as.integer(gfc_dummy == 0 & covid_dummy == 0)
)]

msg("  ✓ Interaction dummies: zirp_x_oil, post_x_oil,")
msg("                          post_x_oil_x_direct, clean_sample")

# Print dummy distribution
dummy_cols <- c("post_shale","gfc_dummy","zirp_era","covid_dummy",
                "hike_cycle","clean_sample")
cat("\n  Dummy variable distributions (%% of CU-quarter obs):\n")
for (d in dummy_cols) {
  if (d %in% names(panel))
    cat(sprintf("    %-25s : %.1f%% obs = 1\n", d,
                mean(panel[[d]], na.rm=TRUE)*100))
}

# =============================================================================
# 3. ADDITIONAL CU-LEVEL ENGINEERING
# =============================================================================
hdr("SECTION 3: Additional CU-Level Variables")

setorderv(panel, c("join_number","year","quarter"))

# ── 3.0  Resolve charge-off column name ──────────────────────────────────────
# chg_tot_lns_ratio may be named differently in the call report
# Common alternatives: chg_off_ratio, net_chargeoffs, acct_XXX derived
chargeoff_col <- intersect(
  c("chg_totlns_ratio",    # primary — confirmed in call report
    "chg_tot_lns_ratio",   # alt spelling
    "chg_tot_ratio",       # seen in call report
    "netchgoffs",          # raw net charge-offs (needs normalization)
    "chg_off_ratio","chargeoff_rate","dq_net_chargeoff"),
  names(panel))[1]

if (is.na(chargeoff_col)) {
  # Try to construct from raw accounts if available
  # net charge-offs / average loans
  raw_opts <- list(
    c("netchgoffs","lns_tot"),   # confirmed: netchgoffs in call report
    c("acct_748","lns_tot"),     # NCUA account code fallback
    c("chargeoffs","lns_tot"),
    c("net_chargeoffs","lns_tot")
  )
  for (opt in raw_opts) {
    if (all(opt %in% names(panel))) {
      panel[, chg_tot_lns_ratio := fifelse(
        get(opt[2]) > 0, get(opt[1]) / get(opt[2]) * 100, NA_real_)]
      panel[!is.na(chg_tot_lns_ratio),
            chg_tot_lns_ratio := winsor(chg_tot_lns_ratio)]
      chargeoff_col <- "chg_tot_lns_ratio"
      msg("  ✓ chg_tot_lns_ratio constructed from %s / %s",
          opt[1], opt[2])
      break
    }
  }
  if (is.na(chargeoff_col))
    msg("  NOTE: chg_tot_lns_ratio not found — will be excluded from dep_vars")
} else {
  msg("  ✓ Charge-off column found: %s", chargeoff_col)
  if (chargeoff_col != "chg_tot_lns_ratio")
    panel[, chg_tot_lns_ratio := get(chargeoff_col)]
}

# ── 3.1 YoY growth for level variables not already done ───────────────────────
cu_yoy <- function(col) {
  panel[, {
    x    <- get(col)
    x_l4 <- shift(x, 4)
    fifelse(!is.na(x) & !is.na(x_l4) & is.finite(x_l4) & x_l4 > 0,
            (x - x_l4) / x_l4 * 100, NA_real_)
  }, by=join_number][[2]]
}

yoy_targets <- c("lns_tot","assets_tot","acct_010")
for (v in yoy_targets) {
  if (v %in% names(panel)) {
    new_nm <- paste0(v, "_yoy")
    panel[, (new_nm) := cu_yoy(v)]
    panel[!is.na(get(new_nm)), (new_nm) := winsor(get(new_nm))]
    msg("  ✓ %s (winsorised)", new_nm)
  }
}

# ── 3.15 oil_group — ensure it exists in panel_model ────────────────────────
if (!"oil_group" %in% names(panel)) {
  state_col <- intersect(c("reporting_state","state_code","state"),
                          names(panel))[1]
  OIL_ST <- c("TX","ND","LA","AK","WY","OK","NM","CO","WV","PA","MT")
  if (!is.na(state_col)) {
    panel[, oil_group := fifelse(
      toupper(get(state_col)) %in% OIL_ST,
      "Oil-State", "Non-Oil")]
    msg("  ✓ oil_group created from %s (%s oil-state CU-qtrs)",
        state_col,
        format(panel[oil_group=="Oil-State",.N], big.mark=","))
  } else {
    msg("  WARNING: Cannot create oil_group — no state column found")
  }
} else {
  msg("  ✓ oil_group already present (%s Oil-State CU-qtrs)",
      format(panel[oil_group=="Oil-State",.N], big.mark=","))
}

# ── 3.2 Lagged dependent variables (for dynamic panel / Arellano-Bond) ────────
lag_dvars <- intersect(c("dq_rate","chg_tot_lns_ratio","netintmrg",
                          "costfds","roa","insured_share_growth",
                          "cert_share","loan_to_share"), names(panel))

for (v in lag_dvars) {
  for (k in 1:4) {
    new_nm <- paste0(v, "_lag", k)
    panel[, (new_nm) := shift(get(v), k), by=join_number]
  }
}
msg("  ✓ Lags 1-4 added for %d dependent variables", length(lag_dvars))

# ── 3.3 First differences (for stationarity) ─────────────────────────────────
diff_vars <- intersect(c("netintmrg","costfds","roa","cert_share",
                          "loan_to_share"), names(panel))
for (v in diff_vars) {
  new_nm <- paste0("d_", v)
  panel[, (new_nm) := get(v) - shift(get(v), 1), by=join_number]
}
msg("  ✓ First differences (d_*) for %d variables", length(diff_vars))

# ── 3.4 Delinquency change (primary credit quality dep var) ───────────────────
if ("dq_rate" %in% names(panel)) {
  panel[, dq_rate_chg := dq_rate - shift(dq_rate, 1), by=join_number]
  panel[, dq_rate_yoy := dq_rate - shift(dq_rate, 4), by=join_number]
  msg("  ✓ dq_rate_chg (QoQ), dq_rate_yoy (4Q change)")
}

# ── 3.5 Asset tier indicators ─────────────────────────────────────────────────
if ("asset_tier" %in% names(panel)) {
  panel[, `:=`(
    tier_small  = as.integer(asset_tier == "T1_under10M"),
    tier_mid    = as.integer(asset_tier == "T2_10to100M"),
    tier_large  = as.integer(asset_tier == "T3_100Mto1B"),
    tier_xlarge = as.integer(asset_tier == "T4_over1B")
  )]
  msg("  ✓ Asset tier binary indicators")
}

# =============================================================================
# 4. MACRO VARIABLE TRANSFORMS
# =============================================================================
hdr("SECTION 4: Macro Variable Transforms")

mac_spine <- unique(macro[, .(
  yyyyqq, cal_date,
  pbrent       = macro_base_pbrent,
  yoy_oil      = macro_base_yoy_oil,
  lurc         = macro_base_lurc,
  pcpi         = macro_base_pcpi,
  rff          = macro_base_rff,
  rmtg         = macro_base_rmtg,
  phpi         = macro_base_phpi,
  uypsav       = macro_base_uypsav,
  yield_curve  = macro_base_yield_curve,
  fomc_regime  = macro_base_fomc_regime,
  real_rate    = macro_base_real_rate
)])[order(yyyyqq)]

# ── 4.1 Oil price cycle position ──────────────────────────────────────────────
mac_spine[, `:=`(
  # Deviation from 8Q moving average (cycle position)
  oil_trend_8q  = frollmean(pbrent, 8, na.rm=TRUE, align="right"),
  oil_trend_12q = frollmean(pbrent, 12, na.rm=TRUE, align="right")
)]
mac_spine[, `:=`(
  oil_cycle_pos  = pbrent - oil_trend_8q,   # above/below 8Q trend
  oil_cycle_pct  = (pbrent - oil_trend_8q) / oil_trend_8q * 100
)]

# ── 4.2 Oil price volatility ──────────────────────────────────────────────────
mac_spine[, `:=`(
  oil_vol_4q   = frollapply(yoy_oil, 4, sd, na.rm=TRUE, align="right"),
  oil_vol_8q   = frollapply(yoy_oil, 8, sd, na.rm=TRUE, align="right")
)]

# ── 4.3 Macro credit tightness composite ──────────────────────────────────────
# Composite: higher = tighter financial conditions
# Components: inverted yield curve + real rate + unemployment
# Normalise each to 0-1 then average
normalise_01 <- function(x) {
  mn <- min(x, na.rm=TRUE); mx <- max(x, na.rm=TRUE)
  if (mx > mn) (x - mn)/(mx - mn) else rep(0.5, length(x))
}

if (all(c("yield_curve","real_rate","lurc") %in% names(mac_spine))) {
  mac_spine[, credit_tightness := {
    yc_tight   <- normalise_01(-yield_curve)    # inverted = tight
    rr_tight   <- normalise_01(-real_rate)      # negative real rate = loose (invert)
    unemp_norm <- normalise_01(lurc)            # high unemployment = stressed
    (yc_tight + rr_tight + unemp_norm) / 3
  }]
  msg("  ✓ credit_tightness composite (yield_curve + real_rate + unemployment)")
}

# ── 4.4 HPI momentum ──────────────────────────────────────────────────────────
if ("phpi" %in% names(mac_spine)) {
  mac_spine[, hpi_yoy := (phpi - shift(phpi,4)) / shift(phpi,4) * 100]
  msg("  ✓ hpi_yoy (HPI YoY %%)")
}

# ── 4.5 Savings rate momentum ─────────────────────────────────────────────────
if ("uypsav" %in% names(mac_spine)) {
  mac_spine[, savings_chg := uypsav - shift(uypsav,1)]
  msg("  ✓ savings_chg (savings rate QoQ change)")
}

# Merge new macro transforms onto panel
new_mac_cols <- c("yyyyqq","oil_trend_8q","oil_cycle_pos","oil_cycle_pct",
                   "oil_vol_4q","oil_vol_8q","credit_tightness",
                   "hpi_yoy","savings_chg")
new_mac_cols <- intersect(new_mac_cols, names(mac_spine))
panel <- merge(panel, mac_spine[, ..new_mac_cols], by="yyyyqq", all.x=TRUE)
msg("  ✓ Macro transforms merged onto panel (%d new cols)", length(new_mac_cols)-1)

# =============================================================================
# 5. STATIONARITY TESTING (ADF + KPSS)
# =============================================================================
hdr("SECTION 5: Stationarity Testing")

# Use aggregate quarterly series (mean across CUs) for time-series tests
agg_qtr <- panel[, lapply(.SD, mean, na.rm=TRUE),
                  by=yyyyqq,
                  .SDcols=intersect(
                    c("dq_rate","dq_rate_chg","netintmrg","d_netintmrg",
                      "costfds","d_costfds","roa","insured_share_growth",
                      "cert_share","d_cert_share","loan_to_share",
                      "macro_base_pbrent","macro_base_yoy_oil",
                      "macro_base_lurc","macro_base_pcpi",
                      "macro_base_yield_curve","macro_base_real_rate",
                      "oil_cycle_pos","credit_tightness"),
                    names(panel))][order(yyyyqq)]

# Run ADF and KPSS on each series
test_stationarity <- function(x, varname, max_lag=8) {
  x_clean <- na.omit(x)
  if (length(x_clean) < 20) return(NULL)

  # ADF test (H0: unit root / non-stationary)
  adf_res <- tryCatch(
    adf.test(x_clean, k=min(max_lag, floor(length(x_clean)^(1/3)))),
    error=function(e) NULL)

  # KPSS test (H0: stationary)
  kpss_res <- tryCatch(
    kpss.test(x_clean, null="Level"),
    error=function(e) NULL)

  data.table(
    variable    = varname,
    n_obs       = length(x_clean),
    adf_stat    = if(!is.null(adf_res)) round(adf_res$statistic, 3) else NA,
    adf_pval    = if(!is.null(adf_res)) round(adf_res$p.value, 3) else NA,
    adf_result  = if(!is.null(adf_res))
                    fifelse(adf_res$p.value < 0.05, "Stationary", "Unit Root")
                  else "Error",
    kpss_stat   = if(!is.null(kpss_res)) round(kpss_res$statistic, 3) else NA,
    kpss_pval   = if(!is.null(kpss_res)) round(kpss_res$p.value, 3) else NA,
    kpss_result = if(!is.null(kpss_res))
                    fifelse(kpss_res$p.value > 0.05, "Stationary", "Non-Stationary")
                  else "Error"
  )
}

test_vars <- setdiff(names(agg_qtr), "yyyyqq")
stat_results <- rbindlist(lapply(test_vars, function(v) {
  test_stationarity(agg_qtr[[v]], v)
}), fill=TRUE)

# Final recommendation
stat_results[, recommendation := fcase(
  adf_result == "Stationary"  & kpss_result == "Stationary",  "USE LEVELS",
  adf_result == "Unit Root"   & kpss_result == "Non-Stationary","USE DIFFERENCES",
  adf_result == "Stationary"  & kpss_result == "Non-Stationary","BORDERLINE — use levels with caution",
  adf_result == "Unit Root"   & kpss_result == "Stationary",    "BORDERLINE — likely I(1)",
  default = "INCONCLUSIVE"
)]

cat("\n  STATIONARITY RESULTS:\n")
cat(sprintf("  %-35s %-12s %-8s %-15s %-8s %-15s %-30s\n",
            "Variable","ADF Stat","ADF p","ADF Result",
            "KPSS p","KPSS Result","Recommendation"))
cat("  ", strrep("-", 120), "\n", sep="")
for (i in 1:nrow(stat_results)) {
  r <- stat_results[i]
  cat(sprintf("  %-35s %-12.3f %-8.3f %-15s %-8.3f %-15s %-30s\n",
              r$variable, r$adf_stat %||% NA, r$adf_pval %||% NA,
              r$adf_result, r$kpss_pval %||% NA,
              r$kpss_result, r$recommendation))
}

# Save stationarity results
saveRDS(stat_results, "Data/stationarity_results.rds")

# ── Chart 03a: Stationarity summary ──────────────────────────────────────────
p_stat <- ggplot(stat_results[!is.na(adf_pval)],
                 aes(x=adf_pval, y=reorder(variable,-adf_pval),
                     colour=recommendation, shape=kpss_result)) +
  geom_point(size=3.5) +
  geom_vline(xintercept=0.05, linetype="dashed",
             colour="red", linewidth=0.5) +
  annotate("text", x=0.06, y=Inf, label="α=0.05",
           vjust=1.5, size=2.8, colour="red") +
  scale_colour_manual(
    values=c("USE LEVELS"="#27ae60",
             "USE DIFFERENCES"="#e74c3c",
             "BORDERLINE — use levels with caution"="#e67e22",
             "BORDERLINE — likely I(1)"="#f39c12",
             "INCONCLUSIVE"="#95a5a6"),
    name="Recommendation") +
  scale_shape_manual(values=c("Stationary"=16,"Non-Stationary"=17),
                     name="KPSS Result") +
  scale_x_continuous(limits=c(0,1),
                     labels=number_format(accuracy=0.01)) +
  labs(title    = "FIGURE 03a — ADF Stationarity Test: p-values for All Model Variables",
       subtitle = "Points left of red line (p<0.05) = ADF rejects unit root = stationary | Shape = KPSS result",
       caption  = "ADF H0: unit root present | KPSS H0: series is stationary | Both tests used together",
       x="ADF p-value", y=NULL) +
  theme_pub() +
  theme(legend.position="right",
        legend.box="vertical",
        legend.text=element_text(size=7))
save_plot(p_stat, "03a_stationarity_tests.png", w=13, h=8)

# =============================================================================
# 6. LAG SELECTION (AIC/BIC/HQ grid for VARX)
# =============================================================================
hdr("SECTION 6: Lag Selection")

# Use stationary series for VAR lag selection
# Core endogenous block: 6 main CU outcomes
endog_vars <- intersect(
  c("dq_rate","netintmrg","insured_share_growth",
    "costfds","cert_share","loan_to_share"),
  names(agg_qtr))

# Use only stationary or near-stationary in levels
stat_ok <- stat_results[recommendation %like% "LEVELS", variable]
endog_use <- intersect(endog_vars, stat_ok)
if (length(endog_use) < 2) {
  # Fallback: use differenced versions
  endog_use <- intersect(
    c("dq_rate","insured_share_growth","macro_base_yoy_oil"),
    names(agg_qtr))
}

msg("  VAR lag selection using: %s", paste(endog_use, collapse=", "))

if (length(endog_use) >= 2) {
  var_data <- na.omit(agg_qtr[, c("yyyyqq", endog_use), with=FALSE])
  var_mat  <- as.matrix(var_data[, endog_use, with=FALSE])

  lag_select <- tryCatch({
    VARselect(var_mat, lag.max=8, type="const")
  }, error=function(e) {
    msg("  WARNING: VARselect failed: %s", e$message)
    NULL
  })

  if (!is.null(lag_select)) {
    cat("\n  VAR Lag Selection Criteria:\n")
    print(lag_select$criteria)

    optimal_lags <- data.table(
      criterion = c("AIC","HQ","SC (BIC)","FPE"),
      optimal_p = as.integer(lag_select$selection)
    )
    cat("\n  Optimal lag by criterion:\n")
    print(optimal_lags)

    # Recommended lag
    rec_lag <- as.integer(names(sort(table(lag_select$selection),
                                     decreasing=TRUE))[1])
    msg("\n  RECOMMENDED VAR LAG ORDER: p = %d", rec_lag)
    msg("  (Use p=%d as primary, p=%d and p=%d as robustness checks)",
        rec_lag,
        max(1, rec_lag-1),
        min(8, rec_lag+1))

    # ── Chart 03b: Lag selection criteria ─────────────────────────────────────
    crit_dt <- as.data.table(t(lag_select$criteria))
    crit_dt[, lag := 1:.N]
    crit_long <- melt(crit_dt, id.vars="lag",
                      variable.name="criterion", value.name="value")
    crit_long[, value_scaled := scale(value)[,1], by=criterion]

    p_lag <- ggplot(crit_long, aes(x=lag, y=value_scaled, colour=criterion)) +
      geom_line(linewidth=0.9) +
      geom_point(size=2.5) +
      geom_vline(xintercept=rec_lag, linetype="dashed",
                 colour="#e74c3c", linewidth=0.7) +
      annotate("label", x=rec_lag, y=Inf,
               label=paste("Recommended\np =", rec_lag),
               vjust=1.3, size=3, colour="#e74c3c", fill="white") +
      scale_colour_brewer(palette="Set1", name="Criterion") +
      scale_x_continuous(breaks=1:8) +
      labs(title    = "FIGURE 03b — VAR Lag Selection: AIC / BIC / HQ Criteria",
           subtitle = sprintf("Recommended lag order: p = %d | Dashed red = recommendation", rec_lag),
           caption  = "Note: Criteria scaled to same axis for comparison | VAR on stationary CU outcome series",
           x="Lag Order (p)", y="Scaled Criterion Value (lower = better)") +
      theme_pub()
    save_plot(p_lag, "03b_lag_selection.png", w=10, h=6)

    # Save recommended lag
    saveRDS(list(lag_select=lag_select, recommended_p=rec_lag),
            "Data/lag_selection.rds")
  }
}

# =============================================================================
# 7. CHOW TEST — STRUCTURAL BREAK AT 2015Q1
# =============================================================================
hdr("SECTION 7: Chow Test — Structural Break at 2015Q1")

chow_results <- list()

chow_vars <- intersect(
  c("dq_rate","netintmrg","costfds","insured_share_growth"),
  names(agg_qtr))

for (v in chow_vars) {
  d <- merge(agg_qtr[!is.na(get(v)), .(yyyyqq, y=get(v))],
             agg_qtr[!is.na(macro_base_yoy_oil),
                      .(yyyyqq, x=macro_base_yoy_oil)],
             by="yyyyqq")
  if (nrow(d) < 20) next

  d <- d[order(yyyyqq)]
  d[, post := as.integer(yyyyqq >= 201501L)]
  d[, ts_idx := .I]

  # Chow test via sctest (structural change test)
  ts_obj <- ts(d$y, start=1, frequency=1)
  xmat   <- cbind(d$x, d$post, d$x * d$post)

  chow_res <- tryCatch({
    # F-test for structural break at 2015Q1
    n_pre    <- sum(d$yyyyqq < 201501L)
    fit_full <- lm(y ~ x + post + x:post, data=d)
    fit_pre  <- lm(y ~ x, data=d[yyyyqq < 201501L])
    fit_post <- lm(y ~ x, data=d[yyyyqq >= 201501L])

    RSS_full <- sum(resid(fit_pre)^2) + sum(resid(fit_post)^2)
    RSS_restr <- sum(resid(lm(y~x, data=d))^2)
    k  <- length(coef(fit_pre))
    n  <- nrow(d)
    F_stat <- ((RSS_restr - RSS_full)/k) / (RSS_full/(n - 2*k))
    p_val  <- pf(F_stat, df1=k, df2=n-2*k, lower.tail=FALSE)

    list(
      variable   = v,
      F_stat     = round(F_stat, 3),
      p_value    = round(p_val, 4),
      n_pre      = n_pre,
      n_post     = nrow(d) - n_pre,
      slope_pre  = round(coef(fit_pre)["x"], 4),
      slope_post = round(coef(fit_post)["x"], 4),
      break_confirmed = p_val < 0.05
    )
  }, error=function(e) {
    list(variable=v, F_stat=NA, p_value=NA,
         break_confirmed=FALSE, error=e$message)
  })
  chow_results[[v]] <- as.data.table(chow_res)
}

chow_tbl <- rbindlist(chow_results, fill=TRUE)
cat("\n  CHOW TEST RESULTS (Structural Break at 2015Q1):\n")
cat(sprintf("  %-30s %-10s %-10s %-12s %-12s %-10s %-10s\n",
            "Variable","F-stat","p-value","Slope Pre","Slope Post","Break?","N pre"))
cat("  ", strrep("-",95), "\n", sep="")
for (i in 1:nrow(chow_tbl)) {
  r <- chow_tbl[i]
  cat(sprintf("  %-30s %-10.3f %-10.4f %-12.4f %-12.4f %-10s %-10d\n",
              r$variable,
              r$F_stat %||% NA_real_,
              r$p_value %||% NA_real_,
              r$slope_pre %||% NA_real_,
              r$slope_post %||% NA_real_,
              ifelse(isTRUE(r$break_confirmed), "YES ***", "No"),
              r$n_pre %||% NA_integer_))
}

saveRDS(chow_tbl, "Data/chow_test_results.rds")

# ── Chart 03c: Chow test slope comparison ─────────────────────────────────────
if (nrow(chow_tbl[!is.na(slope_pre)]) > 0) {
  slope_long <- melt(chow_tbl[!is.na(slope_pre)],
                     id.vars="variable",
                     measure.vars=c("slope_pre","slope_post"),
                     variable.name="era", value.name="slope")
  slope_long[, era := fifelse(era=="slope_pre",
                               "Pre-Shale (2005-2014)",
                               "Post-Shale (2015-2025)")]
  slope_long[, sig := variable %in% chow_tbl[break_confirmed==TRUE, variable]]

  p_chow <- ggplot(slope_long, aes(x=slope, y=variable,
                                    colour=era, shape=sig)) +
    geom_vline(xintercept=0, linewidth=0.4, colour="#888") +
    geom_point(size=4) +
    geom_line(aes(group=variable), colour="#cccccc", linewidth=0.6) +
    scale_colour_manual(values=c("Pre-Shale (2005-2014)"="#1a3a5c",
                                  "Post-Shale (2015-2025)"="#b5470a"),
                        name="Era") +
    scale_shape_manual(values=c("TRUE"=8,"FALSE"=16),
                       name="Break Confirmed (p<0.05)",
                       labels=c("TRUE"="Yes ***","FALSE"="No")) +
    labs(title    = "FIGURE 03c — Chow Test: OLS Slopes Pre vs Post 2015Q1",
         subtitle = "Arrow shows slope shift | Star = statistically significant structural break",
         caption  = "Chow F-test with break at 2015Q1 | H0: same slope in both periods",
         x="OLS Slope (∂outcome/∂PBRENT YoY)", y=NULL) +
    theme_pub() +
    theme(legend.position="right")
  save_plot(p_chow, "03c_chow_test_slopes.png", w=11, h=6)
}

# =============================================================================
# 8. WINSORISATION REVIEW
# =============================================================================
hdr("SECTION 8: Winsorisation Review")

winsor_vars <- intersect(
  c("dq_rate","chg_tot_lns_ratio","netintmrg","roa","costfds",
    "insured_share_growth","cert_growth_yoy","dep_growth_yoy",
    "loan_to_share","cert_share","nim_spread"),
  names(panel))

cat("\n  Variable distribution summary (before/after winsorisation check):\n")
cat(sprintf("  %-30s %8s %8s %8s %8s %8s %8s %7s\n",
            "Variable","Min","P1","P25","Median","P75","P99","Max"))
cat("  ", strrep("-",90), "\n", sep="")

for (v in winsor_vars) {
  x <- panel[[v]]
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) next
  qs <- quantile(x, c(0,0.01,0.25,0.5,0.75,0.99,1), na.rm=TRUE)
  cat(sprintf("  %-30s %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %7.3f\n",
              v, qs[1], qs[2], qs[3], qs[4], qs[5], qs[6], qs[7]))
}

# Apply additional winsorisation to variables not done in Phase 1
needs_winsor <- c("dq_rate","chg_tot_lns_ratio","roa","nim_spread")
for (v in intersect(needs_winsor, names(panel))) {
  panel[!is.na(get(v)),
        (v) := winsor(get(v), p=0.01)]
}
msg("  Applied 1-99th pctile winsorisation to: %s",
    paste(intersect(needs_winsor, names(panel)), collapse=", "))

# ── Chart 03d: Distribution plots for key dep vars ───────────────────────────
dist_vars <- intersect(c("dq_rate","netintmrg","insured_share_growth",
                          "costfds"), names(panel))

dist_plots <- lapply(dist_vars, function(v) {
  d <- panel[!is.na(get(v)) & is.finite(get(v)), .(x=get(v))]
  ggplot(d, aes(x=x)) +
    geom_histogram(bins=60, fill="#1a3a5c", alpha=0.7, colour="white",
                   linewidth=0.1) +
    geom_vline(xintercept=quantile(d$x, 0.01), colour="orange",
               linetype="dashed", linewidth=0.5) +
    geom_vline(xintercept=quantile(d$x, 0.99), colour="orange",
               linetype="dashed", linewidth=0.5) +
    labs(title=v, x=NULL, y="Count") +
    theme_pub() + theme(plot.title=element_text(size=9))
})

p_dist <- wrap_plots(dist_plots, ncol=2) +
  plot_annotation(
    title    = "FIGURE 03d — Distribution of Key Dependent Variables (post-winsorisation)",
    subtitle = "Orange dashed lines = 1st/99th percentile winsorisation bounds",
    caption  = "Source: NCUA Form 5300 Call Report (2005Q1-2025Q4)",
    theme    = theme(plot.title=element_text(face="bold",size=12))
  )
save_plot(p_dist, "03d_variable_distributions.png", w=12, h=9)

# =============================================================================
# 9. VARIABLE REGISTRY & SAVE
# =============================================================================
hdr("SECTION 9: Variable Registry & Save")

# Final model variable list — only include vars that exist in panel
model_vars <- list(

  # Dependent variables (VARX endogenous block)
  # chg_tot_lns_ratio included only if present
  dep_vars = intersect(
    c("dq_rate",
      "chg_totlns_ratio",   # confirmed column name
      "chg_tot_lns_ratio",  # alt — whichever exists
      "netintmrg",
      "insured_share_growth","cert_share","loan_to_share",
      "costfds","pcanetworth"),
    names(panel)),

  # Lagged dep vars (dynamic panel)
  dep_lags = paste0(rep(c("dq_rate","netintmrg","costfds"), each=4),
                    "_lag", 1:4),

  # Primary oil shock regressors
  oil_vars = c("macro_base_yoy_oil",
               "macro_base_yoy_oil_lag1","macro_base_yoy_oil_lag2",
               "macro_base_yoy_oil_lag3","macro_base_yoy_oil_lag4",
               "macro_base_oil_pos","macro_base_oil_neg",
               "oil_cycle_pos","oil_vol_4q"),

  # Exposure interactions (direct + indirect channels)
  exposure_vars = c("oil_x_brent","oil_x_brent_bin",
                    "spillover_x_brent","fomc_x_brent",
                    "oil_bartik_iv","bartik_x_brent"),

  # Macro controls (VARX exogenous block)
  macro_controls = c("macro_base_lurc","macro_base_pcpi",
                     "macro_base_yield_curve","macro_base_rmtg",
                     "macro_base_real_rate","macro_base_uypsav",
                     "macro_base_phpi","credit_tightness","hpi_yoy"),

  # Structural & regime dummies
  dummies = c("post_shale","gfc_dummy","zirp_era","covid_dummy",
              "hike_cycle","post_x_oil","zirp_x_oil",
              "post_x_oil_x_direct"),

  # Identifiers — only include those present
  ids = intersect(
    c("join_number","year","quarter","yyyyqq","cal_date",
      "asset_tier","oil_group","reporting_state","cu_group",
      "oil_exposure_bin","oil_exposure_cont","spillover_exposure"),
    names(panel))
)

# Check availability
cat("\n  MODEL VARIABLE AVAILABILITY CHECK:\n")
for (grp in names(model_vars)) {
  vars_in_grp <- model_vars[[grp]]
  found   <- intersect(vars_in_grp, names(panel))
  missing <- setdiff(vars_in_grp, names(panel))
  cat(sprintf("  %-20s : %d/%d found", grp, length(found), length(vars_in_grp)))
  if (length(missing) > 0)
    cat(sprintf(" | Missing: %s", paste(head(missing,3), collapse=", ")))
  cat("\n")
}

# Final sort
setorderv(panel, c("join_number","year","quarter"))

# Save
saveRDS(panel, "Data/panel_model.rds")
saveRDS(model_vars, "Data/model_variable_list.rds")

msg("\n  Saved: Data/panel_model.rds (%s rows × %s cols)",
    format(nrow(panel),big.mark=","), ncol(panel))
msg("  Saved: Data/model_variable_list.rds")

# =============================================================================
# COMPLETE
# =============================================================================
cat("\n=================================================================\n")
cat(" SCRIPT 03 COMPLETE\n")
cat("=================================================================\n")
cat("  Data/panel_model.rds          Model-ready panel\n")
cat("  Data/stationarity_results.rds ADF + KPSS results\n")
cat("  Data/lag_selection.rds        VAR lag order recommendation\n")
cat("  Data/chow_test_results.rds    Structural break test results\n")
cat("  Data/model_variable_list.rds  Variable registry for Phase 4\n\n")
cat("  Figures/03a_stationarity_tests.png\n")
cat("  Figures/03b_lag_selection.png\n")
cat("  Figures/03c_chow_test_slopes.png\n")
cat("  Figures/03d_variable_distributions.png\n")
cat("=================================================================\n")

