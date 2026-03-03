############################################################
# MACRO v5.0  --  FRB BASELINE 2026  +  FEATURE ENGINEERING
#                 (Production -- FCU terminology)
#
# Changes vs v4:
#   * All 'ficu' -> 'fcu' terminology throughout
#   * All macro variable names prefixed with 'macro_' for
#     clear identification when merged with CU panel data
#   * Comprehensive feature engineering on ALL base FRB vars
#   * Date range trimmed: 2005 Q1 -> 2030 Q4
#   * Final section merges macro onto Part 1 qtrly_enriched_v5.rds
#     to produce the modeling-ready dataset
#
# Source : FRB Baseline 2026 Excel spreadsheet
#          1975 Q1 through 2030 Q4, 119 variables, quarterly.
#          Historical actuals + official Fed projections.
#          No ARIMA. No FRED API. No external proxies.
#
# Naming convention:
#   Raw FRB vars    -> macro_{short_name}
#   Derived series  -> macro_{derived_name}
#   YoY features    -> macro_yoy_{short_name}
#   QoQ features    -> macro_qoq_{short_name}
#   Rolling mean    -> macro_{short_name}_rmean4 / _rmean8
#   Rolling SD      -> macro_{short_name}_rsd4
#   Cyclical dev    -> macro_{short_name}_cyc
#   Acceleration    -> macro_yoy_{short_name}_accel
#   Lagged levels   -> macro_{short_name}_lag1 / _lag2 / _lag4
#   Interactions    -> macro_rate_x_slope, etc.
#
# Outputs:
#   macro_raw_v5.rds            raw FRB table (yearqtr date)
#   macro_features_v5.rds       engineered (2005Q1-2030Q4)
#   macro_forecast_v5.csv       projection rows, key series
#   modeling_panel_v5.rds       Part1 + macro merged dataset
#   plots_v5/                   M01-M12 PDF diagnostics
#
# Runtime: < 2 min
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(readxl)
  library(ggplot2)
  library(scales)
  library(tictoc)
})
shift <- data.table::shift
options(scipen = 999)

# ============================================================
# 1. CONFIG
# ============================================================
FRB_FILE     <- "S:/Projects/Credit_Union_Growth_Forecast/Data/FRB_Baseline_2026.xlsx"
FRB_SHEET    <- "FRB Baseline 2026"
FRB_DATE_COL <- "year_qtr"   # NA = auto-detect

DATA_DIR <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR <- file.path(DATA_DIR, "plots_v5")

HIST_END       <- zoo::as.yearqtr("2025 Q3")
FORECAST_START <- zoo::as.yearqtr("2025 Q4")
FORECAST_END   <- zoo::as.yearqtr("2030 Q4")

# Trim range: keep only 2005 Q1 onward
DATE_FLOOR <- zoo::as.yearqtr("2005 Q1")

PLOT_FROM <- 2005L   # earliest year shown in charts

# ============================================================
# 2. VARIABLE MAP  (FRB column name -> internal short name)
#    Names here are pre-prefix; 'macro_' is added after FE.
# ============================================================
VAR_MAP <- c(
  # Rates & monetary policy
  "RFF"        = "fedfunds",
  "RFFEFF"     = "fedfunds_eff",
  "RFFP"       = "fedfunds_rule",
  "RS3M"       = "gs3m",
  "RS3Y"       = "gs3",
  "RS7Y"       = "gs7",
  "RS10Y"      = "gs10",
  "RS10Y6M"    = "gs10_6m",
  "RS30Y"      = "gs30",
  "RT1Y"       = "gs1",
  "RT3Y"       = "gs3_cmt",
  "RT7Y"       = "gs7_cmt",
  "RT10Y"      = "gs10_cmt",
  "RT20Y"      = "gs20_cmt",
  "RT30Y"      = "gs30_cmt",
  "RMTG"       = "mortgage30",
  "RPRM"       = "prime_rate",
  "MPE2Y"      = "mpe2y",
  "MPE10Y"     = "mpe10y",
  "MPE310Y"    = "mpe310y",
  "RTB"        = "tbill3m_fed",
  "RT10YFED"   = "gs10_fed",
  "RT5YFED"    = "gs5_fed",
  "SOFRRATE"   = "sofr",
  "RT1M"       = "gs1m_cmt",
  # Spreads
  "SONOFF"     = "on_off_10yr",
  "SONOFFS"    = "on_off_5yr",
  "SRCB"       = "baa_spread",
  "SRCBBB10"   = "bbb10_spread",
  "RCBBB"      = "bbb10_yield",
  "SRMTG"      = "mtg_spread",
  "SRPRM"      = "prime_spread",
  "SRDIS"      = "discount_spread",
  "SRIOR"      = "ior_spread",
  "SSOFRRATE"  = "sofr_spread",
  # Forward rates
  "RF1Y1Y"     = "fwd_1y1y",
  "RF1Y2Y"     = "fwd_1y2y",
  "RF1Y3Y"     = "fwd_1y3y",
  "RF1Y4Y"     = "fwd_1y4y",
  "RF1Y5Y"     = "fwd_1y5y",
  "RF1Y6Y"     = "fwd_1y6y",
  "RF1Y7Y"     = "fwd_1y7y",
  "RF1Y8Y"     = "fwd_1y8y",
  "RF1Y9Y"     = "fwd_1y9y",
  "RF5Y1Y"     = "fwd_5y1y",
  "RF5Y2Y"     = "fwd_5y2y",
  "RF5Y3Y"     = "fwd_5y3y",
  "RF5Y4Y"     = "fwd_5y4y",
  "RF5Y5Y"     = "fwd_5y5y",
  # Labour
  "LURC"       = "unrate",
  "NAIRU"      = "nairu",
  "LFPRC"      = "lfpr",
  "LFPRCDEM"   = "lfpr_demo",
  "LUAD"       = "unemp_duration",
  "AWUBS"      = "avg_ui_benefit",
  "LIC"        = "initial_claims",
  # Output & activity
  "GDPS"       = "gdp_real",
  "IP"         = "indpro",
  "IPMFG"      = "indpro_mfg",
  "KBKCS"      = "cap_stock",
  "ECS"        = "pce_total",
  "ECDS"       = "pce_durable",
  "ECDMVS"     = "pce_mvp",
  "ECNDS"      = "pce_nondurable",
  "ECNDFS"     = "pce_food",
  "ECNDGS"     = "pce_gasoline",
  "ECONS"      = "consumption",
  "ECONCDMVS"  = "consumption_mvp",
  "ECSS"       = "pce_services",
  "EFIS"       = "fixed_invest",
  # Prices & inflation
  "PCPI"       = "cpi",
  "PCPIXFE"    = "core_cpi",
  "PPI"        = "ppi",
  "PBRENT"     = "oil_brent",
  "PBXE"       = "pce_deflator_nrg",
  "PHPI"       = "hpi_fed",
  "PI_STAR"    = "infl_target",
  "MU"         = "div_tax_wedge",
  # Housing
  "HMI6M"      = "housing_expect",
  "HP1"        = "housing_permits",
  "CASC20XA"   = "cs20_hpi",
  "USHPI"      = "fhfa_hpi",
  "USPHPI"     = "fhfa_purchase_hpi",
  "RHS"        = "rent_sfh",
  "RHM"        = "rent_mfh",
  "RPS"        = "rent_nonres",
  "UFOREINV"   = "foreclosure_inv",
  "UFORES"     = "foreclosure_start",
  "PHPINEW"    = "new_home_price",
  "PCREPI"     = "cre_price",
  # Financial markets
  "MCAP500S"   = "sp500_mktcap",
  "PPSDJT"     = "djia_total",
  "PPSDJIA"    = "djia",
  "PPSNASDAQ"  = "nasdaq",
  "MSCISC"     = "msci_sc",
  # Credit quality
  "CCDQ"       = "cc_delinq",
  "CCCO"       = "cc_chargeoff",
  "OCLDQ"      = "cons_loan_delinq",
  "OCLCO"      = "cons_loan_chargeoff",
  "CILDQ"      = "ci_loan_delinq",
  "CILCO"      = "ci_loan_chargeoff",
  "ULEV"       = "corp_leverage",
  "HDNNB"      = "consumer_bankrupt",
  # Consumer credit & debt
  "CRC"        = "revolving_credit",
  "CNRC"       = "nonrev_credit",
  "TCC"        = "total_credit",
  "MORTGAGES"  = "mortgage_debt",
  # Household finance
  "YPDS"       = "disp_income",
  "YLABORS"    = "labor_income",
  "YPIPCB"     = "interest_pymts",
  "YPINTS"     = "personal_int_inc",
  "YPDIVS"     = "dividend_inc",
  "YNIMPS"     = "net_int_misc",
  "UYPSAV"     = "savings_rate",
  "UYUPRR"     = "undist_profits",
  # Sentiment
  "CCONF"      = "cons_confidence",
  "FRM15"      = "mortgage15"
)

# ============================================================
# 3. HELPERS
# ============================================================
pct_chg_n <- function(x, n) {
  lag_x <- shift(x, n, type = "lag")
  fifelse(!is.na(lag_x) & abs(lag_x) > 1e-9,
          (x - lag_x) / abs(lag_x) * 100, NA_real_)
}

rollmean_safe <- function(x, k)
  zoo::rollapply(x, width = k, FUN = mean, na.rm = TRUE,
                 fill = NA, align = "right", partial = FALSE)

rollsd_safe <- function(x, k)
  zoo::rollapply(x, width = k, FUN = sd, na.rm = TRUE,
                 fill = NA, align = "right", partial = FALSE)

std_z <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s < 1e-9) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

frb_to_yearqtr <- function(x) {
  x  <- trimws(as.character(x))
  x  <- gsub("Q", ".", x, fixed = TRUE)
  sp <- strsplit(x, "\\.")
  yr <- as.integer(vapply(sp, `[[`, character(1), 1L))
  qr <- as.integer(vapply(sp,
    function(z) if (length(z) >= 2L) z[[2L]] else "1", character(1)))
  zoo::as.yearqtr(yr + (qr - 1L) / 4L)
}

theme_v5 <- function()
  theme_bw(base_size = 11) +
  theme(strip.background  = element_rect(fill = "#e8f0f7"),
        strip.text         = element_text(face = "bold"),
        plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(colour = "grey40", size = 9),
        legend.position    = "bottom",
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 8))

save_plot <- function(p, stem, w = 13, h = 7) {
  path <- file.path(PLOT_DIR, paste0(stem, ".pdf"))
  tryCatch({
    pdf(path, width = w, height = h)
    print(p)
    dev.off()
    message(sprintf("    [saved] %s.pdf", stem))
  }, error = function(e) {
    try(dev.off(), silent = TRUE)
    message(sprintf("    [WARN] Could not save %s: %s", stem, e$message))
  })
  invisible(path)
}

# ============================================================
# START
# ============================================================
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)
setwd(DATA_DIR)

t0 <- proc.time()
tic("MACRO v5.0 total")
message("=======================================================")
message(sprintf("MACRO v5.0 (Production)  %s",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message(sprintf("Historical  : %s -> %s", as.character(DATE_FLOOR), as.character(HIST_END)))
message(sprintf("Projection  : %s -> %s",
                as.character(FORECAST_START), as.character(FORECAST_END)))
message("Source: FRB file only. No ARIMA. No FRED API.")
message("All variables prefixed with 'macro_' for merge clarity.")
message("=======================================================")

# ============================================================
# SECTION 1: LOAD FRB BASELINE 2026
# ============================================================
tic("S1. Load FRB")
message("\n[S1] Loading FRB Baseline 2026...")

if (!file.exists(FRB_FILE))
  stop("FRB file not found:\n  ", FRB_FILE, "\nUpdate FRB_FILE in CONFIG.")

frb_raw <- as.data.table(
  readxl::read_excel(FRB_FILE, sheet = FRB_SHEET,
                     col_names = TRUE,
                     na = c("", "NA", "N/A", "#N/A", "#VALUE!"))
)
message(sprintf("    Raw read : %d rows x %d cols", nrow(frb_raw), ncol(frb_raw)))

# ---- Find date column ----------------------------------------
if (!is.na(FRB_DATE_COL) && FRB_DATE_COL %in% names(frb_raw)) {
  date_col <- FRB_DATE_COL
} else {
  date_col <- NA_character_
  for (cn in names(frb_raw)) {
    v <- as.character(frb_raw[[cn]])
    v <- v[!is.na(v)]
    if (length(v) > 10 && mean(grepl("^\\d{4}[.Q]\\d$", v)) > 0.8) {
      date_col <- cn; break
    }
  }
  if (is.na(date_col))
    stop("Cannot detect date column. Set FRB_DATE_COL in CONFIG.")
}
message(sprintf("    Date col  : '%s'", date_col))

# ---- Parse date ----------------------------------------------
frb_raw[, date := frb_to_yearqtr(get(date_col))]
frb_raw[, (date_col) := NULL]
frb_raw <- frb_raw[!is.na(date)]
setorderv(frb_raw, "date")

# ---- Rename via VAR_MAP --------------------------------------
n_renamed <- 0L
for (old_nm in intersect(names(frb_raw), names(VAR_MAP))) {
  new_nm <- VAR_MAP[[old_nm]]
  if (new_nm %in% names(frb_raw) && new_nm != old_nm) {
    message(sprintf("    SKIP '%s'->'%s': target already exists", old_nm, new_nm))
    next
  }
  setnames(frb_raw, old_nm, new_nm)
  n_renamed <- n_renamed + 1L
}
message(sprintf("    Renamed   : %d variables", n_renamed))

# ---- Coerce to numeric (skip date) ---------------------------
val_cols <- setdiff(names(frb_raw), "date")
frb_raw[, (val_cols) :=
          lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
         .SDcols = val_cols]

# ---- is_forecast flag ----------------------------------------
frb_raw[, is_forecast := (date > HIST_END)]
frb_raw[, date := zoo::as.yearqtr(date)]
stopifnot("date" %in% names(frb_raw), inherits(frb_raw$date, "yearqtr"))

dr <- range(frb_raw$date, na.rm = TRUE)
message(sprintf("    Full range: %s -> %s  (%d rows)",
                as.character(dr[1]), as.character(dr[2]), nrow(frb_raw)))

saveRDS(frb_raw, file.path(DATA_DIR, "macro_raw_v5.rds"))
message("    Saved     : macro_raw_v5.rds")
toc()

# ============================================================
# SECTION 2: DERIVED / COMPOSITE SERIES
# ============================================================
tic("S2. Derived series")
message("\n[S2] Derived / composite series...")

m <- copy(frb_raw)
m[, date := zoo::as.yearqtr(date)]

if (all(c("gs10", "gs3m") %in% names(m))) {
  m[, yield_curve := gs10 - gs3m]
  message("    yield_curve      = gs10 - gs3m")
}
if (all(c("gs10", "gs3") %in% names(m))) {
  m[, spread_2s10s := gs10 - gs3]
  message("    spread_2s10s     = gs10 - gs3")
}
if ("yield_curve" %in% names(m)) {
  m[, yield_curve_inv := fifelse(
      !is.na(yield_curve) & yield_curve < 0, 1L, 0L)]
  m[, yield_curve_inv_run := {
      inv <- fifelse(is.na(yield_curve_inv), 0L, yield_curve_inv)
      sequence(rle(inv)$lengths)
  }]
  message("    yield_curve_inv, yield_curve_inv_run")
}
if ("cpi" %in% names(m)) {
  m[, cpi_yoy := pct_chg_n(cpi, 4L)]
  message("    cpi_yoy")
}
if ("core_cpi" %in% names(m)) {
  m[, core_cpi_yoy := pct_chg_n(core_cpi, 4L)]
  message("    core_cpi_yoy")
}
if (all(c("fedfunds", "cpi_yoy") %in% names(m))) {
  m[, real_rate := fedfunds - cpi_yoy]
  message("    real_rate        = fedfunds - cpi_yoy")
}
if ("baa_spread" %in% names(m)) {
  m[, hy_spread := baa_spread]
  message("    hy_spread        <- baa_spread (alias)")
}
ct_inputs <- intersect(
  c("baa_spread", "bbb10_spread", "cc_delinq", "ci_loan_delinq"),
  names(m))
if (length(ct_inputs) >= 2) {
  m[, credit_tightness :=
      Reduce(`+`, lapply(.SD, std_z)) / length(ct_inputs),
     .SDcols = ct_inputs]
  message(sprintf("    credit_tightness  avg z-score(%s)",
                  paste(ct_inputs, collapse = ", ")))
}
if ("fedfunds" %in% names(m)) {
  m[, fedfunds_trail8 := rollmean_safe(fedfunds, 8L)]
  m[, fedfunds_cycle  := fedfunds - fedfunds_trail8]
  message("    fedfunds_cycle")
}
if ("fedfunds" %in% names(m)) {
  m[, fedfunds_chg := fedfunds - shift(fedfunds, 1L, type = "lag")]
  m[, fomc_regime  := fcase(
      fedfunds_chg >  0.10,  1L,
      fedfunds_chg < -0.10, -1L,
      default = 0L)]
  m[, hike_run := {
      reg <- fifelse(is.na(fomc_regime), 0L, fomc_regime)
      r   <- rle(reg)
      sequence(r$lengths) * rep(sign(r$values), r$lengths)
  }]
  message("    fedfunds_chg, fomc_regime, hike_run")
}
if ("oil_brent" %in% names(m)) {
  m[, oil_qoq_pct := pct_chg_n(oil_brent, 1L)]
  m[, oil_shock   := fifelse(
      !is.na(oil_qoq_pct) & abs(oil_qoq_pct) > 20, 1L, 0L)]
  message("    oil_qoq_pct, oil_shock")
}

message(sprintf("    Derived done: %d rows x %d cols", nrow(m), ncol(m)))
toc()

# ============================================================
# SECTION 3: FEATURE ENGINEERING
# ============================================================
tic("S3. Feature engineering")
message("\n[S3] Feature engineering (YoY / QoQ / rolling / lags)...")

DERIVED_COLS <- c(
  "date", "is_forecast",
  "yield_curve", "spread_2s10s",
  "yield_curve_inv", "yield_curve_inv_run",
  "cpi_yoy", "core_cpi_yoy", "real_rate",
  "hy_spread",
  "credit_tightness",
  "fedfunds_trail8", "fedfunds_cycle", "fedfunds_chg",
  "fomc_regime", "hike_run",
  "oil_qoq_pct", "oil_shock"
)

all_num <- names(m)[vapply(m, is.numeric, logical(1))]
base_fe <- setdiff(all_num, DERIVED_COLS)
message(sprintf("    Base FRB vars for FE: %d", length(base_fe)))

# 3a: YoY % change
message("    3a. YoY %...")
m[, paste0("yoy_", base_fe) :=
    lapply(.SD, pct_chg_n, n = 4L), .SDcols = base_fe]

# 3b: QoQ % change
message("    3b. QoQ %...")
m[, paste0("qoq_", base_fe) :=
    lapply(.SD, pct_chg_n, n = 1L), .SDcols = base_fe]

KEY_MACRO <- intersect(c(
  "fedfunds", "fedfunds_eff", "fedfunds_rule",
  "gs3m", "gs3", "gs7", "gs10", "gs30", "gs1",
  "gs10_cmt", "gs5_fed", "gs10_fed",
  "mortgage30", "mortgage15", "prime_rate", "sofr",
  "yield_curve", "spread_2s10s",
  "baa_spread", "bbb10_spread", "mtg_spread", "prime_spread",
  "real_rate", "fedfunds_cycle", "credit_tightness",
  "unrate", "nairu", "lfpr", "initial_claims", "unemp_duration",
  "gdp_real", "indpro", "pce_total", "consumption", "fixed_invest",
  "cpi", "core_cpi", "ppi", "oil_brent",
  "housing_permits", "housing_expect",
  "fhfa_hpi", "fhfa_purchase_hpi", "cs20_hpi", "cre_price",
  "rent_sfh", "rent_mfh", "foreclosure_inv",
  "sp500_mktcap", "djia", "djia_total", "nasdaq",
  "disp_income", "labor_income", "savings_rate",
  "cc_delinq", "cc_chargeoff",
  "ci_loan_delinq", "ci_loan_chargeoff",
  "cons_confidence", "consumer_bankrupt", "corp_leverage",
  "revolving_credit", "nonrev_credit", "total_credit", "mortgage_debt"
), names(m))

message(sprintf("    Key series for rolling / lags: %d", length(KEY_MACRO)))

# 3c: Rolling means 4q and 8q
message("    3c. Rolling means...")
for (k in c(4L, 8L)) {
  nms <- paste0(KEY_MACRO, "_rmean", k)
  m[, (nms) := lapply(.SD, rollmean_safe, k = k), .SDcols = KEY_MACRO]
}

# 3d: Rolling SD 4q
message("    3d. Rolling SDs...")
m[, paste0(KEY_MACRO, "_rsd4") :=
    lapply(.SD, rollsd_safe, k = 4L), .SDcols = KEY_MACRO]

# 3e: Cyclical deviation (v - rmean8)
message("    3e. Cyclical deviations...")
for (v in KEY_MACRO) {
  rc <- paste0(v, "_rmean8")
  if (rc %in% names(m))
    m[, (paste0(v, "_cyc")) := get(v) - get(rc)]
}

# 3f: YoY acceleration (delta of yoy, lag-4)
message("    3f. YoY acceleration...")
yoy_key <- intersect(paste0("yoy_", KEY_MACRO), names(m))
m[, paste0(yoy_key, "_accel") :=
    lapply(.SD, function(x) x - shift(x, 4L, type = "lag")),
   .SDcols = yoy_key]

# 3g: Lagged levels 1q / 2q / 4q
message("    3g. Lagged levels...")
for (lag_n in c(1L, 2L, 4L)) {
  nms <- paste0(KEY_MACRO, "_lag", lag_n)
  m[, (nms) := lapply(.SD, shift, n = lag_n, type = "lag"),
     .SDcols = KEY_MACRO]
}

# 3h: Interaction terms
message("    3h. Interactions...")
if (all(c("fedfunds", "yield_curve")      %in% names(m)))
  m[, rate_x_slope      := fedfunds * yield_curve]
if (all(c("fedfunds", "hy_spread")        %in% names(m)))
  m[, rate_x_hy         := fedfunds * hy_spread]
if (all(c("fedfunds", "credit_tightness") %in% names(m)))
  m[, rate_x_credit     := fedfunds * credit_tightness]
if (all(c("unrate", "real_rate")          %in% names(m)))
  m[, unemp_x_real_rate := unrate * real_rate]
if (all(c("spread_2s10s", "baa_spread")   %in% names(m)))
  m[, slope_x_credit    := spread_2s10s * baa_spread]

m[, date := zoo::as.yearqtr(date)]
message(sprintf("    FE done: %d rows x %d cols", nrow(m), ncol(m)))
toc()

# ============================================================
# SECTION 3B: TRIM TO 2005 Q1 ONWARD
# ============================================================
message(sprintf("\n[S3b] Trimming to %s onward...", as.character(DATE_FLOOR)))
n_before <- nrow(m)
m <- m[date >= DATE_FLOOR]
message(sprintf("    Removed %d rows before %s  |  Remaining: %d rows",
                n_before - nrow(m), as.character(DATE_FLOOR), nrow(m)))

# ============================================================
# SECTION 3C: ADD 'macro_' PREFIX TO ALL VARIABLE COLUMNS
# ============================================================
message("\n[S3c] Adding 'macro_' prefix to all variable columns...")

no_prefix <- c("date", "is_forecast")
cols_to_prefix <- setdiff(names(m), no_prefix)
new_names <- paste0("macro_", cols_to_prefix)
setnames(m, cols_to_prefix, new_names)
message(sprintf("    Prefixed %d columns with 'macro_'", length(cols_to_prefix)))
message(sprintf("    Final macro table: %d rows x %d cols  |  %s -> %s",
                nrow(m), ncol(m),
                as.character(min(m$date)), as.character(max(m$date))))

# ============================================================
# SECTION 4: QC
# ============================================================
tic("S4. QC")
message("\n[S4] Quality control...")

all_na_c <- names(m)[vapply(m, function(x) all(is.na(x)), logical(1))]
if (length(all_na_c) > 0) {
  m[, (all_na_c) := NULL]
  message(sprintf("    Dropped %d all-NA cols", length(all_na_c)))
}

hist_m  <- m[is_forecast == FALSE]
num_nms <- names(hist_m)[vapply(hist_m, is.numeric, logical(1))]
na_pcts <- vapply(num_nms,
                  function(v) mean(is.na(hist_m[[v]])) * 100, numeric(1))
hi_na   <- sort(na_pcts[na_pcts > 40], decreasing = TRUE)
if (length(hi_na) > 0) {
  message(sprintf("    Cols >40%% NA in historical rows (%d):", length(hi_na)))
  print(head(round(hi_na, 1), 20))
} else {
  message("    All historical cols under 40% NA")
}

p3_req   <- paste0("macro_", c("fedfunds", "gs10", "unrate", "cpi_yoy",
                                "real_rate", "yield_curve", "mortgage30"))
miss_p3  <- setdiff(p3_req, names(m))
if (length(miss_p3) > 0)
  warning("Missing Part-3 required cols: ", paste(miss_p3, collapse = ", "))
else
  message("    All critical Part-3 columns present")

message(sprintf("    Final table: %d rows x %d cols", nrow(m), ncol(m)))
toc()

# ============================================================
# SECTION 5: SAVE MACRO OUTPUTS
# ============================================================
tic("S5. Save macro")
message("\n[S5] Saving macro outputs...")

saveRDS(m, file.path(DATA_DIR, "macro_features_v5.rds"))
message(sprintf("    macro_features_v5.rds  (%d x %d)  %s -> %s",
                nrow(m), ncol(m),
                as.character(min(m$date)), as.character(max(m$date))))

key_exp_base <- intersect(
  paste0("macro_", c(KEY_MACRO,
    "cpi_yoy", "core_cpi_yoy", "real_rate", "fedfunds_cycle",
    "credit_tightness", "yield_curve_inv", "fomc_regime", "hike_run",
    "oil_qoq_pct", "oil_shock", "spread_2s10s")),
  names(m))
key_exp <- c("date", key_exp_base)
fc_exp <- m[is_forecast == TRUE, key_exp, with = FALSE]
fc_exp[, date_label := as.character(date)]
fc_exp[, year := as.integer(format(date, "%Y"))]
fc_exp[, qtr  := as.integer(round((as.numeric(date) %% 1) * 4 + 1))]
setcolorder(fc_exp,
            c("date", "date_label", "year", "qtr",
              setdiff(names(fc_exp),
                      c("date", "date_label", "year", "qtr"))))
fwrite(fc_exp, file.path(DATA_DIR, "macro_forecast_v5.csv"))
message(sprintf("    macro_forecast_v5.csv  (%d rows x %d cols)",
                nrow(fc_exp), ncol(fc_exp)))
toc()

# ============================================================
# SECTION 6: MERGE ONTO PART 1 CU PANEL -> MODELING DATASET
# ============================================================
tic("S6. Merge with Part 1")
message("\n[S6] Merging macro onto Part 1 CU panel...")

panel_path <- file.path(DATA_DIR, "qtrly_enriched_v5.rds")

if (!file.exists(panel_path)) {
  message(sprintf("    WARNING: Part 1 output not found: %s", panel_path))
  message("    Run 1_Data_Prep_v5.R first, then re-run this script.")
  message("    Skipping merge step.")
} else {
  panel <- readRDS(panel_path)
  setDT(panel)

  if ("date" %in% names(panel) && !inherits(panel$date, "yearqtr"))
    panel[, date := zoo::as.yearqtr(date)]

  message(sprintf("    Part 1 panel: %d x %d", nrow(panel), ncol(panel)))

  merge_cols <- setdiff(names(m), c("date", "is_forecast"))

  # Use historical macro only (Part 1 panel is historical)
  macro_hist <- m[is_forecast == FALSE]

  # Remove any stale macro columns from panel
  stale <- intersect(merge_cols, names(panel))
  if (length(stale) > 0) {
    panel[, (stale) := NULL]
    message(sprintf("    Removed %d stale macro columns from panel", length(stale)))
  }

  # Index-based merge for speed
  idx     <- match(panel$date, macro_hist$date)
  n_match <- sum(!is.na(idx))
  message(sprintf("    Date matches: %d / %d panel rows (%.0f%%)",
                  n_match, nrow(panel), n_match / nrow(panel) * 100))

  if (n_match == 0) {
    message("    ERROR: 0 date matches. Check panel$date is yearqtr.")
  } else {
    for (col in merge_cols)
      panel[, (col) := macro_hist[[col]][idx]]

    out_path <- file.path(DATA_DIR, "modeling_panel_v5.rds")
    saveRDS(panel, out_path)
    message(sprintf("    Saved: modeling_panel_v5.rds  (%d x %d)",
                    nrow(panel), ncol(panel)))

    n_macro  <- sum(grepl("^macro_", names(panel)))
    n_panel  <- ncol(panel) - n_macro
    message(sprintf("    Column breakdown: %d CU panel + %d macro = %d total",
                    n_panel, n_macro, ncol(panel)))
  }
}
toc()

# ============================================================
# SECTION 7: DIAGNOSTIC PLOTS  M01 - M12
# ============================================================
tic("S7. Plots")
message("\n[S7] Generating diagnostic plots...")

m_plot <- copy(m)
m_plot[, date_d  := as.Date(date)]
m_plot[, yr_int  := as.integer(format(date, "%Y"))]
m_plot[, period  := fifelse(is_forecast, "Projection", "Historical")]

FC_START_D <- as.Date(FORECAST_START)
FC_END_D   <- as.Date(FORECAST_END)
PERIOD_COLOURS <- c("Historical" = "#1f77b4", "Projection" = "#d62728")

# ---- Helper: single-series time plot -------------------------
plot_ts <- function(vname, y_label, log_sc = FALSE, stem = NULL) {
  actual_name <- if (vname %in% names(m_plot)) vname else paste0("macro_", vname)
  if (!actual_name %in% names(m_plot)) {
    message(sprintf("    SKIP (not in data): %s", vname))
    return(invisible(NULL))
  }
  dt <- m_plot[yr_int >= PLOT_FROM & !is.na(get(actual_name)),
               .(date_d, y = get(actual_name), period)]
  if (nrow(dt) == 0) return(invisible(NULL))
  p <- ggplot(dt, aes(x = date_d, y = y)) +
    annotate("rect", xmin = FC_START_D, xmax = FC_END_D + 100,
             ymin = -Inf, ymax = Inf, fill = "#fffde7", alpha = 0.5) +
    geom_vline(xintercept = FC_START_D,
               linetype = "dotted", colour = "grey50", linewidth = 0.5) +
    geom_line(data = dt[period == "Historical"],
              colour = "#1f77b4", linewidth = 0.9) +
    geom_line(data = dt[period == "Projection"],
              colour = "#d62728", linewidth = 0.85, linetype = "dashed") +
    {if (log_sc) scale_y_log10(labels = comma) else
        scale_y_continuous(labels = comma)} +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    labs(title = paste0(y_label, "  -- FRB Baseline 2026"),
         subtitle = "Blue = historical  |  Red dashed = FRB projection",
         x = NULL, y = y_label) +
    theme_v5()
  if (!is.null(stem)) save_plot(p, stem)
  invisible(p)
}

# ---- Helper: multi-series facet panel ------------------------
plot_facet <- function(vars, title_txt, stem, ncol = 3, w = 14, h = 7) {
  actual_vars <- vapply(vars, function(v) {
    if (v %in% names(m_plot)) v else paste0("macro_", v)
  }, character(1))
  actual_vars <- intersect(actual_vars, names(m_plot))
  if (length(actual_vars) < 2) return(invisible(NULL))
  keep_cols <- c("date_d", "yr_int", "period", actual_vars)
  dt <- melt(
    m_plot[yr_int >= PLOT_FROM, keep_cols, with = FALSE],
    id.vars       = c("date_d", "yr_int", "period"),
    variable.name = "series",
    value.name    = "value"
  )
  dt[, series := gsub("^macro_", "", as.character(series))]
  non_na <- dt[, .(has_data = any(!is.na(value))), by = series]
  dt <- dt[series %in% non_na[has_data == TRUE]$series]
  if (nrow(dt) == 0 || dt[, uniqueN(series)] < 2) return(invisible(NULL))

  p <- ggplot(dt[!is.na(value)],
              aes(x = date_d, y = value, colour = period)) +
    annotate("rect", xmin = FC_START_D, xmax = FC_END_D + 100,
             ymin = -Inf, ymax = Inf, fill = "#fffde7", alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey65", linewidth = 0.25) +
    geom_vline(xintercept = FC_START_D,
               linetype = "dotted", colour = "grey50", linewidth = 0.4) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~ series, scales = "free_y", ncol = ncol) +
    scale_colour_manual(values = PERIOD_COLOURS, name = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(title = paste0(title_txt, "  -- FRB Baseline 2026"),
         subtitle = sprintf("Yellow = FRB projection (%s - %s)",
                            as.character(FORECAST_START),
                            as.character(FORECAST_END)),
         x = NULL, y = NULL) +
    theme_v5()
  save_plot(p, stem, w = w, h = h)
  invisible(p)
}

message("    M01-M06: Individual series...")
plot_ts("fedfunds",    "Fed Funds Rate (%)",           stem = "M01_fedfunds")
plot_ts("unrate",      "Unemployment Rate (%)",        stem = "M02_unrate")
plot_ts("cpi_yoy",     "CPI YoY % Change",             stem = "M03_cpi_yoy")
plot_ts("gdp_real",    "Real GDP (Bil. $)",
        log_sc = TRUE,                                  stem = "M04_gdp_real")
plot_ts("yield_curve", "Yield Curve (RS10Y - RS3M, %)",stem = "M05_yield_curve")
plot_ts("mortgage30",  "30-yr Mortgage Rate (%)",       stem = "M06_mortgage30")

message("    M07: Rates panel...")
plot_facet(
  c("fedfunds", "fedfunds_eff", "fedfunds_rule",
    "yield_curve", "spread_2s10s", "real_rate", "fedfunds_cycle"),
  "Monetary Policy & Yield Curve", "M07_rates_panel", ncol = 3, h = 9)

message("    M08: Labour panel...")
plot_facet(
  c("unrate", "nairu", "lfpr", "lfpr_demo",
    "initial_claims", "unemp_duration", "avg_ui_benefit"),
  "Labour Market", "M08_labour_panel", ncol = 3, h = 8)

message("    M09: Credit panel...")
plot_facet(
  c("baa_spread", "bbb10_spread", "credit_tightness",
    "cc_delinq", "cc_chargeoff",
    "ci_loan_delinq", "ci_loan_chargeoff",
    "corp_leverage", "consumer_bankrupt"),
  "Credit & Financial Conditions", "M09_credit_panel", ncol = 3, h = 10)

message("    M10: Housing panel...")
plot_facet(
  c("fhfa_hpi", "fhfa_purchase_hpi", "cs20_hpi",
    "housing_permits", "housing_expect", "new_home_price",
    "mortgage30", "mtg_spread", "foreclosure_inv", "cre_price"),
  "Housing Market", "M10_housing_panel", ncol = 3, h = 10)

message("    M11: Composite panel...")
plot_facet(
  c("real_rate", "credit_tightness", "fedfunds_cycle",
    "hike_run", "yield_curve_inv_run", "oil_qoq_pct"),
  "Derived / Composite Indicators", "M11_composite_panel", ncol = 3, h = 8)

message("    M12: Coverage heatmap...")
m_cov <- copy(m)
m_cov[, decade := paste0(
  floor(as.integer(format(date, "%Y")) / 5) * 5, "s")]

cov_vars <- intersect(
  paste0("macro_", c(KEY_MACRO,
    "cpi_yoy", "core_cpi_yoy", "real_rate", "spread_2s10s",
    "credit_tightness", "fedfunds_cycle", "hike_run",
    "oil_qoq_pct", "oil_shock")),
  names(m_cov))

if (length(cov_vars) >= 5) {
  cov_dt <- m_cov[,
    lapply(.SD, function(x) as.integer(round(mean(!is.na(x)) * 100))),
    by = decade, .SDcols = cov_vars]
  cov_m <- melt(cov_dt, id.vars = "decade",
                variable.name = "variable", value.name = "pct")
  cov_m[, variable := gsub("^macro_", "", as.character(variable))]

  p_cov <- ggplot(cov_m, aes(x = decade, y = variable, fill = pct)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = paste0(pct, "%")), size = 2.3, colour = "grey15") +
    scale_fill_gradient(low = "#fee0d2", high = "#2ca25f",
                        limits = c(0, 100), name = "% Available") +
    labs(title    = "Variable Coverage -- FRB Baseline 2026 (2005+)",
         subtitle = "Green = fully available  |  Red = sparse or absent",
         x = NULL, y = NULL) +
    theme_v5() +
    theme(axis.text.y = element_text(size = 7))
  save_plot(p_cov, "M12_coverage_heatmap", w = 13, h = 11)
}

n_plots <- length(list.files(PLOT_DIR, pattern = "^M[0-9].*\\.pdf$"))
message(sprintf("    %d PDF plots saved -> %s/", n_plots, PLOT_DIR))
toc()

# ============================================================
# SECTION 8: FINAL SUMMARY
# ============================================================
tot <- as.numeric((proc.time() - t0)["elapsed"])
toc()

message("\n=======================================================")
message(sprintf("MACRO v5.0 COMPLETE  %dh %02dm %02ds",
                floor(tot / 3600),
                floor((tot %% 3600) / 60),
                round(tot %% 60)))
message("Source     : FRB Baseline 2026 only. No ARIMA. No FRED.")
message(sprintf("Date range : %s -> %s  (%d quarters)",
                as.character(min(m$date)),
                as.character(max(m$date)), nrow(m)))
message(sprintf("Rows       : %d historical  |  %d FRB projection",
                sum(!m$is_forecast), sum(m$is_forecast)))
message(sprintf("Variables  : %d total (all macro_ prefixed)",
                ncol(m) - 2L))

n_yoy   <- sum(grepl("^macro_yoy_",  names(m)))
n_qoq   <- sum(grepl("^macro_qoq_",  names(m)))
n_lag   <- sum(grepl("_lag[0-9]$",    names(m)))
n_rmean <- sum(grepl("_rmean",        names(m)))
n_rsd   <- sum(grepl("_rsd",          names(m)))
n_cyc   <- sum(grepl("_cyc$",         names(m)))
n_accel <- sum(grepl("_accel$",       names(m)))
n_inter <- sum(grepl("^macro_.*_x_",  names(m)))

message(sprintf("\n-- Macro feature inventory --"))
message(sprintf("  YoY: %d  QoQ: %d  Lags: %d  RollingMean: %d",
                n_yoy, n_qoq, n_lag, n_rmean))
message(sprintf("  RollingSD: %d  Cyclical: %d  Acceleration: %d  Interactions: %d",
                n_rsd, n_cyc, n_accel, n_inter))

message("\nOutputs:")
message("  macro_raw_v5.rds          raw FRB table")
message(sprintf("  macro_features_v5.rds     %d cols (2005Q1-2030Q4)", ncol(m)))
message("  macro_forecast_v5.csv     projection rows")
message("  modeling_panel_v5.rds     CU panel + macro merged")
message(sprintf("  %d PDF plots             -> %s/", n_plots, PLOT_DIR))
message("=======================================================")

message("\n-- Spot-check: last 4 historical + first 4 projection --")
spot_cols <- intersect(
  c("date", paste0("macro_", c("fedfunds", "unrate", "cpi_yoy", "real_rate",
    "yield_curve", "credit_tightness", "fomc_regime"))),
  names(m))
print(m[date %in% c(tail(m[is_forecast == FALSE]$date, 4L),
                     head(m[is_forecast == TRUE]$date,  4L)),
         spot_cols, with = FALSE])

message("\n-- Next step: run Part 3 regression pipeline --")

############################################################
# END
############################################################
