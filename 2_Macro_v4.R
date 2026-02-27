############################################################
# MACRO v4.0  —  FRB BASELINE 2026  +  FEATURE ENGINEERING
#
# Architecture change vs v3:
#   v3  pulled FRED data (historical only) and extended each
#       series to 2030Q4 via auto.arima().
#   v4  reads a SINGLE pre-built FRB Baseline 2026 spreadsheet
#       that already contains ALL quarterly observations from
#       1975 Q1 through 2030 Q4 (both historical and the Fed's
#       own official macro projections).  No ARIMA forecasting
#       is performed here at all.
#
# Variable coverage (119 series visible in FRB spreadsheet):
#   Rates / monetary policy : RFF, RFFEFF, RFFP, RS3M...RS30Y,
#       RT1Y...RT30Y, RMTG, RPRM, MPE2Y/10Y/310Y,
#       RF1Y1Y...RF1Y9Y, RF5Y1Y...RF5Y5Y, RTB, RT10YFED,
#       RT5YFED, SOFRRATE, SSOFRRATE
#   Spreads : SRCB, SRCBBB10, RCBBB, SRMTG, SRPRM, SRIOR,
#       SRDIS, SONOFF, SONOFFS
#   Labour : LURC, NAIRU, LFPRC, LFPRCDEM, LUAD, AWUBS, LIC
#   Output : GDPS, IP, IPMFG, KBKCS, ECS, ECDS, ECDMVS,
#       ECNDS, ECNDFS, ECNDGS, ECONS, ECONCDMVS, ECSS, EFIS
#   Prices : PCPI, PCPIXFE, PPI, PBRENT, PBXE, PHPI, PI_STAR
#   Housing : HMI6M, HP1, CASC20XA, USHPI, USPHPI, RHS, RHM,
#       RPS, UFOREINV, UFORES, PHPINEW, PCREPI
#   Financial : MCAP500S, PPSDJT, PPSDJIA, PPSNASDAQ, MSCISC
#   Credit quality : CCDQ, CCCO, OCLDQ, OCLCO, CILDQ, CILCO,
#       ULEV, HDNNB
#   Consumer credit : CRC, CNRC, TCC, MORTGAGES
#   Household finance : YPDS, YLABORS, YPIPCB, YPINTS, YPDIVS,
#       YNIMPS, UYPSAV, UYUPRR, CCONF, FRM15, MU
#
# Feature engineering (mirrors Part 1 v3 panel FE exactly):
#   - YoY % change (lag-4)             yoy_{var}
#   - QoQ % change (lag-1)             qoq_{var}
#   - Rolling mean 4q / 8q             {var}_rmean4 / rmean8
#   - Rolling SD 4q                    {var}_rsd4
#   - Cyclical deviation (x - rmean8)  {var}_cyc
#   - YoY acceleration (delta yoy)     yoy_{var}_accel
#   - Lagged levels 1q / 2q / 4q       {var}_lag1 / lag2 / lag4
#   - Composite / derived series       real_rate, spread_2s10s,
#       credit_tightness, fedfunds_cycle, fomc_regime, hike_run,
#       yield_curve, yield_curve_inv, yield_curve_inv_run,
#       oil_shock, cpi_yoy, bank_deps_yoy, real_payems
#   - Interaction terms                rate_x_slope, rate_x_hy,
#       rate_x_credit, unemp_x_real_rate, slope_x_credit
#
# Output column names match macro_v3_arima.R EXACTLY so Part 3
# regression pipeline requires zero changes.
#
# Outputs:
#   macro_raw_v4.rds          raw FRB table (wide, yearqtr date)
#   macro_features_v4.rds     fully engineered (hist + projection)
#   macro_forecast_v4.csv     projection rows, key series
#   qtrly_enriched_v3.rds     overwritten with v4 macro features
#   qtrly_full_v3.rds         overwritten with v4 macro features
#   plots_v4/                 diagnostic PDFs (M01-M12)
#
# Runtime: < 2 min (no ARIMA, no FRED API calls)
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(readxl)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(lubridate)
  library(tictoc)
})
shift <- data.table::shift
options(scipen = 999)

# ============================================================
# CONFIG  <-- edit paths here
# ============================================================

# Path to FRB Baseline 2026 Excel file
FRB_FILE  <- "S:/Projects/Credit_Union_Growth_Forecast/Data/FRB_Baseline_2026.xlsx"
FRB_SHEET <- "FRB Baseline 2026"   # exact sheet name as shown in taskbar

# Column holding the time identifier (e.g. 1975.1, 1975.2 ...)
FRB_DATE_COL <- "year_qtr"         # set to NA to auto-detect

# Working directory where panel RDS files live
DATA_DIR  <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR  <- file.path(DATA_DIR, "plots_v4")

# Demarcation between historical actuals and FRB projections
HIST_END       <- zoo::as.yearqtr("2025 Q3")   # last actual quarter
FORECAST_START <- zoo::as.yearqtr("2025 Q4")
FORECAST_END   <- zoo::as.yearqtr("2030 Q4")

# Optional ntfy notifications
NTFY_TOPIC   <- "your-unique-topic-name"
NTFY_ENABLED <- FALSE

# ============================================================
# VARIABLE MAP  (FRB column name -> internal short name)
# Names on the RIGHT must match what Part 3 FEAT_PAT expects.
# Variables not listed here are still loaded and engineered
# under their original FRB names.
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
  # Labour market
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
  # Consumer credit & debt
  "CRC"        = "revolving_credit",
  "CNRC"       = "nonrev_credit",
  "TCC"        = "total_credit",
  "MORTGAGES"  = "mortgage_debt",
  "HDNNB"      = "consumer_bankrupt",
  # Household income & saving
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
  # Other
  "FRM15"      = "mortgage15"
)

# ============================================================
# HELPERS
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
std_z <- function(x) (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE) + 1e-9)

theme_v4 <- function()
  theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "#e8f0f7"),
        strip.text       = element_text(face = "bold"),
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(colour = "grey40"),
        legend.position  = "bottom")

save_plot <- function(p, stem, w = 12, h = 7) {
  path <- file.path(PLOT_DIR, paste0(stem, ".pdf"))
  tryCatch(
    ggsave(path, p, width = w, height = h, device = cairo_pdf),
    error = function(e) ggsave(path, p, width = w, height = h, device = "pdf"))
  invisible(path)
}

# Parse FRB year_qtr notation: 1975.1, 1975.2, 1975Q1, etc -> yearqtr
frb_to_yearqtr <- function(x) {
  x  <- trimws(as.character(x))
  x  <- gsub("Q", ".", x, fixed = TRUE)  # 1975Q1 -> 1975.1
  sp <- strsplit(x, "\\.")
  yr <- as.integer(vapply(sp, `[[`, character(1), 1L))
  qr <- as.integer(vapply(sp,
    function(z) if (length(z) >= 2L) z[[2L]] else "1", character(1)))
  zoo::as.yearqtr(yr + (qr - 1L) / 4L)
}

# ============================================================
# START
# ============================================================
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)
setwd(DATA_DIR)

t0 <- proc.time()
tic("MACRO v4.0 total")
message("=======================================================")
message(sprintf("MACRO v4.0  --  FRB Baseline 2026  %s",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message(sprintf("Historical through : %s", as.character(HIST_END)))
message(sprintf("Projection through : %s", as.character(FORECAST_END)))
message("No ARIMA: projections come directly from the FRB file")
message("=======================================================")

# ============================================================
# 1. LOAD FRB BASELINE 2026
# ============================================================
tic("1. Load FRB")
message("\n[1] Loading FRB Baseline 2026...")
message(sprintf("    File: %s", FRB_FILE))
message(sprintf("    Sheet: %s", FRB_SHEET))

if (!file.exists(FRB_FILE))
  stop("FRB file not found at: ", FRB_FILE,
       "\nUpdate FRB_FILE in CONFIG section.")

frb_raw <- as.data.table(
  readxl::read_excel(FRB_FILE, sheet = FRB_SHEET,
                     col_names = TRUE,
                     na = c("", "NA", "N/A", "#N/A", "#VALUE!"))
)
message(sprintf("    Raw: %d rows x %d cols", nrow(frb_raw), ncol(frb_raw)))

# ---- Find and parse date column ----------------------------
if (!is.na(FRB_DATE_COL) && FRB_DATE_COL %in% names(frb_raw)) {
  date_col <- FRB_DATE_COL
} else {
  # Auto-detect: look for column whose values look like YYYYQ
  for (cn in names(frb_raw)) {
    vals <- as.character(frb_raw[[cn]])[!is.na(frb_raw[[cn]])]
    if (length(vals) > 10 &&
        mean(grepl("^\\d{4}[\\._Q]\\d$", vals)) > 0.8) {
      date_col <- cn; break
    }
  }
}
if (!exists("date_col"))
  stop("Cannot identify date column. Set FRB_DATE_COL in CONFIG.")

message(sprintf("    Date column: '%s'", date_col))
frb_raw[, date := frb_to_yearqtr(get(date_col))]
frb_raw[, (date_col) := NULL]
frb_raw <- frb_raw[!is.na(date)]
setorderv(frb_raw, "date")

dr    <- range(frb_raw$date, na.rm = TRUE)
n_h   <- sum(frb_raw$date <= HIST_END, na.rm = TRUE)
n_p   <- sum(frb_raw$date >  HIST_END, na.rm = TRUE)
message(sprintf("    Range: %s -> %s  (%d rows)",
                as.character(dr[1]), as.character(dr[2]), nrow(frb_raw)))
message(sprintf("    Historical: %d  |  Projection: %d", n_h, n_p))
message(sprintf("    Variables before renaming: %d", ncol(frb_raw) - 1L))

# ---- Rename via VAR_MAP ------------------------------------
frb_cols  <- setdiff(names(frb_raw), "date")
n_renamed <- 0L
for (old_nm in intersect(frb_cols, names(VAR_MAP))) {
  new_nm <- VAR_MAP[[old_nm]]
  if (new_nm %in% names(frb_raw) && new_nm != old_nm) {
    message(sprintf("    SKIP rename '%s' -> '%s': target exists", old_nm, new_nm))
    next
  }
  setnames(frb_raw, old_nm, new_nm)
  n_renamed <- n_renamed + 1L
}
message(sprintf("    Renamed %d variables", n_renamed))

# ---- Coerce all value columns to numeric -------------------
val_cols <- setdiff(names(frb_raw), "date")
frb_raw[, (val_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
         .SDcols = val_cols]

frb_raw[, is_forecast := date > HIST_END]
saveRDS(frb_raw, "macro_raw_v4.rds")
message("    Saved: macro_raw_v4.rds")
toc()

# ============================================================
# 2. DERIVED / COMPOSITE SERIES
#    Must produce the exact names Part 3 FEAT_PAT searches for:
#    fedfunds, gs10, yield_curve, baa_spread, hy_spread,
#    unrate, cpi_yoy, real_rate, spread_2s10s,
#    credit_tightness, fedfunds_cycle, fomc_regime, hike_run,
#    oil_qoq_pct, oil_shock, mortgage30
# ============================================================
tic("2. Derived series")
message("\n[2] Derived / composite series...")

m <- copy(frb_raw)
setorderv(m, "date")

# -- Yield curve: 10Y Treasury minus 3M Treasury proxy ------
if (!"yield_curve" %in% names(m)) {
  if (all(c("gs10", "gs3m") %in% names(m))) {
    m[, yield_curve := gs10 - gs3m]
    message("    yield_curve = gs10 - gs3m")
  } else if (all(c("gs10_cmt", "gs3m") %in% names(m))) {
    m[, yield_curve := gs10_cmt - gs3m]
    message("    yield_curve = gs10_cmt - gs3m (CMT)")
  }
}

# -- 2s10s term spread (3-yr as proxy for 2-yr) --------------
if (!"spread_2s10s" %in% names(m) &&
    all(c("gs10", "gs3") %in% names(m)))
  m[, spread_2s10s := gs10 - gs3]

# -- Yield curve inversion flag + run counter ----------------
if ("yield_curve" %in% names(m)) {
  m[, yield_curve_inv := fifelse(
    !is.na(yield_curve) & yield_curve < 0, 1L, 0L)]
  m[, yield_curve_inv_run := {
    inv <- ifelse(is.na(yield_curve_inv), 0L, yield_curve_inv)
    sequence(rle(inv)$lengths)
  }]
}

# -- CPI YoY (required before real_rate) --------------------
if ("cpi" %in% names(m))
  m[, cpi_yoy := pct_chg_n(cpi, 4L)]

# -- Core CPI YoY -------------------------------------------
if ("core_cpi" %in% names(m))
  m[, core_cpi_yoy := pct_chg_n(core_cpi, 4L)]

# -- Real fed funds rate ------------------------------------
if (all(c("fedfunds", "cpi_yoy") %in% names(m)))
  m[, real_rate := fedfunds - cpi_yoy]

# -- HY spread alias (Part 3 expects 'hy_spread') -----------
# baa_spread (SRCB) is the best available proxy
if (!"hy_spread" %in% names(m) && "baa_spread" %in% names(m)) {
  m[, hy_spread := baa_spread]
  message("    hy_spread <- baa_spread (proxy; FRB has no true HY OAS)")
}

# -- Credit tightness: avg z-score of spread + delinquency --
ct_inputs <- intersect(c("baa_spread", "bbb10_spread",
                           "cc_delinq",  "ci_loan_delinq"),
                        names(m))
if (length(ct_inputs) >= 2) {
  m[, credit_tightness :=
      Reduce(`+`, lapply(.SD, std_z)) / length(ct_inputs),
     .SDcols = ct_inputs]
  message(sprintf("    credit_tightness (avg z-score of: %s)",
                  paste(ct_inputs, collapse = ", ")))
}

# -- Fed funds cycle (deviation from trailing 8q mean) ------
if ("fedfunds" %in% names(m)) {
  m[, fedfunds_trail8 := rollmean_safe(fedfunds, 8L)]
  m[, fedfunds_cycle  := fedfunds - fedfunds_trail8]
}

# -- FOMC regime + hike run ---------------------------------
if ("fedfunds" %in% names(m)) {
  m[, fedfunds_chg := fedfunds - shift(fedfunds, 1L, type = "lag")]
  m[, fomc_regime  := fcase(
    fedfunds_chg >  0.10,  1L,
    fedfunds_chg < -0.10, -1L,
    default = 0L)]
  m[, hike_run := {
    reg    <- ifelse(is.na(fomc_regime), 0L, fomc_regime)
    r      <- rle(reg)
    sequence(r$lengths) * rep(sign(r$values), r$lengths)
  }]
}

# -- Oil shock (Brent crude QoQ) ----------------------------
oil_src <- intersect(c("oil_brent", "oil_wti"), names(m))[1]
if (!is.na(oil_src)) {
  if (!"oil_wti" %in% names(m)) m[, oil_wti := get(oil_src)]
  m[, oil_qoq_pct := pct_chg_n(oil_wti, 1L)]
  m[, oil_shock   := fifelse(
    !is.na(oil_qoq_pct) & abs(oil_qoq_pct) > 20, 1L, 0L)]
}

# -- M2 growth proxy (disp_income if no M2 in FRB file) -----
if (!"m2" %in% names(m)) {
  if ("total_credit" %in% names(m)) {
    m[, m2 := total_credit]           # rough proxy
    message("    m2 <- total_credit (proxy; M2 not in FRB file)")
  }
}
if ("m2" %in% names(m)) m[, m2_yoy := pct_chg_n(m2, 4L)]

# -- Bank deposit growth proxy ------------------------------
if (!"bank_deps" %in% names(m) && "disp_income" %in% names(m))
  m[, bank_deps := disp_income]
if ("bank_deps" %in% names(m))
  m[, bank_deps_yoy := pct_chg_n(bank_deps, 4L)]

# -- Real payrolls proxy ------------------------------------
if (!"payems" %in% names(m) && "labor_income" %in% names(m))
  m[, payems := labor_income]
if (all(c("payems", "cpi") %in% names(m)))
  m[, real_payems := payems / cpi * 100]

# -- Mortgage affordability composite -----------------------
if (all(c("mortgage30", "fhfa_hpi", "disp_income") %in% names(m)))
  m[, mortgage_afford := mortgage30 * fhfa_hpi / pmax(disp_income / 4, 1)]

message(sprintf("    Derived complete: %d rows x %d cols", nrow(m), ncol(m)))
toc()

# ============================================================
# 3. FEATURE ENGINEERING  (mirrors Part 1 v3 panel FE)
# ============================================================
tic("3. Feature engineering")
message("\n[3] Feature engineering (YoY, QoQ, rolling, lags, accel)...")

# Columns with bespoke derivation -- exclude from generic FE
already_derived <- c(
  "date", "is_forecast",
  "cpi_yoy", "core_cpi_yoy", "m2_yoy", "bank_deps_yoy",
  "real_rate", "fedfunds_cycle", "fedfunds_trail8",
  "credit_tightness", "oil_qoq_pct", "oil_shock",
  "spread_2s10s", "yield_2_10",
  "yield_curve_inv", "yield_curve_inv_run",
  "real_payems", "fomc_regime", "fedfunds_chg", "hike_run"
)

all_num   <- names(m)[vapply(m, is.numeric, logical(1))]
base_fe   <- setdiff(all_num, already_derived)

# 3a: YoY % change
message("    3a. YoY...")
m[, paste0("yoy_", base_fe) :=
    lapply(.SD, pct_chg_n, n = 4L), .SDcols = base_fe]

# 3b: QoQ % change
message("    3b. QoQ...")
m[, paste0("qoq_", base_fe) :=
    lapply(.SD, pct_chg_n, n = 1L), .SDcols = base_fe]

# Key series for rolling stats + lags (expanded vs v3)
key_macro <- intersect(
  c("fedfunds", "fedfunds_eff", "gs10", "gs3", "gs3m", "gs1",
    "gs10_cmt", "mortgage30", "mortgage15", "prime_rate",
    "yield_curve", "spread_2s10s",
    "baa_spread", "bbb10_spread", "hy_spread",
    "unrate", "nairu", "lfpr", "initial_claims",
    "gdp_real", "indpro", "pce_total", "consumption",
    "cpi", "core_cpi", "ppi", "oil_brent", "oil_wti",
    "housing_permits", "housing_expect", "fhfa_hpi", "cs20_hpi",
    "sp500_mktcap", "djia", "nasdaq",
    "disp_income", "savings_rate",
    "cc_delinq", "cc_chargeoff", "ci_loan_delinq",
    "cons_confidence", "consumer_bankrupt", "corp_leverage",
    "revolving_credit", "total_credit", "mortgage_debt",
    "real_rate", "fedfunds_cycle", "credit_tightness"),
  names(m))

message(sprintf("    Key series for rolling/lags: %d", length(key_macro)))

# 3c: Rolling means 4q + 8q
message("    3c. Rolling means (4q / 8q)...")
for (k in c(4L, 8L)) {
  nms <- paste0(key_macro, "_rmean", k)
  m[, (nms) := lapply(.SD, rollmean_safe, k = k), .SDcols = key_macro]
}

# 3d: Rolling SD 4q
message("    3d. Rolling SDs (4q)...")
m[, paste0(key_macro, "_rsd4") :=
    lapply(.SD, rollsd_safe, k = 4L), .SDcols = key_macro]

# 3e: Cyclical deviation from 8q mean
message("    3e. Cyclical deviations...")
for (v in key_macro) {
  rc <- paste0(v, "_rmean8")
  if (rc %in% names(m))
    m[, (paste0(v, "_cyc")) := get(v) - get(rc)]
}

# 3f: YoY acceleration (delta of YoY, lag-4)
message("    3f. YoY acceleration...")
yoy_vars <- intersect(paste0("yoy_", key_macro), names(m))
m[, paste0(yoy_vars, "_accel") :=
    lapply(.SD, function(x) x - shift(x, 4L, type = "lag")),
   .SDcols = yoy_vars]

# 3g: Lagged levels: 1q, 2q, 4q
message("    3g. Lagged levels (1q / 2q / 4q)...")
for (lag_n in c(1L, 2L, 4L)) {
  nms <- paste0(key_macro, "_lag", lag_n)
  m[, (nms) := lapply(.SD, shift, n = lag_n, type = "lag"),
     .SDcols = key_macro]
}

# 3h: Interaction terms
message("    3h. Interactions...")
if (all(c("fedfunds", "yield_curve")      %in% names(m)))
  m[, rate_x_slope    := fedfunds * yield_curve]
if (all(c("fedfunds", "hy_spread")        %in% names(m)))
  m[, rate_x_hy       := fedfunds * hy_spread]
if (all(c("fedfunds", "credit_tightness") %in% names(m)))
  m[, rate_x_credit   := fedfunds * credit_tightness]
if (all(c("unrate", "real_rate")          %in% names(m)))
  m[, unemp_x_real_rate := unrate * real_rate]
if (all(c("spread_2s10s", "baa_spread")   %in% names(m)))
  m[, slope_x_credit  := spread_2s10s * baa_spread]

message(sprintf("    FE complete: %d rows x %d cols", nrow(m), ncol(m)))
toc()

# ============================================================
# 4. QC
# ============================================================
tic("4. QC")
message("\n[4] Quality control...")

all_na_cols <- names(m)[vapply(m, function(x) all(is.na(x)), logical(1))]
if (length(all_na_cols) > 0) {
  m[, (all_na_cols) := NULL]
  message(sprintf("    Dropped %d all-NA cols", length(all_na_cols)))
}

hist_rows <- m[is_forecast == FALSE]
num_nms   <- names(hist_rows)[vapply(hist_rows, is.numeric, logical(1))]
na_pcts   <- vapply(num_nms,
                    function(v) mean(is.na(hist_rows[[v]])) * 100, numeric(1))
hi_na     <- sort(na_pcts[na_pcts > 40], decreasing = TRUE)
if (length(hi_na) > 0) {
  message(sprintf("    Cols >40%% NA in historical rows (%d total):",
                  length(hi_na)))
  print(head(round(hi_na, 1), 20))
} else {
  message("    All historical cols under 40% NA threshold")
}

# Verify critical Part-3 columns are present
part3_required <- c("fedfunds", "gs10", "unrate", "cpi_yoy",
                     "real_rate", "yield_curve", "mortgage30")
missing_p3 <- setdiff(part3_required, names(m))
if (length(missing_p3) > 0)
  message(sprintf("    WARNING: missing Part-3 required cols: %s",
                  paste(missing_p3, collapse = ", ")))
else
  message("    All critical Part-3 columns present")

message(sprintf("    Final macro table: %d rows x %d cols", nrow(m), ncol(m)))
toc()

# ============================================================
# 5. SAVE OUTPUTS
# ============================================================
tic("5. Save")
message("\n[5] Saving outputs...")

saveRDS(m, "macro_features_v4.rds")
message(sprintf("    macro_features_v4.rds  (%d x %d)", nrow(m), ncol(m)))

key_export <- intersect(
  c("date", key_macro,
    "cpi_yoy", "core_cpi_yoy", "real_rate", "fedfunds_cycle",
    "credit_tightness", "yield_curve_inv", "fomc_regime", "hike_run",
    "oil_qoq_pct", "oil_shock", "spread_2s10s"),
  names(m))
fwrite(m[is_forecast == TRUE, key_export, with = FALSE],
       "macro_forecast_v4.csv")
message(sprintf("    macro_forecast_v4.csv  (%d projection rows)",
                sum(m$is_forecast)))
toc()

# ============================================================
# 6. MERGE ONTO QUARTERLY CU PANELS
#    Drop-in replacement for macro_v3_arima.R merge step.
#    Overwrites qtrly_enriched_v3.rds and qtrly_full_v3.rds.
# ============================================================
tic("6. Merge")
message("\n[6] Merging macro onto CU quarterly panels...")

macro_merge_cols <- setdiff(names(m), c("date", "is_forecast"))

merge_macro_v4 <- function(panel_path, out_path, use_forecast) {
  if (!file.exists(panel_path)) {
    message(sprintf("    SKIP: %s not found", panel_path))
    return(invisible(NULL))
  }
  panel <- readRDS(panel_path)
  setDT(panel)
  message(sprintf("    %s: %d x %d (before)",
                  basename(panel_path), nrow(panel), ncol(panel)))

  macro_src <- if (use_forecast) m else m[is_forecast == FALSE]

  # Remove stale macro cols
  stale <- intersect(macro_merge_cols, names(panel))
  if (length(stale) > 0) panel[, (stale) := NULL]

  idx     <- match(panel$date, macro_src$date)
  n_match <- sum(!is.na(idx))
  message(sprintf("    Date matches: %d / %d (%.0f%%)",
                  n_match, nrow(panel),
                  n_match / nrow(panel) * 100))

  if (n_match == 0) {
    message("    ERROR: 0 matches. Check that panel$date is yearqtr.")
    return(invisible(NULL))
  }

  for (col in macro_merge_cols)
    panel[, (col) := macro_src[[col]][idx]]

  saveRDS(panel, out_path)
  message(sprintf("    Saved: %s  (%d x %d)",
                  basename(out_path), nrow(panel), ncol(panel)))
  invisible(panel)
}

merge_macro_v4("qtrly_enriched_v3.rds", "qtrly_enriched_v3.rds",
               use_forecast = FALSE)   # historical macro only
merge_macro_v4("qtrly_full_v3.rds",     "qtrly_full_v3.rds",
               use_forecast = TRUE)    # all rows incl. projections
toc()

# ============================================================
# 7. DIAGNOSTIC PLOTS  (M01-M12)
# ============================================================
tic("7. Plots")
message("\n[7] Diagnostic plots...")

fc_start_d <- as.Date(FORECAST_START)
PLOT_FROM  <- 1990   # start year for time-series charts

plot_ts <- function(vname, y_label, log_sc = FALSE, stem = NULL) {
  if (!vname %in% names(m)) {
    message(sprintf("    SKIP: %s not in table", vname))
    return(invisible(NULL))
  }
  dt <- m[!is.na(get(vname)) & year(as.Date(date)) >= PLOT_FROM,
           .(date = as.Date(date), y = get(vname), is_forecast)]
  if (nrow(dt) == 0) return(invisible(NULL))

  p <- ggplot(dt, aes(x = date, y = y)) +
    annotate("rect",
             xmin = fc_start_d,
             xmax = as.Date(FORECAST_END) + 100,
             ymin = -Inf, ymax = Inf,
             fill = "#fffde7", alpha = 0.5) +
    geom_vline(xintercept = fc_start_d,
               linetype = "dotted", colour = "grey50", linewidth = 0.5) +
    geom_line(data = dt[is_forecast == FALSE],
              colour = "#1f77b4", linewidth = 0.9) +
    geom_line(data = dt[is_forecast == TRUE],
              colour = "#d62728", linewidth = 0.85, linetype = "dashed") +
    {if (log_sc) scale_y_log10(labels = comma) else
        scale_y_continuous(labels = comma)} +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    labs(title    = sprintf("%s — FRB Baseline 2026", y_label),
         subtitle = "Blue = historical  |  Red dashed = FRB projection",
         x = NULL, y = y_label) +
    theme_v4()
  if (!is.null(stem)) save_plot(p, stem, w = 12, h = 5)
  invisible(p)
}

# Individual key series
plot_ts("fedfunds",    "Fed Funds Rate (%)",        stem = "M01_fedfunds")
plot_ts("unrate",      "Unemployment Rate (%)",     stem = "M02_unrate")
plot_ts("cpi_yoy",     "CPI YoY % Change",          stem = "M03_cpi_yoy")
plot_ts("gdp_real",    "Real GDP", log_sc = TRUE,   stem = "M04_gdp_real")
plot_ts("yield_curve", "Yield Curve (10Y-3M, %)",   stem = "M05_yield_curve")
plot_ts("mortgage30",  "30-yr Mortgage Rate (%)",   stem = "M06_mortgage30")

# Panel helper
facet_panel <- function(vars, title_txt, stem, ncol = 3, w = 14, h = 7) {
  vars <- intersect(vars, names(m))
  if (length(vars) < 2) return(invisible(NULL))
  dt <- melt(
    m[year(as.Date(date)) >= PLOT_FROM,
       c("date", "is_forecast", vars), with = FALSE],
    id.vars = c("date", "is_forecast"),
    variable.name = "series", value.name = "value")
  dt[, date := as.Date(date)]

  p <- ggplot(dt[!is.na(value)],
              aes(x = date, y = value, colour = is_forecast)) +
    annotate("rect",
             xmin = fc_start_d, xmax = as.Date(FORECAST_END) + 100,
             ymin = -Inf, ymax = Inf, fill = "#fffde7", alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey65",
               linewidth = 0.3) +
    geom_vline(xintercept = fc_start_d,
               linetype = "dotted", colour = "grey50") +
    geom_line(linewidth = 0.7) +
    facet_wrap(~ series, scales = "free_y", ncol = ncol) +
    scale_colour_manual(
      values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"),
      labels = c("Historical", "FRB Projection"), name = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(title = title_txt,
         subtitle = "Yellow region = forecast period (2025Q4-2030Q4)",
         x = NULL, y = NULL) +
    theme_v4()
  save_plot(p, stem, w = w, h = h)
  invisible(p)
}

facet_panel(
  c("fedfunds", "fedfunds_eff", "yield_curve", "spread_2s10s",
    "real_rate", "fedfunds_cycle"),
  "Monetary Policy & Yield Curve — FRB Baseline 2026",
  "M07_rates_panel", ncol = 3, h = 8)

facet_panel(
  c("unrate", "nairu", "lfpr", "initial_claims", "unemp_duration"),
  "Labour Market — FRB Baseline 2026",
  "M08_labour_panel", ncol = 3, h = 7)

facet_panel(
  c("baa_spread", "bbb10_spread", "credit_tightness",
    "cc_delinq", "cc_chargeoff", "ci_loan_delinq",
    "corp_leverage", "consumer_bankrupt"),
  "Credit & Financial Conditions — FRB Baseline 2026",
  "M09_credit_panel", ncol = 3, h = 9)

facet_panel(
  c("fhfa_hpi", "cs20_hpi", "housing_permits", "housing_expect",
    "mortgage30", "mtg_spread", "foreclosure_inv", "cre_price"),
  "Housing Market — FRB Baseline 2026",
  "M10_housing_panel", ncol = 3, h = 9)

facet_panel(
  c("real_rate", "credit_tightness", "fedfunds_cycle",
    "hike_run", "yield_curve_inv_run", "oil_shock"),
  "Derived / Composite Macro Indicators — FRB Baseline 2026",
  "M11_composite_panel", ncol = 3, h = 8)

# M12: Variable coverage heatmap
message("    M12: Coverage heatmap...")
m_cov <- copy(m[year(as.Date(date)) >= 1975])
m_cov[, decade := paste0(floor(year(as.Date(date)) / 10) * 10, "s")]
cov_vars <- intersect(
  c(key_macro, "cpi_yoy", "real_rate", "spread_2s10s",
    "credit_tightness", "fedfunds_cycle", "hike_run", "oil_shock"),
  names(m_cov))

if (length(cov_vars) >= 5) {
  cov_dt <- m_cov[, lapply(.SD, function(x) mean(!is.na(x)) * 100),
                   by = decade, .SDcols = cov_vars]
  cov_m  <- melt(cov_dt, id.vars = "decade",
                 variable.name = "variable", value.name = "pct")

  p_cov <- ggplot(cov_m, aes(x = decade, y = variable, fill = pct)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.0f%%", pct)),
              size = 2.5, colour = "grey15") +
    scale_fill_gradient(low = "#fee0d2", high = "#2ca25f",
                        limits = c(0, 100), name = "% Available") +
    labs(title    = "Macro Variable Coverage by Decade — FRB Baseline 2026",
         subtitle = "Green = fully available  |  Red = missing / not in FRB file",
         x = NULL, y = NULL) +
    theme_v4() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 8))
  save_plot(p_cov, "M12_coverage_heatmap", w = 13, h = 11)
}

n_plots <- length(list.files(PLOT_DIR, pattern = "^M[0-9].*\\.pdf$"))
message(sprintf("    %d PDF plots -> %s/", n_plots, PLOT_DIR))
toc()

# ============================================================
# 8. FINAL SUMMARY
# ============================================================
tot <- as.numeric((proc.time() - t0)["elapsed"])
toc()

message("\n=======================================================")
message(sprintf("MACRO v4.0 COMPLETE  %dh %02dm %02ds",
                floor(tot / 3600),
                floor((tot %% 3600) / 60),
                round(tot %% 60)))
message("Source         : FRB Baseline 2026 (no ARIMA, no FRED API)")
message(sprintf("Variables      : %d raw -> %d after FE",
                ncol(frb_raw) - 2L, ncol(m) - 2L))
message(sprintf("Date range     : %s -> %s  (%d quarters)",
                as.character(min(m$date)),
                as.character(max(m$date)),
                nrow(m)))
message(sprintf("Historical     : %d rows  |  Projection: %d rows",
                sum(!m$is_forecast), sum(m$is_forecast)))
message("Outputs:")
message("  macro_raw_v4.rds           raw FRB table")
message(sprintf("  macro_features_v4.rds      %d cols (hist + projection)",
                ncol(m)))
message("  macro_forecast_v4.csv      projection rows, key series")
message("  qtrly_enriched_v3.rds      overwritten (historical macro only)")
message("  qtrly_full_v3.rds          overwritten (all macro incl. projection)")
message(sprintf("  %d PDF plots -> %s/", n_plots, PLOT_DIR))
message("=======================================================")

message("\n-- Recent & near-term macro values --")
diag_cols <- intersect(
  c("date", "fedfunds", "unrate", "cpi_yoy", "real_rate",
    "yield_curve", "credit_tightness", "fomc_regime"),
  names(m))
print(m[date %in% c(tail(m[is_forecast == FALSE]$date, 4L),
                     head(m[is_forecast == TRUE]$date,  4L)),
         diag_cols, with = FALSE])

message("\n-- Next step: run part3_regression_pipeline.R --")
message("   (Part 3 reads qtrly_enriched_v3.rds unchanged)")

############################################################
# END
############################################################
