############################################################
# 2_Macrovariables_Feature_Engineering_v2.R
#
# Purpose: Pull macroeconomic data from FRED, engineer
#          features, and merge onto qtrly_enriched.rds
#          (produced by part1_data_prep_v2.R)
#
# Output: qtrly_enriched.rds  (overwritten with macro cols)
#         macro_raw.rds        (raw FRED series, useful for EDA)
#
# Merge key: date (yearqtr)
#   - Macro data is at quarterly frequency (or converted to it)
#   - Every (date × categories) row in qtrly gets the SAME
#     macro value for that quarter — no category subscript
#
# FRED series pulled:
#   Rates     : FEDFUNDS, GS2, GS10, MORTGAGE30US, BAA10Y
#   Spreads   : T10Y3M (yield curve), BAMLH0A0HYM2 (HY OAS)
#   Labour    : UNRATE, PAYEMS, ICSA
#   Activity  : GDPC1 (real GDP), INDPRO, HOUST
#   Prices    : CPIAUCSL, PCEPI, DCOILWTICO
#   Credit    : BUSLOANS, DPSACBW027SBOG (bank deposits),
#               DRBLACBS (CU loan delinquency - optional)
#   Sentiment : UMCSENT (U of Michigan)
#   Money     : M2SL, BOGMBASE
#
# Feature engineering per series:
#   1. Level (macro variables that ARE stationary: spreads, rates)
#   2. YoY % change  (lag-4)
#   3. QoQ % change  (lag-1)
#   4. Rolling mean 4q / 8q
#   5. Rolling SD 4q (volatility)
#   6. Cyclical deviation from 8q mean
#   7. Regime-relative deviation (e.g. value vs ZIRP avg)
#   8. Composite indices: yield curve cycle, credit tightness
#
# Runtime: ~3-5 min (FRED API calls + feature calc)
############################################################

library(data.table);  library(zoo);       library(httr)
library(lubridate);   library(tictoc);    library(fredr)
options(scipen = 999)

# ── ntfy ──────────────────────────────────────────────────
NTFY_TOPIC   <- "your-unique-topic-name"   # ← CHANGE THIS
NTFY_ENABLED <- TRUE
notify <- function(title, msg, tags = NULL) {
  if (!NTFY_ENABLED) return(invisible(NULL))
  tryCatch({
    h <- list(Title = title)
    if (!is.null(tags)) h$Tags <- paste(tags, collapse = ",")
    httr::POST(paste0("https://ntfy.sh/", NTFY_TOPIC),
               body = msg, encode = "raw",
               do.call(httr::add_headers, h))
  }, error = function(e) NULL)
}

t0 <- proc.time()
tic("Macro v2 total")
message("=======================================================")
message(sprintf("MACRO v2.0  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("=======================================================")
notify("Macro v2.0 Started", format(Sys.time(), "%H:%M"), tags = "rocket")

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

# ════════════════════════════════════════════════════════
# CONFIG
# ════════════════════════════════════════════════════════

# FRED API key — get a free key at https://fred.stlouisfed.org/
FRED_API_KEY <- Sys.getenv("FRED_API_KEY")   # store in .Renviron
if (nchar(FRED_API_KEY) == 0) {
  # fallback: hardcode here (not recommended for shared scripts)
  FRED_API_KEY <- "YOUR_FRED_API_KEY_HERE"   # ← CHANGE THIS
  message("  WARNING: FRED_API_KEY not found in environment")
}
fredr_set_key(FRED_API_KEY)

# Date range: pull from 1995 (for long lags) through latest available
FRED_START <- as.Date("1995-01-01")
FRED_END   <- Sys.Date()

# FRED series to pull: named list(fred_id = "short_name")
FRED_SERIES <- list(
  # Interest rates & monetary
  fedfunds    = "FEDFUNDS",        # Effective Fed Funds Rate (monthly → qtrly)
  gs2         = "GS2",             # 2yr Treasury yield
  gs10        = "GS10",            # 10yr Treasury yield
  mortgage30  = "MORTGAGE30US",    # 30yr fixed mortgage rate (weekly → qtrly)
  baa_spread  = "BAA10Y",          # Baa-10yr spread (credit spread proxy)

  # Yield curve
  yield_curve = "T10Y3M",          # 10yr minus 3mo (recession signal)
  yield_2_10  = "T10Y2Y",          # 10yr minus 2yr (common curve)

  # High yield / credit risk
  hy_spread   = "BAMLH0A0HYM2",    # ICE BofA HY OAS spread

  # Labour market
  unrate      = "UNRATE",          # Unemployment rate (%)
  payems      = "PAYEMS",          # Nonfarm payrolls (thousands)
  icsa        = "ICSA",            # Initial jobless claims (weekly → qtrly)

  # Real activity
  gdp_real    = "GDPC1",           # Real GDP (quarterly, seasonally adjusted)
  indpro      = "INDPRO",          # Industrial production index
  housing     = "HOUST",           # Housing starts (thousands, annualised)

  # Prices
  cpi         = "CPIAUCSL",        # CPI all items
  pce         = "PCEPI",           # PCE price index
  oil_wti     = "DCOILWTICO",      # WTI crude oil (daily → qtrly)

  # Credit & banking
  bus_loans   = "BUSLOANS",        # Commercial & industrial loans
  bank_deps   = "DPSACBW027SBOG",  # Deposits at commercial banks
  cons_credit = "TOTALSL",         # Consumer credit outstanding

  # Sentiment & expectations
  umcsent     = "UMCSENT",         # U of Michigan Consumer Sentiment
  inf_exp     = "MICH",            # U of Michigan inflation expectations (1yr)

  # Money supply
  m2          = "M2SL",            # M2 money supply

  # CU-specific from FRED (if available)
  # cu_loans  = "DRBLACBS"         # CU loan delinquency rate (uncomment if needed)
)

# ════════════════════════════════════════════════════════
# 1. PULL FRED DATA
# ════════════════════════════════════════════════════════
tic("1. Pull FRED data")
message("\n[1] Pulling FRED data...")

pull_fred <- function(series_id, short_name, start = FRED_START, end = FRED_END) {
  tryCatch({
    df <- fredr::fredr(
      series_id         = series_id,
      observation_start = start,
      observation_end   = end,
      frequency         = "q",          # quarterly aggregation by FRED
      aggregation_method = "avg"        # average within quarter
    )
    dt <- as.data.table(df)[, .(date = zoo::as.yearqtr(date),
                                  value = value)]
    setnames(dt, "value", short_name)
    dt
  }, error = function(e) {
    message(sprintf("    WARNING: Failed to pull %s (%s): %s",
                    short_name, series_id, e$message))
    NULL
  })
}

macro_list <- list()
for (nm in names(FRED_SERIES)) {
  message(sprintf("    Pulling %-15s (%s)...", nm, FRED_SERIES[[nm]]))
  macro_list[[nm]] <- pull_fred(FRED_SERIES[[nm]], nm)
}

# Remove NULL (failed pulls)
macro_list <- macro_list[!vapply(macro_list, is.null, logical(1))]
message(sprintf("    Successfully pulled %d / %d series",
                length(macro_list), length(FRED_SERIES)))

# Merge all series into wide table on date
macro_wide <- Reduce(
  function(a, b) merge(a, b, by = "date", all = TRUE),
  macro_list
)
setorderv(macro_wide, "date")
message(sprintf("    Raw macro: %d rows x %d cols  (%s to %s)",
                nrow(macro_wide), ncol(macro_wide),
                as.character(min(macro_wide$date, na.rm=TRUE)),
                as.character(max(macro_wide$date, na.rm=TRUE))))

# Save raw for EDA
saveRDS(macro_wide, "macro_raw.rds")
toc()
notify("FRED Data Pulled",
       sprintf("%d series, %d quarters",
               ncol(macro_wide) - 1L, nrow(macro_wide)),
       tags = "white_check_mark")

# ════════════════════════════════════════════════════════
# 2. COMPUTED / DERIVED MACRO SERIES
# ════════════════════════════════════════════════════════
tic("2. Derived macro series")
message("\n[2] Computing derived series...")

# Fed funds cycle: deviation from trailing 8q average
# (captures whether rates are rising/falling relative to recent norm)
if ("fedfunds" %in% names(macro_wide)) {
  macro_wide[, fedfunds_trail8 := zoo::rollapply(
    fedfunds, width=8, FUN=mean, na.rm=TRUE,
    fill=NA, align="right")]
  macro_wide[, fedfunds_cycle := fedfunds - fedfunds_trail8]
}

# Yield curve inversion flag (1 if T10Y3M < 0)
if ("yield_curve" %in% names(macro_wide)) {
  macro_wide[, yield_curve_inv := fifelse(yield_curve < 0, 1L, 0L)]
  # Quarters since last inversion (or de-inversion)
  macro_wide[, yield_curve_inv_run := {
    r <- rle(yield_curve_inv)
    rep(sequence(r$lengths), r$lengths)
  }]
}

# Real rate (Fed Funds minus CPI YoY %)
if (all(c("fedfunds","cpi") %in% names(macro_wide))) {
  macro_wide[, cpi_yoy := (cpi / shift(cpi, 4L) - 1) * 100]
  macro_wide[, real_rate := fedfunds - cpi_yoy]
}

# Term spread (2s10s)
if (all(c("gs2","gs10") %in% names(macro_wide))) {
  macro_wide[, spread_2s10s := gs10 - gs2]
}

# Credit tightness composite: standardise HY spread + BAA spread + bank deposit growth
# (higher = tighter / riskier credit conditions)
if (all(c("hy_spread","baa_spread") %in% names(macro_wide))) {
  std_z <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  macro_wide[, credit_tightness := std_z(hy_spread) + std_z(baa_spread)]
}

# Real payroll growth (payroll deflated by CPI, then YoY %)
if (all(c("payems","cpi") %in% names(macro_wide))) {
  macro_wide[, real_payems := payems / cpi * 100]
}

# Oil price shock indicator: abs(QoQ % change > 20%)
if ("oil_wti" %in% names(macro_wide)) {
  macro_wide[, oil_qoq_pct := (oil_wti / shift(oil_wti, 1L) - 1L) * 100]
  macro_wide[, oil_shock := fifelse(abs(oil_qoq_pct) > 20, 1L, 0L)]
}

# M2 growth rate (YoY %)
if ("m2" %in% names(macro_wide)) {
  macro_wide[, m2_yoy := (m2 / shift(m2, 4L) - 1L) * 100]
}

# Bank deposit growth vs CU context
if ("bank_deps" %in% names(macro_wide)) {
  macro_wide[, bank_deps_yoy := (bank_deps / shift(bank_deps, 4L) - 1L) * 100]
}

message(sprintf("    Derived cols added. Total macro cols: %d",
                ncol(macro_wide) - 1L))
toc()

# ════════════════════════════════════════════════════════
# 3. FEATURE ENGINEERING ON MACRO SERIES
# ════════════════════════════════════════════════════════
tic("3. Macro feature engineering")
message("\n[3] Engineering macro features...")

# Columns to transform (all numeric except date)
macro_fe_cols <- setdiff(names(macro_wide)[
  vapply(macro_wide, is.numeric, logical(1))], "date")

# Remove already-derived YoY/QoQ cols from FE to avoid double-transform
already_derived <- c("cpi_yoy","m2_yoy","bank_deps_yoy",
                     "real_rate","fedfunds_cycle","credit_tightness",
                     "oil_qoq_pct","oil_shock","spread_2s10s",
                     "yield_curve_inv","yield_curve_inv_run",
                     "real_payems","fedfunds_trail8")
base_fe <- setdiff(macro_fe_cols, already_derived)

pct_chg <- function(x, lag_x) {
  fifelse(!is.na(lag_x) & lag_x != 0 & abs(lag_x) > 1e-6,
          (x - lag_x) / abs(lag_x) * 100, NA_real_)
}

# ── 3a: YoY % change (lag-4) ─────────────────────────────
message("    3a. YoY % change (lag-4)...")
macro_wide[, paste0("yoy_", base_fe) :=
             lapply(.SD, function(x) pct_chg(x, shift(x, 4L))),
           .SDcols = base_fe]

# ── 3b: QoQ % change (lag-1) ─────────────────────────────
message("    3b. QoQ % change (lag-1)...")
macro_wide[, paste0("qoq_", base_fe) :=
             lapply(.SD, function(x) pct_chg(x, shift(x, 1L))),
           .SDcols = base_fe]

# ── 3c: Rolling means (4q, 8q) ───────────────────────────
message("    3c. Rolling means (4q, 8q)...")
rollmean_safe <- function(x, k)
  zoo::rollapply(x, width=k, FUN=mean, na.rm=TRUE,
                 fill=NA, align="right", partial=FALSE)

# Apply to both levels and YoY cols of key series
key_macro <- intersect(
  c("fedfunds","gs10","gs2","yield_curve","hy_spread","baa_spread",
    "unrate","payems","gdp_real","cpi","housing","umcsent",
    "real_rate","fedfunds_cycle","credit_tightness"),
  names(macro_wide)
)
for (k in c(4L, 8L)) {
  new_nms <- paste0(key_macro, "_rmean", k)
  macro_wide[, (new_nms) := lapply(.SD, rollmean_safe, k=k),
             .SDcols = key_macro]
}

# ── 3d: Rolling SDs (4q) — volatility ────────────────────
message("    3d. Rolling SDs (4q)...")
rollsd_safe <- function(x, k)
  zoo::rollapply(x, width=k, FUN=sd, na.rm=TRUE,
                 fill=NA, align="right", partial=FALSE)

for (k in c(4L)) {
  new_nms <- paste0(key_macro, "_rsd", k)
  macro_wide[, (new_nms) := lapply(.SD, rollsd_safe, k=k),
             .SDcols = key_macro]
}

# ── 3e: Cyclical deviation from 8q mean ─────────────────
message("    3e. Cyclical deviations...")
for (v in key_macro) {
  rmean_col <- paste0(v, "_rmean8")
  cyc_col   <- paste0(v, "_cyc")
  if (rmean_col %in% names(macro_wide))
    macro_wide[, (cyc_col) := get(v) - get(rmean_col)]
}

# ── 3f: YoY acceleration (momentum) ─────────────────────
message("    3f. YoY acceleration...")
yoy_key <- paste0("yoy_", key_macro)
yoy_key <- intersect(yoy_key, names(macro_wide))
new_nms <- paste0(yoy_key, "_accel")
macro_wide[, (new_nms) := lapply(.SD, function(x) x - shift(x, 4L)),
           .SDcols = yoy_key]

# ── 3g: Lag levels (1q, 2q, 4q) of key series ──────────
message("    3g. Lag levels (1q, 2q, 4q)...")
for (lag_n in c(1L, 2L, 4L)) {
  new_nms <- paste0(key_macro, "_lag", lag_n)
  macro_wide[, (new_nms) := lapply(.SD, shift, n=lag_n, type="lag"),
             .SDcols = key_macro]
}

# ── 3h: Interaction: rate × yield curve slope ────────────
message("    3h. Interaction terms...")
if (all(c("fedfunds","yield_curve") %in% names(macro_wide))) {
  macro_wide[, rate_x_slope := fedfunds * yield_curve]
  macro_wide[, rate_x_hy    := fedfunds * hy_spread]
}

# ── 3i: FOMC regime dummy (hiking vs cutting vs hold) ───
message("    3i. FOMC regime dummy...")
if ("fedfunds" %in% names(macro_wide)) {
  macro_wide[, fedfunds_chg := fedfunds - shift(fedfunds, 1L)]
  macro_wide[, fomc_regime := fcase(
    fedfunds_chg >  0.10, 1L,   # hiking
    fedfunds_chg < -0.10, -1L,  # cutting
    default = 0L                 # on hold
  )]
  # Consecutive hikes/cuts
  macro_wide[, hike_run := {
    r <- rle(fomc_regime)
    rep(sequence(r$lengths) * sign(r$values), r$lengths)
  }]
}

message(sprintf("    Macro feature table: %d rows x %d cols",
                nrow(macro_wide), ncol(macro_wide)))
toc()

# ════════════════════════════════════════════════════════
# 4. QUALITY CHECKS
# ════════════════════════════════════════════════════════
tic("4. QC")
message("\n[4] Quality checks on macro data...")

# Drop all-NA columns
all_na <- names(macro_wide)[
  vapply(macro_wide, function(x) all(is.na(x)), logical(1))]
if (length(all_na) > 0) {
  macro_wide[, (all_na) := NULL]
  message(sprintf("    Dropped %d all-NA columns: %s",
                  length(all_na),
                  paste(head(all_na, 5), collapse=", ")))
}

# Report coverage
n_missing <- vapply(names(macro_wide), function(v) {
  sum(is.na(macro_wide[[v]]))
}, integer(1))
high_miss <- names(n_missing)[n_missing > nrow(macro_wide) * 0.3]
if (length(high_miss) > 0)
  message(sprintf("    High-NA cols (>30%%): %s",
                  paste(high_miss, collapse=", ")))

message(sprintf("    Final macro table: %d rows x %d cols",
                nrow(macro_wide), ncol(macro_wide)))
message(sprintf("    Date range: %s to %s",
                as.character(min(macro_wide$date, na.rm=TRUE)),
                as.character(max(macro_wide$date, na.rm=TRUE))))
toc()

# ════════════════════════════════════════════════════════
# 5. MERGE ONTO qtrly_enriched.rds
# ════════════════════════════════════════════════════════
tic("5. Merge onto quarterly panel")
message("\n[5] Loading qtrly_enriched.rds and merging macro...")

qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)
message(sprintf("    qtrly before merge: %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))

# Macro is date-level (one row per quarter)
# qtrly is (date × categories) — each date appears 7 times
# Use match() assignment instead of merge() to avoid duplicate columns

macro_cols <- setdiff(names(macro_wide), "date")

# Remove any macro columns already in qtrly (avoid duplicates on re-run)
already_in <- intersect(macro_cols, names(qtrly))
if (length(already_in) > 0) {
  message(sprintf("    Removing %d pre-existing macro cols before re-merge...",
                  length(already_in)))
  qtrly[, (already_in) := NULL]
}

# Match on date
idx <- match(qtrly$date, macro_wide$date)
n_matched <- sum(!is.na(idx))
message(sprintf("    Date matches: %d / %d rows",
                n_matched, nrow(qtrly)))

if (n_matched == 0) {
  stop("No date matches between qtrly and macro_wide. Check date format.")
}

# Assign columns directly (no merge → no duplicate column risk)
for (col in macro_cols) {
  qtrly[, (col) := macro_wide[[col]][idx]]
}

message(sprintf("    qtrly after merge: %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))

# Verify a few key series attached correctly
key_spot_check <- intersect(c("fedfunds","gs10","unrate","yield_curve",
                               "fedfunds_cycle","yield_curve_inv"),
                             names(qtrly))
message(sprintf("    Spot-check cols present: %s",
                paste(key_spot_check, collapse=", ")))

# Summary of macro NA rate in merged dataset
macro_in_qtrly <- intersect(macro_cols, names(qtrly))
na_rates <- vapply(macro_in_qtrly, function(v)
  mean(is.na(qtrly[[v]])) * 100, numeric(1))
high_na <- names(na_rates)[na_rates > 20]
if (length(high_na) > 0)
  message(sprintf("    Cols with >20%% NA in merged qtrly: %s",
                  paste(head(high_na, 8), collapse=", ")))
toc()

# ════════════════════════════════════════════════════════
# 6. SAVE UPDATED qtrly_enriched.rds
# ════════════════════════════════════════════════════════
tic("6. Save")
message("\n[6] Saving updated qtrly_enriched.rds...")

# Final column inventory
n_yoy_m  <- sum(startsWith(names(qtrly), "yoy_") &
                  names(qtrly) %in% paste0("yoy_", macro_cols))
n_qoq_m  <- sum(startsWith(names(qtrly), "qoq_") &
                  names(qtrly) %in% paste0("qoq_", macro_cols))
n_lag_m  <- sum(grepl("_lag[0-9]", names(qtrly)) &
                  names(qtrly) %in% grep("_lag", macro_cols, value=TRUE))
n_rm_m   <- sum(grepl("_rmean", names(qtrly)) &
                  names(qtrly) %in% grep("_rmean", macro_cols, value=TRUE))
message(sprintf("    Macro YoY cols      : %d", n_yoy_m))
message(sprintf("    Macro QoQ cols      : %d", n_qoq_m))
message(sprintf("    Macro Lag cols      : %d", n_lag_m))
message(sprintf("    Macro Rolling cols  : %d", n_rm_m))
message(sprintf("    TOTAL cols in qtrly : %d", ncol(qtrly)))

saveRDS(qtrly, "qtrly_enriched.rds")
message(sprintf("    Saved: qtrly_enriched.rds  (%d rows x %d cols)",
                nrow(qtrly), ncol(qtrly)))

# Also save standalone macro feature table for diagnostics
saveRDS(macro_wide, "macro_features.rds")
message("    Saved: macro_features.rds  (standalone)")
toc()

# ════════════════════════════════════════════════════════
# 7. QUICK DIAGNOSTIC PRINT
# ════════════════════════════════════════════════════════
message("\n[7] Macro series summary (most recent 4 quarters):")
recent_qtrs <- tail(sort(unique(macro_wide$date)), 4L)
diag_cols   <- intersect(c("fedfunds","gs2","gs10","yield_curve",
                             "hy_spread","unrate","cpi_yoy",
                             "fedfunds_cycle","real_rate",
                             "credit_tightness"),
                          names(macro_wide))
print(macro_wide[date %in% recent_qtrs, c("date", diag_cols), with=FALSE])

# ════════════════════════════════════════════════════════
# 8. FINAL SUMMARY
# ════════════════════════════════════════════════════════
tot <- as.numeric((proc.time() - t0)["elapsed"])
toc()  # total

message("\n=======================================================")
message(sprintf("MACRO v2.0 COMPLETE  %dh %02dm %02ds",
                floor(tot/3600),
                floor((tot %% 3600) / 60),
                round(tot %% 60)))
message(sprintf("qtrly_enriched.rds   : %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))
message(sprintf("macro_features.rds   : %d rows x %d cols",
                nrow(macro_wide), ncol(macro_wide)))
message(sprintf("macro_raw.rds        : raw FRED series"))
message("=======================================================")

notify("Macro v2.0 DONE",
       sprintf("%dh%02dm | qtrly now %d rows x %d cols",
               floor(tot/3600), floor((tot %% 3600)/60),
               nrow(qtrly), ncol(qtrly)),
       tags = "tada")

############################################################
# END
############################################################
