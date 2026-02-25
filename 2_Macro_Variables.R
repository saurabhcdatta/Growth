############################################################
# 2_Macrovariables_Feature_Engineering_v2.R
#
# Purpose : Pull macroeconomic data from FRED, engineer
#           features, and merge onto qtrly_enriched.rds
#           (produced by part1_data_prep_v2.R)
#
# Outputs : qtrly_enriched.rds   (overwritten with macro cols)
#           macro_raw.rds         (raw FRED series, pre-engineering)
#           macro_features.rds    (fully engineered macro table)
#
# Merge key: date (yearqtr)
#   Macro is at quarterly frequency.
#   Every (date x categories) row in qtrly gets the SAME
#   macro value for that quarter — no category subscript.
#
# FRED series pulled:
#   Rates     : FEDFUNDS, GS2, GS10, MORTGAGE30US, BAA10Y
#   Spreads   : T10Y3M, T10Y2Y, BAMLH0A0HYM2 (HY OAS)
#   Labour    : UNRATE, PAYEMS, ICSA
#   Activity  : GDPC1, INDPRO, HOUST
#   Prices    : CPIAUCSL, PCEPI, DCOILWTICO
#   Credit    : BUSLOANS, DPSACBW027SBOG, TOTALSL
#   Sentiment : UMCSENT, MICH
#   Money     : M2SL
#
# Feature engineering (per base series):
#   1. Level (kept as-is)
#   2. YoY % change  (lag-4)
#   3. QoQ % change  (lag-1)
#   4. Rolling mean  4q / 8q
#   5. Rolling SD    4q
#   6. Cyclical deviation from 8q rolling mean
#   7. YoY acceleration (momentum)
#   8. Lag levels    1q / 2q / 4q
#   9. Composite indices: yield_curve cycle, credit tightness,
#      real rate, FOMC regime, hike/cut run counter
#
# Runtime : ~3-5 min (FRED API + feature calc)
#
# BUG-FIX LOG:
#   1. yield_curve_inv_run  : rep(sequence(), r$lengths) wrong;
#      fixed to sequence(r$lengths) alone.
#   2. shift() namespace    : masked by another package when called
#      outside data.table [...]; fixed via shift <- data.table::shift
#      at top of script (covers ALL shift() calls globally).
############################################################

library(data.table)
library(zoo)
library(httr)
library(lubridate)
library(tictoc)
library(fredr)
options(scipen = 999)

# ── FIX: bind data.table::shift globally so it is never masked ──
# Prevents every "unused argument (4)" error when another package's
# shift() (e.g. from xts or tseries) shadows data.table's version.
shift <- data.table::shift

# ── ntfy ─────────────────────────────────────────────────────────
NTFY_TOPIC   <- "your-unique-topic-name"   # <- CHANGE THIS
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

# ════════════════════════════════════════════════════════════
# CONFIG
# ════════════════════════════════════════════════════════════

# Store your key in .Renviron as:  FRED_API_KEY=xxxxxxxxxxxxxxxx
FRED_API_KEY <- Sys.getenv("FRED_API_KEY")
if (nchar(FRED_API_KEY) == 0) {
  FRED_API_KEY <- "YOUR_FRED_API_KEY_HERE"   # <- fallback / CHANGE THIS
  message("  WARNING: FRED_API_KEY not found in .Renviron")
}
fredr_set_key(FRED_API_KEY)

# Pull from 1995 so 8-quarter rolling stats are valid by ~2000
FRED_START <- as.Date("1995-01-01")
FRED_END   <- Sys.Date()

# Named list: short_name = "FRED_series_id"
FRED_SERIES <- list(
  fedfunds    = "FEDFUNDS",
  gs2         = "GS2",
  gs10        = "GS10",
  mortgage30  = "MORTGAGE30US",
  baa_spread  = "BAA10Y",
  yield_curve = "T10Y3M",
  yield_2_10  = "T10Y2Y",
  hy_spread   = "BAMLH0A0HYM2",
  unrate      = "UNRATE",
  payems      = "PAYEMS",
  icsa        = "ICSA",
  gdp_real    = "GDPC1",
  indpro      = "INDPRO",
  housing     = "HOUST",
  cpi         = "CPIAUCSL",
  pce         = "PCEPI",
  oil_wti     = "DCOILWTICO",
  bus_loans   = "BUSLOANS",
  bank_deps   = "DPSACBW027SBOG",
  cons_credit = "TOTALSL",
  umcsent     = "UMCSENT",
  inf_exp     = "MICH",
  m2          = "M2SL"
)

# ════════════════════════════════════════════════════════════
# 1. PULL FRED DATA
# ════════════════════════════════════════════════════════════
tic("1. Pull FRED data")
message("\n[1] Pulling FRED data...")

pull_fred <- function(series_id, short_name,
                      start = FRED_START, end = FRED_END) {
  tryCatch({
    df <- fredr::fredr(
      series_id          = series_id,
      observation_start  = start,
      observation_end    = end,
      frequency          = "q",
      aggregation_method = "avg"
    )
    dt <- as.data.table(df)[, .(
      date  = zoo::as.yearqtr(date),
      value = value
    )]
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

macro_list <- macro_list[!vapply(macro_list, is.null, logical(1))]
message(sprintf("    Successfully pulled %d / %d series",
                length(macro_list), length(FRED_SERIES)))

macro_wide <- Reduce(
  function(a, b) merge(a, b, by = "date", all = TRUE),
  macro_list
)
setorderv(macro_wide, "date")
message(sprintf("    Raw macro: %d rows x %d cols  (%s to %s)",
                nrow(macro_wide), ncol(macro_wide),
                as.character(min(macro_wide$date, na.rm = TRUE)),
                as.character(max(macro_wide$date, na.rm = TRUE))))

saveRDS(macro_wide, "macro_raw.rds")
toc()
notify("FRED Data Pulled",
       sprintf("%d series, %d quarters",
               ncol(macro_wide) - 1L, nrow(macro_wide)),
       tags = "white_check_mark")

# ════════════════════════════════════════════════════════════
# 2. COMPUTED / DERIVED MACRO SERIES
# ════════════════════════════════════════════════════════════
tic("2. Derived macro series")
message("\n[2] Computing derived series...")

# Fed funds cycle: deviation from trailing 8q average
if ("fedfunds" %in% names(macro_wide)) {
  macro_wide[, fedfunds_trail8 := zoo::rollapply(
    fedfunds, width = 8, FUN = mean, na.rm = TRUE,
    fill = NA, align = "right"
  )]
  macro_wide[, fedfunds_cycle := fedfunds - fedfunds_trail8]
}

# Yield curve inversion flag + within-run counter
# FIX: sequence(r$lengths) alone — no rep() wrapper
if ("yield_curve" %in% names(macro_wide)) {
  macro_wide[, yield_curve_inv := fifelse(yield_curve < 0, 1L, 0L)]
  macro_wide[, yield_curve_inv_run := {
    inv_nona <- ifelse(is.na(yield_curve_inv), 0L, yield_curve_inv)
    r        <- rle(inv_nona)
    sequence(r$lengths)
  }]
}

# Real rate: Fed Funds minus CPI YoY %
if (all(c("fedfunds", "cpi") %in% names(macro_wide))) {
  macro_wide[, cpi_yoy   := (cpi / shift(cpi, 4L, type = "lag") - 1) * 100]
  macro_wide[, real_rate := fedfunds - cpi_yoy]
}

# Term spread 2s10s
if (all(c("gs2", "gs10") %in% names(macro_wide))) {
  macro_wide[, spread_2s10s := gs10 - gs2]
}

# Credit tightness composite (standardised HY + BAA spreads)
if (all(c("hy_spread", "baa_spread") %in% names(macro_wide))) {
  std_z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  macro_wide[, credit_tightness := std_z(hy_spread) + std_z(baa_spread)]
}

# Real payroll (payrolls deflated by CPI)
if (all(c("payems", "cpi") %in% names(macro_wide))) {
  macro_wide[, real_payems := payems / cpi * 100]
}

# Oil price shock indicator
if ("oil_wti" %in% names(macro_wide)) {
  macro_wide[, oil_qoq_pct := (oil_wti / shift(oil_wti, 1L, type = "lag") - 1) * 100]
  macro_wide[, oil_shock   := fifelse(abs(oil_qoq_pct) > 20, 1L, 0L)]
}

# M2 growth YoY %
if ("m2" %in% names(macro_wide)) {
  macro_wide[, m2_yoy := (m2 / shift(m2, 4L, type = "lag") - 1) * 100]
}

# Bank deposit growth YoY %
if ("bank_deps" %in% names(macro_wide)) {
  macro_wide[, bank_deps_yoy :=
               (bank_deps / shift(bank_deps, 4L, type = "lag") - 1) * 100]
}

# FOMC regime: -1 = cutting, 0 = hold, +1 = hiking
# hike_run: signed consecutive-quarter counter
if ("fedfunds" %in% names(macro_wide)) {
  macro_wide[, fedfunds_chg := fedfunds - shift(fedfunds, 1L, type = "lag")]
  macro_wide[, fomc_regime  := fcase(
    fedfunds_chg >  0.10,  1L,
    fedfunds_chg < -0.10, -1L,
    default = 0L
  )]
  # FIX: same sequence(rle) pattern — no rep() wrapper
  macro_wide[, hike_run := {
    reg_nona <- ifelse(is.na(fomc_regime), 0L, fomc_regime)
    r        <- rle(reg_nona)
    counts   <- sequence(r$lengths)
    signs    <- rep(sign(r$values), r$lengths)
    counts * signs
  }]
}

message(sprintf("    Derived cols added. Total macro cols: %d",
                ncol(macro_wide) - 1L))
toc()

# ════════════════════════════════════════════════════════════
# 3. FEATURE ENGINEERING ON MACRO SERIES
# ════════════════════════════════════════════════════════════
tic("3. Macro feature engineering")
message("\n[3] Engineering macro features...")

macro_fe_cols <- setdiff(
  names(macro_wide)[vapply(macro_wide, is.numeric, logical(1))],
  "date"
)

already_derived <- c(
  "cpi_yoy", "m2_yoy", "bank_deps_yoy",
  "real_rate", "fedfunds_cycle", "fedfunds_trail8",
  "credit_tightness", "oil_qoq_pct", "oil_shock",
  "spread_2s10s", "yield_curve_inv", "yield_curve_inv_run",
  "real_payems", "fomc_regime", "fedfunds_chg", "hike_run"
)
base_fe <- setdiff(macro_fe_cols, already_derived)
message(sprintf("    Base series for FE: %d", length(base_fe)))

# pct_chg helper: takes lag integer n, calls shift() internally.
# FIX: avoids lambda shift(x, 4L) scoping issue in lapply(.SD, ...).
pct_chg_n <- function(x, n) {
  lag_x <- shift(x, n, type = "lag")
  fifelse(
    !is.na(lag_x) & lag_x != 0 & abs(lag_x) > 1e-6,
    (x - lag_x) / abs(lag_x) * 100,
    NA_real_
  )
}

# 3a: YoY % change (lag-4)
message("    3a. YoY % change (lag-4)...")
macro_wide[, paste0("yoy_", base_fe) :=
             lapply(.SD, pct_chg_n, n = 4L),
           .SDcols = base_fe]

# 3b: QoQ % change (lag-1)
message("    3b. QoQ % change (lag-1)...")
macro_wide[, paste0("qoq_", base_fe) :=
             lapply(.SD, pct_chg_n, n = 1L),
           .SDcols = base_fe]

# 3c: Rolling means (4q, 8q) on key series
message("    3c. Rolling means (4q, 8q)...")
rollmean_safe <- function(x, k) {
  zoo::rollapply(x, width = k, FUN = mean, na.rm = TRUE,
                 fill = NA, align = "right", partial = FALSE)
}

key_macro <- intersect(
  c("fedfunds", "gs10", "gs2", "yield_curve", "hy_spread", "baa_spread",
    "unrate", "payems", "gdp_real", "cpi", "housing", "umcsent",
    "real_rate", "fedfunds_cycle", "credit_tightness"),
  names(macro_wide)
)

for (k in c(4L, 8L)) {
  new_nms <- paste0(key_macro, "_rmean", k)
  macro_wide[, (new_nms) := lapply(.SD, rollmean_safe, k = k),
             .SDcols = key_macro]
}

# 3d: Rolling SDs (4q) — volatility
message("    3d. Rolling SDs (4q)...")
rollsd_safe <- function(x, k) {
  zoo::rollapply(x, width = k, FUN = sd, na.rm = TRUE,
                 fill = NA, align = "right", partial = FALSE)
}
new_nms <- paste0(key_macro, "_rsd4")
macro_wide[, (new_nms) := lapply(.SD, rollsd_safe, k = 4L),
           .SDcols = key_macro]

# 3e: Cyclical deviation from 8q rolling mean
message("    3e. Cyclical deviations...")
for (v in key_macro) {
  rmean_col <- paste0(v, "_rmean8")
  cyc_col   <- paste0(v, "_cyc")
  if (rmean_col %in% names(macro_wide))
    macro_wide[, (cyc_col) := get(v) - get(rmean_col)]
}

# 3f: YoY acceleration (current YoY minus 4q-ago YoY)
# FIX: shift() here resolves correctly via global binding
message("    3f. YoY acceleration...")
yoy_key <- intersect(paste0("yoy_", key_macro), names(macro_wide))
new_nms <- paste0(yoy_key, "_accel")
macro_wide[, (new_nms) := lapply(.SD, function(x) x - shift(x, 4L, type = "lag")),
           .SDcols = yoy_key]

# 3g: Lag levels (1q, 2q, 4q)
message("    3g. Lag levels (1q, 2q, 4q)...")
for (lag_n in c(1L, 2L, 4L)) {
  new_nms <- paste0(key_macro, "_lag", lag_n)
  macro_wide[, (new_nms) := lapply(.SD, shift, n = lag_n, type = "lag"),
             .SDcols = key_macro]
}

# 3h: Interaction terms
message("    3h. Interaction terms...")
if (all(c("fedfunds", "yield_curve") %in% names(macro_wide)))
  macro_wide[, rate_x_slope := fedfunds * yield_curve]
if (all(c("fedfunds", "hy_spread") %in% names(macro_wide)))
  macro_wide[, rate_x_hy := fedfunds * hy_spread]

message(sprintf("    Macro feature table: %d rows x %d cols",
                nrow(macro_wide), ncol(macro_wide)))
toc()

# ════════════════════════════════════════════════════════════
# 4. QUALITY CHECKS
# ════════════════════════════════════════════════════════════
tic("4. QC")
message("\n[4] Quality checks on macro data...")

all_na <- names(macro_wide)[
  vapply(macro_wide, function(x) all(is.na(x)), logical(1))]
if (length(all_na) > 0) {
  macro_wide[, (all_na) := NULL]
  message(sprintf("    Dropped %d all-NA columns: %s",
                  length(all_na), paste(head(all_na, 5), collapse = ", ")))
} else {
  message("    No all-NA columns found.")
}

n_missing <- vapply(names(macro_wide), function(v)
  sum(is.na(macro_wide[[v]])), integer(1))
high_miss <- names(n_missing)[n_missing > nrow(macro_wide) * 0.3]
if (length(high_miss) > 0)
  message(sprintf("    High-NA cols (>30%%): %s", paste(high_miss, collapse = ", ")))
else
  message("    No columns with >30% NA.")

message(sprintf("    Final macro table: %d rows x %d cols",
                nrow(macro_wide), ncol(macro_wide)))
message(sprintf("    Date range: %s to %s",
                as.character(min(macro_wide$date, na.rm = TRUE)),
                as.character(max(macro_wide$date, na.rm = TRUE))))
toc()

# ════════════════════════════════════════════════════════════
# 5. MERGE ONTO qtrly_enriched.rds
# ════════════════════════════════════════════════════════════
tic("5. Merge onto quarterly panel")
message("\n[5] Loading qtrly_enriched.rds and merging macro...")

qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)
message(sprintf("    qtrly before merge: %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))

macro_cols <- setdiff(names(macro_wide), "date")

# Remove pre-existing macro cols — safe to re-run
already_in <- intersect(macro_cols, names(qtrly))
if (length(already_in) > 0) {
  message(sprintf("    Removing %d pre-existing macro cols before re-merge...",
                  length(already_in)))
  qtrly[, (already_in) := NULL]
}

# match() + direct assignment: avoids merge() duplicate-column risk
# on the (date x categories) panel where each date appears 7 times
idx       <- match(qtrly$date, macro_wide$date)
n_matched <- sum(!is.na(idx))
message(sprintf("    Date matches: %d / %d rows", n_matched, nrow(qtrly)))

if (n_matched == 0)
  stop("No date matches between qtrly and macro_wide. Check date format.")

for (col in macro_cols) {
  qtrly[, (col) := macro_wide[[col]][idx]]
}

message(sprintf("    qtrly after merge: %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))

key_spot <- intersect(
  c("fedfunds", "gs10", "unrate", "yield_curve",
    "fedfunds_cycle", "yield_curve_inv", "real_rate", "credit_tightness"),
  names(qtrly)
)
message(sprintf("    Spot-check cols present: %s", paste(key_spot, collapse = ", ")))

macro_in_qtrly <- intersect(macro_cols, names(qtrly))
na_rates <- vapply(macro_in_qtrly, function(v)
  mean(is.na(qtrly[[v]])) * 100, numeric(1))
high_na <- names(na_rates)[na_rates > 20]
if (length(high_na) > 0)
  message(sprintf("    Cols with >20%% NA in merged qtrly: %s",
                  paste(head(high_na, 8), collapse = ", ")))
else
  message("    No macro cols with >20% NA after merge.")
toc()

# ════════════════════════════════════════════════════════════
# 6. SAVE
# ════════════════════════════════════════════════════════════
tic("6. Save")
message("\n[6] Saving outputs...")

n_yoy_m <- sum(startsWith(names(qtrly), "yoy_") &
                 names(qtrly) %in% paste0("yoy_", macro_cols))
n_qoq_m <- sum(startsWith(names(qtrly), "qoq_") &
                 names(qtrly) %in% paste0("qoq_", macro_cols))
n_lag_m <- sum(grepl("_lag[0-9]", names(qtrly)) &
                 names(qtrly) %in% grep("_lag", macro_cols, value = TRUE))
n_rm_m  <- sum(grepl("_rmean", names(qtrly)) &
                 names(qtrly) %in% grep("_rmean", macro_cols, value = TRUE))
message(sprintf("    Macro YoY cols      : %d", n_yoy_m))
message(sprintf("    Macro QoQ cols      : %d", n_qoq_m))
message(sprintf("    Macro Lag cols      : %d", n_lag_m))
message(sprintf("    Macro Rolling cols  : %d", n_rm_m))
message(sprintf("    TOTAL cols in qtrly : %d", ncol(qtrly)))

saveRDS(qtrly,      "qtrly_enriched.rds")
saveRDS(macro_wide, "macro_features.rds")
message(sprintf("    Saved: qtrly_enriched.rds  (%d rows x %d cols)",
                nrow(qtrly), ncol(qtrly)))
message("    Saved: macro_features.rds  (standalone macro feature table)")
toc()

# ════════════════════════════════════════════════════════════
# 7. DIAGNOSTIC PRINT
# ════════════════════════════════════════════════════════════
message("\n[7] Macro series summary (most recent 4 quarters):")
recent_qtrs <- tail(sort(unique(macro_wide$date)), 4L)
diag_cols   <- intersect(
  c("fedfunds", "gs2", "gs10", "yield_curve", "hy_spread",
    "unrate", "cpi_yoy", "fedfunds_cycle", "real_rate",
    "credit_tightness", "fomc_regime"),
  names(macro_wide)
)
print(macro_wide[date %in% recent_qtrs, c("date", diag_cols), with = FALSE])

# ════════════════════════════════════════════════════════════
# 8. FINAL SUMMARY
# ════════════════════════════════════════════════════════════
tot <- as.numeric((proc.time() - t0)["elapsed"])
toc()  # Macro v2 total

message("\n=======================================================")
message(sprintf("MACRO v2.0 COMPLETE  %dh %02dm %02ds",
                floor(tot / 3600),
                floor((tot %% 3600) / 60),
                round(tot %% 60)))
message(sprintf("qtrly_enriched.rds  : %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))
message(sprintf("macro_features.rds  : %d rows x %d cols",
                nrow(macro_wide), ncol(macro_wide)))
message("macro_raw.rds       : raw FRED series (pre-engineering)")
message("=======================================================")

notify("Macro v2.0 DONE",
       sprintf("%dh%02dm | qtrly now %d rows x %d cols",
               floor(tot / 3600), floor((tot %% 3600) / 60),
               nrow(qtrly), ncol(qtrly)),
       tags = "tada")

############################################################
# END
############################################################
