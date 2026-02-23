############################################################
# PART 1 — FRED PULL + FEATURE ENGINEERING
# Produces: qtrly_enriched.rds
#
# Sections:
#  A) Pull 15 macro series from FRED via fredr
#  B) Aggregate to quarterly, align to yearqtr date key
#  C) Merge macro onto qtrly.rds
#  D) Add lagged target variables  (AR lags: 1,2,3,4,6,8 qtrs)
#  E) Add lagged macro variables   (1,2,4 quarter lags per series)
#  F) Add rolling statistics       (4q and 8q mean + SD on target + key macros)
#  G) Add trend & cycle indicators (time index, quarter dummies,
#                                   regime flags, HP-filter cycle)
#  H) Save qtrly_enriched.rds
############################################################

# ── Packages ─────────────────────────────────────────────
pkgs <- c("data.table", "zoo", "fredr", "mFilter")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

# ── Date utility ─────────────────────────────────────────
# qtrly.rds dates are quarter-start Dates: 2001-01-01, 2001-04-01, 2001-07-01, 2001-10-01
# This helper converts them to yearqtr reliably via the month number,
# avoiding any ambiguity from zoo's as.yearqtr(Date) which can behave
# differently depending on zoo version.
date_to_yearqtr <- function(x) {
  # Accepts Date, POSIXct, or character "YYYY-MM-DD"
  d <- as.Date(x)
  yr <- as.integer(format(d, "%Y"))
  mo <- as.integer(format(d, "%m"))
  # Quarter: Jan-Mar=1, Apr-Jun=2, Jul-Sep=3, Oct-Dec=4
  qtr <- (mo - 1L) %/% 3L + 1L
  zoo::as.yearqtr(paste0(yr, " Q", qtr))
}

# ── USER: set your FRED API key ───────────────────────────
# Option 1: set once in .Renviron: FRED_API_KEY=your_key_here
# Option 2: uncomment the line below
fredr_set_key("9af82eb433b17a1e7942fea4a4d850c1")

if (nchar(Sys.getenv("FRED_API_KEY")) == 0)
  stop("FRED API key not found. Set FRED_API_KEY env var or call fredr_set_key().")

fredr_set_key(Sys.getenv("FRED_API_KEY"))

# ── A) FRED series to pull ────────────────────────────────
# Each entry: fred_id, short_name, aggregation_method
# agg: "avg" for rates/indices, "sum" for flows, "eop" for end-of-period stocks
FRED_SERIES <- list(
  # --- Monetary policy & rates ---
  list(id = "FEDFUNDS",   name = "fedfunds",    agg = "avg"),   # Effective Fed Funds Rate
  list(id = "GS10",       name = "gs10",        agg = "avg"),   # 10-yr Treasury yield
  list(id = "GS2",        name = "gs2",         agg = "avg"),   # 2-yr Treasury yield
  list(id = "MORTGAGE30US",name="mortgage30",   agg = "avg"),   # 30-yr fixed mortgage rate
  list(id = "BAMLH0A0HYM2",name="hy_spread",    agg = "avg"),   # HY credit spread (OAS)

  # --- Real economy ---
  list(id = "GDPC1",      name = "real_gdp",    agg = "avg"),   # Real GDP (quarterly, SA)
  list(id = "UNRATE",     name = "unrate",      agg = "avg"),   # Unemployment rate
  list(id = "PAYEMS",     name = "nonfarm_pay", agg = "avg"),   # Nonfarm payrolls (000s)
  list(id = "INDPRO",     name = "indpro",      agg = "avg"),   # Industrial production index
  list(id = "HOUST",      name = "housing_starts", agg = "avg"),# Housing starts (000s)

  # --- Prices & inflation ---
  list(id = "CPIAUCSL",   name = "cpi",         agg = "avg"),   # CPI all items, SA
  list(id = "PCEPI",      name = "pce_deflator",agg = "avg"),   # PCE price index

  # --- Credit & financial conditions ---
  list(id = "DPSACBW027SBOG", name = "deposits",agg = "avg"),   # Deposits at commercial banks
  list(id = "TOTLL",      name = "bank_loans",  agg = "avg"),   # Total loans & leases

  # --- Consumer confidence / sentiment ---
  list(id = "UMCSENT",    name = "umich_sent",  agg = "avg")    # U of Michigan consumer sentiment
)

# ── B) Fetch and aggregate to quarterly ──────────────────
DATA_START <- as.Date("2000-01-01")   # 2 years before model start for lag room
DATA_END   <- as.Date("2025-12-31")

fetch_fred_quarterly <- function(series_info) {
  message("  Pulling: ", series_info$id, " (", series_info$name, ")")

  raw <- tryCatch(
    fredr(
      series_id         = series_info$id,
      observation_start = DATA_START,
      observation_end   = DATA_END,
      frequency         = "q",
      aggregation_method = series_info$agg
    ),
    error = function(e) {
      warning("Failed to pull ", series_info$id, ": ", e$message)
      NULL
    }
  )

  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  dt <- as.data.table(raw)[, .(date, value)]
  setnames(dt, "value", series_info$name)

  # Convert date to yearqtr using robust helper
  dt[, yq := date_to_yearqtr(date)]
  dt[, date := NULL]
  setnames(dt, "yq", "date")

  # Keep one obs per quarter (aggregation already done by FRED API)
  dt <- dt[, .(value = mean(get(series_info$name), na.rm = TRUE)), by = date]
  setnames(dt, "value", series_info$name)

  dt
}

message("Fetching FRED series...")
fred_list <- lapply(FRED_SERIES, fetch_fred_quarterly)
fred_list <- fred_list[!sapply(fred_list, is.null)]

# Full quarterly date spine from 2000Q1 to 2025Q3
all_dates <- zoo::as.yearqtr(
  seq(as.Date("2000-01-01"), as.Date("2025-09-30"), by = "quarter")
)
macro_dt <- data.table(date = all_dates)

for (dt in fred_list) {
  macro_dt <- merge(macro_dt, dt, by = "date", all.x = TRUE)
}

# Derived macro features (computed before lagging)
macro_dt[, yield_curve := gs10 - gs2]                        # 10y-2y spread (recession signal)
macro_dt[, real_fedfunds := fedfunds - shift(cpi, 4L) / shift(cpi, 4L) * 100]  # approx real rate
macro_dt[, gdp_yoy := (real_gdp / shift(real_gdp, 4L) - 1) * 100]
macro_dt[, cpi_yoy := (cpi / shift(cpi, 4L) - 1) * 100]
macro_dt[, payroll_yoy := (nonfarm_pay / shift(nonfarm_pay, 4L) - 1) * 100]
macro_dt[, deposit_yoy := (deposits / shift(deposits, 4L) - 1) * 100]
macro_dt[, loan_yoy    := (bank_loans / shift(bank_loans, 4L) - 1) * 100]

setorderv(macro_dt, "date")
message("Macro data shape: ", nrow(macro_dt), " rows x ", ncol(macro_dt), " cols")

# ── C) Load qtrly.rds and merge macro ────────────────────
message("Loading qtrly.rds...")
qtrly <- readRDS("qtrly.rds")
setDT(qtrly)

if (!inherits(qtrly$date, "yearqtr"))
  qtrly[, date := date_to_yearqtr(date)]

setorderv(qtrly, c("categories", "date"))

# Merge macro (same macro values across all categories — national series)
qtrly <- merge(qtrly, macro_dt, by = "date", all.x = TRUE)
message("After macro merge: ", nrow(qtrly), " rows x ", ncol(qtrly), " cols")

# ── D) Lagged target (AR terms) — within each category ───
# These are the single most important features for a YoY count series
TARGET <- "yoy_ficu_count"
AR_LAGS <- c(1L, 2L, 3L, 4L, 6L, 8L)

message("Adding AR lags of target...")
qtrly[order(date), paste0(TARGET, "_lag", AR_LAGS) :=
        lapply(AR_LAGS, function(k) shift(get(TARGET), k)),
      by = categories]

# ── E) Lagged macro variables (1, 2, 4 quarter lags) ─────
# Macro lags prevent look-ahead leakage and capture delayed transmission
MACRO_VARS_TO_LAG <- c(
  "fedfunds", "gs10", "yield_curve", "mortgage30",
  "gdp_yoy", "cpi_yoy", "unrate", "payroll_yoy",
  "hy_spread", "deposit_yoy", "loan_yoy", "umich_sent"
)
MACRO_LAGS <- c(1L, 2L, 4L)

# Macro vars are not category-specific; lag once on macro_dt, re-merge
# But since they're already merged, we can lag in place
# (they are identical across categories so data.table by= is fine but redundant)
message("Adding macro lags...")
for (v in MACRO_VARS_TO_LAG) {
  if (!v %in% names(qtrly)) next
  for (k in MACRO_LAGS) {
    new_col <- paste0(v, "_lag", k)
    qtrly[order(date), (new_col) := shift(get(v), k), by = categories]
  }
}

# ── F) Rolling statistics (4q and 8q windows) ────────────
message("Adding rolling statistics...")

roll_mean <- function(x, w) zoo::rollmeanr(x, w, fill = NA)
roll_sd   <- function(x, w) zoo::rollapply(x, w, sd, fill = NA, align = "right")

# Rolling stats on target
for (w in c(4L, 8L)) {
  qtrly[order(date),
        paste0(TARGET, c("_rmean", "_rsd"), w) := list(
          roll_mean(get(TARGET), w),
          roll_sd(get(TARGET), w)
        ),
        by = categories]
}

# Rolling stats on key macro vars
MACRO_ROLL_VARS <- c("fedfunds", "unrate", "gdp_yoy", "cpi_yoy",
                     "yield_curve", "mortgage30", "deposit_yoy")
for (v in MACRO_ROLL_VARS) {
  if (!v %in% names(qtrly)) next
  for (w in c(4L, 8L)) {
    qtrly[order(date),
          paste0(v, c("_rmean", "_rsd"), w) := list(
            roll_mean(get(v), w),
            roll_sd(get(v), w)
          ),
          by = categories]
  }
}

# ── G) Trend & Cycle indicators ───────────────────────────
message("Adding trend and cycle indicators...")

# 1. Linear time index (quarters since start; gives XGBoost a time anchor)
min_date <- min(qtrly$date)
qtrly[, time_idx := as.numeric(date - min_date) * 4]  # 0, 1, 2, ...

# 2. Seasonal: quarter-of-year (1–4) extracted directly from yearqtr
#    yearqtr stores as year + (q-1)/4, so: q = round((date %% 1) * 4) + 1
qtrly[, quarter_num := as.integer(round((as.numeric(date) %% 1) * 4)) + 1L]

# 3. Regime / structural break flags
#    GFC:   2008Q3 – 2009Q2  (Lehman collapse through trough)
#    ZIRP:  2009Q3 – 2015Q4  (zero lower bound era)
#    COVID: 2020Q1 – 2021Q2  (acute disruption window)
#    POST_COVID: 2021Q3 onward
qtrly[, regime_gfc    := as.integer(date >= zoo::as.yearqtr("2008 Q3") &
                                       date <= zoo::as.yearqtr("2009 Q2"))]
qtrly[, regime_zirp   := as.integer(date >= zoo::as.yearqtr("2009 Q3") &
                                       date <= zoo::as.yearqtr("2015 Q4"))]
qtrly[, regime_covid  := as.integer(date >= zoo::as.yearqtr("2020 Q1") &
                                       date <= zoo::as.yearqtr("2021 Q2"))]
qtrly[, regime_post_covid := as.integer(date > zoo::as.yearqtr("2021 Q2"))]
qtrly[, regime_hike   := as.integer(date >= zoo::as.yearqtr("2022 Q1"))]  # Fed rate hike cycle

# 4. Distance (in quarters) from key break points
#    Negative = before event, positive = after
gfc_date    <- zoo::as.yearqtr("2008 Q3")
covid_date  <- zoo::as.yearqtr("2020 Q1")
qtrly[, qtrs_from_gfc    := as.numeric(date - gfc_date) * 4]
qtrly[, qtrs_from_covid  := as.numeric(date - covid_date) * 4]

# 5. HP-filter cycle component of Fed Funds Rate (captures deviations from trend)
#    Computed on national macro (same for all categories), applied once
if ("fedfunds" %in% names(qtrly)) {
  # Use one category's time series for HP (identical across cats)
  ff_series <- qtrly[categories == qtrly$categories[1]][order(date), fedfunds]
  ff_clean  <- ff_series[!is.na(ff_series)]

  if (length(ff_clean) >= 8) {
    hp_out <- tryCatch(
      mFilter::hpfilter(ff_clean, freq = 1600, type = "lambda"),
      error = function(e) NULL
    )
    if (!is.null(hp_out)) {
      # Rebuild full-length vector with NAs for missing head
      n_miss <- sum(is.na(ff_series))
      cycle_full <- c(rep(NA_real_, n_miss), as.numeric(hp_out$cycle))
      # cycle_full length = length(ff_clean) + n_miss; trim to match
      cycle_full <- cycle_full[seq_len(length(ff_series))]

      # Map back by date
      hp_dt <- data.table(
        date           = qtrly[categories == qtrly$categories[1]][order(date), date],
        fedfunds_cycle = cycle_full
      )
      qtrly <- merge(qtrly, hp_dt, by = "date", all.x = TRUE)
    }
  }
}

# 6. Yield curve inversion flag (binary: 1 when 10y-2y < 0)
if ("yield_curve" %in% names(qtrly))
  qtrly[, yield_curve_inv := as.integer(yield_curve < 0)]

# ── H) Final cleanup & save ───────────────────────────────
setorderv(qtrly, c("categories", "date"))

# Report feature count
n_new_cols <- ncol(qtrly) - length(names(readRDS("qtrly.rds")))
message(sprintf(
  "Feature engineering complete.\n  Original cols: %d\n  New cols added: %d\n  Total cols: %d\n  Rows: %d",
  ncol(qtrly) - n_new_cols, n_new_cols, ncol(qtrly), nrow(qtrly)
))

saveRDS(qtrly, "qtrly_enriched.rds")
message("Saved: qtrly_enriched.rds")

# Optional: save macro table separately for inspection
fwrite(macro_dt, "macro_fred_quarterly.csv")
message("Saved: macro_fred_quarterly.csv")
