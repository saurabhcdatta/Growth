############################################################
# PART 8 v1.0 — DISAGGREGATED CU-LEVEL FORECASTING
#
# Phase 2: Individual credit union asset forecasting
#
# Purpose  : Fit ARIMA to each individual credit union's
#            asset time series and project which NCUA
#            asset-size category they will belong to at
#            +1 year, +3 year, and +5 year horizons.
#
# Input    : call_report.rds (or .dta) — CU-level panel
#            with join_number, cu_type, assets_tot, year,
#            quarter, region, reporting_state, cu_name
#
# Method   : auto.arima() per CU on log(assets) series
#            → forecast 4/12/20 quarters → exp() back
#            → assign to asset bucket based on thresholds
#
# Output   : Excel files (FCU and FISCU separately):
#            join_number, cu_name, region, state,
#            asset_bucket_now, _1yr, _3yr, _5yr
#            Publication charts showing migration flows
#
# Parallel : Windows-safe via parallel::parLapply
############################################################

# ════════════════════════════════════════════════════════════
# 0. PACKAGES
# ════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(forecast)
  library(ggplot2)
  library(scales)
  library(parallel)
  library(tictoc)
})

# Optional: haven for .dta files
has_haven <- requireNamespace("haven", quietly = TRUE)

set.seed(42)
options(scipen = 999)

# ════════════════════════════════════════════════════════════
# 1. CONFIG
# ════════════════════════════════════════════════════════════
DATA_DIR   <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
RESULT_DIR <- "results_8_disagg"
PLOT_DIR   <- "plots_8_disagg"

N_CORES <- max(1L, detectCores() - 1L)

# ARIMA config
MIN_OBS     <- 20L    # minimum quarterly observations per CU (5 years)
HORIZON_1Y  <- 4L     # quarters for 1-year forecast
HORIZON_3Y  <- 12L    # quarters for 3-year forecast
HORIZON_5Y  <- 20L    # quarters for 5-year forecast

# Sanity limits: maximum category jumps allowed per horizon
# Prevents unrealistic leaps (e.g., <$10M → $1B+ in 5 years)
MAX_JUMP_1Y <- 1L     # at most 1 category up or down in 1 year
MAX_JUMP_3Y <- 2L     # at most 2 categories in 3 years
MAX_JUMP_5Y <- 3L     # at most 3 categories in 5 years

# Maximum annualised asset growth rate (%) for forecast sanity
MAX_ANNUAL_GROWTH <- 40  # cap: no CU grows > 40% per year sustained

# Asset category thresholds (in dollars)
ASSET_BREAKS <- c(0, 10e6, 50e6, 100e6, 500e6, 1e9, 10e9, Inf)
ASSET_LABELS <- c("1_Less_10M", "2_10M_50M", "3_50M_100M",
                  "4_100M_500M", "5_500M_1B", "6_1B_10B", "7_10B_Plus")

setwd(DATA_DIR)
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR,   showWarnings = FALSE, recursive = TRUE)

message("============================================================")
message("  PART 8: Disaggregated CU-Level Forecasting")
message("============================================================")
message(sprintf("  Cores: %d / %d available", N_CORES, detectCores()))
message(sprintf("  Horizons: +%dQ (1yr), +%dQ (3yr), +%dQ (5yr)",
                HORIZON_1Y, HORIZON_3Y, HORIZON_5Y))

# ════════════════════════════════════════════════════════════
# 2. LOAD CU-LEVEL DATA
# ════════════════════════════════════════════════════════════
message("\n[1] Loading CU-level data...")

# Try RDS first, then .dta
if (file.exists("call_report2.rds")) {
  cr <- readRDS("call_report2.rds")
  setDT(cr)
  message("  Loaded call_report2.rds")
} else if (file.exists("call_report.rds")) {
  cr <- readRDS("call_report.rds")
  setDT(cr)
  message("  Loaded call_report.rds (fallback)")
} else if (has_haven && file.exists("call_report.dta")) {
  cr <- as.data.table(haven::read_dta("call_report.dta"))
  message("  Loaded call_report.dta")
} else {
  # Try any .dta file
  dta_files <- list.files(DATA_DIR, pattern = "\\.dta$", full.names = FALSE)
  if (length(dta_files) > 0 && has_haven) {
    cr <- as.data.table(haven::read_dta(dta_files[1]))
    message(sprintf("  Loaded %s", dta_files[1]))
  } else {
    stop("No call_report.rds or .dta file found in DATA_DIR")
  }
}

# Strip haven labels
if (has_haven) {
  cr <- tryCatch(as.data.table(haven::zap_labels(cr)), error = function(e) cr)
}
for (cn in names(cr)) {
  if (!is.null(attr(cr[[cn]], "label")))  attr(cr[[cn]], "label")  <- NULL
  if (!is.null(attr(cr[[cn]], "labels"))) attr(cr[[cn]], "labels") <- NULL
}

message(sprintf("  Raw: %s rows × %s cols",
                format(nrow(cr), big.mark=","), format(ncol(cr), big.mark=",")))

# ── Build date column ────────────────────────────────────
if (!"date" %in% names(cr)) {
  yr_col  <- intersect(c("year","cryear"), names(cr))[1]
  qtr_col <- intersect(c("quarter","crquarter"), names(cr))[1]
  if (is.na(yr_col) || is.na(qtr_col)) stop("Cannot find year/quarter columns")
  cr[, date := zoo::as.yearqtr(get(yr_col) + (get(qtr_col) - 1L) / 4)]
  message(sprintf("  Built date from %s + %s", yr_col, qtr_col))
} else {
  if (!inherits(cr$date, "yearqtr")) cr[, date := zoo::as.yearqtr(date)]
}

# ── Identify key columns ────────────────────────────────
# Print all column names for diagnostic
message("  Column names in data:")
message(paste("   ", paste(names(cr), collapse = ", ")))

# join_number = unique CU identifier
id_candidates <- c("join_number", "joinnum", "cu_number", "charter_number",
                    "JOIN_NUMBER", "JoinNumber", "CU_NUMBER", "CharterNumber",
                    "id", "cu_id", "credit_union_id", "ncua_id", "cert",
                    "acct_id", "cycle_date")
id_col <- intersect(id_candidates, names(cr))[1]

# If no exact match, try pattern matching
if (is.na(id_col)) {
  id_col <- grep("join|charter|cu_num|ncua|cert_?no", names(cr),
                  ignore.case = TRUE, value = TRUE)[1]
}
if (is.na(id_col)) {
  message("  [ERROR] Could not find CU identifier column.")
  message("  Candidates tried: ", paste(id_candidates, collapse = ", "))
  message("  Available columns (first 30): ", paste(head(names(cr), 30), collapse = ", "))
  stop("Cannot find CU identifier column. Please set id_col manually.")
}
message(sprintf("  CU identifier column: '%s'", id_col))
if (id_col != "join_number") setnames(cr, id_col, "join_number")

# cu_type: 1 = FCU, 2 = FISCU
if (!"cu_type" %in% names(cr)) {
  type_col <- grep("cu_type|type_?code|charter_type|^type$", names(cr),
                    ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(type_col)) {
    setnames(cr, type_col, "cu_type")
    message(sprintf("  CU type column: '%s' → cu_type", type_col))
  } else {
    message("  [WARN] No cu_type column found — will try to infer from data")
  }
}

# assets_tot
if (!"assets_tot" %in% names(cr)) {
  asset_col <- grep("assets_tot|total_assets|^assets$|acct_?010|tot_?asset", names(cr),
                     ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(asset_col)) {
    setnames(cr, asset_col, "assets_tot")
    message(sprintf("  Asset column: '%s' → assets_tot", asset_col))
  } else {
    stop("Cannot find total assets column. Available: ",
         paste(head(grep("asset", names(cr), ignore.case=TRUE, value=TRUE), 10), collapse=", "))
  }
}

# cu_name
if (!"cu_name" %in% names(cr)) {
  name_col <- grep("cu_name|^name$|institution_name|credit_union_name", names(cr),
                    ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(name_col)) {
    setnames(cr, name_col, "cu_name")
    message(sprintf("  Name column: '%s' → cu_name", name_col))
  } else {
    cr[, cu_name := paste0("CU_", join_number)]
    message("  No name column — using CU_{join_number}")
  }
}

# region
if (!"region" %in% names(cr)) {
  reg_col <- grep("^region$|ncua_region|reg_?code", names(cr),
                   ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(reg_col)) {
    setnames(cr, reg_col, "region")
    message(sprintf("  Region column: '%s' → region", reg_col))
  } else {
    cr[, region := NA_character_]
  }
}

# reporting_state
if (!"reporting_state" %in% names(cr)) {
  state_col <- grep("reporting_state|^state$|state_code|^st$|state_name", names(cr),
                     ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(state_col)) {
    setnames(cr, state_col, "reporting_state")
    message(sprintf("  State column: '%s' → reporting_state", state_col))
  } else {
    cr[, reporting_state := NA_character_]
  }
}

# ── Filter to active CUs with assets ────────────────────
cr <- cr[!is.na(assets_tot) & assets_tot > 0 & !is.na(join_number)]
cr <- cr[!is.na(cu_type) & cu_type %in% c(1L, 2L)]
setorderv(cr, c("join_number", "date"))

message(sprintf("  After filtering: %s rows, %s unique CUs",
                format(nrow(cr), big.mark=","),
                format(uniqueN(cr$join_number), big.mark=",")))

# ── Only forecast CUs that are active as of 2025 Q3 ─────
ACTIVE_QTR <- zoo::as.yearqtr("2025 Q3")
active_jns <- cr[date == ACTIVE_QTR & assets_tot > 0, unique(join_number)]
cr <- cr[join_number %in% active_jns]
message(sprintf("  Active CUs as of %s: %s (inactive dropped)",
                as.character(ACTIVE_QTR), format(length(active_jns), big.mark=",")))

# ── Assign current category (latest observation) ────────
assign_bucket <- function(assets) {
  cut(assets, breaks = ASSET_BREAKS, labels = ASSET_LABELS,
      right = TRUE, include.lowest = TRUE)
}

last_obs <- cr[, .SD[which.max(date)], by = join_number]
last_obs[, bucket_now := assign_bucket(assets_tot)]
last_obs[, cu_type_label := fifelse(cu_type == 1L, "FCU", "FISCU")]

message(sprintf("  Latest quarter: %s", as.character(max(cr$date))))
message(sprintf("  FCUs: %s  |  FISCUs: %s",
                format(sum(last_obs$cu_type == 1L), big.mark=","),
                format(sum(last_obs$cu_type == 2L), big.mark=",")))

# ── Build CU task list ───────────────────────────────────
# One task per CU
cu_all <- split(cr[, .(join_number, date, assets_tot)], by = "join_number")
cu_nobs <- vapply(cu_all, nrow, integer(1))

# Split: ARIMA for >=20Q, simple growth rate for 4-19Q, drop <4Q
cu_list_arima  <- cu_all[cu_nobs >= MIN_OBS]
cu_list_growth <- cu_all[cu_nobs >= 4L & cu_nobs < MIN_OBS]
cu_list_drop   <- cu_all[cu_nobs < 4L]

message(sprintf("  CUs for ARIMA (>= %d quarters): %s",
                MIN_OBS, format(length(cu_list_arima), big.mark=",")))
message(sprintf("  CUs for growth-rate (4-%d quarters): %s",
                MIN_OBS - 1L, format(length(cu_list_growth), big.mark=",")))
message(sprintf("  CUs dropped (< 4 quarters): %s",
                format(length(cu_list_drop), big.mark=",")))

# ════════════════════════════════════════════════════════════
# 3. WORKER FUNCTIONS
# ════════════════════════════════════════════════════════════

# ── 3A: Growth-rate forecaster (for short series) ────────
# Uses last 3 years (12Q) or all available data to compute
# average annualised growth, then projects forward.
forecast_growth_rate <- function(cu_dt) {

  suppressPackageStartupMessages(library(data.table))

  jn <- cu_dt$join_number[1]
  setorderv(cu_dt, "date")
  n <- nrow(cu_dt)
  if (n < 4L) return(NULL)

  assets_now <- tail(cu_dt$assets_tot, 1L)
  if (is.na(assets_now) || assets_now <= 0) return(NULL)

  # Use last 12 quarters (3 years) or all available
  recent <- tail(cu_dt, min(12L, n))
  first_asset <- recent$assets_tot[1]
  last_asset  <- tail(recent$assets_tot, 1L)
  n_years     <- nrow(recent) / 4  # quarters to years

  if (is.na(first_asset) || first_asset <= 0 || n_years < 0.5) return(NULL)

  # Annualised growth rate: (end/start)^(1/years) - 1
  annual_gr <- (last_asset / first_asset) ^ (1 / n_years) - 1

  # Cap at MAX_ANNUAL_GROWTH
  annual_gr <- max(-0.50, min(annual_gr, MAX_ANNUAL_GROWTH / 100))

  assets_1y <- assets_now * (1 + annual_gr) ^ 1
  assets_3y <- assets_now * (1 + annual_gr) ^ 3
  assets_5y <- assets_now * (1 + annual_gr) ^ 5

  assign_b <- function(a) {
    if (is.na(a) || a <= 0) return(NA_character_)
    as.character(cut(a, breaks = c(0, 10e6, 50e6, 100e6, 500e6, 1e9, 10e9, Inf),
                     labels = c("1_Less_10M","2_10M_50M","3_50M_100M",
                                "4_100M_500M","5_500M_1B","6_1B_10B","7_10B_Plus"),
                     right = TRUE, include.lowest = TRUE))
  }

  data.table(
    join_number    = jn,
    n_quarters     = n,
    assets_now     = round(assets_now),
    assets_1yr     = round(assets_1y),
    assets_3yr     = round(assets_3y),
    assets_5yr     = round(assets_5y),
    bucket_now     = assign_b(assets_now),
    bucket_1yr     = assign_b(assets_1y),
    bucket_3yr     = assign_b(assets_3y),
    bucket_5yr     = assign_b(assets_5y),
    arima_order    = sprintf("GrowthRate(%.1f%%/yr)", annual_gr * 100),
    capped         = (abs(annual_gr) >= MAX_ANNUAL_GROWTH / 100 - 0.001)
  )
}

# ── 3B: ARIMA forecaster (for long series) ───────────────

forecast_one_cu <- function(cu_dt) {

  suppressPackageStartupMessages({
    library(data.table)
    library(zoo)
    library(forecast)
  })

  jn <- cu_dt$join_number[1]
  setorderv(cu_dt, "date")

  # Use log(assets) for ARIMA — multiplicative growth → additive in log space
  y <- log(pmax(cu_dt$assets_tot, 1))
  n <- length(y)
  if (n < 8L) return(NULL)

  # Build ts object
  start_yr <- as.integer(format(zoo::as.yearqtr(min(cu_dt$date)), "%Y"))
  start_q  <- as.integer(cycle(zoo::as.yearqtr(min(cu_dt$date))))
  y_ts <- ts(y, frequency = 4L, start = c(start_yr, start_q))

  # Fit auto.arima (fast settings for thousands of CUs)
  fit <- tryCatch(
    forecast::auto.arima(y_ts, stepwise = TRUE, approximation = TRUE,
                         max.p = 2L, max.q = 2L, max.P = 1L, max.Q = 1L,
                         max.d = 2L, max.D = 1L),
    error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  # Forecast 20 quarters (5 years)
  fc <- tryCatch(
    forecast::forecast(fit, h = 20L, level = 80),
    error = function(e) NULL)
  if (is.null(fc)) return(NULL)

  # Back-transform from log to level
  fc_mean <- exp(as.numeric(fc$mean))

  # Current assets (last observation)
  assets_now <- tail(cu_dt$assets_tot, 1L)

  # ── Sanity check: cap forecast at MAX_ANNUAL_GROWTH ────
  # Maximum allowed asset level at each horizon based on sustained max growth
  # E.g., 40% annual → multiplier = 1.40^years
  cap_1y <- assets_now * (1 + MAX_ANNUAL_GROWTH / 100) ^ 1
  cap_3y <- assets_now * (1 + MAX_ANNUAL_GROWTH / 100) ^ 3
  cap_5y <- assets_now * (1 + MAX_ANNUAL_GROWTH / 100) ^ 5

  # Also set a floor: assets shouldn't drop below 50% of current in 1yr,
  # 25% in 3yr, or 10% in 5yr (unless CU is truly tiny)
  floor_1y <- assets_now * 0.50
  floor_3y <- assets_now * 0.25
  floor_5y <- assets_now * 0.10

  # Extract and cap
  raw_1y <- fc_mean[min(4L,  length(fc_mean))]
  raw_3y <- fc_mean[min(12L, length(fc_mean))]
  raw_5y <- fc_mean[min(20L, length(fc_mean))]

  assets_1y <- min(max(raw_1y, floor_1y), cap_1y)
  assets_3y <- min(max(raw_3y, floor_3y), cap_3y)
  assets_5y <- min(max(raw_5y, floor_5y), cap_5y)

  # Assign buckets
  assign_b <- function(a) {
    if (is.na(a) || a <= 0) return(NA_character_)
    as.character(cut(a, breaks = c(0, 10e6, 50e6, 100e6, 500e6, 1e9, 10e9, Inf),
                     labels = c("1_Less_10M","2_10M_50M","3_50M_100M",
                                "4_100M_500M","5_500M_1B","6_1B_10B","7_10B_Plus"),
                     right = TRUE, include.lowest = TRUE))
  }

  data.table(
    join_number    = jn,
    n_quarters     = n,
    assets_now     = round(assets_now),
    assets_1yr     = round(assets_1y),
    assets_3yr     = round(assets_3y),
    assets_5yr     = round(assets_5y),
    bucket_now     = assign_b(assets_now),
    bucket_1yr     = assign_b(assets_1y),
    bucket_3yr     = assign_b(assets_3y),
    bucket_5yr     = assign_b(assets_5y),
    arima_order    = paste0("ARIMA(",
                     paste(forecast::arimaorder(fit), collapse = ","), ")"),
    capped         = (raw_1y != assets_1y) | (raw_3y != assets_3y) | (raw_5y != assets_5y)
  )
}

# ════════════════════════════════════════════════════════════
# 4. RUN FORECASTS
# ════════════════════════════════════════════════════════════

# ── 4A: Growth-rate CUs (fast, no parallel needed) ───────
message(sprintf("\n[2a] Growth-rate forecasting %s short-history CUs...",
                format(length(cu_list_growth), big.mark=",")))
tic("Growth-rate forecasts")

growth_results <- lapply(cu_list_growth, function(cu_dt) {
  tryCatch(forecast_growth_rate(cu_dt), error = function(e) NULL)
})
growth_results <- growth_results[!vapply(growth_results, is.null, logical(1))]
message(sprintf("  Growth-rate complete: %d / %d",
                length(growth_results), length(cu_list_growth)))
toc()

# ── 4B: ARIMA CUs (parallel) ────────────────────────────
message(sprintf("\n[2b] ARIMA forecasting %s CUs across %d cores...",
                format(length(cu_list_arima), big.mark=","), N_CORES))
message("  This may take 10-30 minutes depending on hardware.")
tic("CU-level ARIMA")

cl <- makeCluster(N_CORES)
clusterExport(cl, varlist = c("forecast_one_cu", "MAX_ANNUAL_GROWTH"),
              envir = environment())

# Process in batches for progress reporting
batch_size <- max(100L, length(cu_list_arima) %/% 20L)
n_batches  <- ceiling(length(cu_list_arima) / batch_size)
arima_results <- list()

for (b in 1:n_batches) {
  idx_start <- (b - 1L) * batch_size + 1L
  idx_end   <- min(b * batch_size, length(cu_list_arima))
  batch     <- cu_list_arima[idx_start:idx_end]

  batch_res <- tryCatch(
    parLapply(cl, batch, function(cu_dt) {
      tryCatch(forecast_one_cu(cu_dt), error = function(e) NULL)
    }),
    error = function(e) {
      message(sprintf("  [BATCH %d ERROR] %s — running sequentially", b, conditionMessage(e)))
      lapply(batch, function(cu_dt) {
        tryCatch(forecast_one_cu(cu_dt), error = function(e) NULL)
      })
    }
  )

  arima_results <- c(arima_results, batch_res)
  pct <- round(idx_end / length(cu_list_arima) * 100)
  message(sprintf("  Batch %d/%d complete (%d%% — %d CUs done)",
                  b, n_batches, pct, idx_end))
}

tryCatch(stopCluster(cl), error = function(e) NULL)
message("  Cluster stopped.")
toc()

# ── 4C: Combine ARIMA + growth-rate results ──────────────
arima_results <- arima_results[!vapply(arima_results, is.null, logical(1))]
all_results <- c(arima_results, growth_results)

if (length(all_results) == 0L) {
  stop("All CU forecasts failed. Check data quality.")
}

forecasts <- rbindlist(all_results, fill = TRUE)

n_total_eligible <- length(cu_list_arima) + length(cu_list_growth)
n_arima_ok  <- length(arima_results)
n_growth_ok <- length(growth_results)
message(sprintf("  Successfully forecast: %s / %s CUs (%.1f%%)",
                format(nrow(forecasts), big.mark=","),
                format(n_total_eligible, big.mark=","),
                nrow(forecasts) / max(n_total_eligible, 1) * 100))
message(sprintf("    ARIMA models: %d  |  Growth-rate models: %d", n_arima_ok, n_growth_ok))
n_capped <- sum(forecasts$capped, na.rm = TRUE)
message(sprintf("  Growth-rate capped: %d CUs (%.1f%%)",
                n_capped, n_capped / nrow(forecasts) * 100))

# ── Post-processing: enforce maximum category jumps ──────
# A CU cannot jump more than MAX_JUMP_xY categories per horizon
# If it does, clamp the bucket to current + MAX_JUMP
message("  Applying category jump caps...")

bnum <- function(b) as.integer(substr(as.character(b), 1, 1))
bname <- function(n) {
  n <- pmax(1L, pmin(7L, as.integer(n)))
  ASSET_LABELS[n]
}

forecasts[, bnum_now := bnum(bucket_now)]

# 1-year cap
forecasts[, bnum_1yr_raw := bnum(bucket_1yr)]
forecasts[, bnum_1yr_cap := pmax(bnum_now - MAX_JUMP_1Y, pmin(bnum_now + MAX_JUMP_1Y, bnum_1yr_raw))]
forecasts[bnum_1yr_raw != bnum_1yr_cap, bucket_1yr := bname(bnum_1yr_cap)]

# 3-year cap
forecasts[, bnum_3yr_raw := bnum(bucket_3yr)]
forecasts[, bnum_3yr_cap := pmax(bnum_now - MAX_JUMP_3Y, pmin(bnum_now + MAX_JUMP_3Y, bnum_3yr_raw))]
forecasts[bnum_3yr_raw != bnum_3yr_cap, bucket_3yr := bname(bnum_3yr_cap)]

# 5-year cap
forecasts[, bnum_5yr_raw := bnum(bucket_5yr)]
forecasts[, bnum_5yr_cap := pmax(bnum_now - MAX_JUMP_5Y, pmin(bnum_now + MAX_JUMP_5Y, bnum_5yr_raw))]
forecasts[bnum_5yr_raw != bnum_5yr_cap, bucket_5yr := bname(bnum_5yr_cap)]

n_jump_capped <- sum(forecasts$bnum_1yr_raw != forecasts$bnum_1yr_cap, na.rm=TRUE) +
                 sum(forecasts$bnum_3yr_raw != forecasts$bnum_3yr_cap, na.rm=TRUE) +
                 sum(forecasts$bnum_5yr_raw != forecasts$bnum_5yr_cap, na.rm=TRUE)
message(sprintf("  Category-jump capped: %d forecast-horizons across all CUs", n_jump_capped))

# Clean up temp columns
forecasts[, c("bnum_now","bnum_1yr_raw","bnum_1yr_cap",
              "bnum_3yr_raw","bnum_3yr_cap","bnum_5yr_raw","bnum_5yr_cap") := NULL]

# ════════════════════════════════════════════════════════════
# 5. MERGE CU METADATA
# ════════════════════════════════════════════════════════════
message("\n[3] Merging CU metadata...")

meta <- last_obs[, .(join_number, cu_name, cu_type, cu_type_label,
                      region, reporting_state)]
fc_full <- merge(forecasts, meta, by = "join_number", all.x = TRUE)

# Separate FCU and FISCU
fc_fcu   <- fc_full[cu_type == 1L]
fc_fiscu <- fc_full[cu_type == 2L]

message(sprintf("  FCU forecasts:   %s", format(nrow(fc_fcu), big.mark=",")))
message(sprintf("  FISCU forecasts: %s", format(nrow(fc_fiscu), big.mark=",")))

# Save raw data.tables for downstream scripts (8a, 9)
saveRDS(fc_fcu,   file.path(RESULT_DIR, "fc_fcu.rds"))
saveRDS(fc_fiscu, file.path(RESULT_DIR, "fc_fiscu.rds"))
message("  Raw RDS saved: fc_fcu.rds, fc_fiscu.rds")
# ════════════════════════════════════════════════════════════
# 6. EXCEL OUTPUT
# ════════════════════════════════════════════════════════════
message("\n[4] Saving Excel outputs...")

format_excel_output <- function(dt) {
  # Numeric category: extract leading digit from bucket label
  bucket_to_num <- function(b) as.integer(substr(as.character(b), 1, 1))

  out <- dt[, .(
    `Join Number`          = join_number,
    `CU Name`              = cu_name,
    Region                 = region,
    State                  = reporting_state,
    `Current Assets ($)`   = assets_now,
    `Asset Category (Now)` = bucket_now,
    `Cat # Now`            = bucket_to_num(bucket_now),
    `Projected Assets 1Yr` = assets_1yr,
    `Category 1Yr`         = bucket_1yr,
    `Cat # 1Yr`            = bucket_to_num(bucket_1yr),
    `Projected Assets 3Yr` = assets_3yr,
    `Category 3Yr`         = bucket_3yr,
    `Cat # 3Yr`            = bucket_to_num(bucket_3yr),
    `Projected Assets 5Yr` = assets_5yr,
    `Category 5Yr`         = bucket_5yr,
    `Cat # 5Yr`            = bucket_to_num(bucket_5yr),
    `ARIMA Model`          = arima_order,
    `Obs (Quarters)`       = n_quarters
  )]
  setorderv(out, "Current Assets ($)", order = -1L)
  out
}

# CSV always (fallback)
fwrite(format_excel_output(fc_fcu),   file.path(RESULT_DIR, "fcu_forecasts.csv"))
fwrite(format_excel_output(fc_fiscu), file.path(RESULT_DIR, "fiscu_forecasts.csv"))
message("  CSVs saved.")

# Excel with formatting
if (requireNamespace("openxlsx", quietly = TRUE)) {
  tryCatch({
    library(openxlsx)

    write_forecast_xlsx <- function(dt, filename, title) {
      wb <- createWorkbook()
      out <- format_excel_output(dt)

      addWorksheet(wb, "Forecasts")
      writeData(wb, "Forecasts", x = title, startRow = 1)
      writeData(wb, "Forecasts",
        x = sprintf("Individual CU asset forecasts — %s CUs  |  Generated %s",
                     format(nrow(out), big.mark=","), Sys.Date()),
        startRow = 2)
      writeData(wb, "Forecasts", x = out, startRow = 4,
        headerStyle = createStyle(textDecoration = "bold", fgFill = "#D9E2F3",
                                   border = "TopBottomLeftRight", halign = "center"))

      # Format asset columns as currency
      money_cols <- which(names(out) %in% c("Current Assets ($)", "Projected Assets 1Yr",
                                              "Projected Assets 3Yr", "Projected Assets 5Yr"))
      for (mc in money_cols) {
        addStyle(wb, "Forecasts",
          createStyle(numFmt = "$#,##0", halign = "right"),
          rows = 5:(4 + nrow(out)), cols = mc, gridExpand = TRUE)
      }

      # Highlight category changes (cap at 5000 rows for performance)
      n_style_rows <- min(nrow(out), 5000L)
      for (r in 1:n_style_rows) {
        for (horizon in c("Category 1Yr", "Category 3Yr", "Category 5Yr")) {
          col_idx <- which(names(out) == horizon)
          now_val <- out[r, `Asset Category (Now)`]
          fut_val <- out[[horizon]][r]
          if (!is.na(now_val) && !is.na(fut_val) && now_val != fut_val) {
            # Green if moving up, red if moving down
            now_num <- match(now_val, ASSET_LABELS)
            fut_num <- match(fut_val, ASSET_LABELS)
            if (!is.na(now_num) && !is.na(fut_num)) {
              fill_clr <- if (fut_num > now_num) "#E8F5E9" else "#FFEBEE"
              addStyle(wb, "Forecasts",
                createStyle(fgFill = fill_clr, border = "TopBottomLeftRight"),
                rows = r + 4, cols = col_idx, stack = TRUE)
            }
          }
        }
      }

      setColWidths(wb, "Forecasts", cols = 1:ncol(out),
                   widths = c(12, 30, 8, 8, 16, 16, 16, 16, 16, 16, 16, 16, 22, 10))
      addStyle(wb, "Forecasts",
               createStyle(fontSize = 14, textDecoration = "bold"), rows = 1, cols = 1)
      addStyle(wb, "Forecasts",
               createStyle(fontSize = 10, fontColour = "#666666"), rows = 2, cols = 1)

      # Summary sheet
      addWorksheet(wb, "Migration Summary")
      summary_dt <- dt[, .(
        `CUs Now`     = .N,
        `Move Up 1Yr` = sum(match(bucket_1yr, ASSET_LABELS) > match(bucket_now, ASSET_LABELS), na.rm=TRUE),
        `Stay 1Yr`    = sum(bucket_1yr == bucket_now, na.rm=TRUE),
        `Move Down 1Yr` = sum(match(bucket_1yr, ASSET_LABELS) < match(bucket_now, ASSET_LABELS), na.rm=TRUE),
        `Move Up 5Yr` = sum(match(bucket_5yr, ASSET_LABELS) > match(bucket_now, ASSET_LABELS), na.rm=TRUE),
        `Stay 5Yr`    = sum(bucket_5yr == bucket_now, na.rm=TRUE),
        `Move Down 5Yr` = sum(match(bucket_5yr, ASSET_LABELS) < match(bucket_now, ASSET_LABELS), na.rm=TRUE)
      ), by = .(bucket_now)]
      setorderv(summary_dt, "bucket_now")
      setnames(summary_dt, "bucket_now", "Current Category")

      writeData(wb, "Migration Summary", x = "Category Migration Summary", startRow = 1)
      writeData(wb, "Migration Summary", x = summary_dt, startRow = 3,
        headerStyle = createStyle(textDecoration = "bold", fgFill = "#E8F5E9",
                                   border = "TopBottomLeftRight"))
      setColWidths(wb, "Migration Summary", cols = 1:ncol(summary_dt),
                   widths = c(18, rep(14, ncol(summary_dt)-1)))

      saveWorkbook(wb, file.path(RESULT_DIR, filename), overwrite = TRUE)
    }

    write_forecast_xlsx(fc_fcu, "fcu_individual_forecasts.xlsx",
                        "FCU Individual CU Forecasts — Asset Category Migration")
    write_forecast_xlsx(fc_fiscu, "fiscu_individual_forecasts.xlsx",
                        "FISCU Individual CU Forecasts — Asset Category Migration")
    message("  Excel files saved.")
  }, error = function(e) message(sprintf("  [EXCEL WARN] %s", conditionMessage(e))))
} else {
  message("  [NOTE] openxlsx not installed — CSV files available")
}

# ════════════════════════════════════════════════════════════
# 7. PUBLICATION CHARTS
# ════════════════════════════════════════════════════════════
message("\n[5] Generating publication charts...")

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

cat_colors <- c(
  "1_Less_10M"  = "#E74C3C", "2_10M_50M"   = "#E67E22",
  "3_50M_100M"  = "#F1C40F", "4_100M_500M" = "#2ECC71",
  "5_500M_1B"   = "#3498DB", "6_1B_10B"    = "#2C3E50",
  "7_10B_Plus"  = "#8E44AD"
)

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

# ── Helper: build migration flow data ────────────────────
build_migration <- function(dt, horizon_col, horizon_label) {
  dt_m <- dt[!is.na(bucket_now) & !is.na(get(horizon_col))]
  dt_m[, from := bucket_now]
  dt_m[, to   := get(horizon_col)]
  dt_m[, moved := from != to]
  dt_m[, direction := fcase(
    match(to, ASSET_LABELS) > match(from, ASSET_LABELS), "Moved Up",
    match(to, ASSET_LABELS) < match(from, ASSET_LABELS), "Moved Down",
    default = "Same Category"
  )]
  dt_m[, horizon := horizon_label]
  dt_m
}

# Build migration data for all horizons
for (type_label in c("FCU", "FISCU")) {
  dt <- if (type_label == "FCU") fc_fcu else fc_fiscu
  if (nrow(dt) == 0) next

  mig_all <- rbindlist(list(
    build_migration(dt, "bucket_1yr", "+1 Year"),
    build_migration(dt, "bucket_3yr", "+3 Years"),
    build_migration(dt, "bucket_5yr", "+5 Years")
  ))

  # ── D1: Migration Summary Bar Chart ──────────────────
  message(sprintf("  Chart D1_%s: Migration summary...", type_label))
  mig_summ <- mig_all[, .(n = .N), by = .(horizon, direction)]
  mig_summ[, total := sum(n), by = horizon]
  mig_summ[, pct := n / total * 100]
  mig_summ[, horizon := factor(horizon, levels = c("+1 Year", "+3 Years", "+5 Years"))]

  dir_colors <- c("Moved Up" = pal_green, "Same Category" = pal_sky, "Moved Down" = pal_coral)

  p_d1 <- ggplot(mig_summ, aes(x = horizon, y = pct, fill = direction)) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(aes(label = sprintf("%.0f%%", pct)),
              position = position_stack(vjust = 0.5), size = 3.5,
              color = "white", fontface = "bold") +
    scale_fill_manual(values = dir_colors, name = "Migration Direction") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = sprintf("%s Category Migration — How Many CUs Change Size Category?", type_label),
      subtitle = sprintf("Percentage of %s %ss projected to move up, stay, or move down in asset category",
                         format(nrow(dt), big.mark=","), type_label),
      x = "Forecast Horizon", y = "Share of CUs (%)",
      caption = "Based on individual ARIMA forecasts of each CU's asset trajectory"
    ) +
    theme_pub
  save_pub(p_d1, sprintf("D1_%s_migration_summary.pdf", tolower(type_label)), w = 10, h = 7)

  # ── D2: Category Composition — Now vs 1yr vs 3yr vs 5yr ─
  message(sprintf("  Chart D2_%s: Composition comparison...", type_label))
  comp_list <- list()
  for (bkt_col_info in list(
    list(col = "bucket_now", label = "Now"),
    list(col = "bucket_1yr", label = "+1 Year"),
    list(col = "bucket_3yr", label = "+3 Years"),
    list(col = "bucket_5yr", label = "+5 Years"))) {
    tmp <- dt[!is.na(get(bkt_col_info$col)), .N, by = c(bkt_col_info$col)]
    setnames(tmp, c("category", "n"))
    tmp[, period := bkt_col_info$label]
    comp_list[[length(comp_list) + 1L]] <- tmp
  }
  comp_all <- rbindlist(comp_list)
  comp_all[, total := sum(n), by = period]
  comp_all[, pct := n / total * 100]
  comp_all[, period := factor(period, levels = c("Now", "+1 Year", "+3 Years", "+5 Years"))]

  p_d2 <- ggplot(comp_all, aes(x = period, y = pct, fill = category)) +
    geom_col(width = 0.6, alpha = 0.9, color = "white", linewidth = 0.3) +
    scale_fill_manual(values = cat_colors, name = "Asset Category") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = sprintf("%s System Composition — Now Through 5 Years", type_label),
      subtitle = "How the distribution of CUs across asset categories shifts at each forecast horizon",
      x = NULL, y = "Share of CUs (%)",
      caption = "Each bar sums to 100%  |  Colors represent NCUA asset-size categories"
    ) +
    theme_pub +
    theme(legend.position = "right")
  save_pub(p_d2, sprintf("D2_%s_composition_shift.pdf", tolower(type_label)), w = 12, h = 7)

  # ── D3: Net Migration Flows — All Horizons ───────────────
  message(sprintf("  Chart D3_%s: Net migration flows...", type_label))

  net_list <- list()
  for (hz_info in list(
    list(col = "bucket_1yr", label = "+1 Year"),
    list(col = "bucket_3yr", label = "+3 Years"),
    list(col = "bucket_5yr", label = "+5 Years"))) {

    flow <- dt[!is.na(bucket_now) & !is.na(get(hz_info$col)),
               .N, by = .(from = bucket_now, to = get(hz_info$col))]
    setnames(flow, "to", "to_cat")
    flow_moved <- flow[from != to_cat]
    if (nrow(flow_moved) == 0) next

    nc <- merge(
      flow_moved[, .(outflow = sum(N)), by = .(cat = from)],
      flow_moved[, .(inflow = sum(N)), by = .(cat = to_cat)],
      by = "cat", all = TRUE)
    nc[is.na(outflow), outflow := 0L]
    nc[is.na(inflow), inflow := 0L]
    nc[, net := inflow - outflow]
    nc[, horizon := hz_info$label]
    net_list[[length(net_list) + 1L]] <- nc
  }

  if (length(net_list) > 0) {
    net_all <- rbindlist(net_list, fill = TRUE)
    net_all[, cat := factor(cat, levels = ASSET_LABELS)]
    net_all[, horizon := factor(horizon, levels = c("+1 Year", "+3 Years", "+5 Years"))]

    p_d3 <- ggplot(net_all, aes(x = cat, y = net, fill = fifelse(net >= 0, "Gaining", "Losing"))) +
      geom_col(width = 0.6, alpha = 0.9) +
      geom_text(aes(label = sprintf("%+d", net)),
                vjust = fifelse(net_all$net >= 0, -0.3, 1.3),
                size = 2.8, color = "#444444") +
      geom_hline(yintercept = 0, color = "#999999", linewidth = 0.4) +
      facet_wrap(~horizon, ncol = 1) +
      scale_fill_manual(values = c("Gaining" = pal_green, "Losing" = pal_coral),
                        name = "Net Flow") +
      labs(
        title = sprintf("%s — Net Category Migration at Each Horizon", type_label),
        subtitle = "Positive = category gains CUs  |  Negative = loses CUs  |  Faceted by forecast horizon",
        x = "Asset Category", y = "Net Change in Number of CUs",
        caption = "Based on individual ARIMA forecasts of each CU's asset trajectory"
      ) +
      theme_pub +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
            strip.text = element_text(face = "bold", size = 11))
    save_pub(p_d3, sprintf("D3_%s_net_migration_all.pdf", tolower(type_label)), w = 12, h = 12)
  }

  # ── D4: Transition Matrix Heatmaps ─────────────────────
  # Shows FROM (row) → TO (column) counts for each horizon
  message(sprintf("  Chart D4_%s: Transition matrices...", type_label))

  for (hz_info in list(
    list(col = "bucket_1yr", label = "1 Year", short = "1yr"),
    list(col = "bucket_3yr", label = "3 Years", short = "3yr"),
    list(col = "bucket_5yr", label = "5 Years", short = "5yr"))) {

    flow_dt <- dt[!is.na(bucket_now) & !is.na(get(hz_info$col)),
                  .N, by = .(from = bucket_now, to = get(hz_info$col))]
    setnames(flow_dt, "to", "to_cat")

    # Compute percentages (% of each FROM category)
    flow_dt[, from_total := sum(N), by = from]
    flow_dt[, pct := N / from_total * 100]
    flow_dt[, from := factor(from, levels = rev(ASSET_LABELS))]
    flow_dt[, to_cat := factor(to_cat, levels = ASSET_LABELS)]

    # Color: diagonal = blue (stay), off-diagonal = intensity by count
    flow_dt[, is_same := as.character(from) == as.character(to_cat)]

    p_d4 <- ggplot(flow_dt, aes(x = to_cat, y = from, fill = pct)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = sprintf("%d\n(%.0f%%)", N, pct),
                    color = ifelse(pct > 50, "high", "low")),
                size = 3, fontface = "bold", lineheight = 0.85, show.legend = FALSE) +
      scale_color_manual(values = c("high" = "white", "low" = "#333333")) +
      scale_fill_gradient2(low = "#FAFAFA", mid = pal_sky, high = pal_navy,
                           midpoint = 30, name = "% of\nSource",
                           guide = guide_colorbar(barwidth = 1.2, barheight = 8)) +
      labs(
        title = sprintf("%s Transition Matrix — Where Do CUs Move in %s?",
                        type_label, hz_info$label),
        subtitle = "Rows = current category  |  Columns = projected category\nDiagonal = CUs that stay in same category  |  Off-diagonal = movers",
        x = sprintf("Projected Category (%s Out)", hz_info$label),
        y = "Current Category (2025 Q3)",
        caption = sprintf("Each cell: count of CUs (and %% of source category)  |  %s %ss  |  Darker = more CUs",
                          format(nrow(dt), big.mark=","), type_label)
      ) +
      theme_pub +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
            axis.text.y = element_text(size = 9, face = "bold"),
            legend.position = "right")
    save_pub(p_d4, sprintf("D4_%s_transition_%s.pdf", tolower(type_label), hz_info$short),
             w = 11, h = 9)
  }
}

# ════════════════════════════════════════════════════════════
# 8. SUMMARY
# ════════════════════════════════════════════════════════════
message("\n============================================================")
message("  DISAGGREGATED FORECASTING COMPLETE")
message("============================================================")
message(sprintf("  CUs forecast: %s / %s (%.1f%%)",
                format(nrow(forecasts), big.mark=","),
                format(n_total_eligible, big.mark=","),
                nrow(forecasts) / max(n_total_eligible, 1) * 100))
message(sprintf("  FCU:   %s forecasts", format(nrow(fc_fcu), big.mark=",")))
message(sprintf("  FISCU: %s forecasts", format(nrow(fc_fiscu), big.mark=",")))
message("")

# Migration summary
for (type_label in c("FCU", "FISCU")) {
  dt <- if (type_label == "FCU") fc_fcu else fc_fiscu
  if (nrow(dt) == 0) next
  n_up   <- sum(match(dt$bucket_5yr, ASSET_LABELS) > match(dt$bucket_now, ASSET_LABELS), na.rm=TRUE)
  n_same <- sum(dt$bucket_5yr == dt$bucket_now, na.rm=TRUE)
  n_down <- sum(match(dt$bucket_5yr, ASSET_LABELS) < match(dt$bucket_now, ASSET_LABELS), na.rm=TRUE)
  message(sprintf("  %s 5-year migration: %d up | %d same | %d down",
                  type_label, n_up, n_same, n_down))
}

message("")
message(sprintf("  Excel: %s/", RESULT_DIR))
message("    fcu_individual_forecasts.xlsx")
message("    fiscu_individual_forecasts.xlsx")
message(sprintf("  Charts: %s/", PLOT_DIR))
message("    D1 — Migration summary (% up/same/down)")
message("    D2 — System composition: Now, +1yr, +3yr, +5yr")
message("    D3 — Net migration flows at each horizon")
message("    D4 — Transition matrices: 1yr, 3yr, 5yr (FROM→TO heatmaps)")
message("============================================================")
