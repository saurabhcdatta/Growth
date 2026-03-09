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
MIN_OBS     <- 12L    # minimum quarterly observations per CU
HORIZON_1Y  <- 4L     # quarters for 1-year forecast
HORIZON_3Y  <- 12L    # quarters for 3-year forecast
HORIZON_5Y  <- 20L    # quarters for 5-year forecast

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
if (file.exists("call_report.rds")) {
  cr <- readRDS("call_report.rds")
  setDT(cr)
  message("  Loaded call_report.rds")
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
cu_list <- split(cr[, .(join_number, date, assets_tot)], by = "join_number")

# Filter: need at least MIN_OBS quarters
cu_list <- cu_list[vapply(cu_list, nrow, integer(1)) >= MIN_OBS]
message(sprintf("  CUs with >= %d quarters: %s",
                MIN_OBS, format(length(cu_list), big.mark=",")))

# ════════════════════════════════════════════════════════════
# 3. ARIMA WORKER FUNCTION
# ════════════════════════════════════════════════════════════

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
  fc_lo   <- exp(as.numeric(fc$lower[, 1]))
  fc_hi   <- exp(as.numeric(fc$upper[, 1]))

  # Current assets (last observation)
  assets_now <- tail(cu_dt$assets_tot, 1L)

  # Projected assets at horizons
  assets_1y <- fc_mean[min(4L,  length(fc_mean))]
  assets_3y <- fc_mean[min(12L, length(fc_mean))]
  assets_5y <- fc_mean[min(20L, length(fc_mean))]

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
                     paste(forecast::arimaorder(fit), collapse = ","), ")")
  )
}

# ════════════════════════════════════════════════════════════
# 4. RUN FORECASTS IN PARALLEL
# ════════════════════════════════════════════════════════════
message(sprintf("\n[2] Forecasting %s individual CUs across %d cores...",
                format(length(cu_list), big.mark=","), N_CORES))
message("  This may take 10-30 minutes depending on hardware.")
tic("CU-level ARIMA")

cl <- makeCluster(N_CORES)
clusterExport(cl, varlist = c("forecast_one_cu"), envir = environment())

# Process in batches for progress reporting
batch_size <- max(100L, length(cu_list) %/% 20L)
n_batches  <- ceiling(length(cu_list) / batch_size)
all_results <- list()

for (b in 1:n_batches) {
  idx_start <- (b - 1L) * batch_size + 1L
  idx_end   <- min(b * batch_size, length(cu_list))
  batch     <- cu_list[idx_start:idx_end]

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

  all_results <- c(all_results, batch_res)
  pct <- round(idx_end / length(cu_list) * 100)
  message(sprintf("  Batch %d/%d complete (%d%% — %d CUs done)",
                  b, n_batches, pct, idx_end))
}

tryCatch(stopCluster(cl), error = function(e) NULL)
message("  Cluster stopped.")
toc()

# ── Consolidate ──────────────────────────────────────────
all_results <- all_results[!vapply(all_results, is.null, logical(1))]

if (length(all_results) == 0L) {
  stop(sprintf("All %d CU forecasts failed. Check data quality and MIN_OBS setting (%d).",
               length(cu_list), MIN_OBS))
}

forecasts <- rbindlist(all_results, fill = TRUE)

message(sprintf("  Successfully forecast: %s / %s CUs (%.1f%%)",
                format(nrow(forecasts), big.mark=","),
                format(length(cu_list), big.mark=","),
                nrow(forecasts) / length(cu_list) * 100))

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

# ════════════════════════════════════════════════════════════
# 6. EXCEL OUTPUT
# ════════════════════════════════════════════════════════════
message("\n[4] Saving Excel outputs...")

format_excel_output <- function(dt) {
  out <- dt[, .(
    `Join Number`          = join_number,
    `CU Name`              = cu_name,
    Region                 = region,
    State                  = reporting_state,
    `Current Assets ($)`   = assets_now,
    `Asset Category (Now)` = bucket_now,
    `Projected Assets 1Yr` = assets_1yr,
    `Category 1Yr`         = bucket_1yr,
    `Projected Assets 3Yr` = assets_3yr,
    `Category 3Yr`         = bucket_3yr,
    `Projected Assets 5Yr` = assets_5yr,
    `Category 5Yr`         = bucket_5yr,
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

  # ── D2: Category Composition — Now vs Future ─────────
  message(sprintf("  Chart D2_%s: Composition comparison...", type_label))
  comp_now <- dt[, .N, by = bucket_now]
  setnames(comp_now, c("category", "n"))
  comp_now[, period := "Now"]

  comp_5yr <- dt[!is.na(bucket_5yr), .N, by = bucket_5yr]
  setnames(comp_5yr, c("category", "n"))
  comp_5yr[, period := "+5 Years"]

  comp_all <- rbindlist(list(comp_now, comp_5yr))
  comp_all[, total := sum(n), by = period]
  comp_all[, pct := n / total * 100]
  comp_all[, period := factor(period, levels = c("Now", "+5 Years"))]

  p_d2 <- ggplot(comp_all, aes(x = period, y = pct, fill = category)) +
    geom_col(width = 0.5, alpha = 0.9, color = "white", linewidth = 0.3) +
    scale_fill_manual(values = cat_colors, name = "Asset Category") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = sprintf("%s System Composition — Now vs 5 Years Out", type_label),
      subtitle = "How the distribution of CUs across asset categories is projected to shift",
      x = NULL, y = "Share of CUs (%)",
      caption = "Each bar sums to 100%  |  Colors represent NCUA asset-size categories"
    ) +
    theme_pub +
    theme(legend.position = "right")
  save_pub(p_d2, sprintf("D2_%s_composition_shift.pdf", tolower(type_label)), w = 10, h = 7)

  # ── D3: Net Migration Flows (Sankey-style grouped bar) ──
  message(sprintf("  Chart D3_%s: Net migration flows...", type_label))
  flow_5yr <- dt[!is.na(bucket_now) & !is.na(bucket_5yr),
                  .N, by = .(bucket_now, bucket_5yr)]
  setnames(flow_5yr, c("from", "to", "n_cus"))

  # Net change per category
  net_change <- rbind(
    flow_5yr[, .(net = -sum(n_cus)), by = .(cat = from)],   # outflows
    flow_5yr[, .(net = sum(n_cus)), by = .(cat = to)]       # inflows
  )[, .(net = sum(net)), by = cat]
  # Remove self-flows (they cancel)
  net_change <- merge(
    flow_5yr[from != to, .(outflow = sum(n_cus)), by = .(cat = from)],
    flow_5yr[from != to, .(inflow = sum(n_cus)), by = .(cat = to)],
    by = "cat", all = TRUE)
  net_change[is.na(outflow), outflow := 0L]
  net_change[is.na(inflow), inflow := 0L]
  net_change[, net := inflow - outflow]
  net_change[, cat := factor(cat, levels = ASSET_LABELS)]

  p_d3 <- ggplot(net_change, aes(x = cat, y = net, fill = fifelse(net >= 0, "Gaining", "Losing"))) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(aes(label = sprintf("%+d", net)),
              vjust = fifelse(net_change$net >= 0, -0.3, 1.3),
              size = 3.5, color = "#444444") +
    geom_hline(yintercept = 0, color = "#999999", linewidth = 0.5) +
    scale_fill_manual(values = c("Gaining" = pal_green, "Losing" = pal_coral),
                      name = "Net Flow") +
    labs(
      title = sprintf("%s — Net Category Migration in 5 Years", type_label),
      subtitle = "Positive = category gains CUs from other categories  |  Negative = category loses CUs",
      x = "Asset Category", y = "Net Change in Number of CUs",
      caption = "Reflects projected asset growth paths of individual CUs via ARIMA"
    ) +
    theme_pub +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  save_pub(p_d3, sprintf("D3_%s_net_migration.pdf", tolower(type_label)), w = 12, h = 7)
}

# ════════════════════════════════════════════════════════════
# 8. SUMMARY
# ════════════════════════════════════════════════════════════
message("\n============================================================")
message("  DISAGGREGATED FORECASTING COMPLETE")
message("============================================================")
message(sprintf("  CUs forecast: %s / %s (%.1f%%)",
                format(nrow(forecasts), big.mark=","),
                format(length(cu_list), big.mark=","),
                nrow(forecasts) / length(cu_list) * 100))
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
message("    D2 — System composition now vs 5 years")
message("    D3 — Net migration flows by category")
message("============================================================")
