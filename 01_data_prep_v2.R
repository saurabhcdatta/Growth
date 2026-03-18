# =============================================================================
# OIL PRICE SHOCK × CREDIT UNION RESEARCH
# Script 01 — Data Preparation  (v2: streamlined + spillover channel)
# =============================================================================
# Inputs
#   Data/call_report.rds                    NCUA Form 5300 panel (CU × quarter)
#   Data/FRB_Baseline_2026.xlsx             FRB CCAR 2026 Baseline
#   Data/FRB_Severely_Adverse_2026.xlsx     FRB CCAR 2026 Severely Adverse
#   Data/oil_exposure.rds                   Built by 01b_oil_exposure_v2.R
#
# Outputs
#   Data/call_clean.rds     Cleaned call report with exposure + derived vars
#   Data/macro_base.rds     Baseline macro (wide, macro_base_ prefix)
#   Data/macro_severe.rds   Severely adverse macro (wide, macro_severe_ prefix)
#   Data/panel_base.rds     CU × quarter panel merged with baseline macro
#   Data/panel_severe.rds   CU × quarter panel merged with severely adverse
#
# Identifiers : join_number, year, quarter
# Start date  : 2005Q1
# Run order   : 01b must run first to produce Data/oil_exposure.rds
# =============================================================================

# ── Libraries ────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(lubridate)
  library(stringr)
})

msg <- function(...) cat(sprintf(...), "\n")
hdr <- function(s) cat("\n---", s, "---\n")

cat("=================================================================\n")
cat(" OIL SHOCK × CU  |  SCRIPT 01: DATA PREPARATION\n")
cat("=================================================================\n")

# ── CONFIG ────────────────────────────────────────────────────────────────────
START_YEAR   <- 2005L
OIL_STATES   <- c("TX","ND","LA","AK","WY","OK","NM","CO","WV","PA","MT")
ASSET_BREAKS <- c(0, 10e3, 100e3, 1e6, Inf)
ASSET_LABELS <- c("T1_under10M","T2_10to100M","T3_100Mto1B","T4_over1B")
Q_MONTH      <- c("1"=1L,"2"=4L,"3"=7L,"4"=10L)

# =============================================================================
# 1. CALL REPORT — load, clean, classify
# =============================================================================
hdr("SECTION 1: Call Report")

cr <- setDT(readRDS("Data/call_report.rds"))
setnames(cr, tolower(names(cr)))
msg("  Raw: %s rows × %s cols", format(nrow(cr),big.mark=","), ncol(cr))

# ── Identifiers ───────────────────────────────────────────────────────────────
stopifnot("Missing id cols" =
  all(c("join_number","year","quarter") %in% names(cr)))

cr[, `:=`(year    = as.integer(year),
          quarter = as.integer(quarter))]
cr <- cr[year >= START_YEAR]
cr[, `:=`(yyyyqq   = year * 100L + quarter,
          cal_date = as.Date(paste(year, Q_MONTH[as.character(quarter)],
                                   "01", sep="-")))]

msg("  After %dQ1 filter: %s rows | %s unique CUs | %s quarters",
    START_YEAR,
    format(nrow(cr), big.mark=","),
    format(uniqueN(cr$join_number), big.mark=","),
    uniqueN(cr$yyyyqq))

# ── Direct-ratio variables (no normalization needed) ──────────────────────────
direct_ok <- intersect(c("netintmrg","networth","networthalt",
                          "pcanetworth","costfds","roa"), names(cr))
msg("  Direct ratios confirmed: %s", paste(direct_ok, collapse=", "))

# ── Asset tier ────────────────────────────────────────────────────────────────
asset_col <- intersect(c("assets_tot","acct_010"), names(cr))[1]
if (!is.na(asset_col)) {
  cr[, asset_tier := factor(
    findInterval(get(asset_col), ASSET_BREAKS, rightmost.closed=TRUE),
    levels = 1:4, labels = ASSET_LABELS)]
  msg("  Asset tier (using %s):", asset_col)
  print(cr[, .N, by=asset_tier][order(asset_tier)])
}

# ── Preliminary binary oil-state flag (replaced by QCEW in 01b) ──────────────
state_col <- intersect(c("state_code","state"), names(cr))[1]
if (!is.na(state_col)) {
  cr[, oil_state_prelim := toupper(get(state_col)) %in% OIL_STATES]
}

# ── Duplicate check ───────────────────────────────────────────────────────────
dups <- cr[, .N, by=.(join_number,year,quarter)][N > 1, .N]
if (dups > 0) {
  msg("  WARNING: %s duplicate CU-quarter keys — keeping first", dups)
  cr <- unique(cr, by=c("join_number","year","quarter"))
} else {
  msg("  ✓ No duplicates on join_number × year × quarter")
}

# ── CU-level deposit constructs (computed here once, before merges) ───────────
setorderv(cr, c("join_number","year","quarter"))

derived_cu <- list(
  insured_share_growth = quote(
    (insured_tot - shift(insured_tot,4)) / shift(insured_tot,4) * 100),
  cert_growth_yoy = quote(
    (dep_shrcert - shift(dep_shrcert,4)) / shift(dep_shrcert,4) * 100),
  dep_growth_yoy = quote(
    (acct_018 - shift(acct_018,4)) / shift(acct_018,4) * 100),
  cert_share    = quote(dep_shrcert / acct_018),
  loan_to_share = quote(lns_tot / acct_018),
  nim_spread    = quote(yldavgloans - costfds)
)

for (nm in names(derived_cu)) {
  src_vars <- all.vars(derived_cu[[nm]])
  if (all(src_vars %in% names(cr))) {
    if (grepl("yoy|growth", nm)) {
      cr[, (nm) := eval(derived_cu[[nm]]), by = join_number]
    } else {
      cr[, (nm) := eval(derived_cu[[nm]])]
    }
    msg("  ✓ %s constructed", nm)
  } else {
    msg("  SKIP %s (missing: %s)", nm,
        paste(setdiff(src_vars, names(cr)), collapse=", "))
  }
}

saveRDS(cr, "Data/call_clean.rds")
msg("  Saved: Data/call_clean.rds")

# =============================================================================
# 2. FRB SCENARIO LOADER  (single reusable function)
# =============================================================================
hdr("SECTION 2: FRB Scenario Loader")

load_frb <- function(path, prefix) {

  msg("  Loading: %s  [prefix: %s]", basename(path), prefix)

  # Read — try with and without skipping description row
  raw <- tryCatch(
    as.data.table(read_excel(path, .name_repair="unique")),
    error = function(e) stop("Cannot read ", path, ": ", e$message)
  )

  # Drop rows where all values are NA or character descriptions
  raw <- raw[!apply(raw, 1, function(r) all(is.na(r) | !suppressWarnings(!is.na(as.numeric(r[!is.na(r)])))))]

  msg("  Dims after cleaning: %s rows × %s cols", nrow(raw), ncol(raw))

  # ── Detect date column ────────────────────────────────────────────────────
  date_idx <- which(tolower(names(raw)) == "date")[1]
  if (is.na(date_idx)) {
    # Find first column whose values look like years
    date_idx <- which(vapply(raw, function(x) {
      x2 <- suppressWarnings(as.numeric(as.character(x)))
      mean(!is.na(x2) & x2 > 1990 & x2 < 2100) > 0.4 |
        mean(grepl("^\\d{4}", as.character(x), na.rm=TRUE)) > 0.4
    }, logical(1)))[1]
  }
  stopifnot("Date column not found" = !is.na(date_idx))
  date_col <- names(raw)[date_idx]
  msg("  Date column: '%s'", date_col)

  # ── Parse dates ───────────────────────────────────────────────────────────
  # FRB Excel Date column is "YYYY.Q" format (e.g. 1975.1, 2005.4)
  # NOT a standard date — year = floor, quarter = decimal part × 10
  dv <- raw[[date_col]]

  parse_frb_date <- function(x) {
    # Convert to numeric regardless of storage type
    x_num <- suppressWarnings(as.numeric(as.character(x)))

    # Check if it looks like YYYY.Q  (year 1900-2100, decimal 0.1-0.4)
    yr_part <- floor(x_num)
    qn_part <- round((x_num - yr_part) * 10)

    valid <- !is.na(x_num) & yr_part >= 1900 & yr_part <= 2100 &
             qn_part >= 1 & qn_part <= 4

    if (mean(valid, na.rm = TRUE) > 0.8) {
      # YYYY.Q format confirmed
      mo <- c("1"="01","2"="04","3"="07","4"="10")[as.character(qn_part)]
      dates <- as.Date(paste(yr_part, mo, "01", sep="-"))
      attr(dates, "year_src")    <- yr_part
      attr(dates, "quarter_src") <- qn_part
      return(dates)
    }

    # Fallback 1: Excel numeric serial (e.g. 42005 = a real date)
    if (is.numeric(x)) {
      d <- suppressWarnings(as.Date(x, origin = "1899-12-30"))
      if (mean(!is.na(d)) > 0.8) return(d)
    }

    # Fallback 2: ISO string "YYYY-MM-DD" or "YYYY/MM/DD"
    d_chr <- as.character(x)
    d <- suppressWarnings(as.Date(d_chr))
    if (mean(!is.na(d), na.rm = TRUE) > 0.8) return(d)

    # Fallback 3: "YYYY Qn" / "YYYYQn" string
    yr2 <- as.integer(str_extract(d_chr, "\\d{4}"))
    qn2 <- as.integer(str_extract(d_chr, "(?i)(?<=[q ])\\d"))
    mo2 <- c("1"="01","2"="04","3"="07","4"="10")[as.character(qn2)]
    d   <- suppressWarnings(as.Date(paste(yr2, mo2, "01", sep="-")))
    if (mean(!is.na(d), na.rm = TRUE) > 0.8) return(d)

    stop(paste("Cannot parse Date column in", path,
               "\n  Sample values:", paste(head(d_chr, 5), collapse=", ")))
  }

  dates <- parse_frb_date(dv)

  # Extract year/quarter directly from YYYY.Q source when possible
  dv_num  <- suppressWarnings(as.numeric(as.character(dv)))
  yr_src  <- floor(dv_num)
  qn_src  <- round((dv_num - yr_src) * 10)
  use_src <- !is.na(dv_num) & yr_src >= 1900 & qn_src >= 1 & qn_src <= 4

  raw[, date_parsed := dates]
  raw[, year    := fifelse(use_src, as.integer(yr_src),    year(dates))]
  raw[, quarter := fifelse(use_src, as.integer(qn_src),    quarter(dates))]
  raw[, yyyyqq  := year * 100L + quarter]
  raw <- raw[!is.na(date_parsed) & year >= 2004]

  # ── Identify numeric macro columns ────────────────────────────────────────
  id_cols    <- c(date_col, "date_parsed", "year", "quarter", "yyyyqq")
  macro_cols <- setdiff(names(raw), id_cols)
  macro_cols <- macro_cols[vapply(raw[, ..macro_cols],
                                   is.numeric, logical(1))]
  macro_cols <- macro_cols[!grepl("^\\.\\.", macro_cols)]
  msg("  Macro columns identified: %s", length(macro_cols))

  # ── Average to quarterly if sub-quarterly rows exist ─────────────────────
  wide <- raw[, c(.(year=first(year), quarter=first(quarter)),
                  lapply(.SD, function(x) mean(as.numeric(x), na.rm=TRUE))),
              by = yyyyqq, .SDcols = macro_cols]
  wide <- wide[order(yyyyqq)]

  # ── Rename with prefix ────────────────────────────────────────────────────
  new_nms <- paste0(prefix, tolower(macro_cols))
  # Deduplicate if needed
  dupe_nm <- duplicated(new_nms)
  if (any(dupe_nm)) new_nms[dupe_nm] <- paste0(new_nms[dupe_nm],"_v2")
  setnames(wide, macro_cols, new_nms)

  # ── Report key variable coverage ─────────────────────────────────────────
  key <- paste0(prefix, c("pbrent","lurc","pcpi","pcpixfe","rmtg",
                           "phpi","uypsav","ypds","gdps","rff"))
  found   <- intersect(key, names(wide))
  missing <- setdiff(key, names(wide))
  msg("  Key vars found   : %s", paste(found,   collapse=", "))
  if (length(missing)) msg("  Key vars MISSING : %s", paste(missing, collapse=", "))

  msg("  Quarters: %dQ%d – %dQ%d (%d rows)",
      wide[1,year], wide[1,quarter],
      wide[.N,year], wide[.N,quarter], nrow(wide))

  return(wide[])
}

# =============================================================================
# 3. LOAD FRB SCENARIOS
# =============================================================================
hdr("SECTION 3: FRB Scenarios")

macro_base   <- load_frb("Data/FRB_Baseline_2026.xlsx",           "macro_base_")
macro_severe <- load_frb("Data/FRB_Severely_Adverse_2026.xlsx",   "macro_severe_")

saveRDS(macro_base,   "Data/macro_base.rds")
saveRDS(macro_severe, "Data/macro_severe.rds")
msg("  Saved: macro_base.rds (%d cols) | macro_severe.rds (%d cols)",
    ncol(macro_base), ncol(macro_severe))

# =============================================================================
# 4. DERIVED MACRO VARIABLES  (applied to both scenarios)
# =============================================================================
hdr("SECTION 4: Derived Macro Variables")

add_macro_derived <- function(dt, pfx) {

  # Helper: safe column getter
  gc <- function(nm) {
    col <- paste0(pfx, nm)
    if (col %in% names(dt)) dt[[col]] else NULL
  }

  setorderv(dt, "yyyyqq")

  # ── Oil price transforms ──────────────────────────────────────────────────
  pb <- gc("pbrent")
  if (!is.null(pb)) {
    dt[, (paste0(pfx,"yoy_oil"))  := (pb - shift(pb,4)) / shift(pb,4) * 100]
    dt[, (paste0(pfx,"qoq_oil"))  := (pb - shift(pb,1)) / shift(pb,1) * 100]
    dt[, (paste0(pfx,"oil_pos"))  := pmax(get(paste0(pfx,"yoy_oil")), 0, na.rm=TRUE)]
    dt[, (paste0(pfx,"oil_neg"))  := pmin(get(paste0(pfx,"yoy_oil")), 0, na.rm=TRUE)]
    for (k in 1:4)
      dt[, (paste0(pfx,"yoy_oil_lag",k)) := shift(get(paste0(pfx,"yoy_oil")), k)]
    dt[, (paste0(pfx,"oil_rsd4"))  := frollapply(get(paste0(pfx,"qoq_oil")),4,sd,na.rm=TRUE,align="right")]
    dt[, (paste0(pfx,"oil_rmean8")):= frollmean(pb, 8, na.rm=TRUE, align="right")]
    dt[, (paste0(pfx,"oil_cyc"))   := pb - get(paste0(pfx,"oil_rmean8"))]
    msg("  ✓ [%s] Oil transforms (YoY, QoQ, pos/neg, lags 1-4, rsd4, cyc)", pfx)
  }

  # ── Yield curve ───────────────────────────────────────────────────────────
  gs10 <- gc("rs10y") %||% gc("gs10")
  gs3m <- gc("rs3m")  %||% gc("gs3m")
  if (!is.null(gs10) && !is.null(gs3m)) {
    dt[, (paste0(pfx,"yield_curve"))     := gs10 - gs3m]
    dt[, (paste0(pfx,"yield_curve_inv")) := as.integer(gs10 - gs3m < 0)]
    msg("  ✓ [%s] yield_curve, yield_curve_inv", pfx)
  }

  # ── Real rate ─────────────────────────────────────────────────────────────
  rff  <- gc("rff")  %||% gc("fedfunds")
  pcpi <- gc("pcpi") %||% gc("cpi")
  if (!is.null(rff) && !is.null(pcpi)) {
    dt[, (paste0(pfx,"real_rate")) := rff - pcpi]
    msg("  ✓ [%s] real_rate", pfx)
  }

  # ── FOMC regime + hike run ────────────────────────────────────────────────
  if (!is.null(rff)) {
    chg <- c(NA, diff(rff))
    regime <- fifelse(chg >  0.10,  1L,
               fifelse(chg < -0.10, -1L, 0L))
    run <- integer(length(regime))
    for (i in seq_along(regime)) {
      if (is.na(regime[i]) || regime[i] == 0L) {
        run[i] <- 0L
      } else if (i==1 || is.na(regime[i-1]) || regime[i]!=regime[i-1]) {
        run[i] <- regime[i]
      } else {
        run[i] <- run[i-1] + regime[i]
      }
    }
    dt[, (paste0(pfx,"fomc_regime")) := regime]
    dt[, (paste0(pfx,"hike_run"))    := run]
    msg("  ✓ [%s] fomc_regime, hike_run", pfx)
  }

  # ── CPI YoY ───────────────────────────────────────────────────────────────
  if (!is.null(pcpi)) {
    dt[, (paste0(pfx,"cpi_yoy")) :=
         (pcpi - shift(pcpi,4)) / shift(pcpi,4) * 100]
    msg("  ✓ [%s] cpi_yoy", pfx)
  }

  invisible(dt)
}

# Null-coalescing operator
`%||%` <- function(a,b) if (!is.null(a)) a else b

add_macro_derived(macro_base,   "macro_base_")
add_macro_derived(macro_severe, "macro_severe_")

# Overwrite with derived vars
saveRDS(macro_base,   "Data/macro_base.rds")
saveRDS(macro_severe, "Data/macro_severe.rds")

# =============================================================================
# 5. MERGE CALL REPORT × MACRO SCENARIOS
# =============================================================================
hdr("SECTION 5: Panel Assembly")

# Drop id cols from macro before merge (keep only yyyyqq + macro_ cols)
macro_merge <- function(macro_dt) {
  drop <- intersect(c("year","quarter"), names(macro_dt))
  macro_dt[, !drop, with=FALSE]
}

panel_base   <- merge(cr, macro_merge(macro_base),   by="yyyyqq", all.x=TRUE)
panel_severe <- merge(cr, macro_merge(macro_severe),  by="yyyyqq", all.x=TRUE)

# Coverage check
for (nm in c("macro_base_pbrent","macro_severe_pbrent")) {
  pnl <- if (grepl("base",nm)) panel_base else panel_severe
  if (nm %in% names(pnl)) {
    n_ok <- sum(!is.na(pnl[[nm]]))
    msg("  %s coverage: %s/%s rows (%.1f%%)",
        nm, format(n_ok,big.mark=","),
        format(nrow(pnl),big.mark=","), n_ok/nrow(pnl)*100)
  }
}

msg("  panel_base   : %s rows × %s cols",
    format(nrow(panel_base), big.mark=","), ncol(panel_base))
msg("  panel_severe : %s rows × %s cols",
    format(nrow(panel_severe),big.mark=","), ncol(panel_severe))

# =============================================================================
# 6. OIL EXPOSURE MERGE  (from 01b_oil_exposure_v2.R)
# =============================================================================
hdr("SECTION 6: Oil Exposure Merge")

if (file.exists("Data/oil_exposure.rds")) {

  exp_dt <- readRDS("Data/oil_exposure.rds")
  setDT(exp_dt)

  exp_cols <- c("state_code","yyyyqq",
                "mining_emp_share",
                "oil_exposure_cont",
                "oil_exposure_bin",
                "oil_exposure_bin_1pct",
                "oil_exposure_bin_3pct",
                "oil_exposure_tier",
                "oil_exposure_smooth",
                "oil_bartik_iv",
                "spillover_exposure",     # NEW: indirect channel
                "spillover_exposure_wtd", # NEW: trade-weighted version
                "cu_group")               # NEW: Direct / Indirect / Neither

  exp_cols <- intersect(exp_cols, names(exp_dt))
  exp_merge_dt <- exp_dt[, ..exp_cols]

  for (pnl_obj in list(cr, panel_base, panel_severe)) {
    sc <- if (!is.na(state_col)) state_col else "state_code"
    if (sc %in% names(pnl_obj) && "yyyyqq" %in% names(pnl_obj)) {
      # Remove any old exposure cols first
      old <- intersect(exp_cols, names(pnl_obj))
      if (length(old)) pnl_obj[, (old) := NULL]
      # Merge
      merge(pnl_obj, exp_merge_dt,
            by.x = c(sc,"yyyyqq"), by.y = c("state_code","yyyyqq"),
            all.x = TRUE)
    }
  }

  # Re-assign (merge returns new DT; in-place update for each)
  sc <- if (!is.na(state_col)) state_col else "state_code"
  cr           <- merge(cr,           exp_merge_dt, by.x=c(sc,"yyyyqq"),
                        by.y=c("state_code","yyyyqq"), all.x=TRUE)
  panel_base   <- merge(panel_base,   exp_merge_dt, by.x=c(sc,"yyyyqq"),
                        by.y=c("state_code","yyyyqq"), all.x=TRUE)
  panel_severe <- merge(panel_severe, exp_merge_dt, by.x=c(sc,"yyyyqq"),
                        by.y=c("state_code","yyyyqq"), all.x=TRUE)

  # Fill unmatched territories
  fill_cols <- c("oil_exposure_bin","oil_exposure_cont","oil_exposure_bin_1pct",
                 "oil_exposure_bin_3pct","spillover_exposure","spillover_exposure_wtd")
  for (pnl in list(cr, panel_base, panel_severe)) {
    for (col in intersect(fill_cols, names(pnl)))
      pnl[is.na(get(col)), (col) := 0]
    if ("oil_exposure_bin" %in% names(pnl))
      pnl[, oil_exposure_idx := oil_exposure_bin]
  }

  msg("  ✓ Oil exposure merged from oil_exposure.rds")
  if ("cu_group" %in% names(panel_base))
    print(panel_base[, .N, by=cu_group])

} else {
  msg("  WARNING: Data/oil_exposure.rds not found")
  msg("  Run 01b_oil_exposure_v2.R first, then re-run this script")
  # Provisional binary flag so rest of script runs
  for (pnl in list(cr, panel_base, panel_severe)) {
    if (!is.na(state_col) && state_col %in% names(pnl)) {
      pnl[, oil_exposure_idx := as.integer(toupper(get(state_col)) %in% OIL_STATES)]
      pnl[, cu_group := fcase(
        oil_exposure_idx == 1L, "Direct",
        default               = "Indirect"   # provisional — overwritten by 01b
      )]
    }
  }
}

# =============================================================================
# 7. INTERACTION TERMS  (direct + indirect channels)
# =============================================================================
hdr("SECTION 7: Interaction Terms")

add_interactions <- function(pnl, oil_yoy_col) {
  if (!oil_yoy_col %in% names(pnl)) return(invisible(pnl))

  oy <- pnl[[oil_yoy_col]]

  # Direct channel interactions
  if ("oil_exposure_cont" %in% names(pnl)) {
    pnl[, oil_x_brent      := oil_exposure_cont  * oy]   # continuous × YoY
    pnl[, oil_x_brent_bin  := oil_exposure_bin   * oy]   # binary × YoY
    msg("  ✓ oil_x_brent, oil_x_brent_bin")
  }

  # Bartik IV interaction
  if ("oil_bartik_iv" %in% names(pnl)) {
    pnl[, bartik_x_brent := oil_bartik_iv * oy]
    msg("  ✓ bartik_x_brent")
  }

  # Indirect / spillover interactions
  if ("spillover_exposure" %in% names(pnl)) {
    pnl[, spillover_x_brent := spillover_exposure * oy]   # spillover × YoY
    msg("  ✓ spillover_x_brent")
  }
  if ("spillover_exposure_wtd" %in% names(pnl)) {
    pnl[, spillover_wtd_x_brent := spillover_exposure_wtd * oy]
    msg("  ✓ spillover_wtd_x_brent")
  }

  # FOMC regime × oil (deposit migration channel)
  fomc_col <- sub("yoy_oil","fomc_regime", oil_yoy_col)
  if (fomc_col %in% names(pnl)) {
    pnl[, fomc_x_brent := get(fomc_col) * oy]
    msg("  ✓ fomc_x_brent")
  }

  invisible(pnl)
}

add_interactions(panel_base,   "macro_base_yoy_oil")
add_interactions(panel_severe, "macro_severe_yoy_oil")

# =============================================================================
# 8. SAVE FINAL PANELS
# =============================================================================
hdr("SECTION 8: Save")

setorderv(cr,           c("join_number","year","quarter"))
setorderv(panel_base,   c("join_number","year","quarter"))
setorderv(panel_severe, c("join_number","year","quarter"))

saveRDS(cr,           "Data/call_clean.rds")
saveRDS(panel_base,   "Data/panel_base.rds")
saveRDS(panel_severe, "Data/panel_severe.rds")

msg("  call_clean.rds   : %s rows × %s cols",
    format(nrow(cr),big.mark=","), ncol(cr))
msg("  panel_base.rds   : %s rows × %s cols",
    format(nrow(panel_base),big.mark=","), ncol(panel_base))
msg("  panel_severe.rds : %s rows × %s cols",
    format(nrow(panel_severe),big.mark=","), ncol(panel_severe))

# =============================================================================
# 9. DATA QUALITY REPORT
# =============================================================================
hdr("SECTION 9: Data Quality Report")

# ── Call report summary ───────────────────────────────────────────────────────
cat("\n  CALL REPORT\n")
cat(sprintf("  %-22s : %s\n","CU-quarter obs",
            format(nrow(cr),big.mark=",")))
cat(sprintf("  %-22s : %s\n","Unique CUs",
            format(uniqueN(cr$join_number),big.mark=",")))
cat(sprintf("  %-22s : %dQ%d – %dQ%d\n","Quarters",
            min(cr$year), cr[which.min(yyyyqq),quarter],
            max(cr$year), cr[which.max(yyyyqq),quarter]))

if ("asset_tier" %in% names(cr)) {
  cat("  Asset tiers:\n")
  tier_tbl <- cr[,.N,by=asset_tier][order(asset_tier)]
  tier_tbl[, cat(sprintf("    %-20s : %s (%.1f%%)\n",
                          asset_tier,
                          format(N,big.mark=","),
                          N/nrow(cr)*100), by=asset_tier)]
}

if ("cu_group" %in% names(cr)) {
  cat("  CU exposure groups:\n")
  grp_tbl <- cr[,.N,by=cu_group][order(cu_group)]
  grp_tbl[, cat(sprintf("    %-20s : %s (%.1f%%)\n",
                          cu_group,
                          format(N,big.mark=","),
                          N/nrow(cr)*100), by=cu_group)]
}

# ── PBRENT range ──────────────────────────────────────────────────────────────
cat("\n  FRB MACRO PBRENT ($/bbl)\n")
for (nm in c("macro_base_pbrent","macro_severe_pbrent")) {
  pnl <- if (grepl("base",nm)) panel_base else panel_severe
  if (nm %in% names(pnl)) {
    v <- pnl[[nm]]
    cat(sprintf("  %-30s : min=%5.1f  max=%5.1f  latest=%5.1f\n",
                nm, min(v,na.rm=T), max(v,na.rm=T), v[max(which(!is.na(v)))]))
  }
}

# ── Missingness for key variables ─────────────────────────────────────────────
cat("\n  KEY VARIABLE MISSINGNESS (panel_base)\n")
check_vars <- c(
  "join_number","year","quarter",
  # CU outcomes — direct ratios
  "netintmrg","networth","pcanetworth","costfds","roa",
  # Credit quality
  "dq_rate","chg_tot_lns_ratio",
  # Deposit channel
  "insured_tot","dep_shrcert","acct_018",
  "insured_share_growth","cert_share","loan_to_share",
  # Macro — confirmed names
  "macro_base_pbrent","macro_base_lurc","macro_base_pcpi",
  "macro_base_rmtg","macro_base_phpi","macro_base_uypsav",
  "macro_base_yoy_oil","macro_base_yield_curve","macro_base_fomc_regime",
  # Exposure
  "oil_exposure_cont","oil_exposure_bin","spillover_exposure",
  "oil_x_brent","spillover_x_brent","fomc_x_brent","oil_bartik_iv"
)
fv  <- intersect(check_vars, names(panel_base))
pct <- sapply(panel_base[, ..fv],
              function(x) round(mean(is.na(x))*100, 1))
miss_tbl <- data.table(variable=names(pct), pct_missing=pct)[order(-pct_missing)]
print(miss_tbl, row.names=FALSE)

# =============================================================================
# 10. SUMMARY
# =============================================================================
cat("\n=================================================================\n")
cat(" SCRIPT 01 COMPLETE\n")
cat("=================================================================\n")
cat("  Data/call_clean.rds     CU panel + CU-level derived vars\n")
cat("  Data/macro_base.rds     Baseline macro (macro_base_ prefix)\n")
cat("  Data/macro_severe.rds   Severely adverse (macro_severe_ prefix)\n")
cat("  Data/panel_base.rds     Full panel: CU × macro_base + interactions\n")
cat("  Data/panel_severe.rds   Full panel: CU × macro_severe\n")
cat("\n  Direct effect captured via:\n")
cat("    oil_x_brent         oil_exposure_cont × macro_base_yoy_oil\n")
cat("    oil_x_brent_bin     oil_exposure_bin  × macro_base_yoy_oil\n")
cat("    bartik_x_brent      oil_bartik_iv     × macro_base_yoy_oil\n")
cat("  Indirect effect captured via:\n")
cat("    spillover_x_brent   spillover_exposure × macro_base_yoy_oil\n")
cat("    fomc_x_brent        fomc_regime        × macro_base_yoy_oil\n")
cat("    cu_group == 'Indirect': non-oil CUs; β1 = indirect estimate\n")
cat("=================================================================\n")
