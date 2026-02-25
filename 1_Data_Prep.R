############################################################
# PART 1 v2.0 — CALL REPORT DATA PREP & FEATURE ENGINEERING
#
# Pipeline:
#   1.  Load raw call report (Stata .dta)
#   2.  Construct key variables (dep vars, categories, M&A flags)
#   3.  Auto-detect level vs ratio → sum or median aggregation
#   4.  Aggregate to quarterly panel by asset category
#   5.  Feature engineering:
#         • YoY % change  (lag-4)
#         • QoQ % change  (lag-1)
#         • Rolling means & SDs  (4q, 8q, 12q)
#         • Momentum  (YoY acceleration)
#         • Cyclical deviation from rolling mean
#         • Lag features  (1q, 2q, 4q, 8q)
#         • Interaction terms  (size × macro proxies)
#         • Regime indicators
#   6.  Save qtrly_enriched.rds
#
# Runtime: ~5-15 min depending on machine
############################################################

library(haven);      library(data.table); library(dplyr)
library(lubridate);  library(zoo);        library(httr)
library(tictoc)
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
tic("PART 1 v2.0 total")
message("=======================================================")
message(sprintf("PART 1 v2.0  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("=======================================================")
notify("Part 1 v2.0 Started",
       format(Sys.time(), "%H:%M"), tags = "rocket")

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

# ════════════════════════════════════════════════════════
# 1. LOAD RAW DATA
# ════════════════════════════════════════════════════════
tic("1. Load raw data")
message("\n[1] Loading raw call report data...")

cr <- haven::read_dta("Data/OCE_CallReport_2025q3_2000tocurrent.dta")
setDT(cr)
message(sprintf("    Raw data: %s rows x %s cols",
                format(nrow(cr), big.mark=","),
                format(ncol(cr), big.mark=",")))
toc()
notify("Data Loaded",
       sprintf("%s rows x %s cols",
               format(nrow(cr), big.mark=","),
               format(ncol(cr), big.mark=",")),
       tags = "white_check_mark")

# ════════════════════════════════════════════════════════
# 2. CONSTRUCT KEY VARIABLES
# ════════════════════════════════════════════════════════
tic("2. Construct key variables")
message("\n[2] Constructing key variables...")

cr[, `:=`(

  # ── Dependent variable indicators ──────────────────────
  # ficu_count  : is this a federally insured CU?  (cu_type==1)
  # fiscu_count : is this a federally insured state CU? (cu_type==2)
  ficu_count  = fifelse(cu_type == 1, 1, 0),
  fiscu_count = fifelse(cu_type == 2, 1, 0),

  # ── Log asset growth (continuous dep var alternative) ──
  # v2 lesson: use log-difference rather than raw YoY% for assets
  # to handle skewness in large-CU size categories
  ln_assets = log(pmax(assets_tot, 1)),   # guard against 0/NA

  # ── Asset size categories (7 buckets, NCUA standard) ───
  # Derived from assets_cat2 field in the data:
  #   6,7 → bucket 6  (1B-10B),  8 → bucket 7 (>10B)
  categories = fifelse(
    assets_cat2 %in% c(6L, 7L), 6L,
    fifelse(assets_cat2 == 8L,  7L, as.integer(assets_cat2))
  ),

  # ── Date: first day of the quarter ─────────────────────
  date = zoo::as.yearqtr(
    make_date(year = cryear, month = (crquarter - 1L) * 3L + 1L, day = 1L)
  ),

  # ── M&A / Liquidity / Acquisition indicators ───────────
  # merger      : CU undergoing or just completed a merger
  # liquid      : CU in liquidation
  # acquisition : CU being acquired or absorbing another
  merger      = fifelse(outcome %in% c("C", "MC"),          1L, 0L),
  liquid      = fifelse(outcome %in% c("L",  "LC"),         1L, 0L),
  acquisition = fifelse(is.na(outcome) | !(outcome %in% c("L","LC")), 1L, 0L),

  # ── Binary: still operating next quarter? ──────────────
  # Used as a robustness flag — exclude exit-quarter obs
  active = fifelse(is.na(outcome) | outcome == "", 1L, 0L)
)]

# ── Category label map ────────────────────────────────────
CAT_MAP <- c(
  "1" = "1_Less_10M",   "2" = "2_10M_50M",   "3" = "3_50M_100M",
  "4" = "4_100M_500M",  "5" = "5_500M_1B",   "6" = "6_1B_10B",
  "7" = "7_10B_Plus"
)
cr[, cat_label := CAT_MAP[as.character(categories)]]

# Quick sanity check
message(sprintf("    date range: %s to %s",
                as.character(min(cr$date, na.rm=TRUE)),
                as.character(max(cr$date, na.rm=TRUE))))
message(sprintf("    ficu_count total: %s",
                format(sum(cr$ficu_count, na.rm=TRUE), big.mark=",")))
message(sprintf("    categories: %s",
                paste(sort(unique(cr$categories)), collapse=" ")))
message(sprintf("    merger events: %s  liquid: %s  acquisition: %s",
                sum(cr$merger,      na.rm=TRUE),
                sum(cr$liquid,      na.rm=TRUE),
                sum(cr$acquisition, na.rm=TRUE)))
toc()

# ════════════════════════════════════════════════════════
# 3. AUTO-DETECT LEVEL vs RATIO → ASSIGN AGGREGATION OP
# ════════════════════════════════════════════════════════
tic("3. Auto-detect level/ratio")
message("\n[3] Auto-detecting level vs ratio variables...")

# Strategy:
#   • Ratios/rates:  bounded roughly in (-100, 100) or (0,1)/(0,100)
#     AND have small variance relative to mean — typical for rates/shares
#   • Hard rules:
#     - Name ends in _rate, _ratio, _shr, _avg, _pct, _rt → ratio → median
#     - Name starts with acct_ → level (account balance) → sum
#     - Otherwise: check empirical range
#
detect_agg_type <- function(dt, cols) {
  # Name-based rules first (fast, reliable)
  ratio_patterns <- c("_rate$","_ratio$","_shr$","_avg$","_pct$",
                      "_rt$","_oth_fr","_fr_","networth","roa","netmarg",
                      "^chg_","clf$","clf_","busInsdel2mos",
                      "provlnsloss","costfds","retavgasst",
                      "netchgoffs","netintmrg","Inspurothfin",
                      "dq_.*_rate","dq_comm_rate","dq_cc_rate",
                      "dq_auto_rate","dq_re.*_rate","dq_mbl_rate",
                      "_ncomm$","_n_ncomm$")
  level_patterns  <- c("^acct_","^lns_","^ins_","^inv_","^dep_",
                       "^exp_","^inc_","^dq_[^r]","^totdel","members$",
                       "members_pot$","limited_inc$","ficu_count$",
                       "fiscu_count$","^rcv_","^chg_tot","^eq_",
                       "^liab_","^networth_tot","assets_tot$",
                       "fte$","subdebt$","insured_tot$","uninsured")

  result <- character(length(cols))
  names(result) <- cols

  for (col in cols) {
    if (!is.numeric(dt[[col]])) { result[col] <- "skip"; next }

    # Name-based ratio check
    if (any(vapply(ratio_patterns, function(p)
      grepl(p, col, ignore.case=TRUE, perl=TRUE), logical(1)))) {
      result[col] <- "median"; next
    }
    # Name-based level check
    if (any(vapply(level_patterns, function(p)
      grepl(p, col, ignore.case=TRUE, perl=TRUE), logical(1)))) {
      result[col] <- "sum"; next
    }

    # Empirical fallback: sample 10k non-NA values
    vals <- dt[[col]][!is.na(dt[[col]])]
    if (length(vals) == 0) { result[col] <- "sum"; next }
    if (length(vals) > 10000) vals <- sample(vals, 10000)

    rng  <- range(vals, na.rm=TRUE)
    mn   <- abs(mean(vals, na.rm=TRUE))
    cv   <- if (mn > 0) sd(vals, na.rm=TRUE) / mn else Inf

    # Heuristics:
    # - Range in [-2, 100] with low CV → ratio/rate
    # - Large absolute values or high CV → level
    if (rng[1] >= -5 && rng[2] <= 110 && cv < 3) {
      result[col] <- "median"
    } else {
      result[col] <- "sum"
    }
  }
  result
}

# Apply to all numeric columns except identifiers / dep vars we keep raw
id_cols  <- c("cu_num","cryear","crquarter","cu_type","outcome",
              "assets_cat2","categories","cat_label","date",
              "ficu_count","fiscu_count","merger","liquid",
              "acquisition","active","ln_assets","cat_label")
num_cols <- setdiff(names(cr)[vapply(cr, is.numeric, logical(1))], id_cols)

agg_types  <- detect_agg_type(cr, num_cols)
sum_vars   <- names(agg_types)[agg_types == "sum"]
median_vars <- names(agg_types)[agg_types == "median"]
skip_vars  <- names(agg_types)[agg_types == "skip"]

message(sprintf("    SUM vars   : %d", length(sum_vars)))
message(sprintf("    MEDIAN vars: %d", length(median_vars)))
message(sprintf("    SKIP vars  : %d", length(skip_vars)))
toc()

# ════════════════════════════════════════════════════════
# 4. AGGREGATE TO QUARTERLY PANEL
# ════════════════════════════════════════════════════════
tic("4. Quarterly aggregation")
message("\n[4] Aggregating to quarterly panel (date x categories)...")

# Keep only active observations for aggregation
# (merger/liquidation CUs skew counts in exit quarter)
cr_active <- cr[!is.na(date) & !is.na(categories)]

# ── 4a: Count-based dep vars ────────────────────────────
qtrly_counts <- cr_active[, .(
  ficu_count    = sum(ficu_count,    na.rm=TRUE),
  fiscu_count   = sum(fiscu_count,   na.rm=TRUE),
  # M&A event counts per cell — important features
  n_mergers     = sum(merger,        na.rm=TRUE),
  n_liquid      = sum(liquid,        na.rm=TRUE),
  n_acquisition = sum(acquisition,   na.rm=TRUE),
  n_active      = sum(active,        na.rm=TRUE),
  n_total       = .N
), by = .(date, categories)]

# ── 4b: Sum-aggregated level variables ──────────────────
message(sprintf("    Aggregating %d SUM vars...", length(sum_vars)))
sum_expr <- lapply(sum_vars, function(v)
  substitute(sum(X, na.rm=TRUE), list(X=as.name(v))))
names(sum_expr) <- sum_vars

qtrly_sum <- cr_active[, lapply(.SD, sum, na.rm=TRUE),
                         .SDcols = sum_vars,
                         by = .(date, categories)]

# ── 4c: Median-aggregated ratio variables ───────────────
message(sprintf("    Aggregating %d MEDIAN vars...", length(median_vars)))
qtrly_med <- cr_active[, lapply(.SD, median, na.rm=TRUE),
                         .SDcols = median_vars,
                         by = .(date, categories)]

# ── 4d: Merge all ────────────────────────────────────────
qtrly <- merge(qtrly_counts, qtrly_sum, by=c("date","categories"), all=TRUE)
qtrly <- merge(qtrly, qtrly_med,        by=c("date","categories"), all=TRUE)

# Add cat_label
qtrly[, cat_label := CAT_MAP[as.character(categories)]]
setorderv(qtrly, c("categories","date"))

message(sprintf("    Quarterly panel: %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))
message(sprintf("    Date range: %s to %s",
                as.character(min(qtrly$date, na.rm=TRUE)),
                as.character(max(qtrly$date, na.rm=TRUE))))
toc()
notify("Aggregation Done",
       sprintf("%d rows x %d cols", nrow(qtrly), ncol(qtrly)),
       tags="white_check_mark")

# ════════════════════════════════════════════════════════
# 5. FEATURE ENGINEERING
# ════════════════════════════════════════════════════════
message("\n[5] Feature engineering...")

# All numeric columns available for transformation
# (excluding identifiers and count/event dep vars)
base_excl <- c("date","categories","cat_label",
               "ficu_count","fiscu_count",
               "n_mergers","n_liquid","n_acquisition",
               "n_active","n_total")
fe_cols <- setdiff(
  names(qtrly)[vapply(qtrly, is.numeric, logical(1))],
  base_excl
)
message(sprintf("    Base columns for FE: %d", length(fe_cols)))

setorderv(qtrly, c("categories","date"))

# ── Helper: safe percentage change ──────────────────────
pct_chg <- function(x, lag_x) {
  fifelse(!is.na(lag_x) & lag_x != 0,
          (x - lag_x) / abs(lag_x) * 100,
          NA_real_)
}

# ── 5a: YoY % change (lag-4) ────────────────────────────
tic("5a. YoY % change")
message("    5a. YoY % change (lag-4)...")

for (v in fe_cols) {
  new_col <- paste0("yoy_", v)
  qtrly[, (new_col) := pct_chg(get(v), data.table::shift(get(v), n=4L)),
        by = categories]
}
toc()

# ── 5b: QoQ % change (lag-1) ────────────────────────────
tic("5b. QoQ % change")
message("    5b. QoQ % change (lag-1)...")

for (v in fe_cols) {
  new_col <- paste0("qoq_", v)
  qtrly[, (new_col) := pct_chg(get(v), data.table::shift(get(v), n=1L)),
        by = categories]
}
toc()

# ── 5c: Lag levels (1q, 2q, 4q, 8q) ────────────────────
tic("5c. Lag levels")
message("    5c. Lag features (1q, 2q, 4q, 8q)...")

# Limit to key variables to avoid explosion in column count
key_lag_vars <- c(fe_cols[1:min(40L, length(fe_cols))],
                  "assets_tot", "members", "ln_assets")
key_lag_vars <- intersect(unique(key_lag_vars), names(qtrly))

for (lag_n in c(1L, 2L, 4L, 8L)) {
  for (v in key_lag_vars) {
    new_col <- paste0(v, "_lag", lag_n)
    qtrly[, (new_col) := data.table::shift(get(v), n=lag_n),
          by = categories]
  }
}
toc()

# ── 5d: Rolling means (4q, 8q, 12q) ────────────────────
tic("5d. Rolling means")
message("    5d. Rolling means (4q, 8q, 12q)...")

# Apply to YoY columns of key variables
yoy_key  <- paste0("yoy_", key_lag_vars)
yoy_key  <- intersect(yoy_key, names(qtrly))

rollmean_safe <- function(x, k) {
  zoo::rollapply(x, width=k, FUN=mean, na.rm=TRUE,
                 fill=NA, align="right", partial=FALSE)
}

for (k in c(4L, 8L, 12L)) {
  new_nms <- paste0(yoy_key, "_rmean", k)
  qtrly[, (new_nms) :=
          lapply(.SD, function(x) rollmean_safe(x, k)),
        .SDcols = yoy_key,
        by = categories]
}
toc()

# ── 5e: Rolling SDs (4q, 8q) ────────────────────────────
tic("5e. Rolling SDs")
message("    5e. Rolling SDs (volatility) (4q, 8q)...")

rollsd_safe <- function(x, k) {
  zoo::rollapply(x, width=k, FUN=sd, na.rm=TRUE,
                 fill=NA, align="right", partial=FALSE)
}

for (k in c(4L, 8L)) {
  new_nms <- paste0(yoy_key, "_rsd", k)
  qtrly[, (new_nms) :=
          lapply(.SD, function(x) rollsd_safe(x, k)),
        .SDcols = yoy_key,
        by = categories]
}
toc()

# ── 5f: YoY acceleration (momentum) ─────────────────────
tic("5f. Momentum / acceleration")
message("    5f. YoY acceleration (current YoY minus 4q-ago YoY)...")

accel_vars <- paste0("yoy_", key_lag_vars)
accel_vars <- intersect(accel_vars, names(qtrly))
new_nms    <- paste0(accel_vars, "_accel")

for (v in accel_vars) {
  new_col <- paste0(v, "_accel")
  qtrly[, (new_col) := get(v) - data.table::shift(get(v), n=4L),
        by = categories]
}
toc()

# ── 5g: Cyclical deviation from 8q rolling mean ─────────
tic("5g. Cyclical deviation")
message("    5g. Cyclical deviation from 8q rolling mean...")

# deviation = current value minus 8q rolling mean
# captures whether a variable is above/below its recent trend
dev_vars <- fe_cols[1:min(20L, length(fe_cols))]
dev_vars <- intersect(dev_vars, names(qtrly))

for (v in dev_vars) {
  rmean_col <- paste0(v, "_rmean8_dev")
  if (paste0("yoy_", v, "_rmean8") %in% names(qtrly)) {
    # deviation in YoY space
    yoy_v     <- paste0("yoy_", v)
    rmean_v   <- paste0("yoy_", v, "_rmean8")
    if (all(c(yoy_v, rmean_v) %in% names(qtrly))) {
      qtrly[, (paste0(yoy_v, "_cyc")) :=
              get(yoy_v) - get(rmean_v),
            by = categories]
    }
  }
}
toc()

# ── 5h: M&A rate features (per CU) ──────────────────────
tic("5h. M&A rate features")
message("    5h. M&A intensity rates...")

qtrly[ficu_count > 0, `:=`(
  merger_rate      = n_mergers     / ficu_count * 100,
  liquid_rate      = n_liquid      / ficu_count * 100,
  acquisition_rate = n_acquisition / ficu_count * 100,
  exit_rate        = (n_mergers + n_liquid) / ficu_count * 100
)]
# YoY and QoQ of M&A rates
ma_rate_vars <- c("merger_rate","liquid_rate","acquisition_rate","exit_rate")
ma_rate_vars <- intersect(ma_rate_vars, names(qtrly))

for (v in ma_rate_vars) {
  qtrly[, (paste0("yoy_", v)) :=
          pct_chg(get(v), data.table::shift(get(v), n=4L)),
        by = categories]
  qtrly[, (paste0("qoq_", v)) :=
          pct_chg(get(v), data.table::shift(get(v), n=1L)),
        by = categories]
}

# Rolling 4q sum of exit events (pipeline effect)
qtrly[, exit_roll4 := zoo::rollapply(
  n_mergers + n_liquid, width=4, FUN=sum, na.rm=TRUE,
  fill=NA, align="right"), by=categories]
toc()

# ── 5i: Time / trend / regime features ──────────────────
tic("5i. Time and regime features")
message("    5i. Time index and regime indicators...")

# Integer time index (useful for tree models as trend proxy)
all_dates <- sort(unique(qtrly$date))
date_idx  <- data.table(date=all_dates,
                         time_idx=seq_along(all_dates))
qtrly <- merge(qtrly, date_idx, by="date", all.x=TRUE)

# Quarters since key macro regimes
qtrly[, `:=`(
  # Quarters since Great Recession trough (2009 Q2)
  qtrs_from_gfc     = as.integer(
    (as.numeric(date) - as.numeric(zoo::as.yearqtr("2009 Q2"))) * 4),
  # Quarters since ZIRP end (2015 Q4)
  qtrs_from_zirp    = as.integer(
    (as.numeric(date) - as.numeric(zoo::as.yearqtr("2015 Q4"))) * 4),
  # Quarters since COVID shock (2020 Q1)
  qtrs_from_covid   = as.integer(
    (as.numeric(date) - as.numeric(zoo::as.yearqtr("2020 Q1"))) * 4),
  # Quarters since Fed hiking cycle start (2022 Q1)
  qtrs_from_hike    = as.integer(
    (as.numeric(date) - as.numeric(zoo::as.yearqtr("2022 Q1"))) * 4)
)]

# Regime dummies
qtrly[, `:=`(
  # 1 = pre-GFC expansion (before 2007 Q3)
  regime_pre_gfc    = fifelse(date < zoo::as.yearqtr("2007 Q3"), 1L, 0L),
  # 1 = GFC/acute crisis
  regime_gfc        = fifelse(date >= zoo::as.yearqtr("2007 Q3") &
                               date <= zoo::as.yearqtr("2009 Q2"), 1L, 0L),
  # 1 = ZIRP era (low rates)
  regime_zirp       = fifelse(date > zoo::as.yearqtr("2009 Q2") &
                               date <= zoo::as.yearqtr("2015 Q4"), 1L, 0L),
  # 1 = normalisation pre-COVID
  regime_normal     = fifelse(date > zoo::as.yearqtr("2015 Q4") &
                               date < zoo::as.yearqtr("2020 Q1"), 1L, 0L),
  # 1 = COVID shock + ZIRP2
  regime_covid      = fifelse(date >= zoo::as.yearqtr("2020 Q1") &
                               date < zoo::as.yearqtr("2022 Q1"), 1L, 0L),
  # 1 = post-COVID hiking cycle
  regime_hike       = fifelse(date >= zoo::as.yearqtr("2022 Q1"), 1L, 0L),
  # Seasonal dummies (quarter of year)
  q1 = fifelse(as.integer(format(as.Date(date), "%m")) == 1L,  1L, 0L),
  q2 = fifelse(as.integer(format(as.Date(date), "%m")) == 4L,  1L, 0L),
  q3 = fifelse(as.integer(format(as.Date(date), "%m")) == 7L,  1L, 0L),
  q4 = fifelse(as.integer(format(as.Date(date), "%m")) == 10L, 1L, 0L)
)]
toc()

# ── 5j: Log-level of key financial variables ────────────
tic("5j. Log transforms")
message("    5j. Log transforms of key level variables...")

log_vars <- c("assets_tot","members","members_pot",
              "lns_tot","dep_tot","eq_tot","liab_tot",
              "inc_net","exp_tot","inv_tot")
log_vars <- intersect(log_vars, names(qtrly))

for (v in log_vars) {
  qtrly[, (paste0("ln_", v)) :=
          log(pmax(get(v), 1L, na.rm=TRUE))]
}
toc()

# ── 5k: Cross-category share features ───────────────────
tic("5k. System-share features")
message("    5k. Category share of system total...")

# Each category's share of system-wide total
# (captures relative size shifts — useful predictor)
share_vars <- c("ficu_count","assets_tot","members",
                "lns_tot","dep_tot")
share_vars <- intersect(share_vars, names(qtrly))

for (v in share_vars) {
  sys_col <- paste0("sys_", v)
  sh_col  <- paste0("share_", v)
  # System total per date
  qtrly[, (sys_col) := sum(get(v), na.rm=TRUE), by=date]
  qtrly[get(sys_col) > 0,
        (sh_col) := get(v) / get(sys_col) * 100]
  # YoY change in share
  qtrly[, (paste0("yoy_", sh_col)) :=
          pct_chg(get(sh_col), data.table::shift(get(sh_col), n=4L)),
        by = categories]
  # Drop sys_ helper col (not needed downstream)
  qtrly[, (sys_col) := NULL]
}
toc()

notify("Feature Engineering Done",
       sprintf("Dataset now %d rows x %d cols",
               nrow(qtrly), ncol(qtrly)),
       tags="white_check_mark")

# ════════════════════════════════════════════════════════
# 6. DEPENDENT VARIABLE CONSTRUCTION (v2 improvements)
# ════════════════════════════════════════════════════════
tic("6. Dependent variables")
message("\n[6] Constructing dependent variables (v2)...")

setorderv(qtrly, c("categories","date"))

qtrly[, `:=`(
  yoy_ficu_pct    = pct_chg(ficu_count, data.table::shift(ficu_count, n=4L)),
  qoq_ficu_pct    = pct_chg(ficu_count, data.table::shift(ficu_count, n=1L)),
  ld_ficu         = log(pmax(ficu_count,1)) -
                    log(pmax(data.table::shift(ficu_count, n=4L), 1L)),
  net_entry_rate  = fifelse(
    data.table::shift(ficu_count, n=1L) > 0,
    (ficu_count - data.table::shift(ficu_count, n=1L)) /
      data.table::shift(ficu_count, n=1L) * 100,
    NA_real_
  ),
  ficu_count_lag4 = data.table::shift(ficu_count, n=4L)
), by = categories]

message(sprintf("    yoy_ficu_pct range: [%.2f%%, %.2f%%]",
                min(qtrly$yoy_ficu_pct, na.rm=TRUE),
                max(qtrly$yoy_ficu_pct, na.rm=TRUE)))
message(sprintf("    Non-NA yoy_ficu_pct: %d / %d rows",
                sum(!is.na(qtrly$yoy_ficu_pct)),
                nrow(qtrly)))
toc()

# ════════════════════════════════════════════════════════
# 7. QUALITY CHECKS & CLEANUP
# ════════════════════════════════════════════════════════
tic("7. QC and cleanup")
message("\n[7] Quality checks...")

# Report column count by type
n_yoy  <- sum(startsWith(names(qtrly), "yoy_"))
n_qoq  <- sum(startsWith(names(qtrly), "qoq_"))
n_lag  <- sum(grepl("_lag[0-9]", names(qtrly)))
n_rm   <- sum(grepl("_rmean", names(qtrly)))
n_rsd  <- sum(grepl("_rsd",   names(qtrly)))
n_reg  <- sum(startsWith(names(qtrly), "regime_"))
n_ln   <- sum(startsWith(names(qtrly), "ln_"))
n_sh   <- sum(startsWith(names(qtrly), "share_"))

message(sprintf("    YoY columns    : %d", n_yoy))
message(sprintf("    QoQ columns    : %d", n_qoq))
message(sprintf("    Lag columns    : %d", n_lag))
message(sprintf("    Rolling means  : %d", n_rm))
message(sprintf("    Rolling SDs    : %d", n_rsd))
message(sprintf("    Regime dummies : %d", n_reg))
message(sprintf("    Log transforms : %d", n_ln))
message(sprintf("    Share features : %d", n_sh))
message(sprintf("    TOTAL columns  : %d", ncol(qtrly)))

# Remove all-NA columns
pre_clean  <- ncol(qtrly)
all_na     <- names(qtrly)[vapply(qtrly, function(x) all(is.na(x)), logical(1))]
if (length(all_na) > 0) {
  qtrly[, (all_na) := NULL]
  message(sprintf("    Dropped %d all-NA columns", length(all_na)))
}

# Remove zero-variance columns
num_nms <- names(qtrly)[vapply(qtrly, is.numeric, logical(1))]
zero_var <- num_nms[vapply(num_nms, function(v) {
  x <- qtrly[[v]]
  x <- x[!is.na(x)]
  length(x) > 0 && var(x) == 0
}, logical(1))]
if (length(zero_var) > 0) {
  qtrly[, (zero_var) := NULL]
  message(sprintf("    Dropped %d zero-variance columns", length(zero_var)))
}

message(sprintf("    Final: %d rows x %d cols  (removed %d cols)",
                nrow(qtrly), ncol(qtrly), pre_clean - ncol(qtrly)))
toc()

# ════════════════════════════════════════════════════════
# 8. SAVE
# ════════════════════════════════════════════════════════
tic("8. Save")
message("\n[8] Saving qtrly_enriched.rds...")

# Filter to 2005 onwards (per modelling decision)
qtrly_save <- qtrly[date >= zoo::as.yearqtr("2000 Q1")]
message(sprintf("    Saving %d rows x %d cols",
                nrow(qtrly_save), ncol(qtrly_save)))

saveRDS(qtrly_save, "qtrly_enriched.rds")

# Also save a lightweight version with just dep vars + key features
# useful for quick EDA and diagnostics
key_cols <- c("date","categories","cat_label",
              "ficu_count","fiscu_count",
              "yoy_ficu_pct","qoq_ficu_pct","net_entry_rate",
              "ficu_count_lag4","ld_ficu",
              "n_mergers","n_liquid","n_acquisition",
              "merger_rate","liquid_rate","exit_rate","exit_roll4",
              "assets_tot","members","ln_assets_tot",
              "time_idx","regime_pre_gfc","regime_gfc",
              "regime_zirp","regime_normal","regime_covid","regime_hike",
              "qtrs_from_gfc","qtrs_from_zirp","qtrs_from_covid",
              "qtrs_from_hike","q1","q2","q3","q4")
key_cols <- intersect(key_cols, names(qtrly_save))
saveRDS(qtrly_save[, key_cols, with=FALSE], "qtrly_key.rds")

message("    Saved: qtrly_enriched.rds  (full)")
message("    Saved: qtrly_key.rds       (key vars only)")
toc()

# ════════════════════════════════════════════════════════
# 9. FINAL SUMMARY
# ════════════════════════════════════════════════════════
tot <- as.numeric((proc.time()-t0)["elapsed"])
toc()  # PART 1 total

message("\n=======================================================")
message(sprintf("PART 1 v2.0 COMPLETE  %dh %02dm %02ds",
                floor(tot/3600),
                floor((tot%%3600)/60),
                round(tot%%60)))
message(sprintf("Output: qtrly_enriched.rds  (%d rows x %d cols)",
                nrow(qtrly_save), ncol(qtrly_save)))
message(sprintf("Dep vars: yoy_ficu_pct  qoq_ficu_pct  net_entry_rate  ld_ficu"))
message("=======================================================")

# Quick preview of dep var by category
message("\nDep var summary by category:")
print(qtrly_save[!is.na(yoy_ficu_pct),
                  .(n     = .N,
                    mean  = round(mean(yoy_ficu_pct),  2),
                    sd    = round(sd(yoy_ficu_pct),    2),
                    min   = round(min(yoy_ficu_pct),   2),
                    max   = round(max(yoy_ficu_pct),   2)),
                  by=cat_label][order(cat_label)])

notify("Part 1 v2.0 DONE",
       sprintf("%dh%02dm | %d rows x %d cols\nqtrly_enriched.rds saved",
               floor(tot/3600), floor((tot%%3600)/60),
               nrow(qtrly_save), ncol(qtrly_save)),
       tags="tada")

############################################################
# END
############################################################
