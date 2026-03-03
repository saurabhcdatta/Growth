############################################################
# PART 1 v5.0 — PRODUCTION DATA PREP + FEATURE ENGINEERING
#              (No ARIMA Forecasting)
#
# Architecture vs v4:
#   Removed:
#     * Section 4  — ARIMA forecast of every panel variable
#     * Section 5  — Combine historical + forecast panel
#     * Section 10 — All ARIMA-related diagnostic plots (P1-P11)
#     * Forecast template, CI columns, horizon summary
#     * `forecast` package dependency
#   Kept / Enhanced:
#     * Section 1  — Load & construct key variables
#     * Section 2  — Auto-detect aggregation type
#     * Section 3  — Aggregate to quarterly panel
#     * Section 4  — Feature engineering (all 6a-6k, renumbered)
#     * Section 5  — Dependent variable construction
#     * Section 6  — QC
#     * Section 7  — Save outputs
#     * Section 8  — Historical diagnostic plots (P1-P5)
#
# Terminology:
#   v5 uses FCU (Federal Credit Union) and FISCU (Federally
#   Insured State-Chartered Credit Union) throughout.
#   Column names: fcu_count, fiscu_count, fcu_assets, etc.
#
# Outputs:
#   qtrly_enriched_v5.rds          historical panel with full FE
#   5 diagnostic PDF plots -> plots_v5/
#
# Purpose:
#   Produce a clean, feature-rich historical panel ready for
#   downstream modeling (Part 2 macro merge, Part 3 regression).
#   ARIMA projections are handled separately or not at all --
#   this script focuses purely on data preparation.
#
# Runtime: ~2-5 min (no ARIMA fitting)
############################################################

suppressPackageStartupMessages({
  library(data.table); library(zoo);        library(lubridate)
  library(httr);       library(ggplot2)
  library(scales);     library(tictoc)
})
shift <- data.table::shift   # prevent masking
options(scipen = 999)
set.seed(42)

# ================================================================
# CONFIG
# ================================================================

# Last quarter of actual data -- set to match your latest call report
HIST_END <- zoo::as.yearqtr("2025 Q3")

# ntfy push notifications (set TRUE only if you have an ntfy topic)
NTFY_TOPIC   <- "your-unique-topic-name"
NTFY_ENABLED <- FALSE

# -- Paths --------------------------------------------------------
DATA_DIR <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR <- file.path(DATA_DIR, "plots_v5")

# ================================================================
# HELPERS
# ================================================================
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

save_plot <- function(p, stem, w = 14, h = 9) {
  path <- file.path(PLOT_DIR, paste0(stem, ".pdf"))
  tryCatch({
    pdf(path, width = w, height = h)
    print(p)
    dev.off()
  }, error = function(e) {
    try(dev.off(), silent = TRUE)
    message(sprintf("    [WARN] could not save %s: %s", stem, e$message))
  })
  invisible(path)
}

pct_chg <- function(x, lag_x)
  fifelse(!is.na(lag_x) & lag_x != 0,
          (x - lag_x) / abs(lag_x) * 100, NA_real_)

CAT_MAP <- c("1"="1_Less_10M","2"="2_10M_50M","3"="3_50M_100M",
             "4"="4_100M_500M","5"="5_500M_1B","6"="6_1B_10B","7"="7_10B_Plus")

CAT_COLOURS <- c("1_Less_10M"="#1f77b4","2_10M_50M"="#ff7f0e",
                 "3_50M_100M"="#2ca02c","4_100M_500M"="#d62728",
                 "5_500M_1B"="#9467bd","6_1B_10B"="#8c564b","7_10B_Plus"="#e377c2")

theme_v5 <- function()
  theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "#e8f0f7"),
        strip.text       = element_text(face = "bold"),
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(colour = "grey40"),
        legend.position  = "bottom")

# ================================================================
# START
# ================================================================
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

t0 <- proc.time()
tic("PART 1 v5.0 total")
message("=======================================================")
message(sprintf("PART 1 v5.0 (Production)  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("Mode: Historical data prep + feature engineering (no ARIMA)")
message("=======================================================")
notify("Part 1 v5.0 Started", "Historical data prep + FE", tags = "rocket")

setwd(DATA_DIR)

# ================================================================
# 1. LOAD & CONSTRUCT KEY VARIABLES
# ================================================================
tic("1. Load + key vars")
message("\n[1] Loading raw call report data...")

cr <- readRDS("call_report.rds")
setDT(cr)
message(sprintf("    Raw: %s rows x %s cols",
                format(nrow(cr), big.mark=","),
                format(ncol(cr), big.mark=",")))

# -- Build date from year/quarter columns if not already present --
if (!"date" %in% names(cr)) {
  yr_col  <- if ("year"    %in% names(cr)) "year"    else
             if ("cryear"  %in% names(cr)) "cryear"  else stop("No year column found")
  qtr_col <- if ("quarter" %in% names(cr)) "quarter" else
             if ("crquarter" %in% names(cr)) "crquarter" else stop("No quarter column found")
  cr[, date := zoo::as.yearqtr(
    make_date(get(yr_col), (get(qtr_col) - 1L)*3L + 1L, 1L))]
  message(sprintf("    Constructed date from %s + %s", yr_col, qtr_col))
} else {
  if (!inherits(cr$date, "yearqtr"))
    cr[, date := zoo::as.yearqtr(date)]
  message("    date column already present")
}

# -- Construct indicator columns if not already in the RDS --------
if (!"fcu_count" %in% names(cr)) {
  cr[, `:=`(
    fcu_count  = fifelse(cu_type == 1, 1, 0),
    fiscu_count = fifelse(cu_type == 2, 1, 0)
  )]
}
# Per-CU-type asset columns
cr[, `:=`(
  fcu_assets_tot  = fifelse(cu_type == 1, assets_tot, 0),
  fiscu_assets_tot = fifelse(cu_type == 2, assets_tot, 0)
)]
if (!"ln_assets" %in% names(cr))
  cr[, ln_assets := log(pmax(assets_tot, 1))]

if (!"categories" %in% names(cr)) {
  cr[, categories := fifelse(assets_cat2 %in% c(6L,7L), 6L,
                       fifelse(assets_cat2 == 8L, 7L,
                               as.integer(assets_cat2)))]
}
if (!"merger" %in% names(cr))
  cr[, merger      := fifelse(outcome %in% c("C","MC"),           1L, 0L)]
if (!"liquid" %in% names(cr))
  cr[, liquid      := fifelse(outcome %in% c("L","LC"),           1L, 0L)]
if (!"acquisition" %in% names(cr))
  cr[, acquisition := fifelse(is.na(outcome)|!(outcome %in% c("L","LC")), 1L, 0L)]
if (!"active" %in% names(cr))
  cr[, active      := fifelse(is.na(outcome)|outcome == "",        1L, 0L)]
cr[, cat_label := CAT_MAP[as.character(categories)]]

DATA_END <- max(cr$date, na.rm = TRUE)
message(sprintf("    Data range: %s -> %s",
                as.character(min(cr$date, na.rm=TRUE)),
                as.character(DATA_END)))
toc()

# ================================================================
# 2. AUTO-DETECT AGGREGATION TYPE
# ================================================================
tic("2. Auto-detect agg type")
message("\n[2] Auto-detecting level vs ratio variables...")

detect_agg_type <- function(dt, cols) {
  ratio_pat <- c("_rate$","_ratio$","_shr$","_avg$","_pct$","_rt$",
                 "_oth_fr","_fr_","networth","roa","netmarg","^chg_",
                 "provlnsloss","costfds","retavgasst","netchgoffs",
                 "netintmrg","dq_.*_rate")
  level_pat <- c("^acct_","^lns_","^ins_","^inv_","^dep_","^exp_",
                 "^inc_","^eq_","^liab_","members$","members_pot$",
                 "fcu_count$","fiscu_count$","fcu_assets$","fiscu_assets$",
                 "assets_tot$","^rcv_","^networth_tot")
  result <- setNames(character(length(cols)), cols)
  for (col in cols) {
    if (!is.numeric(dt[[col]])) { result[col] <- "skip"; next }
    if (any(vapply(ratio_pat, function(p) grepl(p,col,ignore.case=TRUE,perl=TRUE), logical(1))))
    { result[col] <- "median"; next }
    if (any(vapply(level_pat, function(p) grepl(p,col,ignore.case=TRUE,perl=TRUE), logical(1))))
    { result[col] <- "sum"; next }
    vals <- dt[[col]][!is.na(dt[[col]])]
    if (length(vals) == 0) { result[col] <- "sum"; next }
    if (length(vals) > 10000) vals <- sample(vals, 10000)
    rng <- range(vals, na.rm=TRUE); mn <- abs(mean(vals,na.rm=TRUE))
    cv  <- if (mn > 0) sd(vals,na.rm=TRUE)/mn else Inf
    result[col] <- if (rng[1] >= -5 && rng[2] <= 110 && cv < 3) "median" else "sum"
  }
  result
}

id_cols  <- c("cu_num","year","quarter","cu_type","outcome",
              "assets_cat2","categories","cat_label","date",
              "fcu_count","fiscu_count","fcu_assets","fiscu_assets",
              "merger","liquid","acquisition","active","ln_assets")
num_cols    <- setdiff(names(cr)[vapply(cr,is.numeric,logical(1))], id_cols)
agg_types   <- detect_agg_type(cr, num_cols)
sum_vars    <- names(agg_types)[agg_types == "sum"]
median_vars <- names(agg_types)[agg_types == "median"]
message(sprintf("    SUM: %d  MEDIAN: %d", length(sum_vars), length(median_vars)))
toc()

# ================================================================
# 3. AGGREGATE TO QUARTERLY PANEL
# ================================================================
tic("3. Quarterly aggregation")
message("\n[3] Aggregating to quarterly panel by category...")

cr_a <- cr[!is.na(date) & !is.na(categories)]

qtrly_counts <- cr_a[, .(
  fcu_count    = sum(fcu_count,       na.rm=TRUE),
  fiscu_count   = sum(fiscu_count,      na.rm=TRUE),
  fcu_assets   = sum(fcu_assets_tot,  na.rm=TRUE),
  fiscu_assets  = sum(fiscu_assets_tot, na.rm=TRUE),
  n_mergers     = sum(merger,           na.rm=TRUE),
  n_liquid      = sum(liquid,           na.rm=TRUE),
  n_acquisition = sum(acquisition,      na.rm=TRUE),
  n_active      = sum(active,           na.rm=TRUE),
  n_total       = .N
), by = .(date, categories)]

qtrly_sum <- cr_a[, lapply(.SD, sum, na.rm=TRUE),
                    .SDcols = sum_vars, by = .(date, categories)]
qtrly_med <- cr_a[, lapply(.SD, median, na.rm=TRUE),
                    .SDcols = median_vars, by = .(date, categories)]

qtrly <- Reduce(function(a,b) merge(a,b,by=c("date","categories"),all=TRUE),
                list(qtrly_counts, qtrly_sum, qtrly_med))
qtrly[, cat_label := CAT_MAP[as.character(categories)]]
setorderv(qtrly, c("categories","date"))
message(sprintf("    Historical panel: %d rows x %d cols", nrow(qtrly), ncol(qtrly)))
toc()

# ================================================================
# 4. FEATURE ENGINEERING ON HISTORICAL PANEL
# ================================================================
# All feature engineering from v4 Sections 6a-6k applied to
# the historical panel. Lags, rolling windows, YoY, QoQ,
# momentum, cyclical, M&A rates, regime dummies, logs, shares.
# ================================================================
tic("4. Feature engineering")
message("\n[4] Feature engineering on historical panel...")

base_excl <- c("date","categories","cat_label",
               "fcu_count","fiscu_count","fcu_assets","fiscu_assets",
               "n_mergers","n_liquid","n_acquisition","n_active","n_total")

fe_cols <- setdiff(names(qtrly)[vapply(qtrly,is.numeric,logical(1))],
                   base_excl)
message(sprintf("    FE base columns: %d", length(fe_cols)))

setorderv(qtrly, c("categories","date"))

# 4a: YoY % change (lag-4)
tic("4a YoY"); message("    4a. YoY % change...")
for (v in fe_cols)
  qtrly[, (paste0("yoy_",v)) :=
               pct_chg(get(v), shift(get(v),4L)), by=categories]
toc()

# 4b: QoQ % change (lag-1)
tic("4b QoQ"); message("    4b. QoQ % change...")
for (v in fe_cols)
  qtrly[, (paste0("qoq_",v)) :=
               pct_chg(get(v), shift(get(v),1L)), by=categories]
toc()

# 4c: Lag levels (1q, 2q, 4q, 8q) -- key vars to cap column count
tic("4c Lags"); message("    4c. Lag levels...")
key_lag_vars <- c(fe_cols[1:min(40L,length(fe_cols))],
                  "assets_tot","members","fcu_count","fiscu_count",
                  "fcu_assets","fiscu_assets")
key_lag_vars <- intersect(unique(key_lag_vars), names(qtrly))
for (lag_n in c(1L,2L,4L,8L))
  for (v in key_lag_vars)
    qtrly[, (paste0(v,"_lag",lag_n)) :=
                 shift(get(v),lag_n), by=categories]
toc()

# 4d: Rolling means (4q, 8q, 12q) on YoY of key vars
tic("4d Rolling means"); message("    4d. Rolling means...")
rollmean_safe <- function(x, k)
  zoo::rollapply(x, width=k, FUN=mean, na.rm=TRUE,
                 fill=NA, align="right", partial=FALSE)
yoy_key <- paste0("yoy_", key_lag_vars)
yoy_key <- intersect(yoy_key, names(qtrly))
for (k in c(4L,8L,12L)) {
  nms <- paste0(yoy_key,"_rmean",k)
  qtrly[, (nms) := lapply(.SD, rollmean_safe, k=k),
             .SDcols=yoy_key, by=categories]
}
toc()

# 4e: Rolling SDs (4q, 8q)
tic("4e Rolling SDs"); message("    4e. Rolling SDs...")
rollsd_safe <- function(x, k)
  zoo::rollapply(x, width=k, FUN=sd, na.rm=TRUE,
                 fill=NA, align="right", partial=FALSE)
for (k in c(4L,8L)) {
  nms <- paste0(yoy_key,"_rsd",k)
  qtrly[, (nms) := lapply(.SD, rollsd_safe, k=k),
             .SDcols=yoy_key, by=categories]
}
toc()

# 4f: YoY acceleration (momentum)
tic("4f Momentum"); message("    4f. Momentum...")
for (v in yoy_key)
  qtrly[, (paste0(v,"_accel")) :=
               get(v) - shift(get(v),4L), by=categories]
toc()

# 4g: Cyclical deviation from 8q rolling mean
tic("4g Cyclical"); message("    4g. Cyclical deviation...")
dev_vars <- fe_cols[1:min(20L,length(fe_cols))]
dev_vars <- intersect(dev_vars, names(qtrly))
for (v in dev_vars) {
  yv <- paste0("yoy_",v); rv <- paste0("yoy_",v,"_rmean8")
  if (all(c(yv,rv) %in% names(qtrly)))
    qtrly[, (paste0(yv,"_cyc")) :=
                 get(yv) - get(rv), by=categories]
}
toc()

# 4h: M&A rate features
tic("4h M&A rates"); message("    4h. M&A rates...")
qtrly[fcu_count > 0, `:=`(
  merger_rate      = n_mergers     / fcu_count * 100,
  liquid_rate      = n_liquid      / fcu_count * 100,
  acquisition_rate = n_acquisition / fcu_count * 100,
  exit_rate        = (n_mergers + n_liquid) / fcu_count * 100
)]
ma_vars <- intersect(c("merger_rate","liquid_rate","acquisition_rate","exit_rate"),
                     names(qtrly))
for (v in ma_vars) {
  qtrly[, (paste0("yoy_",v)) :=
               pct_chg(get(v), shift(get(v),4L)), by=categories]
  qtrly[, (paste0("qoq_",v)) :=
               pct_chg(get(v), shift(get(v),1L)), by=categories]
}
qtrly[, exit_roll4 := zoo::rollapply(
  n_mergers+n_liquid, width=4, FUN=sum, na.rm=TRUE,
  fill=NA, align="right"), by=categories]
toc()

# 4i: Time index and regime features
tic("4i Time/regime"); message("    4i. Time index and regime features...")
all_dates <- sort(unique(qtrly$date))
date_idx  <- data.table(date=all_dates, time_idx=seq_along(all_dates))
qtrly <- merge(qtrly, date_idx, by="date", all.x=TRUE)

qtrly[, `:=`(
  qtrs_from_gfc   = as.integer((as.numeric(date)-as.numeric(zoo::as.yearqtr("2009 Q2")))*4),
  qtrs_from_zirp  = as.integer((as.numeric(date)-as.numeric(zoo::as.yearqtr("2015 Q4")))*4),
  qtrs_from_covid = as.integer((as.numeric(date)-as.numeric(zoo::as.yearqtr("2020 Q1")))*4),
  qtrs_from_hike  = as.integer((as.numeric(date)-as.numeric(zoo::as.yearqtr("2022 Q1")))*4)
)]
qtrly[, `:=`(
  regime_pre_gfc = fifelse(date < zoo::as.yearqtr("2007 Q3"), 1L, 0L),
  regime_gfc     = fifelse(date>=zoo::as.yearqtr("2007 Q3") &
                             date<=zoo::as.yearqtr("2009 Q2"), 1L, 0L),
  regime_zirp    = fifelse(date>zoo::as.yearqtr("2009 Q2") &
                             date<=zoo::as.yearqtr("2015 Q4"), 1L, 0L),
  regime_normal  = fifelse(date>zoo::as.yearqtr("2015 Q4") &
                             date<zoo::as.yearqtr("2020 Q1"), 1L, 0L),
  regime_covid   = fifelse(date>=zoo::as.yearqtr("2020 Q1") &
                             date<zoo::as.yearqtr("2022 Q1"), 1L, 0L),
  regime_hike    = fifelse(date>=zoo::as.yearqtr("2022 Q1"), 1L, 0L),
  q1 = fifelse(as.integer(format(as.Date(date),"%m"))==1L,  1L, 0L),
  q2 = fifelse(as.integer(format(as.Date(date),"%m"))==4L,  1L, 0L),
  q3 = fifelse(as.integer(format(as.Date(date),"%m"))==7L,  1L, 0L),
  q4 = fifelse(as.integer(format(as.Date(date),"%m"))==10L, 1L, 0L)
)]
toc()

# 4j: Log transforms
tic("4j Logs"); message("    4j. Log transforms...")
log_vars <- intersect(c("assets_tot","members","members_pot",
                         "lns_tot","dep_tot","eq_tot","liab_tot",
                         "inc_net","exp_tot","inv_tot"),
                       names(qtrly))
for (v in log_vars)
  qtrly[, (paste0("ln_",v)) := log(pmax(get(v),1L,na.rm=TRUE))]
toc()

# 4k: System-share features
tic("4k System-share"); message("    4k. System-share features...")
share_vars <- intersect(c("fcu_count","fiscu_count","fcu_assets","fiscu_assets",
                           "assets_tot","members","lns_tot","dep_tot"),
                         names(qtrly))
for (v in share_vars) {
  sys_col <- paste0("sys_",v); sh_col <- paste0("share_",v)
  qtrly[, (sys_col) := sum(get(v),na.rm=TRUE), by=date]
  qtrly[get(sys_col)>0, (sh_col) := get(v)/get(sys_col)*100]
  qtrly[, (paste0("yoy_",sh_col)) :=
               pct_chg(get(sh_col), shift(get(sh_col),4L)), by=categories]
  qtrly[, (sys_col) := NULL]
}
toc()
toc()  # FE total

# ================================================================
# 5. DEPENDENT VARIABLE CONSTRUCTION
# ================================================================
tic("5. Dependent variables")
message("\n[5] Constructing dependent variables...")

setorderv(qtrly, c("categories","date"))

qtrly[, `:=`(
  # FCU
  yoy_fcu_pct      = pct_chg(fcu_count, shift(fcu_count,4L)),
  qoq_fcu_pct      = pct_chg(fcu_count, shift(fcu_count,1L)),
  ld_fcu           = log(pmax(fcu_count,1L)) - log(pmax(shift(fcu_count,4L),1L)),
  net_entry_rate    = fifelse(shift(fcu_count,1L)>0,
                       (fcu_count-shift(fcu_count,1L))/shift(fcu_count,1L)*100, NA_real_),
  fcu_count_lag4   = shift(fcu_count,4L),
  # FISCU
  yoy_fiscu_pct     = pct_chg(fiscu_count, shift(fiscu_count,4L)),
  qoq_fiscu_pct     = pct_chg(fiscu_count, shift(fiscu_count,1L)),
  ld_fiscu          = log(pmax(fiscu_count,1L)) - log(pmax(shift(fiscu_count,4L),1L)),
  net_entry_rate_fiscu = fifelse(shift(fiscu_count,1L)>0,
                          (fiscu_count-shift(fiscu_count,1L))/shift(fiscu_count,1L)*100, NA_real_),
  fiscu_count_lag4  = shift(fiscu_count,4L),
  # FCU Assets
  yoy_fcu_assets_pct  = pct_chg(fcu_assets, shift(fcu_assets,4L)),
  qoq_fcu_assets_pct  = pct_chg(fcu_assets, shift(fcu_assets,1L)),
  fcu_assets_lag4     = shift(fcu_assets,4L),
  # FISCU Assets
  yoy_fiscu_assets_pct  = pct_chg(fiscu_assets, shift(fiscu_assets,4L)),
  qoq_fiscu_assets_pct  = pct_chg(fiscu_assets, shift(fiscu_assets,1L)),
  fiscu_assets_lag4     = shift(fiscu_assets,4L),
  # Combined assets
  ln_assets_tot     = log(pmax(assets_tot,1L)),
  yoy_assets_pct    = pct_chg(assets_tot, shift(assets_tot,4L))
), by=categories]

toc()

# ================================================================
# 6. QC
# ================================================================
tic("6. QC")
message("\n[6] Quality checks...")

all_na <- names(qtrly)[vapply(qtrly,function(x)all(is.na(x)),logical(1))]
if (length(all_na)>0) { qtrly[,(all_na):=NULL]
  message(sprintf("    Dropped %d all-NA cols",length(all_na))) }

# Zero-variance check
num_nms    <- names(qtrly)[vapply(qtrly,is.numeric,logical(1))]
zero_var   <- num_nms[vapply(num_nms, function(v) {
  x <- qtrly[[v]]; x <- x[!is.na(x)]; length(x)>0 && var(x)==0 }, logical(1))]
if (length(zero_var)>0) { qtrly[,(zero_var):=NULL]
  message(sprintf("    Dropped %d zero-var cols",length(zero_var))) }

message(sprintf("    Final panel: %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))
toc()

# ================================================================
# 7. SAVE OUTPUTS
# ================================================================
tic("7. Save")
message("\n[7] Saving outputs...")

saveRDS(qtrly, "qtrly_enriched_v5.rds")
message(sprintf("    qtrly_enriched_v5.rds  -- %d x %d",
                nrow(qtrly), ncol(qtrly)))
toc()

# ================================================================
# 8. DIAGNOSTIC PLOTS (Historical Only)
# ================================================================
tic("8. Plots")
message("\n[8] Generating historical diagnostic plots...")

# P1: FCU count by category over time
message("    P1: FCU count by category...")
p1 <- ggplot(qtrly[!is.na(fcu_count)],
             aes(x=as.Date(date), y=fcu_count, colour=cat_label)) +
  geom_line(linewidth=0.9) +
  facet_wrap(~cat_label, scales="free_y", ncol=3) +
  scale_colour_manual(values=CAT_COLOURS, guide="none") +
  scale_x_date(date_labels="%Y", date_breaks="5 years") +
  scale_y_continuous(labels=comma) +
  labs(title="FCU Count by Asset Category -- Historical",
       subtitle=sprintf("Data: %s -> %s",
                        as.character(min(qtrly$date, na.rm=TRUE)),
                        as.character(DATA_END)),
       x=NULL, y="FCU Count") + theme_v5()
save_plot(p1, "P01_fcu_count_historical", w=14, h=10)

# P2: FISCU count by category
message("    P2: FISCU count by category...")
p2 <- ggplot(qtrly[!is.na(fiscu_count)],
             aes(x=as.Date(date), y=fiscu_count, colour=cat_label)) +
  geom_line(linewidth=0.9) +
  facet_wrap(~cat_label, scales="free_y", ncol=3) +
  scale_colour_manual(values=CAT_COLOURS, guide="none") +
  scale_x_date(date_labels="%Y", date_breaks="5 years") +
  scale_y_continuous(labels=comma) +
  labs(title="FISCU Count by Asset Category -- Historical",
       subtitle=sprintf("Data: %s -> %s",
                        as.character(min(qtrly$date, na.rm=TRUE)),
                        as.character(DATA_END)),
       x=NULL, y="FISCU Count") + theme_v5()
save_plot(p2, "P02_fiscu_count_historical", w=14, h=10)

# P3: Total assets by category
message("    P3: Total assets...")
p3 <- ggplot(qtrly[!is.na(assets_tot)],
             aes(x=as.Date(date), y=assets_tot, colour=cat_label)) +
  geom_line(linewidth=0.9) +
  facet_wrap(~cat_label, scales="free_y", ncol=3) +
  scale_colour_manual(values=CAT_COLOURS, guide="none") +
  scale_x_date(date_labels="%Y", date_breaks="5 years") +
  scale_y_continuous(labels=dollar_format(scale=1e-6, suffix="M")) +
  labs(title="Total Assets by Category -- Historical ($000s)",
       subtitle=sprintf("Data: %s -> %s",
                        as.character(min(qtrly$date, na.rm=TRUE)),
                        as.character(DATA_END)),
       x=NULL, y="Total Assets ($000s)") + theme_v5()
save_plot(p3, "P03_assets_historical", w=14, h=10)

# P4: System-wide totals
message("    P4: System totals...")
sys_agg <- qtrly[, .(
  fcu_count    = sum(fcu_count,    na.rm=TRUE),
  fiscu_count   = sum(fiscu_count,   na.rm=TRUE),
  assets_tot    = sum(assets_tot,    na.rm=TRUE)
), by=date]

sys_long <- melt(sys_agg, id.vars="date",
                 variable.name="series", value.name="value")
sys_long[, label := fcase(
  series=="fcu_count",  "System FCU Count",
  series=="fiscu_count", "System FISCU Count",
  series=="assets_tot",  "System Total Assets ($000s)",
  default=as.character(series))]

p4 <- ggplot(sys_long, aes(x=as.Date(date), y=value)) +
  geom_line(linewidth=1.0, colour="#1f77b4") +
  facet_wrap(~label, scales="free_y", ncol=3) +
  scale_x_date(date_labels="%Y", date_breaks="5 years") +
  scale_y_continuous(labels=comma) +
  labs(title="System-Wide Totals -- Historical",
       subtitle=sprintf("Data: %s -> %s",
                        as.character(min(qtrly$date, na.rm=TRUE)),
                        as.character(DATA_END)),
       x=NULL, y=NULL) + theme_v5()
save_plot(p4, "P04_system_totals_historical", w=14, h=6)

# P5: YoY % change in key dep vars
message("    P5: YoY dep var trends...")
dep_vars_plot <- intersect(c("yoy_fcu_pct","yoy_fiscu_pct",
                              "yoy_fcu_assets_pct","yoy_fiscu_assets_pct",
                              "yoy_assets_pct"),
                            names(qtrly))
if (length(dep_vars_plot) > 0) {
  dep_long <- melt(qtrly[, c("date","cat_label",dep_vars_plot), with=FALSE],
                   id.vars=c("date","cat_label"),
                   variable.name="dep_var", value.name="value")
  dep_long[, dep_label := fcase(
    dep_var=="yoy_fcu_pct",         "FCU YoY %",
    dep_var=="yoy_fiscu_pct",        "FISCU YoY %",
    dep_var=="yoy_fcu_assets_pct",  "FCU Assets YoY %",
    dep_var=="yoy_fiscu_assets_pct", "FISCU Assets YoY %",
    dep_var=="yoy_assets_pct",       "Total Assets YoY %",
    default=as.character(dep_var))]

  p5 <- ggplot(dep_long[!is.na(value)],
               aes(x=as.Date(date), y=value, colour=cat_label)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
    geom_line(linewidth=0.7, alpha=0.8) +
    facet_wrap(~dep_label, scales="free_y", ncol=3) +
    scale_colour_manual(values=CAT_COLOURS, name="Category") +
    scale_x_date(date_labels="%Y", date_breaks="5 years") +
    labs(title="YoY % Change in Dependent Variables -- Historical",
         subtitle=sprintf("Data: %s -> %s",
                          as.character(min(qtrly$date, na.rm=TRUE)),
                          as.character(DATA_END)),
         x=NULL, y="YoY % Change") + theme_v5()
  save_plot(p5, "P05_yoy_dep_vars_historical", w=14, h=10)
}

n_plots <- length(list.files(PLOT_DIR, pattern="\\.pdf$"))
toc()

# ================================================================
# 9. FINAL SUMMARY
# ================================================================
tot <- as.numeric((proc.time()-t0)["elapsed"])
toc()  # Part 1 v5 total

message("\n=======================================================")
message(sprintf("PART 1 v5.0 COMPLETE  %dh %02dm %02ds",
                floor(tot/3600), floor((tot%%3600)/60), round(tot%%60)))
message(sprintf("Historical data   : %s -> %s",
                as.character(min(qtrly$date, na.rm=TRUE)),
                as.character(DATA_END)))
message(sprintf("Panel dimensions  : %d rows x %d cols",
                nrow(qtrly), ncol(qtrly)))
message(sprintf("Categories        : %d", length(unique(qtrly$cat_label))))
message("Outputs saved:")
message(sprintf("  qtrly_enriched_v5.rds     %d x %d",
                nrow(qtrly), ncol(qtrly)))
message(sprintf("  %d PDF plots -> %s/", n_plots, PLOT_DIR))
message("=======================================================")

# -- Quick column inventory --
n_yoy   <- sum(grepl("^yoy_",   names(qtrly)))
n_qoq   <- sum(grepl("^qoq_",   names(qtrly)))
n_lag   <- sum(grepl("_lag[0-9]", names(qtrly)))
n_rmean <- sum(grepl("_rmean",   names(qtrly)))
n_rsd   <- sum(grepl("_rsd",     names(qtrly)))
n_ln    <- sum(grepl("^ln_",     names(qtrly)))
n_share <- sum(grepl("^share_",  names(qtrly)))
n_regime<- sum(grepl("^regime_", names(qtrly)))
message(sprintf("\n-- Feature inventory --"))
message(sprintf("  YoY: %d  QoQ: %d  Lags: %d  RollingMean: %d  RollingSD: %d",
                n_yoy, n_qoq, n_lag, n_rmean, n_rsd))
message(sprintf("  Log: %d  Share: %d  Regime: %d  Seasonal: 4  Time: 5",
                n_ln, n_share, n_regime))
message(sprintf("  Total features: ~%d", ncol(qtrly)))

message("\n-- Next step: run macro merge (Part 2) then regression pipeline (Part 3) --")

notify("Part 1 v5.0 DONE",
       sprintf("%dh%02dm | %d rows x %d cols | %d plots",
               floor(tot/3600), floor((tot%%3600)/60),
               nrow(qtrly), ncol(qtrly), n_plots),
       tags="tada")

############################################################
# END
############################################################
