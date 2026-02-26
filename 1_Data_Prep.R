############################################################
# PART 1 v3.0 — PANEL ARIMA PROJECTION + FEATURE ENGINEERING
#
# What's new vs v2:
#   • Extends every aggregated panel variable 5 years forward
#     using auto.arima() (or ETS) per variable × category
#   • Forecast horizon configurable: FORECAST_START → FORECAST_END
#     Default: 2025 Q4 → 2030 Q4  (21 future quarters)
#   • 80% and 95% confidence intervals stored for every
#     key series in every future quarter
#   • Full v2 feature engineering re-runs on the combined
#     historical + forecast panel so ALL lag, rolling,
#     YoY and regime features are valid for future rows
#   • Horizon flags embedded: is_1yr / is_3yr / is_5yr
#   • Three output datasets:
#       qtrly_enriched_v3.rds   — historical (same role as v2)
#       qtrly_forecast_v3.rds   — forecast quarters only
#       qtrly_full_v3.rds       — historical + forecast (full)
#   • Diagnostic outputs:
#       arima_diagnostics_v3.csv — model order + AICc per series
#       arima_models_v3.rds      — all fitted model objects
#       forecast_horizon_summary_v3.csv — point fcst at 1/3/5yr
#   • 11 diagnostic PDF plots → plots_v3/
#
# ── HOW TO CHANGE THE HORIZON ──────────────────────────────
#   Edit only these two lines in the CONFIG section:
#     FORECAST_START <- zoo::as.yearqtr("2025 Q4")
#     FORECAST_END   <- zoo::as.yearqtr("2030 Q4")
#
# Runtime: ~25-60 min depending on # panel variables
############################################################

suppressPackageStartupMessages({
  library(data.table); library(zoo);        library(lubridate)
  library(forecast);   library(httr);       library(ggplot2)
  library(scales);     library(tictoc)
})
shift <- data.table::shift   # prevent masking
options(scipen = 999)
set.seed(42)

# ════════════════════════════════════════════════════════════
# CONFIG  ←── only these lines need changing for a new horizon
# ════════════════════════════════════════════════════════════

FORECAST_START  <- zoo::as.yearqtr("2025 Q4")  # first quarter to forecast
FORECAST_END    <- zoo::as.yearqtr("2030 Q4")  # last  quarter to forecast

# Milestone horizons (for summary tables and plot markers)
HORIZON_1YR  <- zoo::as.yearqtr("2026 Q4")
HORIZON_3YR  <- zoo::as.yearqtr("2028 Q4")
HORIZON_5YR  <- FORECAST_END

# ARIMA settings
MIN_ARIMA_OBS  <- 20L      # minimum non-NA observations to fit ARIMA
ARIMA_SEASONAL <- TRUE     # allow seasonal ARIMA (P,D,Q)[4]
# Method: "auto"  = auto.arima only
#         "ets"   = ETS only
#         "both"  = fit both, keep lower AICc
ARIMA_METHOD   <- "auto"

# ntfy push notifications
NTFY_TOPIC   <- "your-unique-topic-name"   # ← CHANGE THIS
NTFY_ENABLED <- TRUE

# ── Paths ───────────────────────────────────────────────────
DATA_DIR <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR <- file.path(DATA_DIR, "plots_v3")

# ════════════════════════════════════════════════════════════
# HELPERS
# ════════════════════════════════════════════════════════════
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
  tryCatch(
    ggsave(path, plot = p, width = w, height = h, device = cairo_pdf),
    error = function(e)
      ggsave(path, plot = p, width = w, height = h, device = "pdf")
  )
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

theme_v3 <- function()
  theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "#e8f0f7"),
        strip.text       = element_text(face = "bold"),
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(colour = "grey40"),
        legend.position  = "bottom")

# ── Derived config ────────────────────────────────────────────
HORIZON_Q <- as.integer(round(
  (as.numeric(FORECAST_END) - as.numeric(FORECAST_START)) * 4)) + 1L

future_quarters <- seq(FORECAST_START, FORECAST_END, by = 0.25)

# ════════════════════════════════════════════════════════════
# START
# ════════════════════════════════════════════════════════════
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

t0 <- proc.time()
tic("PART 1 v3.0 total")
message("=======================================================")
message(sprintf("PART 1 v3.0  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message(sprintf("Forecast: %s → %s  (%d quarters)",
                as.character(FORECAST_START),
                as.character(FORECAST_END), HORIZON_Q))
message("=======================================================")
notify("Part 1 v3.0 Started",
       sprintf("%s→%s (%dq)", as.character(FORECAST_START),
               as.character(FORECAST_END), HORIZON_Q), tags = "rocket")

setwd(DATA_DIR)

# ════════════════════════════════════════════════════════════
# 1. LOAD & CONSTRUCT KEY VARIABLES  (identical to v2)
# ════════════════════════════════════════════════════════════
tic("1. Load + key vars")
message("\n[1] Loading raw call report data...")

cr <- readRDS("call_report.rds")
setDT(cr)
message(sprintf("    Raw: %s rows x %s cols",
                format(nrow(cr), big.mark=","),
                format(ncol(cr), big.mark=",")))

# ── Build date from year/quarter columns if date not already present ──
if (!"date" %in% names(cr)) {
  # Raw call report: construct date from year + quarter columns
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

# ── Construct indicator columns if not already in the RDS ─────────────
if (!"ficu_count" %in% names(cr)) {
  cr[, `:=`(
    ficu_count  = fifelse(cu_type == 1, 1, 0),
    fiscu_count = fifelse(cu_type == 2, 1, 0)
  )]
}
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
message(sprintf("    Data ends at: %s", as.character(DATA_END)))

# Auto-adjust FORECAST_START if data is newer than configured
if (FORECAST_START <= DATA_END) {
  FORECAST_START  <- DATA_END + 0.25
  future_quarters <- seq(FORECAST_START, FORECAST_END, by = 0.25)
  HORIZON_Q       <- length(future_quarters)
  message(sprintf("    [AUTO] FORECAST_START moved to %s (%d quarters)",
                  as.character(FORECAST_START), HORIZON_Q))
}
toc()

# ════════════════════════════════════════════════════════════
# 2. AUTO-DETECT AGGREGATION TYPE  (identical to v2)
# ════════════════════════════════════════════════════════════
tic("2. Auto-detect agg type")
message("\n[2] Auto-detecting level vs ratio variables...")

detect_agg_type <- function(dt, cols) {
  ratio_pat <- c("_rate$","_ratio$","_shr$","_avg$","_pct$","_rt$",
                 "_oth_fr","_fr_","networth","roa","netmarg","^chg_",
                 "provlnsloss","costfds","retavgasst","netchgoffs",
                 "netintmrg","dq_.*_rate")
  level_pat <- c("^acct_","^lns_","^ins_","^inv_","^dep_","^exp_",
                 "^inc_","^eq_","^liab_","members$","members_pot$",
                 "ficu_count$","fiscu_count$","assets_tot$","^rcv_","^networth_tot")
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
              "ficu_count","fiscu_count","merger","liquid",
              "acquisition","active","ln_assets")
num_cols    <- setdiff(names(cr)[vapply(cr,is.numeric,logical(1))], id_cols)
agg_types   <- detect_agg_type(cr, num_cols)
sum_vars    <- names(agg_types)[agg_types == "sum"]
median_vars <- names(agg_types)[agg_types == "median"]
message(sprintf("    SUM: %d  MEDIAN: %d", length(sum_vars), length(median_vars)))
toc()

# ════════════════════════════════════════════════════════════
# 3. AGGREGATE TO QUARTERLY PANEL  (identical to v2)
# ════════════════════════════════════════════════════════════
tic("3. Quarterly aggregation")
message("\n[3] Aggregating to quarterly panel by category...")

cr_a <- cr[!is.na(date) & !is.na(categories)]

qtrly_counts <- cr_a[, .(
  ficu_count    = sum(ficu_count,    na.rm=TRUE),
  fiscu_count   = sum(fiscu_count,   na.rm=TRUE),
  n_mergers     = sum(merger,        na.rm=TRUE),
  n_liquid      = sum(liquid,        na.rm=TRUE),
  n_acquisition = sum(acquisition,   na.rm=TRUE),
  n_active      = sum(active,        na.rm=TRUE),
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

# ════════════════════════════════════════════════════════════
# 4. ARIMA FORECAST OF EVERY PANEL VARIABLE
# ════════════════════════════════════════════════════════════
tic("4. ARIMA forecasting")
message(sprintf("\n[4] Fitting ARIMA models — %d future quarters...", HORIZON_Q))

cats         <- sort(unique(qtrly$cat_label))
fc_vars      <- setdiff(names(qtrly)[vapply(qtrly,is.numeric,logical(1))],
                        "categories")
# CI kept only for primary dep vars (keeps output manageable)
ci_vars      <- c("ficu_count","fiscu_count","assets_tot","members",
                  "lns_tot","dep_tot","eq_tot")

total_series <- length(fc_vars) * length(cats)
done         <- 0L
message(sprintf("    %d variables × %d categories = %d series",
                length(fc_vars), length(cats), total_series))

arima_diag   <- list()   # diagnostics per series
arima_models <- list()   # fitted model objects

# Wide forecast storage — preallocate one row per (cat × future_quarter)
# then fill in columns as we go
fc_template  <- CJ(cat_label = cats, date = future_quarters)
# Pull integer category code from qtrly (safe — no regex needed)
cat_cat_map  <- unique(qtrly[, .(cat_label, categories)])
fc_template  <- merge(fc_template, cat_cat_map, by="cat_label", all.x=TRUE)
fc_template[, `:=`(
  is_forecast = TRUE,
  horizon_q   = as.integer(round((as.numeric(date) -
                  as.numeric(FORECAST_START))*4)) + 1L,
  is_1yr      = date <= HORIZON_1YR,
  is_3yr      = date <= HORIZON_3YR,
  is_5yr      = date <= HORIZON_5YR
)]
# setorderv only after categories is populated via merge
setorderv(fc_template, c("categories","date"))

fit_one_series <- function(y_vec, n_future, method, seasonal) {
  # Returns list: mean, lo80, hi80, lo95, hi95, diag_info
  n_ok <- sum(!is.na(y_vec))

  if (n_ok < MIN_ARIMA_OBS) {
    last_val <- if (n_ok > 0) tail(y_vec[!is.na(y_vec)], 1) else 0
    mv <- rep(last_val, n_future)
    return(list(mean=mv, lo80=mv, hi80=mv, lo95=mv, hi95=mv,
                method="carry_forward", p=NA,d=NA,q=NA,P=NA,D=NA,Q=NA,
                aicc=NA, n_obs=n_ok, note="too_few_obs"))
  }

  y_ts <- ts(zoo::na.approx(y_vec, na.rm=FALSE), frequency=4)
  y_ts[is.na(y_ts)] <- median(y_ts, na.rm=TRUE)

  fit <- tryCatch({
    if (method == "ets") {
      forecast::ets(y_ts)
    } else if (method == "both") {
      fa <- forecast::auto.arima(y_ts, seasonal=seasonal,
                                 stepwise=TRUE, approximation=TRUE,
                                 max.p=4,max.q=4,max.P=2,max.Q=2,
                                 max.d=2,max.D=1)
      fe <- forecast::ets(y_ts)
      if (isTRUE(fa$aicc <= fe$aicc)) fa else fe
    } else {
      forecast::auto.arima(y_ts, seasonal=seasonal,
                           stepwise=TRUE, approximation=TRUE,
                           max.p=4,max.q=4,max.P=2,max.Q=2,
                           max.d=2,max.D=1)
    }
  }, error = function(e)
    tryCatch(forecast::ets(y_ts, model="AAN"),
             error = function(e2) NULL)
  )

  if (is.null(fit)) {
    mv <- rep(tail(as.numeric(y_ts), 1), n_future)
    return(list(mean=mv, lo80=mv, hi80=mv, lo95=mv, hi95=mv,
                method="naive",p=NA,d=NA,q=NA,P=NA,D=NA,Q=NA,
                aicc=NA, n_obs=n_ok, note="fit_failed"))
  }

  fc <- forecast::forecast(fit, h=n_future, level=c(80,95))

  is_arima <- inherits(fit, "ARIMA") || inherits(fit, "Arima")
  if (is_arima) {
    ord <- arimaorder(fit)
    p_ <- ord["p"]; d_ <- ord["d"]; q_ <- ord["q"]
    P_ <- if("P" %in% names(ord)) ord["P"] else NA_integer_
    D_ <- if("D" %in% names(ord)) ord["D"] else NA_integer_
    Q_ <- if("Q" %in% names(ord)) ord["Q"] else NA_integer_
    mth <- sprintf("ARIMA(%d,%d,%d)", p_, d_, q_)
    if (!is.na(P_) && !is.na(D_) && !is.na(Q_) && (P_+D_+Q_)>0)
      mth <- sprintf("%s(%d,%d,%d)[4]", mth, P_, D_, Q_)
  } else {
    p_<-NA;d_<-NA;q_<-NA;P_<-NA;D_<-NA;Q_<-NA
    mth <- paste0("ETS(", paste(fit$components, collapse=""), ")")
  }

  list(mean = as.numeric(fc$mean),
       lo80 = as.numeric(fc$lower[,1]), hi80 = as.numeric(fc$upper[,1]),
       lo95 = as.numeric(fc$lower[,2]), hi95 = as.numeric(fc$upper[,2]),
       method=mth, p=p_,d=d_,q=q_,P=P_,D=D_,Q=Q_,
       aicc  = tryCatch(fit$aicc, error=function(e) NA_real_),
       n_obs = n_ok, note="ok", fit_obj=fit)
}

for (cat in cats) {
  cat_dt <- qtrly[cat_label == cat]
  setorderv(cat_dt, "date")

  for (vname in fc_vars) {
    done <- done + 1L
    key  <- paste(cat, vname, sep="|")

    res <- fit_one_series(cat_dt[[vname]], HORIZON_Q,
                          ARIMA_METHOD, ARIMA_SEASONAL)

    # Write mean forecast into template
    fc_template[cat_label == cat, (vname) := res$mean]

    # Write CI into named columns only for ci_vars
    if (vname %in% ci_vars) {
      fc_template[cat_label==cat, (paste0(vname,"_lo80")) := res$lo80]
      fc_template[cat_label==cat, (paste0(vname,"_hi80")) := res$hi80]
      fc_template[cat_label==cat, (paste0(vname,"_lo95")) := res$lo95]
      fc_template[cat_label==cat, (paste0(vname,"_hi95")) := res$hi95]
    }

    # Store diagnostics
    arima_diag[[key]] <- data.table(
      cat_label=cat, variable=vname,
      method=res$method, p=res$p, d=res$d, q=res$q,
      P=res$P, D=res$D, Q=res$Q,
      aicc=res$aicc, n_obs=res$n_obs, note=res$note
    )

    # Store model object (skip carry_forward / naive)
    if (!is.null(res$fit_obj))
      arima_models[[key]] <- res$fit_obj

    if (done %% 500 == 0 || done == total_series)
      message(sprintf("    [%d/%d  %.0f%%]  last: %s | %s  method=%s",
                      done, total_series, done/total_series*100,
                      cat, vname, res$method))
  }
}

arima_diag_dt <- rbindlist(arima_diag, fill=TRUE)
message(sprintf("    ARIMA fitted: %d  ETS: %d  fallback: %d",
                sum(grepl("^ARIMA", arima_diag_dt$method), na.rm=TRUE),
                sum(grepl("^ETS",   arima_diag_dt$method), na.rm=TRUE),
                sum(arima_diag_dt$note != "ok", na.rm=TRUE)))
toc()

# ════════════════════════════════════════════════════════════
# 5. COMBINE HISTORICAL + FORECAST PANEL
# ════════════════════════════════════════════════════════════
tic("5. Combine panels")
message("\n[5] Combining historical and forecast panels...")

qtrly[, is_forecast := FALSE]

# ── Step 1: ensure both tables have the same full column set ─────────────
all_cols <- union(names(qtrly), names(fc_template))
for (cn in setdiff(all_cols, names(qtrly)))     qtrly[,       (cn) := NA]
for (cn in setdiff(all_cols, names(fc_template))) fc_template[, (cn) := NA]

# ── Step 2: reconcile class mismatches column by column ─────────────────
# rbindlist is strict about class attributes (e.g. yearqtr vs numeric,
# integer vs numeric). We coerce fc_template columns to match qtrly.
reconcile_classes <- function(dt_target, dt_source) {
  for (cn in names(dt_target)) {
    if (!cn %in% names(dt_source)) next
    cls_t <- class(dt_target[[cn]])[1]
    cls_s <- class(dt_source[[cn]])[1]
    if (identical(cls_t, cls_s)) next

    # yearqtr: always preserve from qtrly
    if (cls_t == "yearqtr" && cls_s != "yearqtr") {
      dt_source[, (cn) := zoo::as.yearqtr(as.numeric(get(cn)))]
    } else if (cls_t %in% c("integer","numeric") && cls_s == "integer") {
      dt_source[, (cn) := as.numeric(get(cn))]
    } else if (cls_t %in% c("integer","numeric") && cls_s == "numeric") {
      dt_source[, (cn) := as.integer(get(cn))]  # rare — keep numeric
      dt_source[, (cn) := as.numeric(get(cn))]
    } else if (cls_t == "character" && cls_s != "character") {
      dt_source[, (cn) := as.character(get(cn))]
    } else if (cls_t == "logical" && cls_s != "logical") {
      dt_source[, (cn) := as.logical(get(cn))]
    }
  }
  invisible(dt_source)
}
reconcile_classes(qtrly, fc_template)   # coerce fc_template → qtrly classes

# ── Step 3: bind — use ignore.attr=TRUE as a final safety net ───────────
qtrly_full <- tryCatch(
  rbindlist(list(qtrly, fc_template), fill=TRUE, ignore.attr=FALSE),
  error = function(e) {
    message(sprintf("    [WARN] rbindlist strict failed: %s", e$message))
    message("    Retrying with ignore.attr=TRUE ...")
    rbindlist(list(qtrly, fc_template), fill=TRUE, ignore.attr=TRUE)
  }
)

# ── Step 4: ensure date is yearqtr after bind ────────────────────────────
if (!inherits(qtrly_full$date, "yearqtr"))
  qtrly_full[, date := zoo::as.yearqtr(as.numeric(date))]

# Ensure categories is integer
if (!is.integer(qtrly_full$categories))
  qtrly_full[, categories := as.integer(categories)]

setorderv(qtrly_full, c("categories","date"))

message(sprintf("    Full panel: %d rows × %d cols  (%d hist + %d forecast)",
                nrow(qtrly_full), ncol(qtrly_full),
                nrow(qtrly), nrow(fc_template)))
toc()

# ════════════════════════════════════════════════════════════
# 6. FEATURE ENGINEERING ON FULL PANEL  (v2 logic, extended)
# ════════════════════════════════════════════════════════════
# Running FE on the combined panel means every future quarter
# gets correct lag-1, lag-4, rolling-mean, YoY, etc. because
# the historical rows immediately precede the forecast rows.
# ════════════════════════════════════════════════════════════
tic("6. Feature engineering")
message("\n[6] Feature engineering on full panel (historical + forecast)...")

base_excl <- c("date","categories","cat_label","is_forecast",
               "horizon_q","is_1yr","is_3yr","is_5yr",
               "ficu_count","fiscu_count",
               "n_mergers","n_liquid","n_acquisition","n_active","n_total",
               grep("_lo80$|_hi80$|_lo95$|_hi95$", names(qtrly_full), value=TRUE))

fe_cols <- setdiff(names(qtrly_full)[vapply(qtrly_full,is.numeric,logical(1))],
                   base_excl)
message(sprintf("    FE base columns: %d", length(fe_cols)))

setorderv(qtrly_full, c("categories","date"))

# 6a: YoY % change (lag-4)
tic("6a YoY"); message("    6a. YoY % change...")
for (v in fe_cols)
  qtrly_full[, (paste0("yoy_",v)) :=
               pct_chg(get(v), shift(get(v),4L)), by=categories]
toc()

# 6b: QoQ % change (lag-1)
tic("6b QoQ"); message("    6b. QoQ % change...")
for (v in fe_cols)
  qtrly_full[, (paste0("qoq_",v)) :=
               pct_chg(get(v), shift(get(v),1L)), by=categories]
toc()

# 6c: Lag levels (1q, 2q, 4q, 8q) — limited to key vars to cap column count
tic("6c Lags"); message("    6c. Lag levels...")
key_lag_vars <- c(fe_cols[1:min(40L,length(fe_cols))],
                  "assets_tot","members","ficu_count","fiscu_count")
key_lag_vars <- intersect(unique(key_lag_vars), names(qtrly_full))
for (lag_n in c(1L,2L,4L,8L))
  for (v in key_lag_vars)
    qtrly_full[, (paste0(v,"_lag",lag_n)) :=
                 shift(get(v),lag_n), by=categories]
toc()

# 6d: Rolling means (4q, 8q, 12q) on YoY of key vars
tic("6d Rolling means"); message("    6d. Rolling means...")
rollmean_safe <- function(x, k)
  zoo::rollapply(x, width=k, FUN=mean, na.rm=TRUE,
                 fill=NA, align="right", partial=FALSE)
yoy_key <- paste0("yoy_", key_lag_vars)
yoy_key <- intersect(yoy_key, names(qtrly_full))
for (k in c(4L,8L,12L)) {
  nms <- paste0(yoy_key,"_rmean",k)
  qtrly_full[, (nms) := lapply(.SD, rollmean_safe, k=k),
             .SDcols=yoy_key, by=categories]
}
toc()

# 6e: Rolling SDs (4q, 8q)
tic("6e Rolling SDs"); message("    6e. Rolling SDs...")
rollsd_safe <- function(x, k)
  zoo::rollapply(x, width=k, FUN=sd, na.rm=TRUE,
                 fill=NA, align="right", partial=FALSE)
for (k in c(4L,8L)) {
  nms <- paste0(yoy_key,"_rsd",k)
  qtrly_full[, (nms) := lapply(.SD, rollsd_safe, k=k),
             .SDcols=yoy_key, by=categories]
}
toc()

# 6f: YoY acceleration (momentum)
tic("6f Momentum"); message("    6f. Momentum...")
for (v in yoy_key)
  qtrly_full[, (paste0(v,"_accel")) :=
               get(v) - shift(get(v),4L), by=categories]
toc()

# 6g: Cyclical deviation from 8q rolling mean
tic("6g Cyclical"); message("    6g. Cyclical deviation...")
dev_vars <- fe_cols[1:min(20L,length(fe_cols))]
dev_vars <- intersect(dev_vars, names(qtrly_full))
for (v in dev_vars) {
  yv <- paste0("yoy_",v); rv <- paste0("yoy_",v,"_rmean8")
  if (all(c(yv,rv) %in% names(qtrly_full)))
    qtrly_full[, (paste0(yv,"_cyc")) :=
                 get(yv) - get(rv), by=categories]
}
toc()

# 6h: M&A rate features
tic("6h M&A rates"); message("    6h. M&A rates...")
qtrly_full[ficu_count > 0, `:=`(
  merger_rate      = n_mergers     / ficu_count * 100,
  liquid_rate      = n_liquid      / ficu_count * 100,
  acquisition_rate = n_acquisition / ficu_count * 100,
  exit_rate        = (n_mergers + n_liquid) / ficu_count * 100
)]
ma_vars <- intersect(c("merger_rate","liquid_rate","acquisition_rate","exit_rate"),
                     names(qtrly_full))
for (v in ma_vars) {
  qtrly_full[, (paste0("yoy_",v)) :=
               pct_chg(get(v), shift(get(v),4L)), by=categories]
  qtrly_full[, (paste0("qoq_",v)) :=
               pct_chg(get(v), shift(get(v),1L)), by=categories]
}
qtrly_full[, exit_roll4 := zoo::rollapply(
  n_mergers+n_liquid, width=4, FUN=sum, na.rm=TRUE,
  fill=NA, align="right"), by=categories]
toc()

# 6i: Time index and regime features
tic("6i Time/regime"); message("    6i. Time index and regime features...")
all_dates_full <- sort(unique(qtrly_full$date))
date_idx_full  <- data.table(date=all_dates_full,
                              time_idx=seq_along(all_dates_full))
qtrly_full <- merge(qtrly_full, date_idx_full, by="date", all.x=TRUE)

qtrly_full[, `:=`(
  qtrs_from_gfc   = as.integer((as.numeric(date)-as.numeric(zoo::as.yearqtr("2009 Q2")))*4),
  qtrs_from_zirp  = as.integer((as.numeric(date)-as.numeric(zoo::as.yearqtr("2015 Q4")))*4),
  qtrs_from_covid = as.integer((as.numeric(date)-as.numeric(zoo::as.yearqtr("2020 Q1")))*4),
  qtrs_from_hike  = as.integer((as.numeric(date)-as.numeric(zoo::as.yearqtr("2022 Q1")))*4)
)]
qtrly_full[, `:=`(
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

# 6j: Log transforms
tic("6j Logs"); message("    6j. Log transforms...")
log_vars <- intersect(c("assets_tot","members","members_pot",
                         "lns_tot","dep_tot","eq_tot","liab_tot",
                         "inc_net","exp_tot","inv_tot"),
                       names(qtrly_full))
for (v in log_vars)
  qtrly_full[, (paste0("ln_",v)) := log(pmax(get(v),1L,na.rm=TRUE))]
toc()

# 6k: System-share features
tic("6k System-share"); message("    6k. System-share features...")
share_vars <- intersect(c("ficu_count","fiscu_count","assets_tot",
                           "members","lns_tot","dep_tot"),
                         names(qtrly_full))
for (v in share_vars) {
  sys_col <- paste0("sys_",v); sh_col <- paste0("share_",v)
  qtrly_full[, (sys_col) := sum(get(v),na.rm=TRUE), by=date]
  qtrly_full[get(sys_col)>0, (sh_col) := get(v)/get(sys_col)*100]
  qtrly_full[, (paste0("yoy_",sh_col)) :=
               pct_chg(get(sh_col), shift(get(sh_col),4L)), by=categories]
  qtrly_full[, (sys_col) := NULL]
}
toc()
toc()  # FE total

# ════════════════════════════════════════════════════════════
# 7. DEPENDENT VARIABLE CONSTRUCTION  (v2 + v3 horizon flags)
# ════════════════════════════════════════════════════════════
tic("7. Dependent variables")
message("\n[7] Constructing dependent variables...")

setorderv(qtrly_full, c("categories","date"))

qtrly_full[, `:=`(
  # FICU
  yoy_ficu_pct      = pct_chg(ficu_count, shift(ficu_count,4L)),
  qoq_ficu_pct      = pct_chg(ficu_count, shift(ficu_count,1L)),
  ld_ficu           = log(pmax(ficu_count,1L)) - log(pmax(shift(ficu_count,4L),1L)),
  net_entry_rate    = fifelse(shift(ficu_count,1L)>0,
                       (ficu_count-shift(ficu_count,1L))/shift(ficu_count,1L)*100, NA_real_),
  ficu_count_lag4   = shift(ficu_count,4L),
  # FISCU
  yoy_fiscu_pct     = pct_chg(fiscu_count, shift(fiscu_count,4L)),
  qoq_fiscu_pct     = pct_chg(fiscu_count, shift(fiscu_count,1L)),
  ld_fiscu          = log(pmax(fiscu_count,1L)) - log(pmax(shift(fiscu_count,4L),1L)),
  net_entry_rate_fiscu = fifelse(shift(fiscu_count,1L)>0,
                          (fiscu_count-shift(fiscu_count,1L))/shift(fiscu_count,1L)*100, NA_real_),
  fiscu_count_lag4  = shift(fiscu_count,4L),
  # Assets
  ln_assets_tot     = log(pmax(assets_tot,1L))
), by=categories]

toc()

# ════════════════════════════════════════════════════════════
# 8. QC
# ════════════════════════════════════════════════════════════
tic("8. QC")
message("\n[8] Quality checks...")

all_na <- names(qtrly_full)[vapply(qtrly_full,function(x)all(is.na(x)),logical(1))]
if (length(all_na)>0) { qtrly_full[,(all_na):=NULL]
  message(sprintf("    Dropped %d all-NA cols",length(all_na))) }

# Zero-variance check on historical rows only
hist_only  <- qtrly_full[is_forecast==FALSE]
num_nms    <- names(qtrly_full)[vapply(qtrly_full,is.numeric,logical(1))]
zero_var   <- num_nms[vapply(num_nms, function(v) {
  x <- hist_only[[v]]; x <- x[!is.na(x)]; length(x)>0 && var(x)==0 }, logical(1))]
if (length(zero_var)>0) { qtrly_full[,(zero_var):=NULL]
  message(sprintf("    Dropped %d zero-var cols",length(zero_var))) }

message(sprintf("    Final full panel: %d rows × %d cols",
                nrow(qtrly_full), ncol(qtrly_full)))
toc()

# ════════════════════════════════════════════════════════════
# 9. SAVE OUTPUTS
# ════════════════════════════════════════════════════════════
tic("9. Save")
message("\n[9] Saving outputs...")

ci_drop <- grep("_lo80$|_hi80$|_lo95$|_hi95$|horizon_q|is_1yr|is_3yr|is_5yr",
                names(qtrly_full), value=TRUE)

# (a) Historical only — drop forecast rows + CI cols
qtrly_hist <- qtrly_full[is_forecast==FALSE, !ci_drop, with=FALSE]
saveRDS(qtrly_hist, "qtrly_enriched_v3.rds")
message(sprintf("    qtrly_enriched_v3.rds  — %d × %d  (historical)",
                nrow(qtrly_hist), ncol(qtrly_hist)))

# (b) Forecast quarters only
qtrly_fc <- qtrly_full[is_forecast==TRUE]
saveRDS(qtrly_fc, "qtrly_forecast_v3.rds")
message(sprintf("    qtrly_forecast_v3.rds  — %d × %d  (forecast)",
                nrow(qtrly_fc), ncol(qtrly_fc)))

# (c) Full panel (historical + forecast) — used by Parts 2 & 3 v3
saveRDS(qtrly_full, "qtrly_full_v3.rds")
message(sprintf("    qtrly_full_v3.rds      — %d × %d  (full)",
                nrow(qtrly_full), ncol(qtrly_full)))

# (d) Diagnostics
fwrite(arima_diag_dt, "arima_diagnostics_v3.csv")
saveRDS(arima_models, "arima_models_v3.rds")
message(sprintf("    arima_diagnostics_v3.csv  (%d series)", nrow(arima_diag_dt)))
message(sprintf("    arima_models_v3.rds       (%d fitted objects)", length(arima_models)))

# (e) Horizon summary table
horizon_sum <- qtrly_full[is_forecast==TRUE &
                            date %in% c(HORIZON_1YR,HORIZON_3YR,HORIZON_5YR),
                           .(cat_label, date,
                             ficu_count, fiscu_count, assets_tot, members,
                             yoy_ficu_pct, yoy_fiscu_pct, ln_assets_tot,
                             ficu_lo95 = if("ficu_count_lo95"%in%names(qtrly_full))
                                           ficu_count_lo95 else NA_real_,
                             ficu_hi95 = if("ficu_count_hi95"%in%names(qtrly_full))
                                           ficu_count_hi95 else NA_real_)]
horizon_sum[, horizon := fcase(
  date==HORIZON_1YR, "1-Year",
  date==HORIZON_3YR, "3-Year",
  date==HORIZON_5YR, "5-Year",
  default=NA_character_)]
fwrite(horizon_sum, "forecast_horizon_summary_v3.csv")
message("    forecast_horizon_summary_v3.csv")
toc()

# ════════════════════════════════════════════════════════════
# 10. DIAGNOSTIC PLOTS
# ════════════════════════════════════════════════════════════
tic("10. Plots")
message("\n[10] Generating diagnostic plots...")

fc_start_date <- as.Date(FORECAST_START)

# Helper: standard time-series plot with CI ribbon
plot_fc_series <- function(var_nm, y_label, lo95=NULL, hi95=NULL,
                            lo80=NULL, hi80=NULL,
                            y_scale="comma", stem=NULL) {

  hist_dt <- qtrly_full[is_forecast==FALSE & !is.na(get(var_nm)),
                         .(date=as.Date(date), cat_label, y=get(var_nm))]
  fc_dt   <- qtrly_full[is_forecast==TRUE,
                         .(date=as.Date(date), cat_label, y=get(var_nm))]
  if (nrow(fc_dt)==0 || nrow(hist_dt)==0) return(invisible(NULL))

  has_ci95 <- !is.null(lo95) && lo95 %in% names(qtrly_full) &&
              !is.null(hi95) && hi95 %in% names(qtrly_full)
  has_ci80 <- !is.null(lo80) && lo80 %in% names(qtrly_full) &&
              !is.null(hi80) && hi80 %in% names(qtrly_full)

  if (has_ci95) {
    ci95 <- qtrly_full[is_forecast==TRUE,
                        .(date=as.Date(date), cat_label,
                          lo=get(lo95), hi=get(hi95))]
    fc_dt <- merge(fc_dt, ci95, by=c("date","cat_label"), all.x=TRUE)
  }
  if (has_ci80) {
    ci80 <- qtrly_full[is_forecast==TRUE,
                        .(date=as.Date(date), cat_label,
                          lo80=get(lo80), hi80=get(hi80))]
    fc_dt <- merge(fc_dt, ci80, by=c("date","cat_label"), all.x=TRUE)
  }

  scale_y_fn <- if (y_scale=="dollar")
    scale_y_continuous(labels=dollar_format(scale=1e-6, suffix="M"))
  else
    scale_y_continuous(labels=comma)

  p <- ggplot() +
    annotate("rect", xmin=fc_start_date, xmax=as.Date(FORECAST_END)+100,
             ymin=-Inf, ymax=Inf, fill="#fffde7", alpha=0.55) +
    # 95% CI (lightest)
    {if(has_ci95 && "lo" %in% names(fc_dt))
       geom_ribbon(data=fc_dt, aes(x=date,ymin=lo,ymax=hi,fill=cat_label),
                   alpha=0.12) else NULL} +
    # 80% CI (darker)
    {if(has_ci80 && "lo80" %in% names(fc_dt))
       geom_ribbon(data=fc_dt, aes(x=date,ymin=lo80,ymax=hi80,fill=cat_label),
                   alpha=0.22) else NULL} +
    geom_line(data=hist_dt, aes(x=date,y=y,colour=cat_label), linewidth=0.9) +
    geom_line(data=fc_dt,   aes(x=date,y=y,colour=cat_label),
              linewidth=0.85, linetype="dashed") +
    geom_vline(xintercept=fc_start_date, linetype="dotted",
               colour="grey50", linewidth=0.6) +
    # Horizon markers
    geom_vline(xintercept=c(as.Date(HORIZON_1YR),
                             as.Date(HORIZON_3YR),
                             as.Date(HORIZON_5YR)),
               linetype="dotdash", colour="grey65", linewidth=0.4) +
    annotate("text", x=as.Date(HORIZON_1YR)+10, y=Inf,
             label="1yr", size=2.8, colour="grey50", vjust=1.5, hjust=0) +
    annotate("text", x=as.Date(HORIZON_3YR)+10, y=Inf,
             label="3yr", size=2.8, colour="grey50", vjust=1.5, hjust=0) +
    annotate("text", x=as.Date(HORIZON_5YR)-10, y=Inf,
             label="5yr", size=2.8, colour="grey50", vjust=1.5, hjust=1) +
    facet_wrap(~cat_label, scales="free_y", ncol=3) +
    scale_colour_manual(values=CAT_COLOURS, guide="none") +
    scale_fill_manual(values=CAT_COLOURS, guide="none") +
    scale_x_date(date_labels="%Y", date_breaks="3 years") +
    scale_y_fn +
    labs(title    = sprintf("%s — History & ARIMA 5-Year Forecast", y_label),
         subtitle = sprintf("Dashed = forecast  |  Bands: 80%% (dark) & 95%% (light) CI  |  Yellow = forecast region  |  %s→%s",
                            as.character(FORECAST_START),
                            as.character(FORECAST_END)),
         x=NULL, y=y_label) +
    theme_v3()

  if (!is.null(stem)) save_plot(p, stem, w=14, h=10)
  invisible(p)
}

# P1–P4: primary series
message("    P1: FICU count..."); plot_fc_series(
  "ficu_count","FICU Count","ficu_count_lo95","ficu_count_hi95",
  "ficu_count_lo80","ficu_count_hi80", stem="P01_ficu_count_forecast")

message("    P2: FISCU count..."); plot_fc_series(
  "fiscu_count","FISCU Count","fiscu_count_lo95","fiscu_count_hi95",
  "fiscu_count_lo80","fiscu_count_hi80", stem="P02_fiscu_count_forecast")

message("    P3: Total assets..."); plot_fc_series(
  "assets_tot","Total Assets ($000s)",
  "assets_tot_lo95","assets_tot_hi95",
  "assets_tot_lo80","assets_tot_hi80",
  y_scale="dollar", stem="P03_assets_forecast")

message("    P4: Members..."); plot_fc_series(
  "members","Member Count", stem="P04_members_forecast")

# P5: System-wide totals
message("    P5: System totals...")
sys_agg <- qtrly_full[, .(
  ficu_count    = sum(ficu_count,    na.rm=TRUE),
  fiscu_count   = sum(fiscu_count,   na.rm=TRUE),
  assets_tot    = sum(assets_tot,    na.rm=TRUE),
  ficu_lo95     = sum(ficu_count_lo95, na.rm=TRUE),
  ficu_hi95     = sum(ficu_count_hi95, na.rm=TRUE),
  assets_lo95   = sum(assets_tot_lo95, na.rm=TRUE),
  assets_hi95   = sum(assets_tot_hi95, na.rm=TRUE),
  is_forecast   = any(is_forecast)
), by=date]

sys_long <- melt(sys_agg[, .(date, is_forecast,
                               ficu_count, fiscu_count, assets_tot)],
                 id.vars=c("date","is_forecast"),
                 variable.name="series", value.name="value")
sys_long[, label := fcase(
  series=="ficu_count",  "System FICU Count",
  series=="fiscu_count", "System FISCU Count",
  series=="assets_tot",  "System Total Assets ($000s)",
  default=as.character(series))]

p5 <- ggplot(sys_long, aes(x=as.Date(date), y=value,
                            colour=is_forecast)) +
  annotate("rect", xmin=fc_start_date, xmax=as.Date(FORECAST_END)+100,
           ymin=-Inf, ymax=Inf, fill="#fffde7", alpha=0.55) +
  geom_line(linewidth=1.0) +
  geom_vline(xintercept=c(as.Date(HORIZON_1YR),as.Date(HORIZON_3YR)),
             linetype="dotdash", colour="grey65", linewidth=0.4) +
  facet_wrap(~label, scales="free_y", ncol=3) +
  scale_colour_manual(values=c("FALSE"="#1f77b4","TRUE"="#d62728"),
                      labels=c("FALSE"="Historical","TRUE"="Forecast"),name=NULL) +
  scale_x_date(date_labels="%Y", date_breaks="3 years") +
  scale_y_continuous(labels=comma) +
  labs(title="System-Wide Totals — Historical & ARIMA 5-Year Forecast",
       subtitle=sprintf("Red = ARIMA forecast  |  1yr=%s  3yr=%s  5yr=%s",
                        as.character(HORIZON_1YR),
                        as.character(HORIZON_3YR),
                        as.character(HORIZON_5YR)),
       x=NULL, y=NULL) + theme_v3()
save_plot(p5, "P05_system_totals_forecast", w=14, h=6)

# P6: YoY % change forecasts (dep vars)
message("    P6: YoY dep var forecasts...")
plot_fc_series("yoy_ficu_pct",  "FICU YoY % Change",  stem="P06_yoy_ficu_forecast")
plot_fc_series("yoy_fiscu_pct", "FISCU YoY % Change", stem="P07_yoy_fiscu_forecast")

# P8: ARIMA order distribution
message("    P8: ARIMA diagnostics...")
if (nrow(arima_diag_dt)>0) {
  ord_cnt <- arima_diag_dt[note=="ok", .N, by=method][order(-N)]
  p8 <- ggplot(head(ord_cnt,25), aes(x=reorder(method,N), y=N)) +
    geom_col(fill="#2171b5") +
    geom_text(aes(label=N), hjust=-0.1, size=3) +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0,0.2))) +
    labs(title="Most Frequent ARIMA Orders (All Series)",
         x=NULL, y="# Series") + theme_v3()
  save_plot(p8, "P08_arima_model_orders", w=10, h=7)

  p8b <- ggplot(arima_diag_dt[!is.na(aicc)],
                aes(x=aicc, fill=cat_label)) +
    geom_histogram(bins=40, colour="white", alpha=0.8) +
    facet_wrap(~cat_label, scales="free", ncol=3) +
    scale_fill_manual(values=CAT_COLOURS, guide="none") +
    labs(title="AICc Distribution by Category",
         subtitle="Lower = better ARIMA fit", x="AICc",y="Count") + theme_v3()
  save_plot(p8b, "P09_arima_aicc_distribution", w=13, h=8)
}

# P10: Fan chart for FICU at system level
message("    P10: Fan charts...")
p10 <- ggplot() +
  annotate("rect", xmin=fc_start_date, xmax=as.Date(FORECAST_END)+100,
           ymin=-Inf, ymax=Inf, fill="#fffde7", alpha=0.55) +
  geom_ribbon(data=sys_agg[is_forecast==TRUE & !is.na(ficu_lo95)],
              aes(x=as.Date(date), ymin=ficu_lo95, ymax=ficu_hi95),
              fill="#1f77b4", alpha=0.15) +
  geom_line(data=sys_agg,
            aes(x=as.Date(date), y=ficu_count,
                colour=is_forecast), linewidth=1.0) +
  geom_vline(xintercept=fc_start_date,
             linetype="dotted", colour="grey50") +
  scale_colour_manual(values=c("FALSE"="#1f77b4","TRUE"="#d62728"),
                      labels=c("FALSE"="Historical","TRUE"="Forecast"),name=NULL) +
  scale_x_date(date_labels="%Y", date_breaks="2 years") +
  scale_y_continuous(labels=comma) +
  labs(title="System FICU Count — Fan Chart (95% CI)",
       subtitle="Band = 95% CI summed across all categories",
       x=NULL, y="System FICU Count") + theme_v3()
save_plot(p10, "P10_system_ficu_fan_chart", w=12, h=6)

# P11: Horizon heatmap
message("    P11: Horizon heatmap...")
if (nrow(horizon_sum)>0 && "ficu_count" %in% names(horizon_sum)) {
  p11 <- ggplot(horizon_sum, aes(x=horizon, y=cat_label, fill=ficu_count)) +
    geom_tile(colour="white", linewidth=0.8) +
    geom_text(aes(label=comma(round(ficu_count))), size=3.2) +
    scale_fill_gradient(low="#deebf7", high="#084594",
                        name="FICU Count", labels=comma) +
    scale_y_discrete(limits=rev) +
    scale_x_discrete(limits=c("1-Year","3-Year","5-Year")) +
    labs(title="FICU Count Point Forecast at 1, 3, and 5-Year Horizons",
         subtitle="ARIMA mean forecast", x=NULL, y=NULL) +
    theme_v3() + theme(legend.position="right")
  save_plot(p11, "P11_ficu_horizon_heatmap", w=9, h=6)
}

n_plots <- length(list.files(PLOT_DIR, pattern="\\.pdf$"))
toc()

# ════════════════════════════════════════════════════════════
# 11. FINAL SUMMARY
# ════════════════════════════════════════════════════════════
tot <- as.numeric((proc.time()-t0)["elapsed"])
toc()  # Part 1 v3 total

message("\n=======================================================")
message(sprintf("PART 1 v3.0 COMPLETE  %dh %02dm %02ds",
                floor(tot/3600), floor((tot%%3600)/60), round(tot%%60)))
message(sprintf("Historical data   : 2000 Q1 → %s", as.character(DATA_END)))
message(sprintf("Forecast period   : %s → %s  (%d quarters)",
                as.character(FORECAST_START),
                as.character(FORECAST_END), HORIZON_Q))
message(sprintf("Milestones        : 1yr=%s  3yr=%s  5yr=%s",
                as.character(HORIZON_1YR),
                as.character(HORIZON_3YR),
                as.character(HORIZON_5YR)))
message(sprintf("ARIMA fitted      : %d  |  ETS: %d  |  fallback: %d",
                sum(grepl("^ARIMA",arima_diag_dt$method),na.rm=TRUE),
                sum(grepl("^ETS",  arima_diag_dt$method),na.rm=TRUE),
                sum(arima_diag_dt$note!="ok",na.rm=TRUE)))
message("Outputs saved:")
message(sprintf("  qtrly_enriched_v3.rds     %d × %d  (historical)",
                nrow(qtrly_hist), ncol(qtrly_hist)))
message(sprintf("  qtrly_forecast_v3.rds     %d × %d  (forecast)",
                nrow(qtrly_fc), ncol(qtrly_fc)))
message(sprintf("  qtrly_full_v3.rds         %d × %d  (full panel)",
                nrow(qtrly_full), ncol(qtrly_full)))
message("  arima_diagnostics_v3.csv   arima_models_v3.rds")
message("  forecast_horizon_summary_v3.csv")
message(sprintf("  %d PDF plots → %s/", n_plots, PLOT_DIR))
message("=======================================================")

message("\n── System-wide forecast at key horizons ──")
for (h_lbl in c("1-Year","3-Year","5-Year")) {
  sub <- horizon_sum[horizon==h_lbl,
                      .(ficu  = sum(ficu_count,  na.rm=TRUE),
                        fiscu = sum(fiscu_count, na.rm=TRUE),
                        assets_B = sum(assets_tot, na.rm=TRUE)/1e6)]
  if (nrow(sub)>0)
    message(sprintf("  %s:  FICU=%s  FISCU=%s  Assets=$%.1fB",
                    h_lbl,
                    format(round(sub$ficu),  big.mark=","),
                    format(round(sub$fiscu), big.mark=","),
                    sub$assets_B))
}

message("\n── Next step: run macro_v3_arima.R to extend FRED series ──")
message("   Then run part2_v3.R and part3_v3.R for modeling")

notify("Part 1 v3.0 DONE",
       sprintf("%dh%02dm | %dq | %d ARIMA | %d plots",
               floor(tot/3600), floor((tot%%3600)/60),
               HORIZON_Q,
               sum(arima_diag_dt$note=="ok",na.rm=TRUE),
               n_plots),
       tags="tada")

############################################################
# END
############################################################
