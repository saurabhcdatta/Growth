############################################################
# MACRO v3.0 — FRED ARIMA PROJECTION + FEATURE ENGINEERING
#
# What's new vs v2:
#   • Extends every FRED macro series to FORECAST_END using
#     auto.arima() — the same method as Part 1 v3
#   • Fills future rows of qtrly_full_v3.rds with forecast
#     macro values so the full panel is model-ready
#   • All macro feature engineering (YoY, QoQ, rolling, lags,
#     composites) runs on the COMBINED historical + forecast
#     macro table, so every future quarter has valid features
#   • Confidence intervals stored for key macro series
#   • Scenario overrides: user can replace ARIMA paths with
#     custom assumptions (e.g. "Fed cuts to 3% by 2026 Q1")
#   • Outputs:
#       macro_raw_v3.rds          — raw FRED (historical)
#       macro_features_v3.rds     — full engineered macro (hist + fcst)
#       macro_forecast_v3.csv     — point forecasts at all future qtrs
#       macro_scenarios_v3.csv    — scenario override table
#       qtrly_enriched_v3.rds     — historical panel + macro (overwrites)
#       qtrly_full_v3.rds         — full panel + macro (overwrites)
#   • 8 PDF diagnostic plots → plots_v3/
#
# ── HOW TO CHANGE THE HORIZON ──────────────────────────────
#   Match FORECAST_START / FORECAST_END to Part 1 v3 values
#
# ── HOW TO ADD SCENARIO OVERRIDES ──────────────────────────
#   Edit the SCENARIO_OVERRIDES list in CONFIG. Each entry:
#     list(variable = "fedfunds",
#          start_q  = "2026 Q1",
#          end_q    = "2027 Q4",
#          values   = c(4.0, 3.75, 3.5, 3.25, 3.0, 3.0, 3.0, 3.0))
#   Leave SCENARIO_OVERRIDES <- list() to use ARIMA only.
#
# Runtime: ~5-15 min (FRED pull + ARIMA + FE)
############################################################

suppressPackageStartupMessages({
  library(data.table); library(zoo);       library(httr)
  library(lubridate);  library(forecast);  library(fredr)
  library(ggplot2);    library(scales);    library(tictoc)
})
shift <- data.table::shift
options(scipen = 999)
set.seed(42)

# ════════════════════════════════════════════════════════════
# CONFIG  ←── match to Part 1 v3
# ════════════════════════════════════════════════════════════

FORECAST_START <- zoo::as.yearqtr("2025 Q4")
FORECAST_END   <- zoo::as.yearqtr("2030 Q4")
HORIZON_1YR    <- zoo::as.yearqtr("2026 Q4")
HORIZON_3YR    <- zoo::as.yearqtr("2028 Q4")
HORIZON_5YR    <- FORECAST_END

# ARIMA settings
ARIMA_SEASONAL <- TRUE
ARIMA_METHOD   <- "auto"   # "auto" | "ets" | "both"
MIN_ARIMA_OBS  <- 16L      # macro series tend to be shorter

# ── Scenario overrides (ARIMA replaced by user assumptions) ──
# Set to list() to use pure ARIMA for every series.
# Each element is a list with:
#   variable : short name matching FRED_SERIES keys
#   start_q  : first quarter to override (as string)
#   end_q    : last  quarter to override (as string)
#   values   : numeric vector of length = quarters in range
# Example (Fed funds gradual cut scenario):
SCENARIO_OVERRIDES <- list(
  # list(variable = "fedfunds",
  #      start_q  = "2026 Q1",
  #      end_q    = "2027 Q4",
  #      values   = c(4.25, 4.0, 3.75, 3.5, 3.25, 3.25, 3.0, 3.0))
)

# FRED API key — store in .Renviron as: FRED_API_KEY=xxxxxxxx
FRED_API_KEY <- Sys.getenv("FRED_API_KEY")
if (nchar(FRED_API_KEY) == 0) {
  FRED_API_KEY <- "YOUR_FRED_API_KEY_HERE"   # ← fallback
  message("  WARNING: FRED_API_KEY not in .Renviron")
}
fredr_set_key(FRED_API_KEY)

FRED_START <- as.Date("1995-01-01")   # extra history for rolling stats

# Named list of FRED series to pull
FRED_SERIES <- list(
  fedfunds    = "FEDFUNDS",      gs2         = "GS2",
  gs10        = "GS10",          mortgage30  = "MORTGAGE30US",
  baa_spread  = "BAA10Y",        yield_curve = "T10Y3M",
  yield_2_10  = "T10Y2Y",        hy_spread   = "BAMLH0A0HYM2",
  unrate      = "UNRATE",        payems      = "PAYEMS",
  icsa        = "ICSA",          gdp_real    = "GDPC1",
  indpro      = "INDPRO",        housing     = "HOUST",
  cpi         = "CPIAUCSL",      pce         = "PCEPI",
  oil_wti     = "DCOILWTICO",    bus_loans   = "BUSLOANS",
  bank_deps   = "DPSACBW027SBOG",cons_credit = "TOTALSL",
  umcsent     = "UMCSENT",       inf_exp     = "MICH",
  m2          = "M2SL"
)

# ntfy
NTFY_TOPIC   <- "your-unique-topic-name"   # ← CHANGE THIS
NTFY_ENABLED <- TRUE

# Paths
DATA_DIR <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR <- file.path(DATA_DIR, "plots_v3")

# ════════════════════════════════════════════════════════════
# HELPERS
# ════════════════════════════════════════════════════════════
notify <- function(title, msg, tags = NULL) {
  if (!NTFY_ENABLED) return(invisible(NULL))
  tryCatch({
    h <- list(Title=title)
    if (!is.null(tags)) h$Tags <- paste(tags,collapse=",")
    httr::POST(paste0("https://ntfy.sh/",NTFY_TOPIC),
               body=msg, encode="raw",
               do.call(httr::add_headers,h))
  }, error=function(e) NULL)
}

save_plot <- function(p, stem, w=12, h=7) {
  path <- file.path(PLOT_DIR, paste0(stem,".pdf"))
  tryCatch(ggsave(path,p,width=w,height=h,device=cairo_pdf),
           error=function(e) ggsave(path,p,width=w,height=h,device="pdf"))
  invisible(path)
}

pct_chg_n <- function(x, n) {
  lag_x <- shift(x, n, type="lag")
  fifelse(!is.na(lag_x) & lag_x != 0 & abs(lag_x) > 1e-6,
          (x - lag_x) / abs(lag_x) * 100, NA_real_)
}

theme_v3 <- function()
  theme_bw(base_size=11) +
  theme(strip.background = element_rect(fill="#e8f0f7"),
        strip.text       = element_text(face="bold"),
        plot.title       = element_text(face="bold", size=12),
        plot.subtitle    = element_text(colour="grey40"),
        legend.position  = "bottom")

HORIZON_Q     <- as.integer(round(
  (as.numeric(FORECAST_END)-as.numeric(FORECAST_START))*4)) + 1L
future_qtrs   <- seq(FORECAST_START, FORECAST_END, by=0.25)

# ════════════════════════════════════════════════════════════
# START
# ════════════════════════════════════════════════════════════
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive=TRUE)

t0 <- proc.time()
tic("MACRO v3.0 total")
message("=======================================================")
message(sprintf("MACRO v3.0  %s", format(Sys.time(),"%Y-%m-%d %H:%M:%S")))
message(sprintf("Forecast: %s → %s  (%d quarters)",
                as.character(FORECAST_START),
                as.character(FORECAST_END), HORIZON_Q))
if (length(SCENARIO_OVERRIDES)>0)
  message(sprintf("Scenario overrides: %d series",
                  length(SCENARIO_OVERRIDES)))
message("=======================================================")
notify("Macro v3.0 Started",
       sprintf("%s→%s (%dq)",
               as.character(FORECAST_START),
               as.character(FORECAST_END),HORIZON_Q), tags="rocket")

setwd(DATA_DIR)

# ════════════════════════════════════════════════════════════
# 1. PULL FRED DATA  (identical to v2)
# ════════════════════════════════════════════════════════════
tic("1. Pull FRED")
message("\n[1] Pulling FRED data...")

pull_fred <- function(series_id, short_name, start=FRED_START,
                      end=Sys.Date()) {
  tryCatch({
    df <- fredr::fredr(series_id=series_id,
                       observation_start=start, observation_end=end,
                       frequency="q", aggregation_method="avg")
    dt <- as.data.table(df)[, .(date=zoo::as.yearqtr(date), value)]
    setnames(dt, "value", short_name); dt
  }, error=function(e) {
    message(sprintf("    WARNING: Failed %s (%s): %s",
                    short_name, series_id, e$message)); NULL
  })
}

macro_list <- lapply(names(FRED_SERIES), function(nm) {
  message(sprintf("    Pulling %-15s (%s)...", nm, FRED_SERIES[[nm]]))
  pull_fred(FRED_SERIES[[nm]], nm)
})
names(macro_list) <- names(FRED_SERIES)
macro_list <- macro_list[!vapply(macro_list,is.null,logical(1))]

macro_hist <- Reduce(function(a,b) merge(a,b,by="date",all=TRUE),
                     macro_list)
setorderv(macro_hist, "date")
DATA_END_MACRO <- max(macro_hist$date, na.rm=TRUE)

message(sprintf("    %d / %d series pulled | %s → %s",
                ncol(macro_hist)-1L, length(FRED_SERIES),
                as.character(min(macro_hist$date,na.rm=TRUE)),
                as.character(DATA_END_MACRO)))

saveRDS(macro_hist, "macro_raw_v3.rds")
toc()

# ════════════════════════════════════════════════════════════
# 2. ARIMA FORECAST EVERY MACRO SERIES
# ════════════════════════════════════════════════════════════
tic("2. ARIMA macro forecast")
message(sprintf("\n[2] Fitting ARIMA for %d macro series × %d quarters...",
                length(macro_list), HORIZON_Q))

macro_vars   <- setdiff(names(macro_hist), "date")
fc_diag_list <- list()
fc_wide_list <- list()    # one data.table per variable

fit_macro_arima <- function(y_vec, n_future, method, seasonal) {
  n_ok <- sum(!is.na(y_vec))
  if (n_ok < MIN_ARIMA_OBS) {
    last_val <- if(n_ok>0) tail(y_vec[!is.na(y_vec)],1) else 0
    mv <- rep(last_val, n_future)
    return(list(mean=mv, lo80=mv, hi80=mv, lo95=mv, hi95=mv,
                method="carry_forward", note="too_few_obs",
                aicc=NA_real_))
  }
  y_ts <- ts(zoo::na.approx(y_vec, na.rm=FALSE), frequency=4)
  y_ts[is.na(y_ts)] <- median(y_ts, na.rm=TRUE)

  fit <- tryCatch({
    if (method=="ets") {
      forecast::ets(y_ts)
    } else if (method=="both") {
      fa <- forecast::auto.arima(y_ts, seasonal=seasonal, stepwise=TRUE,
                                 approximation=TRUE, max.p=4,max.q=4,
                                 max.P=2,max.Q=2,max.d=2,max.D=1)
      fe <- forecast::ets(y_ts)
      if (isTRUE(fa$aicc<=fe$aicc)) fa else fe
    } else {
      forecast::auto.arima(y_ts, seasonal=seasonal, stepwise=TRUE,
                           approximation=TRUE, max.p=4,max.q=4,
                           max.P=2,max.Q=2,max.d=2,max.D=1)
    }
  }, error=function(e)
    tryCatch(forecast::ets(y_ts, model="AAN"), error=function(e2) NULL)
  )

  if (is.null(fit)) {
    mv <- rep(tail(as.numeric(y_ts),1), n_future)
    return(list(mean=mv, lo80=mv, hi80=mv, lo95=mv, hi95=mv,
                method="naive", note="fit_failed", aicc=NA_real_))
  }
  fc <- forecast::forecast(fit, h=n_future, level=c(80,95))

  is_arima <- inherits(fit,"ARIMA")||inherits(fit,"Arima")
  if (is_arima) {
    ord <- arimaorder(fit)
    p_<-ord["p"]; d_<-ord["d"]; q_<-ord["q"]
    P_<-if("P"%in%names(ord)) ord["P"] else NA
    D_<-if("D"%in%names(ord)) ord["D"] else NA
    Q_<-if("Q"%in%names(ord)) ord["Q"] else NA
    mth <- sprintf("ARIMA(%d,%d,%d)",p_,d_,q_)
    if (!is.na(P_)&&!is.na(D_)&&!is.na(Q_)&&(P_+D_+Q_)>0)
      mth <- sprintf("%s(%d,%d,%d)[4]",mth,P_,D_,Q_)
  } else {
    mth <- paste0("ETS(",paste(fit$components,collapse=""),")")
  }
  list(mean=as.numeric(fc$mean),
       lo80=as.numeric(fc$lower[,1]), hi80=as.numeric(fc$upper[,1]),
       lo95=as.numeric(fc$lower[,2]), hi95=as.numeric(fc$upper[,2]),
       method=mth, note="ok",
       aicc=tryCatch(fit$aicc, error=function(e) NA_real_))
}

# Build a wide future-quarter template
macro_fc <- data.table(date=future_qtrs)

for (vname in macro_vars) {
  res <- fit_macro_arima(macro_hist[[vname]], HORIZON_Q,
                         ARIMA_METHOD, ARIMA_SEASONAL)
  macro_fc[, (vname)              := res$mean]
  macro_fc[, (paste0(vname,"_lo80")) := res$lo80]
  macro_fc[, (paste0(vname,"_hi80")) := res$hi80]
  macro_fc[, (paste0(vname,"_lo95")) := res$lo95]
  macro_fc[, (paste0(vname,"_hi95")) := res$hi95]

  fc_diag_list[[vname]] <- data.table(
    variable=vname, method=res$method,
    aicc=res$aicc, note=res$note,
    n_obs=sum(!is.na(macro_hist[[vname]]))
  )
  message(sprintf("    %-18s  %s", vname, res$method))
}

fc_diag_dt <- rbindlist(fc_diag_list, fill=TRUE)
toc()

# ════════════════════════════════════════════════════════════
# 3. APPLY SCENARIO OVERRIDES (if any)
# ════════════════════════════════════════════════════════════
if (length(SCENARIO_OVERRIDES) > 0) {
  message("\n[3] Applying scenario overrides...")
  for (sc in SCENARIO_OVERRIDES) {
    vname <- sc$variable
    sq    <- zoo::as.yearqtr(sc$start_q)
    eq    <- zoo::as.yearqtr(sc$end_q)
    vals  <- sc$values
    sc_qtrs <- seq(sq, eq, by=0.25)

    if (!vname %in% names(macro_fc)) {
      message(sprintf("    SKIP: %s not in macro_fc", vname)); next
    }
    if (length(vals) != length(sc_qtrs)) {
      message(sprintf("    SKIP: %s — values length %d ≠ quarters %d",
                      vname, length(vals), length(sc_qtrs))); next
    }
    for (i in seq_along(sc_qtrs)) {
      macro_fc[date==sc_qtrs[i], (vname) := vals[i]]
    }
    message(sprintf("    Override applied: %-15s  %s → %s  (%d quarters)",
                    vname, as.character(sq), as.character(eq),
                    length(sc_qtrs)))
  }
} else {
  message("\n[3] No scenario overrides (pure ARIMA)")
}

# ════════════════════════════════════════════════════════════
# 4. DERIVED MACRO SERIES ON COMBINED (HIST + FORECAST) TABLE
# ════════════════════════════════════════════════════════════
tic("4. Derived + combined macro table")
message("\n[4] Computing derived series on combined macro table...")

# Stack historical + forecast rows (mean values only for derived calcs)
ci_cols <- grep("_lo80$|_hi80$|_lo95$|_hi95$",
                names(macro_fc), value=TRUE)
macro_fc_base <- macro_fc[, !ci_cols, with=FALSE]
macro_fc_base[, is_forecast := TRUE]
macro_hist[,   is_forecast := FALSE]

# Align columns
for (cn in setdiff(names(macro_hist),  names(macro_fc_base))) macro_fc_base[,(cn):=NA]
for (cn in setdiff(names(macro_fc_base),names(macro_hist)))   macro_hist[,  (cn):=NA]

macro_full <- rbindlist(list(macro_hist, macro_fc_base), fill=TRUE)
setorderv(macro_full, "date")

# ── Derived series (v2 logic — runs on full combined table) ──

# Fed funds cycle: deviation from trailing 8q mean
if ("fedfunds" %in% names(macro_full)) {
  macro_full[, fedfunds_trail8 := zoo::rollapply(
    fedfunds, width=8, FUN=mean, na.rm=TRUE,
    fill=NA, align="right")]
  macro_full[, fedfunds_cycle := fedfunds - fedfunds_trail8]
}

# Yield curve inversion flag + within-run counter
if ("yield_curve" %in% names(macro_full)) {
  macro_full[, yield_curve_inv := fifelse(yield_curve < 0, 1L, 0L)]
  macro_full[, yield_curve_inv_run := {
    inv  <- ifelse(is.na(yield_curve_inv),0L,yield_curve_inv)
    r    <- rle(inv)
    sequence(r$lengths)
  }]
}

# Real rate: fedfunds minus CPI YoY
if (all(c("fedfunds","cpi") %in% names(macro_full))) {
  macro_full[, cpi_yoy   := (cpi/shift(cpi,4L,type="lag") - 1)*100]
  macro_full[, real_rate := fedfunds - cpi_yoy]
}

# 2s10s term spread
if (all(c("gs2","gs10") %in% names(macro_full)))
  macro_full[, spread_2s10s := gs10 - gs2]

# Credit tightness (standardised HY + BAA)
if (all(c("hy_spread","baa_spread") %in% names(macro_full))) {
  std_z <- function(x) (x - mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  macro_full[, credit_tightness := std_z(hy_spread) + std_z(baa_spread)]
}

# Real payrolls
if (all(c("payems","cpi") %in% names(macro_full)))
  macro_full[, real_payems := payems/cpi*100]

# Oil shock indicator
if ("oil_wti" %in% names(macro_full)) {
  macro_full[, oil_qoq_pct := (oil_wti/shift(oil_wti,1L,type="lag")-1)*100]
  macro_full[, oil_shock   := fifelse(abs(oil_qoq_pct)>20, 1L, 0L)]
}

# M2 growth
if ("m2" %in% names(macro_full))
  macro_full[, m2_yoy := (m2/shift(m2,4L,type="lag")-1)*100]

# Bank deposit growth
if ("bank_deps" %in% names(macro_full))
  macro_full[, bank_deps_yoy := (bank_deps/shift(bank_deps,4L,type="lag")-1)*100]

# FOMC regime + hike run counter
if ("fedfunds" %in% names(macro_full)) {
  macro_full[, fedfunds_chg := fedfunds - shift(fedfunds,1L,type="lag")]
  macro_full[, fomc_regime  := fcase(
    fedfunds_chg >  0.10,  1L,
    fedfunds_chg < -0.10, -1L,
    default = 0L)]
  macro_full[, hike_run := {
    reg   <- ifelse(is.na(fomc_regime),0L,fomc_regime)
    r     <- rle(reg)
    counts <- sequence(r$lengths)
    signs  <- rep(sign(r$values), r$lengths)
    counts * signs
  }]
}

message(sprintf("    Combined macro: %d rows × %d cols  (%d hist + %d forecast)",
                nrow(macro_full), ncol(macro_full),
                sum(!macro_full$is_forecast), sum(macro_full$is_forecast)))
toc()

# ════════════════════════════════════════════════════════════
# 5. MACRO FEATURE ENGINEERING ON COMBINED TABLE
# ════════════════════════════════════════════════════════════
tic("5. Macro FE")
message("\n[5] Macro feature engineering (full combined table)...")

macro_fe_cols <- setdiff(names(macro_full)[vapply(macro_full,is.numeric,logical(1))],
                         "date")
already_derived <- c("cpi_yoy","m2_yoy","bank_deps_yoy","real_rate",
                     "fedfunds_cycle","fedfunds_trail8","credit_tightness",
                     "oil_qoq_pct","oil_shock","spread_2s10s",
                     "yield_curve_inv","yield_curve_inv_run",
                     "real_payems","fomc_regime","fedfunds_chg","hike_run","is_forecast")
base_fe <- setdiff(macro_fe_cols, already_derived)

# 5a: YoY % change (lag-4)
message("    5a. YoY...")
macro_full[, paste0("yoy_",base_fe) :=
             lapply(.SD, pct_chg_n, n=4L), .SDcols=base_fe]

# 5b: QoQ % change (lag-1)
message("    5b. QoQ...")
macro_full[, paste0("qoq_",base_fe) :=
             lapply(.SD, pct_chg_n, n=1L), .SDcols=base_fe]

# 5c: Rolling means (4q, 8q) on key series
message("    5c. Rolling means...")
rollmean_safe <- function(x,k)
  zoo::rollapply(x,width=k,FUN=mean,na.rm=TRUE,fill=NA,align="right",partial=FALSE)

key_macro <- intersect(
  c("fedfunds","gs10","gs2","yield_curve","hy_spread","baa_spread",
    "unrate","payems","gdp_real","cpi","housing","umcsent",
    "real_rate","fedfunds_cycle","credit_tightness","spread_2s10s"),
  names(macro_full))

for (k in c(4L,8L)) {
  nms <- paste0(key_macro,"_rmean",k)
  macro_full[, (nms) := lapply(.SD,rollmean_safe,k=k), .SDcols=key_macro]
}

# 5d: Rolling SDs (4q)
message("    5d. Rolling SDs...")
rollsd_safe <- function(x,k)
  zoo::rollapply(x,width=k,FUN=sd,na.rm=TRUE,fill=NA,align="right",partial=FALSE)
macro_full[, paste0(key_macro,"_rsd4") :=
             lapply(.SD,rollsd_safe,k=4L), .SDcols=key_macro]

# 5e: Cyclical deviation from 8q mean
message("    5e. Cyclical deviations...")
for (v in key_macro) {
  rc <- paste0(v,"_rmean8"); cy <- paste0(v,"_cyc")
  if (rc %in% names(macro_full))
    macro_full[, (cy) := get(v) - get(rc)]
}

# 5f: YoY acceleration
message("    5f. Acceleration...")
yoy_macro <- intersect(paste0("yoy_",key_macro), names(macro_full))
macro_full[, paste0(yoy_macro,"_accel") :=
             lapply(.SD, function(x) x - shift(x,4L,type="lag")),
           .SDcols=yoy_macro]

# 5g: Lag levels (1q, 2q, 4q)
message("    5g. Lags...")
for (lag_n in c(1L,2L,4L)) {
  nms <- paste0(key_macro,"_lag",lag_n)
  macro_full[, (nms) := lapply(.SD,shift,n=lag_n,type="lag"),
             .SDcols=key_macro]
}

# 5h: Interaction terms
message("    5h. Interactions...")
if (all(c("fedfunds","yield_curve") %in% names(macro_full)))
  macro_full[, rate_x_slope := fedfunds * yield_curve]
if (all(c("fedfunds","hy_spread") %in% names(macro_full)))
  macro_full[, rate_x_hy := fedfunds * hy_spread]

message(sprintf("    Macro feature table: %d rows × %d cols",
                nrow(macro_full), ncol(macro_full)))
toc()

# ════════════════════════════════════════════════════════════
# 6. QC
# ════════════════════════════════════════════════════════════
tic("6. QC")
message("\n[6] QC...")

all_na <- names(macro_full)[vapply(macro_full,function(x)all(is.na(x)),logical(1))]
if (length(all_na)>0) { macro_full[,(all_na):=NULL]
  message(sprintf("    Dropped %d all-NA cols",length(all_na))) }

# High-NA warning (historical rows only)
hist_m <- macro_full[is_forecast==FALSE]
num_nms <- names(hist_m)[vapply(hist_m,is.numeric,logical(1))]
na_rates <- vapply(num_nms, function(v) mean(is.na(hist_m[[v]]))*100, numeric(1))
hi_na <- names(na_rates)[na_rates > 30]
if (length(hi_na)>0)
  message(sprintf("    High-NA cols (hist >30%%): %s",
                  paste(head(hi_na,8),collapse=", ")))
else
  message("    No historical cols with >30% NA")

message(sprintf("    Final macro table: %d rows × %d cols",
                nrow(macro_full), ncol(macro_full)))
toc()

# ════════════════════════════════════════════════════════════
# 7. MERGE ONTO qtrly_enriched_v3 AND qtrly_full_v3
# ════════════════════════════════════════════════════════════
tic("7. Merge onto panels")
message("\n[7] Merging macro onto quarterly panels...")

macro_cols <- setdiff(names(macro_full), c("date","is_forecast"))

merge_macro <- function(panel_path, out_path, use_full_macro) {
  if (!file.exists(panel_path)) {
    message(sprintf("    SKIP: %s not found", panel_path)); return(invisible(NULL))
  }
  panel <- readRDS(panel_path); setDT(panel)
  message(sprintf("    %s: %d rows × %d cols (before merge)",
                  basename(panel_path), nrow(panel), ncol(panel)))

  # Choose macro rows: historical only (for enriched), or full (for full panel)
  macro_src <- if (use_full_macro) macro_full else macro_full[is_forecast==FALSE]

  # Remove pre-existing macro cols
  already_in <- intersect(macro_cols, names(panel))
  if (length(already_in)>0) panel[,(already_in):=NULL]

  idx <- match(panel$date, macro_src$date)
  n_match <- sum(!is.na(idx))
  message(sprintf("    Date matches: %d / %d rows (%.0f%%)",
                  n_match, nrow(panel), n_match/nrow(panel)*100))

  if (n_match==0) {
    message("    ERROR: 0 date matches — check yearqtr format"); return(invisible(NULL))
  }

  for (col in macro_cols)
    panel[, (col) := macro_src[[col]][idx]]

  saveRDS(panel, out_path)
  message(sprintf("    Saved: %s  (%d × %d)",
                  basename(out_path), nrow(panel), ncol(panel)))
  invisible(panel)
}

merge_macro("qtrly_enriched_v3.rds", "qtrly_enriched_v3.rds",
            use_full_macro = FALSE)   # historical macro only

merge_macro("qtrly_full_v3.rds",     "qtrly_full_v3.rds",
            use_full_macro = TRUE)    # forecast macro for future rows

toc()

# ════════════════════════════════════════════════════════════
# 8. SAVE MACRO OUTPUTS
# ════════════════════════════════════════════════════════════
tic("8. Save macro outputs")
message("\n[8] Saving macro outputs...")

saveRDS(macro_full, "macro_features_v3.rds")
message(sprintf("    macro_features_v3.rds  (%d × %d)",
                nrow(macro_full), ncol(macro_full)))

# CSV of point forecasts at all future quarters for key series
key_fc_export <- intersect(
  c("date",key_macro,"cpi_yoy","real_rate","fedfunds_cycle",
    "credit_tightness","yield_curve_inv","fomc_regime","hike_run"),
  names(macro_full))
fc_export <- macro_full[is_forecast==TRUE, key_fc_export, with=FALSE]
fwrite(fc_export, "macro_forecast_v3.csv")
message(sprintf("    macro_forecast_v3.csv  (%d quarters × %d key series)",
                nrow(fc_export), ncol(fc_export)-1L))

# Scenario override reference table
if (length(SCENARIO_OVERRIDES)>0) {
  sc_rows <- lapply(SCENARIO_OVERRIDES, function(sc) {
    data.table(variable=sc$variable, start_q=sc$start_q,
               end_q=sc$end_q, n_quarters=length(sc$values),
               min_val=min(sc$values), max_val=max(sc$values))
  })
  fwrite(rbindlist(sc_rows), "macro_scenarios_v3.csv")
  message("    macro_scenarios_v3.csv")
}

fwrite(fc_diag_dt, "macro_arima_diagnostics_v3.csv")
message(sprintf("    macro_arima_diagnostics_v3.csv  (%d series)", nrow(fc_diag_dt)))
toc()

# ════════════════════════════════════════════════════════════
# 9. DIAGNOSTIC PLOTS
# ════════════════════════════════════════════════════════════
tic("9. Plots")
message("\n[9] Diagnostic plots...")

fc_start_date <- as.Date(FORECAST_START)

# Helper: single-series time-series plot with forecast + CI
plot_macro_series <- function(vname, y_label, lo95=NULL, hi95=NULL,
                               lo80=NULL, hi80=NULL, stem=NULL) {
  dt <- macro_full[, .(date=as.Date(date), y=get(vname),
                        is_forecast=is_forecast)]
  dt <- dt[!is.na(y)]
  if (nrow(dt)==0) return(invisible(NULL))

  ci_src <- macro_fc  # has CI columns
  has95 <- !is.null(lo95) && lo95 %in% names(ci_src)
  has80 <- !is.null(lo80) && lo80 %in% names(ci_src)

  p <- ggplot(dt, aes(x=date, y=y)) +
    annotate("rect", xmin=fc_start_date,
             xmax=as.Date(FORECAST_END)+100,
             ymin=-Inf, ymax=Inf, fill="#fffde7", alpha=0.55) +
    {if(has95) geom_ribbon(
        data=ci_src[, .(date=as.Date(date),
                         lo=get(lo95),hi=get(hi95))],
        aes(x=date,ymin=lo,ymax=hi), fill="#1f77b4",
        inherit.aes=FALSE, alpha=0.12) else NULL} +
    {if(has80) geom_ribbon(
        data=ci_src[, .(date=as.Date(date),
                         lo=get(lo80),hi=get(hi80))],
        aes(x=date,ymin=lo,ymax=hi), fill="#1f77b4",
        inherit.aes=FALSE, alpha=0.22) else NULL} +
    geom_line(colour="#1f77b4", linewidth=0.85) +
    geom_line(data=dt[is_forecast==TRUE],
              aes(x=date,y=y), colour="#d62728",
              linewidth=0.85, linetype="dashed") +
    geom_vline(xintercept=fc_start_date, linetype="dotted",
               colour="grey50", linewidth=0.6) +
    geom_vline(xintercept=c(as.Date(HORIZON_1YR),
                             as.Date(HORIZON_3YR)),
               linetype="dotdash", colour="grey65", linewidth=0.4) +
    scale_x_date(date_labels="%Y", date_breaks="3 years") +
    scale_y_continuous(labels=comma) +
    labs(title    = sprintf("%s — History & ARIMA Forecast", y_label),
         subtitle = sprintf("Red dashed = forecast  |  Bands = 80%% (dark) & 95%% (light) CI | %s→%s",
                            as.character(FORECAST_START),
                            as.character(FORECAST_END)),
         x=NULL, y=y_label) +
    theme_v3()

  if (!is.null(stem)) save_plot(p, stem, w=12, h=5)
  invisible(p)
}

# Key rate series
plot_macro_series("fedfunds", "Fed Funds Rate (%)",
                  "fedfunds_lo95","fedfunds_hi95",
                  "fedfunds_lo80","fedfunds_hi80",
                  stem="M01_fedfunds_forecast")

plot_macro_series("unrate", "Unemployment Rate (%)",
                  "unrate_lo95","unrate_hi95",
                  "unrate_lo80","unrate_hi80",
                  stem="M02_unrate_forecast")

plot_macro_series("cpi", "CPI (Index, 1982-84=100)",
                  "cpi_lo95","cpi_hi95",
                  stem="M03_cpi_forecast")

plot_macro_series("gdp_real", "Real GDP (Billions $2012)",
                  "gdp_real_lo95","gdp_real_hi95",
                  stem="M04_gdp_forecast")

plot_macro_series("yield_curve", "Yield Curve (T10Y-T3M, %)",
                  "yield_curve_lo95","yield_curve_hi95",
                  stem="M05_yield_curve_forecast")

plot_macro_series("hy_spread", "HY OAS Spread (bps)",
                  "hy_spread_lo95","hy_spread_hi95",
                  stem="M06_hy_spread_forecast")

# Composite series plot (no CI — derived quantities)
message("    Composite macro panel...")
composite_vars <- intersect(c("real_rate","credit_tightness",
                               "fedfunds_cycle","hike_run"),
                             names(macro_full))
if (length(composite_vars)>0) {
  comp_long <- melt(
    macro_full[, c("date","is_forecast",composite_vars), with=FALSE],
    id.vars=c("date","is_forecast"),
    variable.name="series", value.name="value")
  comp_long[, label := fcase(
    series=="real_rate",        "Real Rate (FF - CPI YoY)",
    series=="credit_tightness", "Credit Tightness Index",
    series=="fedfunds_cycle",   "Fed Funds Cycle (dev from 8q avg)",
    series=="hike_run",         "Hike Run Counter",
    default=as.character(series))]

  pc <- ggplot(comp_long[!is.na(value)],
               aes(x=as.Date(date), y=value, colour=is_forecast)) +
    annotate("rect", xmin=fc_start_date,
             xmax=as.Date(FORECAST_END)+100,
             ymin=-Inf, ymax=Inf, fill="#fffde7", alpha=0.4) +
    geom_line(linewidth=0.8) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
    geom_vline(xintercept=fc_start_date,
               linetype="dotted", colour="grey50") +
    facet_wrap(~label, scales="free_y", ncol=2) +
    scale_colour_manual(values=c("FALSE"="#1f77b4","TRUE"="#d62728"),
                        labels=c("Historical","Forecast"), name=NULL) +
    scale_x_date(date_labels="%Y", date_breaks="4 years") +
    labs(title="Derived / Composite Macro Indicators",
         subtitle="Red = ARIMA-based forecast extension",
         x=NULL, y=NULL) + theme_v3()
  save_plot(pc, "M07_composite_indicators", w=13, h=8)
}

# ARIMA diagnostics: model orders
message("    ARIMA order summary...")
if (nrow(fc_diag_dt)>0) {
  pd <- ggplot(fc_diag_dt[note=="ok"], aes(x=reorder(variable,aicc),
                                             y=aicc, fill=method)) +
    geom_col() +
    geom_text(aes(label=method), hjust=-0.05, size=2.5) +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0,0.3))) +
    labs(title="Macro ARIMA Model Orders and AICc",
         x=NULL, y="AICc") +
    theme_v3() + theme(legend.position="none")
  save_plot(pd, "M08_macro_arima_orders", w=11, h=8)
}

n_plots <- length(list.files(PLOT_DIR, pattern="^M[0-9].*\\.pdf$"))
toc()

# ════════════════════════════════════════════════════════════
# 10. FINAL SUMMARY
# ════════════════════════════════════════════════════════════
tot <- as.numeric((proc.time()-t0)["elapsed"])
toc()  # MACRO v3 total

message("\n=======================================================")
message(sprintf("MACRO v3.0 COMPLETE  %dh %02dm %02ds",
                floor(tot/3600), floor((tot%%3600)/60), round(tot%%60)))
message(sprintf("FRED series       : %d pulled", length(macro_list)))
message(sprintf("Forecast horizon  : %s → %s  (%d quarters)",
                as.character(FORECAST_START),
                as.character(FORECAST_END), HORIZON_Q))
message(sprintf("ARIMA fitted      : %d  |  fallback: %d",
                sum(fc_diag_dt$note=="ok",na.rm=TRUE),
                sum(fc_diag_dt$note!="ok",na.rm=TRUE)))
if (length(SCENARIO_OVERRIDES)>0)
  message(sprintf("Scenario overrides: %d series applied",
                  length(SCENARIO_OVERRIDES)))
message("Outputs saved:")
message("  macro_raw_v3.rds             (raw FRED historical)")
message(sprintf("  macro_features_v3.rds        (%d rows × %d cols, hist+fcst)",
                nrow(macro_full), ncol(macro_full)))
message("  macro_forecast_v3.csv        (key series point fcst)")
message("  macro_arima_diagnostics_v3.csv")
if (length(SCENARIO_OVERRIDES)>0)
  message("  macro_scenarios_v3.csv")
message("  qtrly_enriched_v3.rds        (overwritten with macro)")
message("  qtrly_full_v3.rds            (overwritten with macro)")
message(sprintf("  %d PDF plots → %s/", n_plots, PLOT_DIR))
message("=======================================================")

message("\n── Recent & near-term macro forecast ──")
diag_cols <- intersect(
  c("date","fedfunds","unrate","cpi_yoy","real_rate",
    "yield_curve","credit_tightness","fomc_regime"),
  names(macro_full))
recent <- tail(sort(unique(macro_full$date)), 4L)
near   <- head(macro_full[is_forecast==TRUE]$date, 4L)
print(macro_full[date %in% c(recent,near), diag_cols, with=FALSE])

message("\n── Next step: run part2_v3.R and part3_v3.R ──")

notify("Macro v3.0 DONE",
       sprintf("%dh%02dm | %dq | %d ARIMA | %d plots",
               floor(tot/3600), floor((tot%%3600)/60),
               HORIZON_Q,
               sum(fc_diag_dt$note=="ok",na.rm=TRUE),
               n_plots),
       tags="tada")

############################################################
# END
############################################################
