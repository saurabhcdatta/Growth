## =====================================================================
## 15_diagnostics.R  --  Per-institution validation
##
## The guardrails in script 12 bound what can come out. They do not show
## that any given projection is right. This script measures that directly:
##
##   HOLDOUT BACKTEST  hide the last 8 quarters, refit each institution's
##                     selected model on what remains, project into the
##                     hidden period, and compare against what actually
##                     happened -- including whether the predicted asset
##                     category was the right one.
##   DATA CHECKS       gaps, jumps, frozen values, thin history: the things
##                     that make a projection unreliable regardless of model
##   RESIDUAL CHECKS   whether the fitted model actually captured the series
##   TRIAGE            every institution gets OK / REVIEW / EXCLUDE with the
##                     reasons named, so nothing is unexamined
##
## The bucket hit rate at [15.6] is the number to quote when someone asks
## how good these are. Note what it measures: the method's accuracy two
## years out, over the whole cohort. It does not prove any one institution's
## five-year projection is correct, and nothing can.
## =====================================================================

library(dplyr)
library(tidyr)
library(forecast)
library(parallel)
library(ggplot2)
library(scales)

## prep <- readRDS("cohort_prep.rds"); list2env(prep, .GlobalEnv)
## cvr  <- readRDS("cohort_cv.rds");   list2env(cvr,  .GlobalEnv)
## fitr <- readRDS("cohort_fits.rds"); list2env(fitr, .GlobalEnv)

HOLD <- 8              # quarters held out for the backtest (2 years)

## The published pipeline subtracts a measured overshoot (BIAS_PA in script
## 12). The backtest must mirror that, or it validates a model you do not
## publish. Both are reported below:
##   RAW        the model with no correction. This is the CALIBRATION
##              source -- set BIAS_PA in script 12 from this figure.
##   CORRECTED  the same model with the correction applied. This is what
##              you actually publish, and its bias should sit near zero.
## Do NOT feed the corrected figure back into BIAS_PA: that double-counts.
## Works whether you continued in script 12's session (BIAS_PA is already
## in the environment) or started fresh from cohort_fits.rds (where it
## lives inside the saved `guardrails` list).
BIAS_PA_APPLIED <- if (exists("BIAS_PA")) BIAS_PA else
                   if (exists("guardrails")) guardrails$BIAS_PA else 0
BIAS_Q_APPLIED  <- log(1 + BIAS_PA_APPLIED) / 4
cat("Bias correction carried from script 12:",
    round(100 * BIAS_PA_APPLIED, 2), "% a year\n")
if (BIAS_PA_APPLIED == 0)
  warning("No bias correction found. If script 12 applied one, load ",
          "cohort_fits.rds or rerun 12 in this session, or the backtest ",
          "will validate the uncorrected model.")
JUMP_PCT <- 0.25       # quarter-on-quarter asset jump that looks like an event
FROZEN_Q <- 4          # identical assets this many quarters = suspect

DIAG_DIR <- file.path(getwd(), "diagnostics")
dir.create(DIAG_DIR, showWarnings = FALSE)

## ---------------------------------------------------------------------
## [15.1] Data quality, straight off each institution's series
## ---------------------------------------------------------------------
dq <- bind_rows(lapply(cu_series, function(s) {
  y <- s$y; n <- s$n
  d <- if (n > 1) diff(y) else numeric(0)
  runs <- if (n > 1) rle(round(diff(y), 8) == 0) else NULL
  max_frozen <- if (!is.null(runs) && any(runs$values))
    max(runs$lengths[runs$values]) else 0
  data.frame(
    join_number = s$join_number,
    n_obs = n,
    hist_years = round(n / 4, 1),
    gaps = length(setdiff(min(s$q_index):max(s$q_index), s$q_index)),
    n_big_jumps = sum(abs(d) > log(1 + JUMP_PCT)),
    max_jump_pct = if (length(d)) round(100 * (exp(max(abs(d))) - 1), 1) else 0,
    max_frozen_q = max_frozen,
    n_acq = sum(diff(c(0, s$xreg[, "acq_cum"])) > 0),
    vol_pa = if (length(d) > 4) round(sd(d) * 2, 4) else NA_real_,
    stringsAsFactors = FALSE)
}))

summary(dq$hist_years)
table(dq$n_big_jumps > 0, dq$n_acq > 0,
      dnn = c("has big jump", "has recorded acquisition"))

## A large jump with no recorded acquisition is the signature of a merger
## the regressor missed, or a restatement. Worth looking at directly.
unexplained <- dq %>% filter(n_big_jumps > 0, n_acq == 0) %>%
  left_join(cohort %>% select(join_number, cu_name, asset_cat_now), by = "join_number") %>%
  arrange(desc(max_jump_pct))
nrow(unexplained)
head(as.data.frame(unexplained %>% select(cu_name, asset_cat_now, max_jump_pct,
                                          n_big_jumps, hist_years)), 20)

## ---------------------------------------------------------------------
## [15.2] Holdout backtest and residual diagnostics, in one pass
## ---------------------------------------------------------------------
back_one <- function(s, cv_winners, HOLD, BREAKS, CAT_LABELS, BIAS_Q_APPLIED) {

  n <- s$n
  out <- data.frame(join_number = s$join_number, bt_ok = FALSE,
                    bt_err_log = NA_real_, bt_err_log_raw = NA_real_,
                    bt_pct_err = NA_real_, bt_pct_err_raw = NA_real_,
                    bt_cat_pred = NA_character_, bt_cat_actual = NA_character_,
                    bt_cat_hit = NA, lb_p = NA_real_, resid_sd = NA_real_,
                    stringsAsFactors = FALSE)

  w <- cv_winners[cv_winners$join_number == s$join_number, ]
  if (nrow(w) == 0 || n < 32 + HOLD) return(out)
  cn <- w                              # orders travel with the winner row

  ntr <- n - HOLD
  ## Benchmark winners are backtestable without refitting anything: apply
  ## the same rule to the training window. Skipping them left 45% of the
  ## cohort unbacktested and pushed them all into EXCLUDE.
  if (is.na(cn$p[1])) {
    g <- if (grepl("^Flat", cn$spec[1])) 0 else stats::median(diff(s$y[1:ntr]))
    raw_log  <- s$y[ntr] + g * HOLD
    pred_log <- raw_log - BIAS_Q_APPLIED * HOLD      # as published
    act_log  <- s$y[n]
    out$bt_ok <- TRUE
    out$bt_err_log     <- pred_log - act_log
    out$bt_err_log_raw <- raw_log  - act_log
    out$bt_pct_err     <- 100 * (exp(pred_log) / exp(act_log) - 1)
    out$bt_pct_err_raw <- 100 * (exp(raw_log)  / exp(act_log) - 1)
    out$bt_cat_pred   <- as.character(cut(exp(pred_log), BREAKS, CAT_LABELS, right = FALSE))
    out$bt_cat_actual <- as.character(cut(exp(act_log),  BREAKS, CAT_LABELS, right = FALSE))
    out$bt_cat_hit    <- out$bt_cat_pred == out$bt_cat_actual
    return(out)
  }

  ytr <- ts(s$y[1:ntr], frequency = 4)
  X   <- s$xreg
  keep <- apply(X[1:ntr, , drop = FALSE], 2, function(z) length(unique(z)) > 1)
  Xtr <- if (any(keep)) X[1:ntr, keep, drop = FALSE] else NULL
  Xfu <- if (any(keep)) X[(ntr + 1):n, keep, drop = FALSE] else NULL

  fit <- tryCatch(forecast::Arima(ytr, order = c(cn$p, cn$d, cn$q),
      seasonal = list(order = c(cn$P, cn$D, cn$Q), period = 4),
      xreg = Xtr, include.drift = cn$drift, method = "ML"),
    error = function(e) NULL, warning = function(w) NULL)
  if (is.null(fit)) return(out)

  fc <- tryCatch(as.numeric(forecast::forecast(fit, h = HOLD, xreg = Xfu)$mean),
                 error = function(e) NULL)
  if (is.null(fc) || any(!is.finite(fc))) return(out)

  raw_log  <- fc[HOLD]
  pred_log <- raw_log - BIAS_Q_APPLIED * HOLD        # as published
  act_log  <- s$y[n]
  pred_a <- exp(pred_log); act_a <- exp(act_log)

  out$bt_ok <- TRUE
  out$bt_err_log     <- pred_log - act_log
  out$bt_err_log_raw <- raw_log  - act_log
  out$bt_pct_err     <- 100 * (pred_a / act_a - 1)
  out$bt_pct_err_raw <- 100 * (exp(raw_log) / act_a - 1)
  out$bt_cat_pred   <- as.character(cut(pred_a, BREAKS, CAT_LABELS, right = FALSE))
  out$bt_cat_actual <- as.character(cut(act_a,  BREAKS, CAT_LABELS, right = FALSE))
  out$bt_cat_hit    <- out$bt_cat_pred == out$bt_cat_actual

  ## Residual behaviour of the model fitted on the full series
  ff <- tryCatch(forecast::Arima(ts(s$y, frequency = 4),
      order = c(cn$p, cn$d, cn$q),
      seasonal = list(order = c(cn$P, cn$D, cn$Q), period = 4),
      xreg = if (any(apply(X, 2, function(z) length(unique(z)) > 1)))
        X[, apply(X, 2, function(z) length(unique(z)) > 1), drop = FALSE] else NULL,
      include.drift = cn$drift, method = "ML"),
    error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(ff)) {
    r <- residuals(ff)
    out$lb_p <- tryCatch(Box.test(r, lag = 8, type = "Ljung-Box",
                                  fitdf = length(coef(ff)))$p.value,
                         error = function(e) NA_real_)
    out$resid_sd <- sd(r, na.rm = TRUE)
  }
  out
}

t0 <- Sys.time()
cl <- makeCluster(max(1, parallel::detectCores(logical = FALSE) - 1))
clusterEvalQ(cl, library(forecast))
clusterExport(cl, c("back_one", "cv_winners", "HOLD", "BREAKS", "CAT_LABELS",
                    "BIAS_Q_APPLIED"), envir = globalenv())
bt_list <- parLapplyLB(cl, cu_series, function(s)
  back_one(s, cv_winners, HOLD, BREAKS, CAT_LABELS, BIAS_Q_APPLIED))
stopCluster(cl)
difftime(Sys.time(), t0, units = "mins")

backtest <- bind_rows(bt_list)
mean(backtest$bt_ok)

## ---------------------------------------------------------------------
## [15.3] One diagnostic record per institution
## ---------------------------------------------------------------------
diag_cu <- cu %>%
  select(join_number, cu_name, region, state, cu_type, assets_now,
         asset_cat_now, cat_1Yr, cat_5Yr, growth_pa, basis, jump_5Yr,
         cv_rmse_h20, g_simple, g_peer, shrink_w) %>%
  left_join(dq, by = "join_number") %>%
  left_join(backtest, by = "join_number") %>%
  mutate(
    ## Reasons, accumulated
    r_short   = hist_years < 5,
    r_gaps    = gaps > 0,
    r_frozen  = max_frozen_q >= FROZEN_Q,
    r_unexpl  = n_big_jumps > 0 & n_acq == 0,
    r_nobt    = !bt_ok,
    r_bterr   = bt_ok & abs(bt_pct_err) > 25,
    r_btcat   = bt_ok & !bt_cat_hit,
    r_resid   = !is.na(lb_p) & lb_p < 0.01,
    r_fallback = grepl("^Peer", basis),
    r_capped  = grepl("capped", basis),

    n_flags = r_short + r_gaps + r_frozen + r_unexpl + r_nobt +
              r_bterr + r_resid + r_fallback,

    triage = case_when(
      r_short | r_nobt | r_unexpl | r_fallback        ~ "EXCLUDE from institution reporting",
      r_bterr | r_frozen | r_gaps | n_flags >= 2      ~ "REVIEW",
      TRUE                                             ~ "OK"),

    reasons = trimws(paste0(
      ifelse(r_short,   "short history; ", ""),
      ifelse(r_gaps,    "reporting gaps; ", ""),
      ifelse(r_frozen,  "assets unchanged for 4+ quarters; ", ""),
      ifelse(r_unexpl,  "large jump with no recorded acquisition; ", ""),
      ifelse(r_nobt,    "could not be backtested; ", ""),
      ifelse(r_bterr,   "backtest error over 25%; ", ""),
      ifelse(r_btcat,   "backtest landed in wrong category; ", ""),
      ifelse(r_resid,   "model left structure in residuals; ", ""),
      ifelse(r_fallback, "peer growth used; ", ""),
      ifelse(r_capped,  "growth capped by guardrail; ", ""))))

table(diag_cu$triage)
round(100 * table(diag_cu$triage) / nrow(diag_cu), 1)

## ---------------------------------------------------------------------
## [15.4] Is anything systematically wrong, rather than individually noisy
## ---------------------------------------------------------------------
bias_check <- diag_cu %>% filter(bt_ok) %>%
  summarise(n = n(),
            median_pct_err = round(median(bt_pct_err), 2),
            mean_pct_err = round(mean(bt_pct_err), 2),
            share_over = round(100 * mean(bt_pct_err > 0), 1),
            p10 = round(quantile(bt_pct_err, .10), 1),
            p90 = round(quantile(bt_pct_err, .90), 1),
            median_pct_err_raw = round(median(bt_pct_err_raw), 2),
            share_over_raw = round(100 * mean(bt_pct_err_raw > 0), 1))
as.data.frame(bias_check)

cat("\n--- BIAS, BEFORE AND AFTER THE CORRECTION ---\n")
cat("Raw model       : median error", bias_check$median_pct_err_raw, "% over",
    HOLD / 4, "years,", bias_check$share_over_raw, "% overshooting\n")
cat("As published    : median error", bias_check$median_pct_err, "% over",
    HOLD / 4, "years,", bias_check$share_over, "% overshooting\n")
cat("Correction applied in script 12:", round(100 * BIAS_PA_APPLIED, 2), "% a year\n")
cat("\nSet BIAS_PA in script 12 from the RAW figure, never the published one:\n")
cat("re-reading the corrected figure back into BIAS_PA double-counts.\n")
cat("Implied raw annual overshoot:",
    round(100 * (exp(median(diag_cu$bt_err_log_raw, na.rm = TRUE) /
                       (HOLD / 4)) - 1), 2), "%\n\n")

by_size <- diag_cu %>% filter(bt_ok) %>%
  group_by(asset_cat_now) %>%
  summarise(n = n(), cat_hit_rate = round(100 * mean(bt_cat_hit), 1),
            median_abs_err = round(median(abs(bt_pct_err)), 1),
            p90_abs_err = round(quantile(abs(bt_pct_err), .90), 1),
            .groups = "drop") %>%
  arrange(match(asset_cat_now, CAT_LABELS))
as.data.frame(by_size)

by_basis <- diag_cu %>% filter(bt_ok) %>%
  group_by(basis) %>%
  summarise(n = n(), cat_hit_rate = round(100 * mean(bt_cat_hit), 1),
            median_abs_err = round(median(abs(bt_pct_err)), 1), .groups = "drop")
as.data.frame(by_basis)

## ---------------------------------------------------------------------
## [15.5] Did the COUNTS come out right? The workbook is a count product,
## so this is the check that matters most for the deliverable.
## ---------------------------------------------------------------------
count_check <- diag_cu %>% filter(bt_ok) %>%
  summarise(across(everything(), ~NA), .groups = "drop") %>% slice(0)

count_check <- bind_rows(
  diag_cu %>% filter(bt_ok) %>% count(cat = bt_cat_pred) %>%
    rename(predicted = n),
  NULL) %>%
  full_join(diag_cu %>% filter(bt_ok) %>% count(cat = bt_cat_actual) %>%
              rename(actual = n), by = "cat") %>%
  replace_na(list(predicted = 0, actual = 0)) %>%
  mutate(cat = factor(cat, levels = CAT_LABELS),
         error = predicted - actual,
         pct_error = round(100 * error / pmax(actual, 1), 1)) %>%
  arrange(cat)

as.data.frame(count_check)
cat("\nOverall category hit rate two years out:",
    round(100 * mean(diag_cu$bt_cat_hit, na.rm = TRUE), 1), "%\n")
cat("Largest count error in any category:", max(abs(count_check$error)), "\n")

## ---------------------------------------------------------------------
## [15.5b] TRANSITION CALIBRATION
##
## Every guardrail bounds one institution at a time. None of them looks at
## the distribution of outcomes, so the forecast can be individually
## defensible everywhere and still move too many institutions in one
## direction. This compares the movement the backtest PREDICTED over the
## holdout window against what actually happened in the same window.
##
## If predicted-up materially exceeds actual-up, the projections are
## systematically too optimistic and every bucket count leans large. That
## is a bias, not noise, and no amount of per-institution review finds it.
## ---------------------------------------------------------------------
## diag_cu already carries asset_cat_now; the starting category for the
## backtest is the one held HOLD quarters ago, which comes from the panel.
trans <- diag_cu %>% filter(bt_ok) %>%
  mutate(pred_i = match(bt_cat_pred, CAT_LABELS),
         act_i  = match(bt_cat_actual, CAT_LABELS))

## The holdout starts HOLD quarters back, so the starting category is the
## one the institution was in then, not today.
start_cat <- hist %>% filter(q_index == N_Q - HOLD) %>%
  mutate(cat0 = as.character(cut(assets_tot, BREAKS, CAT_LABELS, right = FALSE))) %>%
  select(join_number, cat0)

trans <- trans %>% left_join(start_cat, by = "join_number") %>%
  filter(!is.na(cat0)) %>%
  mutate(i0 = match(cat0, CAT_LABELS))

trans_summary <- trans %>%
  summarise(n = n(),
            pred_up   = sum(pred_i > i0), act_up   = sum(act_i > i0),
            pred_down = sum(pred_i < i0), act_down = sum(act_i < i0),
            pred_same = sum(pred_i == i0), act_same = sum(act_i == i0)) %>%
  mutate(up_ratio   = round(pred_up / pmax(act_up, 1), 2),
         down_ratio = round(pred_down / pmax(act_down, 1), 2))

print(as.data.frame(trans_summary), row.names = FALSE)

cat("\n--- TRANSITION CALIBRATION over", HOLD, "quarters ---\n")
cat("Predicted moving up  :", trans_summary$pred_up,
    " actual:", trans_summary$act_up,
    sprintf(" (ratio %.2f)\n", trans_summary$up_ratio))
cat("Predicted moving down:", trans_summary$pred_down,
    " actual:", trans_summary$act_down,
    sprintf(" (ratio %.2f)\n", trans_summary$down_ratio))
cat("A ratio near 1.0 means the movement rate is calibrated.\n")
cat("Above ~1.3 on the up side means the projections are systematically\n")
cat("too optimistic and every bucket count leans large.\n\n")

## The calibration figure for the next refresh: measured on the RAW model,
## so it is not contaminated by the correction already applied.
bias_g <- diag_cu %>% filter(bt_ok) %>%
  summarise(median_log_err     = median(bt_err_log, na.rm = TRUE),
            median_log_err_raw = median(bt_err_log_raw, na.rm = TRUE)) %>%
  mutate(annual_bias_pct     = round(100 * (exp(median_log_err / (HOLD / 4)) - 1), 2),
         annual_bias_pct_raw = round(100 * (exp(median_log_err_raw / (HOLD / 4)) - 1), 2))
as.data.frame(bias_g)
cat("Residual overshoot after correction:", bias_g$annual_bias_pct,
    "% a year (should be near zero)\n")
cat("Raw overshoot, for setting BIAS_PA next refresh:",
    bias_g$annual_bias_pct_raw, "% a year\n")

## ---------------------------------------------------------------------
## [15.6] Spot-check plots: every EXCLUDE, plus a random sample of OK
## ---------------------------------------------------------------------
set.seed(1)
plot_ids <- c(
  diag_cu %>% filter(triage == "EXCLUDE from institution reporting") %>%
    slice_head(n = 40) %>% pull(join_number),
  diag_cu %>% filter(triage == "REVIEW") %>% slice_sample(n = 20) %>% pull(join_number),
  diag_cu %>% filter(triage == "OK") %>% slice_sample(n = 20) %>% pull(join_number))
plot_ids <- unique(plot_ids)
length(plot_ids)

pdf(file.path(DIAG_DIR, "spot_checks.pdf"), width = 9, height = 5)
for (jn in plot_ids) {
  s <- cu_series[[which(vapply(cu_series, function(z) z$join_number, 0) == jn)]]
  info <- diag_cu %>% filter(join_number == jn)
  hist_df <- data.frame(
    date = as.Date(paste0(qgrid$year[s$q_index], "-",
                          (qgrid$quarter[s$q_index] - 1) * 3 + 1, "-01")),
    assets = exp(s$y))
  last_d <- max(hist_df$date)
  fc_df <- data.frame(
    date = seq(last_d, by = "3 months", length.out = 21)[-1],
    assets = exp(tail(s$y, 1) + info$growth_pa / 100 * 0 +
                   log(1 + info$growth_pa / 100) * (1:20) / 4))

  p <- ggplot() +
    geom_line(data = hist_df, aes(date, assets), colour = "#1F3B63", linewidth = 0.7) +
    geom_line(data = fc_df, aes(date, assets), colour = "#C0392B",
              linewidth = 0.8, linetype = "22") +
    geom_hline(yintercept = BREAKS[is.finite(BREAKS) & BREAKS > 0],
               colour = "grey80", linewidth = 0.3) +
    scale_y_log10(labels = label_dollar(scale_cut = cut_short_scale())) +
    labs(title = paste0(info$cu_name, "  (", info$asset_cat_now, " -> ", info$cat_5Yr, ")"),
         subtitle = paste0(info$triage, " | ", info$basis, " | ",
                           info$growth_pa, "% p.a. | backtest error ",
                           ifelse(is.na(info$bt_pct_err), "n/a",
                                  paste0(round(info$bt_pct_err, 1), "%"))),
         caption = ifelse(nzchar(info$reasons), info$reasons, "no flags"),
         x = NULL, y = "Assets (log scale)") +
    theme_minimal(base_size = 10, base_family = "sans") +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, colour = "grey30"),
          plot.caption = element_text(size = 8, colour = "grey45", hjust = 0),
          panel.grid.minor = element_blank())
  print(p)
}
dev.off()

cat("\nSpot-check plots written to", file.path(DIAG_DIR, "spot_checks.pdf"), "\n")

## ---------------------------------------------------------------------
## [15.7] Carry forward. 14_export writes a Diagnostics tab from this.
## ---------------------------------------------------------------------
diag_out <- diag_cu %>%
  transmute(`Join Number` = join_number, `CU Name` = cu_name, Region = region,
            State = state, `CU Type` = cu_type,
            `Current Assets ($)` = round(assets_now),
            `Category Now` = asset_cat_now, `Category 5Yr` = cat_5Yr,
            `Growth % p.a.` = growth_pa, `Basis` = basis,
            `History (years)` = hist_years, `Reporting gaps` = gaps,
            `Large jumps` = n_big_jumps, `Acquisitions` = n_acq,
            `Backtest error %` = round(bt_pct_err, 1),
            `Backtest category correct` = ifelse(is.na(bt_cat_hit), "n/a",
                                                 ifelse(bt_cat_hit, "yes", "no")),
            `CV error (5yr, log)` = round(cv_rmse_h20, 3),
            `Ljung-Box p` = round(lb_p, 3),
            Triage = triage, Reasons = reasons) %>%
  arrange(factor(Triage, levels = c("EXCLUDE from institution reporting",
                                    "REVIEW", "OK")),
          desc(`Current Assets ($)`))

saveRDS(list(diag_cu = diag_cu, diag_out = diag_out, dq = dq,
             backtest = backtest, bias_check = bias_check, by_size = by_size,
             by_basis = by_basis, count_check = count_check,
             unexplained = unexplained, trans_summary = trans_summary,
             bias_g = bias_g, HOLD = HOLD),
        file = "cohort_diagnostics.rds")
