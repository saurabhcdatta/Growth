## =====================================================================
## 7_reconcile_shares.R  --  Coherent forecasts by total x shares
##
## The 42 independent models let institutions appear from nowhere: in
## Region 2 / CU type 2 the large buckets grew by more than the small ones
## shrank, so the group total rose 523 -> 539 over five years. Credit
## unions only enter a larger bucket by leaving a smaller one, so a growing
## group total is an adding-up failure, not a forecast.
##
## This script forecasts each region x cu_type group in two pieces:
##   TOTAL   one ARIMA on the group total (declines with consolidation)
##   SHARES  six additive log-ratio series, one per non-reference bucket,
##           each ARIMA'd, then softmax-ed back to seven shares summing to 1
## Counts = share x total, integer-rounded by largest remainder so the
## seven buckets sum exactly to the forecast total.
##
## Buckets can still gain share -- the substantive story survives -- but the
## group can no longer grow unless the total model says it grows.
##
## Run after 3_fit_forecast.R (uses its output only for comparison).
## =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forecast)

source("0_xlsx_helpers.R")

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)
## fitr <- readRDS("cu_count_fits.rds"); list2env(fitr, .GlobalEnv)

## ---------------------------------------------------------------------
## [7.1] Settings
## ---------------------------------------------------------------------
H_MAX    <- 20
N_SIM    <- 500          # simulation paths for prediction intervals
MID_H    <- 8            # template's "2 Year Out"; set 12 for 3-year
MID_LAB  <- if (MID_H == 8) "Count 2 Year Out" else "Count 3 Year Out"
SMALL_N  <- 5            # buckets below this are flagged as too small to quote
FC_START <- c(2026, 2)

set.seed(20260821)

CHART_DIR <- file.path(getwd(), "plots_reconciled")
dir.create(CHART_DIR, showWarnings = FALSE)

OUT_XLSX <- file.path(getwd(),
  paste0("CU_Growth_Summary_Reconciled_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

TPL_CATS <- c("Less than 10M", "10M-50M", "50M-100M", "100M-500M",
              "500M-1B", "1B-10B", "10B")
names(TPL_CATS) <- CAT_LABELS

groups <- cells %>% distinct(region, cu_type) %>% arrange(region, cu_type)
nrow(groups)   # 6

fq_index <- tibble::tibble(
  horizon_q = 1:H_MAX,
  year      = FC_START[1] + (FC_START[2] - 1 + 0:(H_MAX - 1)) %/% 4,
  quarter   = (FC_START[2] - 1 + 0:(H_MAX - 1)) %% 4 + 1) %>%
  mutate(q_label = paste0(year, "Q", quarter))

## Integer allocation that does not wobble.
##
## Largest-remainder rounding is applied independently at each horizon, so a
## bucket sitting near a .5 boundary can round down at one horizon and up at
## the next -- producing paths like 2, 1, 2, 2 that read as a forecast of
## decline-then-recovery when nothing of the sort is being forecast.
##
## Instead: round each bucket's own (smooth) path to the nearest integer, which
## is monotone whenever the underlying path is, then absorb the small residual
## needed to hit the total into the single largest bucket, where +/-2 on a base
## of several hundred is invisible.
alloc_smooth <- function(total_int, shares, big_idx) {
  raw  <- shares / sum(shares) * total_int
  out  <- round(raw)
  out[out < 0] <- 0
  resid <- total_int - sum(out)
  out[big_idx] <- max(out[big_idx] + resid, 0)
  as.integer(out)
}

## ---------------------------------------------------------------------
## [7.2] Fit total and share models, one group at a time
## ---------------------------------------------------------------------
recon      <- list()
total_diag <- list()
share_diag <- list()

for (g in seq_len(nrow(groups))) {

  rg <- groups$region[g]; tp <- groups$cu_type[g]
  gname <- sprintf("Region%dCU%d", rg, tp)

  ## Wide matrix of counts: quarters x 7 categories
  W <- counts %>%
    filter(region == rg, cu_type == tp) %>%
    select(q_index, asset_cat, n_cu) %>%
    pivot_wider(names_from = asset_cat, values_from = n_cu) %>%
    arrange(q_index)
  M <- as.matrix(W[, CAT_LABELS])
  Tv <- rowSums(M)

  ## ---- TOTAL -------------------------------------------------------
  Ttot <- ts(Tv, start = c(2005, 1), frequency = 4)
  lam  <- if (min(Tv) > 0) 0 else NULL       # log scale: positive, geometric

  ft <- auto.arima(Ttot, lambda = lam, biasadj = TRUE, allowdrift = TRUE,
                   seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

  ## Same publication rule as script 3: a drift term, so the path is not flat
  if (!("drift" %in% names(coef(ft)))) {
    ft <- Arima(Ttot, order = c(0, 1, 1), include.drift = TRUE,
                lambda = lam, biasadj = TRUE)
  }
  fct <- forecast(ft, h = H_MAX, level = c(80, 95), biasadj = TRUE)
  tot_point <- pmax(as.numeric(fct$mean), 1)

  total_diag[[gname]] <- tibble::tibble(
    group = gname, region = rg, cu_type = tp,
    spec = paste0("ARIMA(", paste(arimaorder(ft)[1:3], collapse = ","), ")",
                  if ("drift" %in% names(coef(ft))) " w/ drift" else "",
                  if (!is.null(lam)) " on log scale" else ""),
    total_now = Tv[length(Tv)],
    total_1yr = round(tot_point[4]), total_mid = round(tot_point[MID_H]),
    total_5yr = round(tot_point[H_MAX]),
    chg_5yr_pct = round(100 * (tot_point[H_MAX] - Tv[length(Tv)]) /
                          max(Tv[length(Tv)], 1), 1))

  ## ---- SHARES ------------------------------------------------------
  ## Multiplicative zero replacement, then additive log-ratio against the
  ## largest bucket (the most stable denominator).
  S <- M / Tv
  eps <- 0.5 / pmax(Tv, 1)
  S <- t(apply(cbind(S, eps), 1, function(z) {
    s <- z[1:7]; e <- z[8]
    s[s == 0] <- e
    s / sum(s)
  }))
  colnames(S) <- CAT_LABELS

  ref <- CAT_LABELS[which.max(colMeans(S))]
  oth <- setdiff(CAT_LABELS, ref)

  alr_fits <- list(); alr_specs <- character(0)
  for (k in oth) {
    yk <- ts(log(S[, k] / S[, ref]), start = c(2005, 1), frequency = 4)
    fk <- auto.arima(yk, allowdrift = TRUE, seasonal = TRUE,
                     stepwise = FALSE, approximation = FALSE)
    alr_fits[[k]] <- fk
    alr_specs <- c(alr_specs, paste0(k, ": ARIMA(",
                                     paste(arimaorder(fk)[1:3], collapse = ","), ")",
                                     if ("drift" %in% names(coef(fk))) " w/ drift" else ""))
  }
  share_diag[[gname]] <- tibble::tibble(group = gname, reference_bucket = ref,
                                        alr_models = paste(alr_specs, collapse = " | "))

  ## Point shares from the mean alr forecasts
  alr_mean <- sapply(oth, function(k) as.numeric(forecast(alr_fits[[k]], h = H_MAX)$mean))
  if (is.null(dim(alr_mean))) alr_mean <- matrix(alr_mean, nrow = H_MAX)
  denom      <- 1 + rowSums(exp(alr_mean))
  share_pt   <- cbind(exp(alr_mean) / denom, 1 / denom)
  colnames(share_pt) <- c(oth, ref)
  share_pt   <- share_pt[, CAT_LABELS, drop = FALSE]

  ## ---- SIMULATION for intervals -------------------------------------
  sim_counts <- array(NA_real_, dim = c(N_SIM, H_MAX, 7),
                      dimnames = list(NULL, NULL, CAT_LABELS))
  for (s in seq_len(N_SIM)) {
    tot_s <- pmax(as.numeric(simulate(ft, nsim = H_MAX, future = TRUE)), 1)
    alr_s <- sapply(oth, function(k) as.numeric(simulate(alr_fits[[k]], nsim = H_MAX, future = TRUE)))
    if (is.null(dim(alr_s))) alr_s <- matrix(alr_s, nrow = H_MAX)
    dn    <- 1 + rowSums(exp(alr_s))
    sh    <- cbind(exp(alr_s) / dn, 1 / dn)
    colnames(sh) <- c(oth, ref)
    sim_counts[s, , ] <- sh[, CAT_LABELS, drop = FALSE] * tot_s
  }

  ## ---- Assemble, with integer counts that sum to the total ----------
  big_idx <- which.max(colMeans(S)[CAT_LABELS])
  pts <- t(sapply(seq_len(H_MAX), function(h)
    alloc_smooth(round(tot_point[h]), share_pt[h, ], big_idx)))
  colnames(pts) <- CAT_LABELS

  gr <- expand_grid(horizon_q = 1:H_MAX, asset_cat = CAT_LABELS) %>%
    left_join(fq_index, by = "horizon_q") %>%
    mutate(region = rg, cu_type = tp, group = gname,
           point = as.vector(t(pts[, CAT_LABELS])),
           share = as.vector(t(share_pt[, CAT_LABELS])),
           lo80 = NA_real_, hi80 = NA_real_, lo95 = NA_real_, hi95 = NA_real_)

  for (h in 1:H_MAX) for (k in CAT_LABELS) {
    q <- quantile(sim_counts[, h, k], c(.10, .90, .025, .975), na.rm = TRUE)
    idx <- which(gr$horizon_q == h & gr$asset_cat == k)
    gr$lo80[idx] <- round(q[1]); gr$hi80[idx] <- round(q[2])
    gr$lo95[idx] <- round(q[3]); gr$hi95[idx] <- round(q[4])
  }

  recon[[gname]] <- gr
  cat(sprintf("[%d/%d] %-14s total %4d -> %4d (%+.1f%%)  ref bucket: %s\n",
              g, nrow(groups), gname, Tv[length(Tv)], round(tot_point[H_MAX]),
              total_diag[[gname]]$chg_5yr_pct, ref))
}

recon_all  <- bind_rows(recon)
total_tbl  <- bind_rows(total_diag)
share_tbl  <- bind_rows(share_diag)

print(as.data.frame(total_tbl), row.names = FALSE)

## ---------------------------------------------------------------------
## [7.3] Audit: every group total must now be internally consistent
## ---------------------------------------------------------------------
audit_sum <- recon_all %>%
  filter(horizon_q %in% c(4, MID_H, H_MAX)) %>%
  group_by(group, horizon_q) %>%
  summarise(bucket_sum = sum(point), .groups = "drop") %>%
  left_join(total_tbl %>%
              select(group, total_1yr, total_mid, total_5yr) %>%
              pivot_longer(-group, values_to = "total_model") %>%
              mutate(horizon_q = case_when(name == "total_1yr" ~ 4,
                                           name == "total_mid" ~ MID_H,
                                           TRUE ~ H_MAX)) %>%
              select(-name),
            by = c("group", "horizon_q")) %>%
  mutate(gap = bucket_sum - total_model)

print(as.data.frame(audit_sum), row.names = FALSE)
cat("\nMax |gap| between bucket sum and total model:",
    max(abs(audit_sum$gap)), "(should be 0)\n")

## ---------------------------------------------------------------------
## [7.4] Before / after comparison -- this is what answers the field team
## ---------------------------------------------------------------------
indep <- summary_tbl %>%
  select(region, cu_type, asset_cat, actual_2026Q1,
         indep_1yr = fc_1yr, indep_5yr = fc_5yr) %>%
  mutate(asset_cat = as.character(asset_cat))

compare <- recon_all %>%
  filter(horizon_q %in% c(4, H_MAX)) %>%
  select(region, cu_type, asset_cat, horizon_q, point) %>%
  pivot_wider(names_from = horizon_q, values_from = point,
              names_prefix = "recon_h") %>%
  rename(recon_1yr = recon_h4, recon_5yr = !!paste0("recon_h", H_MAX)) %>%
  left_join(indep, by = c("region", "cu_type", "asset_cat")) %>%
  select(region, cu_type, asset_cat, actual_2026Q1,
         indep_1yr, recon_1yr, indep_5yr, recon_5yr) %>%
  mutate(diff_5yr = recon_5yr - indep_5yr)

group_compare <- compare %>%
  group_by(region, cu_type) %>%
  summarise(actual = sum(actual_2026Q1), indep_5yr = sum(indep_5yr),
            recon_5yr = sum(recon_5yr), .groups = "drop") %>%
  mutate(indep_chg = indep_5yr - actual, recon_chg = recon_5yr - actual)

print(as.data.frame(group_compare), row.names = FALSE)

## ---------------------------------------------------------------------
## [7.5] Charts from the reconciled numbers
## ---------------------------------------------------------------------
FC_LABS <- c("Current", "1 Year Out", sub("Count ", "", MID_LAB), "5 Year Out")
COLS4 <- c("#1F3B63", "#4F81BD", "#E9A13B", "#C0392B"); names(COLS4) <- FC_LABS

theme_sum <- theme_minimal(base_size = 11, base_family = "sans") +
  theme(plot.title = element_text(face = "bold", size = 12.5),
        plot.subtitle = element_text(size = 9.5, colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey45", hjust = 0),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "top", legend.title = element_blank(),
        axis.text.x = element_text(size = 8.5),
        plot.margin = margin(10, 14, 8, 10))

tab_data <- list()

for (g in seq_len(nrow(groups))) {

  rg <- groups$region[g]; tp <- groups$cu_type[g]
  gname <- sprintf("Region%dCU%d", rg, tp)

  cur <- counts %>% filter(region == rg, cu_type == tp, q_index == N_Q) %>%
    arrange(match(asset_cat, CAT_LABELS)) %>% pull(n_cu)

  wide <- recon_all %>% filter(group == gname, horizon_q %in% c(4, MID_H, H_MAX)) %>%
    select(asset_cat, horizon_q, point) %>%
    pivot_wider(names_from = horizon_q, values_from = point) %>%
    arrange(match(asset_cat, CAT_LABELS))

  d <- tibble::tibble(cat_no = seq_along(CAT_LABELS),
                      cat_lab = unname(TPL_CATS[CAT_LABELS]),
                      Current = cur,
                      f1 = wide[[as.character(4)]],
                      fm = wide[[as.character(MID_H)]],
                      f5 = wide[[as.character(H_MAX)]])
  ## Buckets this small cannot support a point estimate; mark them
  d <- d %>% mutate(small = pmax(Current, f1, fm, f5) < SMALL_N,
                    cat_lab = ifelse(small, paste0(cat_lab, " *"), cat_lab))
  tab_data[[gname]] <- d

  dl <- d %>%
    rename(!!FC_LABS[2] := f1, !!FC_LABS[3] := fm, !!FC_LABS[4] := f5) %>%
    pivot_longer(-c(cat_no, cat_lab), names_to = "horizon", values_to = "count") %>%
    mutate(horizon = factor(horizon, levels = FC_LABS),
           cat_lab = factor(cat_lab, levels = unname(TPL_CATS)))

  p <- ggplot(dl, aes(cat_lab, count, fill = horizon)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    geom_text(aes(label = ifelse(count >= SMALL_N, comma(count), "")),
              position = position_dodge(width = 0.8), vjust = -0.35,
              size = 2.6, colour = "grey20", family = "sans") +
    scale_fill_manual(values = COLS4) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.14))) +
    labs(title = paste0("Region ", rg, ", CU Type ", tp,
                        " - credit union counts by asset category"),
         subtitle = paste0("Reconciled forecasts: group total ", sum(cur), " -> ",
                           sum(d$f5), " over 5 years; buckets sum to the total"),
         x = NULL, y = "Number of credit unions",
         caption = paste0("Group total forecast by ARIMA; bucket shares forecast in ",
                          "log-ratio space and rescaled to the total.\n",
                          "Source: NCUA 5300 Call Report panel (OCE combined file, 2026Q1). ",
                          "Nominal asset categories.")) +
    theme_sum

  ggsave(file.path(CHART_DIR, paste0(gname, ".png")), p,
         width = 9, height = 4.6, dpi = 300, bg = "white")
}

## ---------------------------------------------------------------------
## [7.6] Workbook: 6 group tabs + method + reconciliation evidence
## ---------------------------------------------------------------------
SH <- list()

method <- c(
  "RECONCILED CREDIT UNION COUNT FORECASTS",
  "",
  paste("Prepared:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "Sample 2005Q1-2026Q1. Regions 1, 2, 3; cu_type 1 and 2.",
  "",
  "WHY THIS WORKBOOK EXISTS",
  "  The first version modelled all 42 region x type x asset cells independently.",
  "  Nothing constrained them to add up, and in Region 2 / CU type 2 the large",
  "  buckets grew by more than the small ones shrank, so the group total rose over",
  "  five years. A credit union only enters a larger bucket by leaving a smaller",
  "  one, so a rising group total implies institutions appearing from nowhere.",
  "  Net new charters are a handful per year nationally and cannot supply it.",
  "",
  "WHAT CHANGED",
  "  Each region x charter type is now forecast in two pieces:",
  "    TOTAL   one ARIMA on the group total, on a log scale with drift, so the",
  "            total follows the observed consolidation trend and stays positive.",
  "    SHARES  the seven bucket shares are transformed to six additive log-ratios",
  "            against the largest bucket, each forecast by ARIMA, then mapped",
  "            back to seven shares that sum to 1 by construction.",
  "  Counts = share x total. Each bucket's path is rounded to the nearest whole",
  "  credit union and the small residual is absorbed into the largest bucket, so",
  "  the seven buckets sum exactly to the total without small buckets appearing to",
  "  fall and recover on rounding alone.",
  "",
  paste0("  Buckets with fewer than ", SMALL_N, " credit unions are marked with * and should be"),
  "  read as directional. A count that small carries no meaningful point estimate.",
  "",
  "WHAT THIS DOES AND DOES NOT FIX",
  "  Buckets can still gain share, so the substantive finding survives: state",
  "  charters can gain ground on federal ones, and nominal asset growth still",
  "  pushes institutions up through fixed dollar thresholds. What can no longer",
  "  happen is a group growing in total. Note the direction of the original",
  "  question: Region 2 as a whole was never growing -- cu_type 1 there falls",
  "  faster than cu_type 2 rises. It was the type 2 SHARE that was growing.",
  "",
  "  Not fixed: the seven totals across regions still are not tied to a national",
  "  total, and share forecasts assume the log-ratio dynamics are stable.",
  "",
  "INTERVALS",
  paste0("  80% and 95% intervals come from ", N_SIM, " simulated paths of the total",
         " and the share"),
  "  models jointly, so they include both sources of uncertainty. They are wider",
  "  than the independent-model intervals, which is honest rather than worse.",
  "",
  "TABS",
  "  Group Totals    - the total model per region x charter type",
  "  Reconciliation  - independent vs reconciled, by cell and by group",
  "  Share Models    - reference bucket and log-ratio specifications",
  "  Region<r>CU<t>  - the summary table and chart for each group")

SH[[1]] <- list(name = "Method", rows = c(xl_line(method[1], 1, S_TITLE),
  vapply(seq_along(method)[-1], function(i) xl_line(method[i], i, S_NORM), "")),
  cols = col_widths(list(c(1, 1, 100))), freeze = NULL, autofilter = NULL, images = list())

b <- xl_block(total_tbl, 3, col_styles = c(S_NORM, S_INT, S_INT, S_NORM,
                                           S_INT, S_INT, S_INT, S_INT, S_DEC))
SH[[2]] <- list(name = "Group Totals",
  rows = c(xl_line("Group total models (the binding constraint)", 1, S_TITLE), b$xml),
  cols = col_widths(list(c(1, 3, 14), c(4, 4, 44), c(5, 9, 14))),
  freeze = NULL, autofilter = NULL, images = list())

b1 <- xl_block(group_compare, 3,
               col_styles = c(S_INT, S_INT, S_INT, S_INT, S_INT, S_INT, S_INT))
b2 <- xl_block(as.data.frame(compare), b1$next_row + 2,
               col_styles = c(S_INT, S_INT, S_NORM, rep(S_INT, 6)))
SH[[3]] <- list(name = "Reconciliation",
  rows = c(xl_line("Group level: independent models vs reconciled", 1, S_TITLE),
           b1$xml,
           xl_line("Cell level: independent models vs reconciled", b1$next_row + 1, S_SUB),
           b2$xml),
  cols = col_widths(list(c(1, 2, 10), c(3, 3, 16), c(4, 9, 14))),
  freeze = NULL, autofilter = NULL, images = list())

b <- xl_block(share_tbl, 3, col_styles = c(S_NORM, S_NORM, S_NORM))
SH[[4]] <- list(name = "Share Models",
  rows = c(xl_line("Log-ratio share models", 1, S_TITLE), b$xml),
  cols = col_widths(list(c(1, 2, 18), c(3, 3, 120))),
  freeze = NULL, autofilter = NULL, images = list())

for (g in seq_len(nrow(groups))) {
  rg <- groups$region[g]; tp <- groups$cu_type[g]
  gname <- sprintf("Region%dCU%d", rg, tp)
  d <- tab_data[[gname]]

  body <- data.frame(
    `Cat No` = d$cat_no, `Asset Categories` = d$cat_lab,
    `Current Count` = d$Current, `Count 1 Year Out` = d$f1,
    MID = d$fm, `Count 5 Year Out` = d$f5,
    check.names = FALSE, stringsAsFactors = FALSE)
  names(body)[5] <- MID_LAB

  rows <- c(
    xl_line(c(paste("Region", rg), paste("CU Type", tp)), 1, c(S_TITLE, S_TITLE), col = 2),
    xl_block(body, 3, col_styles = c(S_INT, S_NORM, S_INT, S_INT, S_INT, S_INT))$xml,
    xl_line(c("", "Total", sum(d$Current), sum(d$f1), sum(d$fm), sum(d$f5)), 11,
            c(S_NORM, S_BOLD, rep(S_INTBOLD, 4)), col = 1),
    xl_line("Sample 2005Q1-2026Q1. Buckets are reconciled to the group total.", 13),
    xl_line("See the Method tab for why these differ from the independent-model workbook.", 14),
    xl_line(if (any(d$small))
              paste0("* Fewer than ", SMALL_N, " credit unions: treat as directional only, ",
                     "not as a point forecast.")
            else "", 15))

  png_g <- file.path(CHART_DIR, paste0(gname, ".png"))
  SH[[4 + g]] <- list(name = gname, rows = rows,
    cols = col_widths(list(c(1, 1, 8), c(2, 2, 18), c(3, 6, 18))),
    freeze = NULL, autofilter = NULL,
    images = if (file.exists(png_g))
      list(list(file = png_g, col = 0, row = 15, w = 9, h = 4.6)) else list())
}

vapply(SH, function(s) s$name, "")
xlsx_write(SH, OUT_XLSX)

saveRDS(list(recon_all = recon_all, total_tbl = total_tbl, share_tbl = share_tbl,
             compare = compare, group_compare = group_compare,
             audit_sum = audit_sum),
        file = "cu_count_reconciled.rds")
