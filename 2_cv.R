## =====================================================================
## 2_cv.R  --  Expanding-window time series cross-validation
## Scores a fixed candidate set of ARIMA specs per cell and picks a winner.
## Expect roughly 10-25 minutes for all 56 cells with ORIGIN_STEP = 1.
## =====================================================================

library(dplyr)
library(tidyr)
library(forecast)

## Re-load if starting a fresh session
## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)

## ---------------------------------------------------------------------
## [2.1] CV settings
## ---------------------------------------------------------------------
MIN_TRAIN   <- 40        # first origin uses 2000Q1-2009Q4 (10 years)
H_MAX       <- 20        # forecast out 20 quarters = 5 years
ORIGIN_STEP <- 1         # set to 2 to halve runtime
H_REPORT    <- c(4, 12, 20)   # 1yr, 3yr, 5yr

origins <- seq(MIN_TRAIN, N_Q - 1, by = ORIGIN_STEP)
length(origins)

## ---------------------------------------------------------------------
## [2.2] Candidate specifications
## Drift is only legal when d == 1 and D == 0, so the table respects that.
## ---------------------------------------------------------------------
cand_base <- tibble::tribble(
  ~p, ~d, ~q, ~P, ~D, ~Q, ~drift, ~source,
   0,  1,  0,  0,  0,  0,  FALSE,  "grid",   # random walk
   0,  1,  0,  0,  0,  0,  TRUE,   "grid",   # random walk with drift
   0,  1,  1,  0,  0,  0,  FALSE,  "grid",
   0,  1,  1,  0,  0,  0,  TRUE,   "grid",
   1,  1,  0,  0,  0,  0,  TRUE,   "grid",
   1,  1,  1,  0,  0,  0,  TRUE,   "grid",
   2,  1,  0,  0,  0,  0,  TRUE,   "grid",
   1,  1,  1,  0,  0,  0,  FALSE,  "grid",
   0,  1,  1,  0,  1,  1,  FALSE,  "grid",   # seasonal
   1,  1,  0,  1,  0,  0,  TRUE,   "grid"
)

## ---------------------------------------------------------------------
## [2.3] Per-cell auto.arima orders, appended to the grid
## ---------------------------------------------------------------------
model_cells <- cell_diag %>% filter(status == "MODEL")
nrow(model_cells)

auto_orders <- vector("list", nrow(model_cells))

for (i in seq_len(nrow(model_cells))) {
  y  <- ts_list[[model_cells$tab_name[i]]]
  a1 <- auto.arima(y, seasonal = TRUE,  stepwise = FALSE, approximation = FALSE)
  a2 <- auto.arima(y, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  o1 <- arimaorder(a1); o2 <- arimaorder(a2)
  g1 <- c(o1, if (length(o1) == 3) c(0, 0, 0) else NULL)
  g2 <- c(o2, c(0, 0, 0))
  auto_orders[[i]] <- tibble::tibble(
    p = c(g1[1], g2[1]), d = c(g1[2], g2[2]), q = c(g1[3], g2[3]),
    P = c(g1[4], g2[4]), D = c(g1[5], g2[5]), Q = c(g1[6], g2[6]),
    drift = c("drift" %in% names(coef(a1)), "drift" %in% names(coef(a2))),
    source = c("auto.arima (seasonal allowed)", "auto.arima (non-seasonal)"))
  cat(sprintf("[%2d/%2d] %-16s auto: %s\n", i, nrow(model_cells),
              model_cells$tab_name[i], paste(o1, collapse = ",")))
}
names(auto_orders) <- model_cells$tab_name

## ---------------------------------------------------------------------
## [2.4] The CV loop
## err_store[[tab]] is an array: origins x horizons x candidates
## ---------------------------------------------------------------------
cv_summary <- list()
err_store  <- list()
cand_store <- list()

t0 <- Sys.time()

for (i in seq_len(nrow(model_cells))) {

  tab <- model_cells$tab_name[i]
  y   <- ts_list[[tab]]
  yv  <- as.numeric(y)

  cand <- bind_rows(cand_base, auto_orders[[tab]]) %>%
    mutate(drift = drift & d == 1 & D == 0) %>%
    distinct(p, d, q, P, D, Q, drift, .keep_all = TRUE) %>%
    mutate(spec = sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[4]%s",
                          p, d, q, P, D, Q, ifelse(drift, " w/ drift", "")))
  cand_store[[tab]] <- cand

  err <- array(NA_real_, dim = c(length(origins), H_MAX, nrow(cand)))

  for (oi in seq_along(origins)) {
    n_tr  <- origins[oi]
    train <- subset(y, end = n_tr)
    h_o   <- min(H_MAX, N_Q - n_tr)

    for (k in seq_len(nrow(cand))) {
      fit <- tryCatch(
        Arima(train,
              order        = c(cand$p[k], cand$d[k], cand$q[k]),
              seasonal     = list(order = c(cand$P[k], cand$D[k], cand$Q[k]),
                                  period = 4),
              include.drift = cand$drift[k],
              method = "ML"),
        error = function(e) NULL, warning = function(w) NULL)
      if (is.null(fit)) next

      fc <- tryCatch(as.numeric(forecast(fit, h = h_o)$mean),
                     error = function(e) NULL)
      if (is.null(fc)) next

      fc <- pmax(fc, 0)                      # counts cannot go negative
      err[oi, seq_len(h_o), k] <- yv[(n_tr + 1):(n_tr + h_o)] - fc
    }
  }

  err_store[[tab]] <- err

  ## Score each candidate
  scores <- tibble::tibble(cand_id = seq_len(nrow(cand)), spec = cand$spec,
                           source = cand$source)
  scores$n_fits    <- apply(err, 3, function(m) sum(!is.na(m[, 1])))
  scores$rmse_all  <- apply(err, 3, function(m) sqrt(mean(m^2, na.rm = TRUE)))
  scores$mae_all   <- apply(err, 3, function(m) mean(abs(m), na.rm = TRUE))
  for (h in H_REPORT) {
    scores[[paste0("rmse_h", h)]] <- apply(err, 3, function(m) sqrt(mean(m[, h]^2, na.rm = TRUE)))
    scores[[paste0("mae_h",  h)]] <- apply(err, 3, function(m) mean(abs(m[, h]), na.rm = TRUE))
  }

  ## A candidate must have fit at a supermajority of origins to be eligible
  scores <- scores %>%
    mutate(eligible = n_fits >= 0.8 * length(origins) & is.finite(rmse_all)) %>%
    arrange(desc(eligible), rmse_all) %>%
    mutate(rank = row_number(), winner = rank == 1 & eligible)

  cv_summary[[tab]] <- scores

  cat(sprintf("[%2d/%2d] %-16s winner: %-38s CV RMSE %.2f  (%s elapsed)\n",
              i, nrow(model_cells), tab,
              scores$spec[1], scores$rmse_all[1],
              format(round(difftime(Sys.time(), t0, units = "mins"), 1))))
}

difftime(Sys.time(), t0, units = "mins")

## ---------------------------------------------------------------------
## [2.5] Eyeball the selection across all cells
## ---------------------------------------------------------------------
winners <- bind_rows(lapply(names(cv_summary), function(tab)
  cv_summary[[tab]] %>% filter(winner) %>% mutate(tab_name = tab)))

print(as.data.frame(winners[, c("tab_name", "spec", "rmse_all",
                                "rmse_h4", "rmse_h12", "rmse_h20")]),
      row.names = FALSE)

table(winners$spec)

saveRDS(list(cv_summary = cv_summary, cand_store = cand_store,
             winners = winners, origins = origins,
             MIN_TRAIN = MIN_TRAIN, H_MAX = H_MAX, H_REPORT = H_REPORT),
        file = "cu_count_cv.rds")
