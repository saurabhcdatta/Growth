## =====================================================================
## 5_export_excel.R  --  Write the workbook using BASE R ONLY
##
## No openxlsx, no writexl, no Java. The OOXML writer lives in
## 0_xlsx_helpers.R; this script just builds the sheet content.
##
## Sheets: README | Summary | Coherence | Cell Index | 42 cell tabs |
##         Charts - Overview
## Run after 4_plots.R.
## =====================================================================

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)
## cvr  <- readRDS("cu_count_cv.rds");   list2env(cvr,  .GlobalEnv)
## fitr <- readRDS("cu_count_fits.rds"); list2env(fitr, .GlobalEnv)

PLOT_DIR <- file.path(getwd(), "plots")
EMBED_IMAGES <- dir.exists(PLOT_DIR) &&
  length(list.files(PLOT_DIR, pattern = "\\.png$")) > 0
EMBED_IMAGES

OUT_XLSX <- file.path(getwd(),
  paste0("CU_Count_Forecasts_2026Q1_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

source("0_xlsx_helpers.R")

## ---------------------------------------------------------------------
## [5.1] Sheet accumulator
## ---------------------------------------------------------------------
SH <- list()
add_sheet <- function(name, rows, cols = "", freeze = NULL,
                      autofilter = NULL, images = list()) {
  SH[[length(SH) + 1]] <<- list(name = name, rows = rows, cols = cols,
                                freeze = freeze, autofilter = autofilter,
                                images = images)
}

## ---------------------------------------------------------------------
## [5.2] README
## ---------------------------------------------------------------------
readme <- c(
  "NCUA CREDIT UNION COUNT FORECASTS BY REGION, CHARTER TYPE, AND ASSET CATEGORY",
  "",
  paste("Prepared:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "Source:  OCE_combined_2026q1_2000tocurrent.dta (5300 Call Report panel)",
  "Sample:  2005Q1 - 2026Q1 quarterly (data reliable from 2005Q1)",
  "Identifier: join_number (cu_number is not stable across conversions/mergers)",
  "Universe: cu_type 1 and 2; regions 1, 2 and 3 (variable `region`, not `region_hist`).",
  "          Region 8 (ONES) is excluded.",
  "",
  "TARGET VARIABLE",
  "  Count of credit unions in each cell in each quarter. 3 regions x 2 charter types",
  "  x 7 asset categories = 42 series.",
  "",
  "ASSET CATEGORIES (NOMINAL dollars, left-closed / right-open)",
  "  A1_LT10M      assets_tot <  $10,000,000",
  "  A2_10to50M    $10M  <= assets_tot < $50M",
  "  A3_50to100M   $50M  <= assets_tot < $100M",
  "  A4_100to500M  $100M <= assets_tot < $500M",
  "  A5_500Mto1B   $500M <= assets_tot < $1B",
  "  A6_1Bto10B    $1B   <= assets_tot < $10B",
  "  A7_GE10B      assets_tot >= $10B",
  "",
  "METHOD",
  "  For each cell a fixed set of ARIMA specifications (a standard grid plus the",
  "  full-sample auto.arima orders, seasonal and non-seasonal, plus log-scale",
  "  variants) is scored by expanding-window cross-validation. First origin uses",
  "  2005Q1-2012Q4;",
  "  the origin advances one quarter at a time through 2025Q4. At each origin every",
  "  candidate is refit and forecast up to 20 quarters ahead. The winner is the",
  "  candidate with the lowest RMSE pooled over horizons 1-20, then refit on the",
  "  full sample to produce the published forecasts.",
  "  Point forecasts and interval bounds are floored at zero and rounded to integers.",
  "",
  "HORIZONS",
  "  1-year ahead = 2027Q1 (h=4)   3-year = 2029Q1 (h=12)   5-year = 2031Q1 (h=20)",
  "",
  "CAVEATS - READ BEFORE USING",
  "  1. Cells flagged EMPTY carry no model; cells flagged SPARSE use a naive",
  "     forecast and their intervals should not be taken literally. The $10B+",
  "     categories remain thin even after dropping region 8.",
  "  2. Asset thresholds are nominal and fixed across 26 years. Part of the decline",
  "     in the smaller categories is bracket creep from inflation and nominal asset",
  "     growth, not consolidation. No price adjustment has been applied.",
  "  3. The 56 models are estimated independently, so the forecasts are not",
  "     coherent: they do not sum to a directly modeled total, and a credit union",
  "     crossing a threshold is not tracked out of one cell and into the next. See",
  "     the Coherence tab for the size of the gap.",
  "  4. Cross-validation errors measure out-of-sample accuracy of the selection",
  "     procedure. The ARIMA prediction intervals on each cell tab condition on the",
  "     winning model being correct and are therefore narrower than the CV errors",
  "     imply. Where the two disagree, trust the CV columns.",
  "  5. Reliability is horizon-dependent. The 1-year numbers are dependable for the",
  "     large middle categories; the 5-year numbers extrapolate a consolidation pace",
  "     that is a policy and rate-environment outcome, not a time series property.",
  "  6. FLAT-FORECAST OVERRIDE. Where cross-validation selected a model whose",
  "     forecast path is essentially constant, and the series has a statistically",
  "     detectable trend, the next-best non-flat candidate was published instead.",
  "     The Summary tab records which cells were overridden, what CV actually",
  "     preferred, and the CV RMSE cost of the override. An override buys a more",
  "     plausible path at some expected accuracy cost; it is a judgment call, not",
  "     a statistical improvement.",
  "",
  "TAB NAMING",
  "  R<region>_T<cu_type>_<asset category>. See the Cell Index tab.")

rows <- c(xl_line(readme[1], 1, S_TITLE),
          vapply(seq_along(readme)[-1], function(i)
            xl_line(readme[i], i, S_NORM), ""))
add_sheet("README", rows, col_widths(list(c(1, 1, 105))))

## ---------------------------------------------------------------------
## [5.3] Summary
## ---------------------------------------------------------------------
sum_styles <- rep(S_NORM, ncol(summary_tbl))
names(sum_styles) <- names(summary_tbl)
sum_styles[c("actual_2026Q1", "fc_1yr", "fc_3yr", "fc_5yr",
             "chg_1yr", "chg_3yr", "chg_5yr")] <- S_INT
sum_styles[c("cv_rmse_all", "cv_rmse_h4", "cv_rmse_h12", "cv_rmse_h20",
             "aicc", "lb_pvalue", "chg_5yr_pct", "override_rmse_cost")] <- S_DEC

b <- xl_block(summary_tbl, start_row = 3, col_styles = as.integer(sum_styles))
rows <- c(xl_line("Forecast summary - all 56 cells", 1, S_TITLE), b$xml)

af <- sprintf("A3:%s%d", xl_col(ncol(summary_tbl)), 3 + nrow(summary_tbl))
add_sheet("Summary", rows,
          col_widths(list(c(1, 2, 14), c(3, 5, 10), c(6, 6, 46),
                          c(7, 14, 13), c(15, 15, 46), c(16, 16, 24),
                          c(17, 20, 14), c(21, 27, 13))),
          freeze = list(x = 2, y = 3), autofilter = af)

## ---------------------------------------------------------------------
## [5.4] Coherence
## ---------------------------------------------------------------------
coh_styles <- rep(S_NORM, ncol(coherence))
names(coh_styles) <- names(coherence)
coh_styles[c("bottom_up", "direct_total", "gap")] <- S_INT
coh_styles["gap_pct"] <- S_DEC

b <- xl_block(coherence, start_row = 4, col_styles = as.integer(coh_styles))
rows <- c(xl_line("Sum of the 56 cell forecasts vs. a directly modeled total", 1, S_TITLE),
          xl_line(paste("Direct total model: ARIMA order",
                        paste(forecast::arimaorder(fit_total), collapse = ",")), 2),
          b$xml)
add_sheet("Coherence", rows, col_widths(list(c(1, 2, 14), c(3, 7, 16))))

## ---------------------------------------------------------------------
## [5.5] Cell Index
## ---------------------------------------------------------------------
cell_index <- merge(cells, cell_diag[, c("tab_name", "n_nonzero", "mean_count",
                                         "last_count", "status")],
                    by = "tab_name", sort = FALSE)
cell_index <- cell_index[order(cell_index$cell_id),
                         c("cell_id", "tab_name", "label", "region", "cu_type",
                           "asset_cat", "n_nonzero", "mean_count", "last_count",
                           "status")]
names(cell_index)[7:9] <- c("quarters_nonzero", "mean_count", "count_2026Q1")

b <- xl_block(cell_index, start_row = 3,
              col_styles = c(S_NORM, S_NORM, S_NORM, S_NORM, S_NORM, S_NORM,
                             S_INT, S_DEC, S_INT, S_NORM))
rows <- c(xl_line("Index of the 56 cell tabs", 1, S_TITLE), b$xml)
add_sheet("Cell Index", rows,
          col_widths(list(c(1, 1, 8), c(2, 2, 18), c(3, 3, 46),
                          c(4, 9, 13), c(10, 10, 24))),
          freeze = list(x = 0, y = 3))

## ---------------------------------------------------------------------
## [5.6] The 42 cell tabs
## ---------------------------------------------------------------------
for (i in seq_len(nrow(cells))) {

  tab <- cells$tab_name[i]
  d   <- diagnostics[diagnostics$tab_name == tab, ]
  hist_tbl <- counts[counts$region == cells$region[i] &
                     counts$cu_type == cells$cu_type[i] &
                     counts$asset_cat == cells$asset_cat[i], ]
  hist_tbl <- hist_tbl[order(hist_tbl$q_index), c("q_label", "year", "quarter", "n_cu")]
  names(hist_tbl)[4] <- "count"

  hdr <- data.frame(
    Field = c("Cell", "Region", "Charter type (cu_type)", "Asset category",
              "Status", "Published model", "Selection basis",
              "Trend detected", "Flat-forecast override",
              "Model CV preferred", "CV RMSE cost of override",
              "5-year change (%)", "AICc (full sample)",
              "Ljung-Box p (lag 8)", "Actual count 2026Q1"),
    Value = c(cells$label[i], cells$region[i], cells$cu_type[i],
              CAT_PRETTY[as.character(cells$asset_cat[i])],
              d$status, d$spec, d$method,
              ifelse(isTRUE(d$has_trend), "yes", "no"),
              ifelse(isTRUE(d$override), "YES", "no"),
              ifelse(isTRUE(d$override), d$cv_winner_spec, "same as published"),
              ifelse(isTRUE(d$override), format(d$override_rmse_cost), "-"),
              ifelse(is.na(d$chg_5yr_pct), "n/a", format(d$chg_5yr_pct)),
              ifelse(is.na(d$aicc), "n/a", format(d$aicc)),
              ifelse(is.na(d$lb_pvalue), "n/a", format(d$lb_pvalue)),
              tail(hist_tbl$count, 1)),
    stringsAsFactors = FALSE)

  rows <- xl_line(cells$label[i], 1, S_TITLE)
  b <- xl_block(hdr, start_row = 3); rows <- c(rows, b$xml); r <- b$next_row + 1

  ## Forecast block
  rows <- c(rows, xl_line("FORECAST (point, floored at 0 and rounded)", r, S_SUB))
  r <- r + 1
  fq <- fc_tables[[tab]]
  if (!is.null(fq) && nrow(fq) > 0) {
    fq_out <- fq[, c("q_label", "horizon_q", "horizon_label", "point",
                     "lo80", "hi80", "lo95", "hi95")]
    key <- fq_out$horizon_q %in% c(4, 12, 20)
    b <- xl_block(fq_out, start_row = r,
                  col_styles = c(S_NORM, S_INT, S_NORM, rep(S_INT, 5)))
    ## bold the 1/3/5-year rows by rewriting them
    hdr_row <- b$xml[1]; body <- b$xml[-1]
    for (k in which(key)) {
      rr <- r + k
      cs <- vapply(seq_len(ncol(fq_out)), function(j)
        xl_cell(j, rr, fq_out[[j]][k],
                if (j %in% c(2, 4:8)) S_INTBOLD else S_BOLD), "")
      body[k] <- paste0('<row r="', rr, '">', paste(cs, collapse = ""), '</row>')
    }
    rows <- c(rows, hdr_row, body); r <- b$next_row + 1
  } else {
    rows <- c(rows, xl_line("No forecast produced - cell is structurally empty.", r))
    r <- r + 2
  }

  ## Candidate comparison block
  rows <- c(rows, xl_line("CROSS-VALIDATION: ALL CANDIDATES, EXPANDING WINDOW", r, S_SUB))
  r <- r + 1
  if (!is.null(cv_summary[[tab]])) {
    cvt <- as.data.frame(cv_summary[[tab]])[, c("rank", "spec", "source", "n_fits",
                                                "rmse_all", "mae_all", "rmse_h4",
                                                "rmse_h12", "rmse_h20", "winner")]
    cvt$winner <- ifelse(cvt$winner, "WINNER", "")
    b <- xl_block(cvt, start_row = r,
                  col_styles = c(S_INT, S_NORM, S_NORM, S_INT, rep(S_DEC, 5), S_NORM))
    ## shade the winning row
    hdr_row <- b$xml[1]; body <- b$xml[-1]
    cs <- vapply(seq_len(ncol(cvt)), function(j)
      xl_cell(j, r + 1, cvt[[j]][1], if (j %in% c(5:9)) S_FLAGINT else S_FLAG), "")
    body[1] <- paste0('<row r="', r + 1, '">', paste(cs, collapse = ""), '</row>')
    rows <- c(rows, hdr_row, body); r <- b$next_row + 1
  } else {
    rows <- c(rows, xl_line("No cross-validation run for this cell.", r))
    r <- r + 2
  }

  ## History block
  rows <- c(rows, xl_line("HISTORY 2005Q1 - 2026Q1", r, S_SUB)); r <- r + 1
  b <- xl_block(hist_tbl, start_row = r,
                col_styles = c(S_NORM, S_INT, S_INT, S_INT))
  rows <- c(rows, b$xml)

  img <- list()
  png_i <- file.path(PLOT_DIR, paste0(tab, ".png"))
  if (EMBED_IMAGES && file.exists(png_i))
    img <- list(list(file = png_i, col = 10, row = 2, w = 9, h = 5))

  add_sheet(tab, rows,
            col_widths(list(c(1, 1, 24), c(2, 2, 44), c(3, 9, 14))),
            images = img)
  cat(sprintf("built tab %2d/%d: %s\n", i, nrow(cells), tab))
}

## ---------------------------------------------------------------------
## [5.7] Charts - Overview
## ---------------------------------------------------------------------
ov_files <- file.path(PLOT_DIR,
  paste0("OVERVIEW_R", rep(c(1, 2, 3), each = 2), "_T", rep(c(1, 2), 3), ".png"))
ov_files <- ov_files[file.exists(ov_files)]

if (EMBED_IMAGES && length(ov_files)) {
  imgs <- lapply(seq_along(ov_files), function(k)
    list(file = ov_files[k], col = 1, row = 2 + (k - 1) * 40, w = 12, h = 7.5))
  add_sheet("Charts - Overview",
            xl_line("Overview panels: one block per region x charter type", 1, S_TITLE),
            col_widths(list(c(1, 1, 4))), images = imgs)
}

length(SH)   # expect 46 (47 with the overview tab)

## ---------------------------------------------------------------------
## [5.8] Write it
## ---------------------------------------------------------------------
xlsx_write(SH, OUT_XLSX)
