## =====================================================================
## 4_export_excel.R  --  Write the workbook
## README | Summary | Coherence | Cell index | 56 cell tabs
## =====================================================================

library(dplyr)
library(tidyr)
library(openxlsx)

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)
## cvr  <- readRDS("cu_count_cv.rds");   list2env(cvr,  .GlobalEnv)
## fitr <- readRDS("cu_count_fits.rds"); list2env(fitr, .GlobalEnv)

OUT_XLSX <- file.path(getwd(),
                      paste0("CU_Count_Forecasts_2026Q1_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

options("openxlsx.dateFormat" = "yyyy-mm-dd")
modifyBaseFont(wb <- createWorkbook(), fontSize = 10, fontName = "Arial")

st_title   <- createStyle(fontSize = 13, textDecoration = "bold")
st_head    <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1",
                          border = "TopBottom", borderColour = "#4F81BD", wrapText = TRUE)
st_sub     <- createStyle(textDecoration = "bold", fontColour = "#1F497D")
st_int     <- createStyle(numFmt = "#,##0")
st_dec2    <- createStyle(numFmt = "0.00")
st_note    <- createStyle(fontSize = 9, fontColour = "#7F7F7F", wrapText = TRUE)
st_flag    <- createStyle(fgFill = "#FFF2CC")

## ---------------------------------------------------------------------
## [4.1] README
## ---------------------------------------------------------------------
readme <- c(
  "NCUA CREDIT UNION COUNT FORECASTS BY REGION, CHARTER TYPE, AND ASSET CATEGORY",
  "",
  paste("Prepared:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  paste("Source:  OCE_combined_2026q1_2000tocurrent.dta (5300 Call Report panel)"),
  "Sample:  2000Q1 - 2026Q1 quarterly, credit union x quarter panel",
  "Universe: cu_type 1 and 2; region 1, 2, 3, 8 (variable `region`, not `region_hist`)",
  "",
  "TARGET VARIABLE",
  "  Count of credit unions in each cell in each quarter. 4 regions x 2 charter types",
  "  x 7 asset categories = 56 series.",
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
  "  full-sample auto.arima orders, seasonal and non-seasonal) is scored by",
  "  expanding-window time series cross-validation. First origin uses 2000Q1-2009Q4;",
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
  "  1. Region 8 is ONES, a supervisory office rather than a geography. Its",
  "     small-asset cells are structurally empty and several region 1/2/3 large-asset",
  "     cells are near-empty. Cells flagged EMPTY carry no model; cells flagged",
  "     SPARSE use a naive forecast and their intervals should not be taken",
  "     literally.",
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
  "",
  "TAB NAMING",
  "  R<region>_T<cu_type>_<asset category>. See the Cell Index tab.")

addWorksheet(wb, "README", tabColour = "#1F497D")
writeData(wb, "README", readme, startCol = 1, startRow = 1, colNames = FALSE)
addStyle(wb, "README", st_title, rows = 1, cols = 1)
setColWidths(wb, "README", cols = 1, widths = 105)

## ---------------------------------------------------------------------
## [4.2] Summary
## ---------------------------------------------------------------------
addWorksheet(wb, "Summary", tabColour = "#1F497D")
writeData(wb, "Summary", "Forecast summary - all 56 cells", startRow = 1)
addStyle(wb, "Summary", st_title, rows = 1, cols = 1)
writeData(wb, "Summary", summary_tbl, startRow = 3, headerStyle = st_head)
freezePane(wb, "Summary", firstActiveRow = 4, firstActiveCol = 3)
setColWidths(wb, "Summary", cols = 1:ncol(summary_tbl), widths = "auto")
addFilter(wb, "Summary", rows = 3, cols = 1:ncol(summary_tbl))
n_s <- nrow(summary_tbl)
addStyle(wb, "Summary", st_int,  rows = 4:(3 + n_s), cols = 7:13, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Summary", st_dec2, rows = 4:(3 + n_s), cols = 16:21, gridExpand = TRUE, stack = TRUE)
flag_rows <- which(summary_tbl$status != "MODEL") + 3
if (length(flag_rows)) addStyle(wb, "Summary", st_flag, rows = flag_rows,
                                cols = 1:ncol(summary_tbl), gridExpand = TRUE, stack = TRUE)

## ---------------------------------------------------------------------
## [4.3] Coherence
## ---------------------------------------------------------------------
addWorksheet(wb, "Coherence", tabColour = "#1F497D")
writeData(wb, "Coherence", "Sum of the 56 cell forecasts vs. a directly modeled total", startRow = 1)
addStyle(wb, "Coherence", st_title, rows = 1, cols = 1)
writeData(wb, "Coherence",
          paste("Direct total model:", paste(arimaorder(fit_total), collapse = ",")),
          startRow = 2)
writeData(wb, "Coherence", coherence, startRow = 4, headerStyle = st_head)
setColWidths(wb, "Coherence", cols = 1:ncol(coherence), widths = "auto")
addStyle(wb, "Coherence", st_int, rows = 5:(4 + nrow(coherence)), cols = 3:5,
         gridExpand = TRUE, stack = TRUE)

## ---------------------------------------------------------------------
## [4.4] Cell index
## ---------------------------------------------------------------------
cell_index <- cells %>%
  left_join(cell_diag %>% select(tab_name, n_nonzero, mean_count, last_count, status),
            by = "tab_name") %>%
  select(cell_id, tab_name, label, region, cu_type, asset_cat,
         quarters_nonzero = n_nonzero, mean_count, count_2026Q1 = last_count, status)

addWorksheet(wb, "Cell Index", tabColour = "#1F497D")
writeData(wb, "Cell Index", "Index of the 56 cell tabs", startRow = 1)
addStyle(wb, "Cell Index", st_title, rows = 1, cols = 1)
writeData(wb, "Cell Index", cell_index, startRow = 3, headerStyle = st_head)
setColWidths(wb, "Cell Index", cols = 1:ncol(cell_index), widths = "auto")

## ---------------------------------------------------------------------
## [4.5] The 56 cell tabs
## ---------------------------------------------------------------------
for (i in seq_len(nrow(cells))) {

  tab <- cells$tab_name[i]
  sh  <- tab
  d   <- diagnostics %>% filter(tab_name == tab)
  hist_tbl <- counts %>%
    filter(region == cells$region[i], cu_type == cells$cu_type[i],
           asset_cat == cells$asset_cat[i]) %>%
    arrange(q_index) %>% select(q_label, year, quarter, count = n_cu)

  addWorksheet(wb, sh,
               tabColour = if (d$status == "MODEL") "#4F81BD" else "#C0504D")

  ## Header block
  hdr <- data.frame(
    Field = c("Cell", "Region", "Charter type (cu_type)", "Asset category",
              "Status", "Selected model", "Selection basis",
              "AICc (full sample)", "Ljung-Box p (lag 8)",
              "Actual count 2026Q1"),
    Value = c(cells$label[i], cells$region[i], cells$cu_type[i],
              CAT_PRETTY[as.character(cells$asset_cat[i])],
              d$status, d$spec, d$method,
              ifelse(is.na(d$aicc), "n/a", format(d$aicc)),
              ifelse(is.na(d$lb_pvalue), "n/a", format(d$lb_pvalue)),
              tail(hist_tbl$count, 1)),
    stringsAsFactors = FALSE)

  writeData(wb, sh, cells$label[i], startRow = 1)
  addStyle(wb, sh, st_title, rows = 1, cols = 1)
  writeData(wb, sh, hdr, startRow = 3, headerStyle = st_head)
  setColWidths(wb, sh, cols = 1, widths = 24)
  setColWidths(wb, sh, cols = 2:8, widths = 15)

  ## Forecast block
  r <- 3 + nrow(hdr) + 2
  writeData(wb, sh, "FORECAST (point, floored at 0 and rounded)", startRow = r)
  addStyle(wb, sh, st_sub, rows = r, cols = 1)
  fq <- fc_tables[[tab]]
  if (nrow(fq) > 0) {
    fq_out <- fq %>% select(q_label, horizon_q, horizon_label, point,
                            lo80, hi80, lo95, hi95)
    writeData(wb, sh, fq_out, startRow = r + 1, headerStyle = st_head)
    addStyle(wb, sh, st_int, rows = (r + 2):(r + 1 + nrow(fq_out)), cols = 4:8,
             gridExpand = TRUE, stack = TRUE)
    key_rows <- which(fq_out$horizon_q %in% c(4, 12, 20)) + r + 1
    addStyle(wb, sh, createStyle(textDecoration = "bold"), rows = key_rows,
             cols = 1:8, gridExpand = TRUE, stack = TRUE)
    r <- r + 1 + nrow(fq_out) + 2
  } else {
    writeData(wb, sh, "No forecast produced - cell is structurally empty.",
              startRow = r + 1)
    r <- r + 3
  }

  ## Candidate comparison block
  writeData(wb, sh, "CROSS-VALIDATION: ALL CANDIDATES, EXPANDING WINDOW", startRow = r)
  addStyle(wb, sh, st_sub, rows = r, cols = 1)
  if (!is.null(cv_summary[[tab]])) {
    cvt <- cv_summary[[tab]] %>%
      select(rank, spec, source, n_fits, rmse_all, mae_all,
             rmse_h4, rmse_h12, rmse_h20, winner)
    writeData(wb, sh, cvt, startRow = r + 1, headerStyle = st_head)
    addStyle(wb, sh, st_dec2, rows = (r + 2):(r + 1 + nrow(cvt)), cols = 5:9,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sh, st_flag, rows = r + 2, cols = 1:10, gridExpand = TRUE, stack = TRUE)
    r <- r + 1 + nrow(cvt) + 2
  } else {
    writeData(wb, sh, "No cross-validation run for this cell.", startRow = r + 1)
    r <- r + 3
  }

  ## History block
  writeData(wb, sh, "HISTORY 2000Q1 - 2026Q1", startRow = r)
  addStyle(wb, sh, st_sub, rows = r, cols = 1)
  writeData(wb, sh, hist_tbl, startRow = r + 1, headerStyle = st_head)
  addStyle(wb, sh, st_int, rows = (r + 2):(r + 1 + nrow(hist_tbl)), cols = 4,
           gridExpand = TRUE, stack = TRUE)

  cat(sprintf("wrote tab %2d/56: %s\n", i, sh))
}

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
cat("\nWorkbook written to:\n", OUT_XLSX, "\n")
