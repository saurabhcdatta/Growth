## =====================================================================
## 5_export_excel.R  --  Write the workbook using BASE R ONLY
##
## No openxlsx, no writexl, no Java. The .xlsx is assembled as raw OOXML
## parts and zipped. Three zip fallbacks are tried in order, so this works
## with or without Rtools on the machine.
##
## Sheets: README | Summary | Coherence | Cell Index | 56 cell tabs |
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

## =====================================================================
## [5.0] XML helpers. These are the only functions in the pipeline --
##       everything below them is sequential and inspectable.
## =====================================================================

xl_col <- function(n) {
  s <- ""
  while (n > 0) { r <- (n - 1) %% 26; s <- paste0(LETTERS[r + 1], s); n <- (n - 1) %/% 26 }
  s
}

xl_esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("[\001-\010\013\014\016-\037]", "", x)
}

## Style slots, in the same order as cellXfs in [5.1]
S_NORM <- 0; S_HDR <- 1; S_TITLE <- 2; S_SUB <- 3; S_INT <- 4
S_DEC  <- 5; S_FLAG <- 6; S_BOLD <- 7; S_INTBOLD <- 8; S_WRAP <- 9
S_FLAGINT <- 10

xl_cell <- function(col, row, v, s = S_NORM) {
  ref <- paste0(xl_col(col), row)
  st  <- if (s == 0) "" else paste0(' s="', s, '"')
  if (length(v) == 0 || is.na(v)) return(paste0('<c r="', ref, '"', st, '/>'))
  if (is.logical(v)) v <- as.integer(v)
  if (is.numeric(v)) {
    if (!is.finite(v)) return(paste0('<c r="', ref, '" t="inlineStr"', st,
                                     '><is><t>n/a</t></is></c>'))
    return(paste0('<c r="', ref, '"', st, '><v>',
                  format(v, scientific = FALSE, trim = TRUE, digits = 15), '</v></c>'))
  }
  v <- as.character(v)
  if (!nzchar(v)) return(paste0('<c r="', ref, '"', st, '/>'))
  paste0('<c r="', ref, '" t="inlineStr"', st, '><is><t xml:space="preserve">',
         xl_esc(v), '</t></is></c>')
}

## Turn a data frame into <row> XML. Returns the XML and the next free row.
xl_block <- function(df, start_row, start_col = 1L, col_styles = NULL,
                     header = TRUE, hdr_style = S_HDR) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (j in seq_along(df)) if (is.factor(df[[j]])) df[[j]] <- as.character(df[[j]])
  nc <- ncol(df); nr <- nrow(df)
  if (is.null(col_styles)) col_styles <- rep(S_NORM, nc)
  col_styles <- rep_len(col_styles, nc)
  out <- character(0); r <- start_row
  if (header) {
    cs <- vapply(seq_len(nc), function(j)
      xl_cell(start_col + j - 1, r, names(df)[j], hdr_style), "")
    out <- c(out, paste0('<row r="', r, '">', paste(cs, collapse = ""), '</row>'))
    r <- r + 1
  }
  if (nr > 0) for (i in seq_len(nr)) {
    cs <- vapply(seq_len(nc), function(j)
      xl_cell(start_col + j - 1, r, df[[j]][i], col_styles[j]), "")
    out <- c(out, paste0('<row r="', r, '">', paste(cs, collapse = ""), '</row>'))
    r <- r + 1
  }
  list(xml = out, next_row = r)
}

## A single line of text in column A
xl_line <- function(txt, row, style = S_NORM, col = 1L) {
  paste0('<row r="', row, '">', xl_cell(col, row, txt, style), '</row>')
}

## =====================================================================
## [5.1] Static parts: styles.xml
## =====================================================================
styles_xml <- paste0(
'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
'<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
'<numFmts count="2">',
  '<numFmt numFmtId="164" formatCode="#,##0"/>',
  '<numFmt numFmtId="165" formatCode="0.00"/>',
'</numFmts>',
'<fonts count="5">',
  '<font><sz val="10"/><name val="Arial"/></font>',
  '<font><b/><sz val="10"/><name val="Arial"/></font>',
  '<font><b/><sz val="13"/><name val="Arial"/></font>',
  '<font><b/><sz val="10"/><color rgb="FF1F497D"/><name val="Arial"/></font>',
  '<font><sz val="9"/><color rgb="FF7F7F7F"/><name val="Arial"/></font>',
'</fonts>',
'<fills count="4">',
  '<fill><patternFill patternType="none"/></fill>',
  '<fill><patternFill patternType="gray125"/></fill>',
  '<fill><patternFill patternType="solid"><fgColor rgb="FFDCE6F1"/><bgColor indexed="64"/></patternFill></fill>',
  '<fill><patternFill patternType="solid"><fgColor rgb="FFFFF2CC"/><bgColor indexed="64"/></patternFill></fill>',
'</fills>',
'<borders count="2">',
  '<border><left/><right/><top/><bottom/><diagonal/></border>',
  '<border><left/><right/>',
    '<top style="thin"><color rgb="FF4F81BD"/></top>',
    '<bottom style="thin"><color rgb="FF4F81BD"/></bottom><diagonal/></border>',
'</borders>',
'<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
'<cellXfs count="11">',
  '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>',                                                  # 0 normal
  '<xf numFmtId="0" fontId="1" fillId="2" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment vertical="center" wrapText="1"/></xf>',  # 1 header
  '<xf numFmtId="0" fontId="2" fillId="0" borderId="0" xfId="0" applyFont="1"/>',                                    # 2 title
  '<xf numFmtId="0" fontId="3" fillId="0" borderId="0" xfId="0" applyFont="1"/>',                                    # 3 subhead
  '<xf numFmtId="164" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',                          # 4 integer
  '<xf numFmtId="165" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',                          # 5 2dp
  '<xf numFmtId="0" fontId="0" fillId="3" borderId="0" xfId="0" applyFill="1"/>',                                    # 6 flag fill
  '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>',                                    # 7 bold
  '<xf numFmtId="164" fontId="1" fillId="0" borderId="0" xfId="0" applyNumberFormat="1" applyFont="1"/>',            # 8 bold integer
  '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0" applyAlignment="1"><alignment wrapText="1" vertical="top"/></xf>',  # 9 wrap
  '<xf numFmtId="164" fontId="0" fillId="3" borderId="0" xfId="0" applyNumberFormat="1" applyFill="1"/>',            # 10 flag integer
'</cellXfs>',
'<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
'</styleSheet>')

## =====================================================================
## [5.2] Sheet accumulator
## Each entry: name, rows (character vector of <row>), cols XML,
##             freeze spec, autofilter ref, images list
## =====================================================================
SH <- list()

add_sheet <- function(name, rows, cols = "", freeze = NULL,
                      autofilter = NULL, images = list()) {
  stopifnot(nchar(name) <= 31)
  SH[[length(SH) + 1]] <<- list(name = name, rows = rows, cols = cols,
                                freeze = freeze, autofilter = autofilter,
                                images = images)
}

col_widths <- function(specs) {
  ## specs: list of c(min, max, width)
  if (!length(specs)) return("")
  paste0("<cols>",
         paste(vapply(specs, function(s)
           sprintf('<col min="%d" max="%d" width="%.1f" customWidth="1"/>',
                   s[1], s[2], s[3]), ""), collapse = ""),
         "</cols>")
}

## ---------------------------------------------------------------------
## [5.3] README
## ---------------------------------------------------------------------
readme <- c(
  "NCUA CREDIT UNION COUNT FORECASTS BY REGION, CHARTER TYPE, AND ASSET CATEGORY",
  "",
  paste("Prepared:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "Source:  OCE_combined_2026q1_2000tocurrent.dta (5300 Call Report panel)",
  "Sample:  2000Q1 - 2026Q1 quarterly, credit union x quarter panel",
  "Identifier: join_number (cu_number is not stable across conversions/mergers)",
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
  "  5. Reliability is horizon-dependent. The 1-year numbers are dependable for the",
  "     large middle categories; the 5-year numbers extrapolate a consolidation pace",
  "     that is a policy and rate-environment outcome, not a time series property.",
  "",
  "TAB NAMING",
  "  R<region>_T<cu_type>_<asset category>. See the Cell Index tab.")

rows <- c(xl_line(readme[1], 1, S_TITLE),
          vapply(seq_along(readme)[-1], function(i)
            xl_line(readme[i], i, S_NORM), ""))
add_sheet("README", rows, col_widths(list(c(1, 1, 105))))

## ---------------------------------------------------------------------
## [5.4] Summary
## ---------------------------------------------------------------------
sum_styles <- rep(S_NORM, ncol(summary_tbl))
names(sum_styles) <- names(summary_tbl)
sum_styles[c("actual_2026Q1", "fc_1yr", "fc_3yr", "fc_5yr",
             "chg_1yr", "chg_3yr", "chg_5yr")] <- S_INT
sum_styles[c("cv_rmse_all", "cv_rmse_h4", "cv_rmse_h12", "cv_rmse_h20",
             "aicc", "lb_pvalue")] <- S_DEC

b <- xl_block(summary_tbl, start_row = 3, col_styles = as.integer(sum_styles))
rows <- c(xl_line("Forecast summary - all 56 cells", 1, S_TITLE), b$xml)

af <- sprintf("A3:%s%d", xl_col(ncol(summary_tbl)), 3 + nrow(summary_tbl))
add_sheet("Summary", rows,
          col_widths(list(c(1, 2, 14), c(3, 5, 10), c(6, 6, 46),
                          c(7, 13, 13), c(14, 14, 40), c(15, 15, 24),
                          c(16, 21, 12))),
          freeze = list(x = 2, y = 3), autofilter = af)

## ---------------------------------------------------------------------
## [5.5] Coherence
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
## [5.6] Cell Index
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
## [5.7] The 56 cell tabs
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
              "Status", "Selected model", "Selection basis",
              "AICc (full sample)", "Ljung-Box p (lag 8)", "Actual count 2026Q1"),
    Value = c(cells$label[i], cells$region[i], cells$cu_type[i],
              CAT_PRETTY[as.character(cells$asset_cat[i])],
              d$status, d$spec, d$method,
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
  rows <- c(rows, xl_line("HISTORY 2000Q1 - 2026Q1", r, S_SUB)); r <- r + 1
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
  cat(sprintf("built tab %2d/56: %s\n", i, tab))
}

## ---------------------------------------------------------------------
## [5.8] Charts - Overview
## ---------------------------------------------------------------------
ov_files <- file.path(PLOT_DIR,
  paste0("OVERVIEW_R", rep(c(1, 2, 3, 8), each = 2), "_T", rep(c(1, 2), 4), ".png"))
ov_files <- ov_files[file.exists(ov_files)]

if (EMBED_IMAGES && length(ov_files)) {
  imgs <- lapply(seq_along(ov_files), function(k)
    list(file = ov_files[k], col = 1, row = 2 + (k - 1) * 40, w = 12, h = 7.5))
  add_sheet("Charts - Overview",
            xl_line("Overview panels: one block per region x charter type", 1, S_TITLE),
            col_widths(list(c(1, 1, 4))), images = imgs)
}

length(SH)   # expect 60 (or 61 with the overview tab)

## =====================================================================
## [5.9] Assemble the OOXML package
## =====================================================================
BUILD <- file.path(tempdir(), paste0("xlsxbuild_", format(Sys.time(), "%H%M%S")))
unlink(BUILD, recursive = TRUE)
dir.create(file.path(BUILD, "_rels"), recursive = TRUE)
dir.create(file.path(BUILD, "xl", "_rels"), recursive = TRUE)
dir.create(file.path(BUILD, "xl", "worksheets", "_rels"), recursive = TRUE)

has_img <- vapply(SH, function(s) length(s$images) > 0, logical(1))
if (any(has_img)) {
  dir.create(file.path(BUILD, "xl", "media"), recursive = TRUE)
  dir.create(file.path(BUILD, "xl", "drawings", "_rels"), recursive = TRUE)
}

writeLines(styles_xml, file.path(BUILD, "xl", "styles.xml"), useBytes = TRUE)

writeLines(paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
  '</Relationships>'), file.path(BUILD, "_rels", ".rels"), useBytes = TRUE)

## workbook.xml
sheet_tags <- vapply(seq_along(SH), function(k)
  sprintf('<sheet name="%s" sheetId="%d" r:id="rId%d"/>', xl_esc(SH[[k]]$name), k, k), "")
writeLines(paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
  'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
  '<sheets>', paste(sheet_tags, collapse = ""), '</sheets></workbook>'),
  file.path(BUILD, "xl", "workbook.xml"), useBytes = TRUE)

wb_rels <- c(vapply(seq_along(SH), function(k)
  sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet%d.xml"/>', k, k), ""),
  sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>', length(SH) + 1))
writeLines(paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  paste(wb_rels, collapse = ""), '</Relationships>'),
  file.path(BUILD, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)

## Worksheets, drawings, media
EMU <- 914400
img_seq <- 0

for (k in seq_along(SH)) {
  s <- SH[[k]]

  pane <- ""
  if (!is.null(s$freeze)) {
    tl <- paste0(xl_col(s$freeze$x + 1), s$freeze$y + 1)
    pane <- sprintf(paste0('<pane xSplit="%d" ySplit="%d" topLeftCell="%s" ',
                           'activePane="bottomRight" state="frozen"/>',
                           '<selection pane="bottomRight" activeCell="%s" sqref="%s"/>'),
                    s$freeze$x, s$freeze$y, tl, tl, tl)
  }

  drawing_tag <- ""
  if (length(s$images) > 0) {
    img_seq <- img_seq + 1
    anchors <- character(0)
    rel_lines <- character(0)
    for (m in seq_along(s$images)) {
      im <- s$images[[m]]
      media_name <- sprintf("image%d_%d.png", img_seq, m)
      file.copy(im$file, file.path(BUILD, "xl", "media", media_name), overwrite = TRUE)
      rel_lines <- c(rel_lines, sprintf(
        '<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/%s"/>',
        m, media_name))
      anchors <- c(anchors, sprintf(paste0(
        '<xdr:oneCellAnchor>',
        '<xdr:from><xdr:col>%d</xdr:col><xdr:colOff>0</xdr:colOff>',
        '<xdr:row>%d</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>',
        '<xdr:ext cx="%.0f" cy="%.0f"/>',
        '<xdr:pic><xdr:nvPicPr><xdr:cNvPr id="%d" name="Picture %d"/>',
        '<xdr:cNvPicPr><a:picLocks noChangeAspect="1"/></xdr:cNvPicPr></xdr:nvPicPr>',
        '<xdr:blipFill><a:blip xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" r:embed="rId%d"/>',
        '<a:stretch><a:fillRect/></a:stretch></xdr:blipFill>',
        '<xdr:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%.0f" cy="%.0f"/></a:xfrm>',
        '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></xdr:spPr></xdr:pic>',
        '<xdr:clientData/></xdr:oneCellAnchor>'),
        im$col, im$row, im$w * EMU, im$h * EMU, m + 1, m, m, im$w * EMU, im$h * EMU))
    }
    writeLines(paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<xdr:wsDr xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing" ',
      'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
      paste(anchors, collapse = ""), '</xdr:wsDr>'),
      file.path(BUILD, "xl", "drawings", sprintf("drawing%d.xml", img_seq)), useBytes = TRUE)
    writeLines(paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      paste(rel_lines, collapse = ""), '</Relationships>'),
      file.path(BUILD, "xl", "drawings", "_rels",
                sprintf("drawing%d.xml.rels", img_seq)), useBytes = TRUE)
    writeLines(paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      sprintf('<Relationship Id="rIdDr" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing" Target="../drawings/drawing%d.xml"/>', img_seq),
      '</Relationships>'),
      file.path(BUILD, "xl", "worksheets", "_rels",
                sprintf("sheet%d.xml.rels", k)), useBytes = TRUE)
    drawing_tag <- '<drawing r:id="rIdDr"/>'
  }

  af_tag <- if (is.null(s$autofilter)) "" else sprintf('<autoFilter ref="%s"/>', s$autofilter)

  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<sheetViews><sheetView workbookViewId="0">', pane, '</sheetView></sheetViews>',
    '<sheetFormatPr defaultRowHeight="12.75"/>',
    s$cols,
    '<sheetData>', paste(s$rows, collapse = ""), '</sheetData>',
    af_tag,
    '<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>',
    drawing_tag,
    '</worksheet>'),
    file.path(BUILD, "xl", "worksheets", sprintf("sheet%d.xml", k)), useBytes = TRUE)
}

## [Content_Types].xml
ct <- c('<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
        '<Default Extension="xml" ContentType="application/xml"/>')
if (any(has_img)) ct <- c(ct, '<Default Extension="png" ContentType="image/png"/>')
ct <- c(ct,
  '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
  '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
  vapply(seq_along(SH), function(k) sprintf(
    '<Override PartName="/xl/worksheets/sheet%d.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>', k), ""))
if (img_seq > 0) ct <- c(ct, vapply(seq_len(img_seq), function(k) sprintf(
  '<Override PartName="/xl/drawings/drawing%d.xml" ContentType="application/vnd.openxmlformats-officedocument.drawing+xml"/>', k), ""))
writeLines(paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
  paste(ct, collapse = ""), '</Types>'),
  file.path(BUILD, "[Content_Types].xml"), useBytes = TRUE)

## =====================================================================
## [5.10] Zip it. Three fallbacks, in order of preference.
## =====================================================================
if (file.exists(OUT_XLSX)) unlink(OUT_XLSX)
old_wd <- getwd()
zipped <- FALSE

## (a) the `zip` package, if it happens to be installed
if (!zipped && requireNamespace("zip", quietly = TRUE)) {
  zipped <- tryCatch({
    zip::zipr(OUT_XLSX, files = list.files(BUILD, full.names = TRUE),
              include_directories = FALSE); TRUE
  }, error = function(e) FALSE)
  cat("zip::zipr ->", zipped, "\n")
}

## (b) a zip.exe on PATH (Rtools)
if (!zipped) {
  setwd(BUILD)
  zipped <- tryCatch({
    st <- utils::zip(OUT_XLSX, files = list.files(".", recursive = TRUE, all.files = TRUE),
                     flags = "-r9Xq")
    st == 0 && file.exists(OUT_XLSX)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  setwd(old_wd)
  cat("utils::zip ->", zipped, "\n")
}

## (c) PowerShell Compress-Archive -- present on any Windows install
if (!zipped) {
  tmp_zip <- sub("\\.xlsx$", ".zip", OUT_XLSX)
  if (file.exists(tmp_zip)) unlink(tmp_zip)
  ps <- sprintf(
    "Compress-Archive -Path '%s\\*' -DestinationPath '%s' -CompressionLevel Optimal -Force",
    gsub("/", "\\\\", BUILD), gsub("/", "\\\\", tmp_zip))
  st <- system2("powershell", c("-NoProfile", "-Command", shQuote(ps)),
                stdout = TRUE, stderr = TRUE)
  if (file.exists(tmp_zip)) { file.rename(tmp_zip, OUT_XLSX); zipped <- TRUE }
  cat("PowerShell Compress-Archive ->", zipped, "\n")
}

if (!zipped) stop("No zip method available. The assembled parts are in: ", BUILD,
                  "\nZip that folder's CONTENTS (not the folder itself) and rename to .xlsx.")

cat("\nWorkbook written to:\n", OUT_XLSX, "\n",
    "Size:", round(file.size(OUT_XLSX) / 1024^2, 2), "MB\n")
