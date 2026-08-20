## =====================================================================
## 0_xlsx_helpers.R  --  Minimal base-R .xlsx writer (no openxlsx, no Java)
##
## Sourced by 5_export_excel.R and 6_summary_tabs.R.
## Builds OOXML parts and zips them, with three zip fallbacks.
##
## Usage:
##   source("0_xlsx_helpers.R")
##   SH <- list()
##   SH[[1]] <- list(name = "Sheet1", rows = <character vector of <row>>,
##                   cols = col_widths(list(c(1,3,14))), freeze = list(x=0,y=3),
##                   autofilter = "A3:F20",
##                   images = list(list(file="x.png", col=0, row=15, w=9, h=5)))
##   xlsx_write(SH, "out.xlsx")
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

## Style slots, in the same order as cellXfs in styles_xml below
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

## One or more free-standing cells on a single row
xl_line <- function(txt, row, style = S_NORM, col = 1L) {
  cs <- vapply(seq_along(txt), function(i)
    xl_cell(col + i - 1, row, txt[i], rep_len(style, length(txt))[i]), "")
  paste0('<row r="', row, '">', paste(cs, collapse = ""), '</row>')
}

col_widths <- function(specs) {
  if (!length(specs)) return("")
  paste0("<cols>",
         paste(vapply(specs, function(s)
           sprintf('<col min="%d" max="%d" width="%.1f" customWidth="1"/>',
                   s[1], s[2], s[3]), ""), collapse = ""),
         "</cols>")
}

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
  '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>',
  '<xf numFmtId="0" fontId="1" fillId="2" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment vertical="center" wrapText="1"/></xf>',
  '<xf numFmtId="0" fontId="2" fillId="0" borderId="0" xfId="0" applyFont="1"/>',
  '<xf numFmtId="0" fontId="3" fillId="0" borderId="0" xfId="0" applyFont="1"/>',
  '<xf numFmtId="164" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',
  '<xf numFmtId="165" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',
  '<xf numFmtId="0" fontId="0" fillId="3" borderId="0" xfId="0" applyFill="1"/>',
  '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>',
  '<xf numFmtId="164" fontId="1" fillId="0" borderId="0" xfId="0" applyNumberFormat="1" applyFont="1"/>',
  '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0" applyAlignment="1"><alignment wrapText="1" vertical="top"/></xf>',
  '<xf numFmtId="164" fontId="0" fillId="3" borderId="0" xfId="0" applyNumberFormat="1" applyFill="1"/>',
'</cellXfs>',
'<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
'</styleSheet>')

## ---------------------------------------------------------------------
## Assemble the package and zip it
## ---------------------------------------------------------------------
xlsx_write <- function(SH, out_path) {

  stopifnot(length(SH) > 0)
  nms <- vapply(SH, function(s) s$name, "")
  if (any(duplicated(nms))) stop("Duplicate sheet names: ",
                                 paste(nms[duplicated(nms)], collapse = ", "))
  if (any(nchar(nms) > 31)) stop("Sheet name over 31 chars: ",
                                 paste(nms[nchar(nms) > 31], collapse = ", "))

  BUILD <- file.path(tempdir(), paste0("xlsxbuild_", format(Sys.time(), "%H%M%OS3")))
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

  sheet_tags <- vapply(seq_along(SH), function(k)
    sprintf('<sheet name="%s" sheetId="%d" r:id="rId%d"/>', xl_esc(SH[[k]]$name), k, k), "")
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<sheets>', paste(sheet_tags, collapse = ""), '</sheets></workbook>'),
    file.path(BUILD, "xl", "workbook.xml"), useBytes = TRUE)

  wb_rels <- c(vapply(seq_along(SH), function(k) sprintf(
    '<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet%d.xml"/>', k, k), ""),
    sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>', length(SH) + 1))
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    paste(wb_rels, collapse = ""), '</Relationships>'),
    file.path(BUILD, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)

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
      anchors <- character(0); rel_lines <- character(0)
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
        file.path(BUILD, "xl", "worksheets", "_rels", sprintf("sheet%d.xml.rels", k)),
        useBytes = TRUE)
      drawing_tag <- '<drawing r:id="rIdDr"/>'
    }

    af_tag <- if (is.null(s$autofilter)) "" else sprintf('<autoFilter ref="%s"/>', s$autofilter)

    writeLines(paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<sheetViews><sheetView workbookViewId="0">', pane, '</sheetView></sheetViews>',
      '<sheetFormatPr defaultRowHeight="12.75"/>',
      if (is.null(s$cols)) "" else s$cols,
      '<sheetData>', paste(s$rows, collapse = ""), '</sheetData>',
      af_tag,
      '<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>',
      drawing_tag,
      '</worksheet>'),
      file.path(BUILD, "xl", "worksheets", sprintf("sheet%d.xml", k)), useBytes = TRUE)
  }

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

  ## ---- zip, three fallbacks ----
  if (file.exists(out_path)) unlink(out_path)
  old_wd <- getwd(); zipped <- FALSE

  if (requireNamespace("zip", quietly = TRUE)) {
    zipped <- tryCatch({
      zip::zipr(out_path, files = list.files(BUILD, full.names = TRUE),
                include_directories = FALSE); file.exists(out_path)
    }, error = function(e) FALSE)
    cat("  zip::zipr ->", zipped, "\n")
  }

  if (!zipped) {
    setwd(BUILD)
    zipped <- tryCatch({
      st <- utils::zip(out_path,
                       files = list.files(".", recursive = TRUE, all.files = TRUE),
                       flags = "-r9Xq")
      st == 0 && file.exists(out_path)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    setwd(old_wd)
    cat("  utils::zip ->", zipped, "\n")
  }

  if (!zipped) {
    tmp_zip <- sub("\\.xlsx$", ".zip", out_path)
    if (file.exists(tmp_zip)) unlink(tmp_zip)
    ps <- sprintf(
      "Compress-Archive -Path '%s\\*' -DestinationPath '%s' -CompressionLevel Optimal -Force",
      gsub("/", "\\\\", BUILD), gsub("/", "\\\\", tmp_zip))
    system2("powershell", c("-NoProfile", "-Command", shQuote(ps)),
            stdout = TRUE, stderr = TRUE)
    if (file.exists(tmp_zip)) { file.rename(tmp_zip, out_path); zipped <- TRUE }
    cat("  PowerShell Compress-Archive ->", zipped, "\n")
  }

  if (!zipped) stop("No zip method available. Parts are in: ", BUILD,
                    "\nZip that folder's CONTENTS (not the folder) and rename to .xlsx.")

  cat("  wrote", out_path, "-", round(file.size(out_path) / 1024^2, 2), "MB\n")
  invisible(out_path)
}
