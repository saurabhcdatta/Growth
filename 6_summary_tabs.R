## =====================================================================
## 6_summary_tabs.R  --  Region x charter type summary workbook
##
## One tab per region x cu_type (Region1CU1 ... Region3CU2), each holding
## the 7-row asset category table from CU_Growth_Summary_Table.xlsx with a
## chart placed below it.
##
## Run after 3_fit_forecast.R. Uses the base-R writer in 0_xlsx_helpers.R.
## =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

source("0_xlsx_helpers.R")

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)
## fitr <- readRDS("cu_count_fits.rds"); list2env(fitr, .GlobalEnv)

## ---------------------------------------------------------------------
## [6.1] Settings
##
## NOTE: the template's third forecast column reads "Count 2 Year Out",
## but every other deliverable in this project reports 1 / 3 / 5 years.
## Set MID_H to 8 to follow the template literally, or 12 for a 3-year
## column consistent with the rest of the pack.
## ---------------------------------------------------------------------
MID_H   <- 8
MID_LAB <- if (MID_H == 8) "Count 2 Year Out" else "Count 3 Year Out"

CHART_DIR <- file.path(getwd(), "plots_summary")
dir.create(CHART_DIR, showWarnings = FALSE)

OUT_XLSX <- file.path(getwd(),
  paste0("CU_Growth_Summary_Table_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

## Template's own category wording, in template order
TPL_CATS <- c("Less than 10M", "10M-50M", "50M-100M", "100M-500M",
              "500M-1B", "1B-10B", "10B")
names(TPL_CATS) <- CAT_LABELS

FC_LABS <- c("Current", "1 Year Out",
             sub("Count ", "", MID_LAB), "5 Year Out")

## ---------------------------------------------------------------------
## [6.2] Pull the four counts for every cell
## ---------------------------------------------------------------------
mid_vals <- bind_rows(fc_tables[sapply(fc_tables, nrow) > 0], .id = "tab_name") %>%
  filter(horizon_q == MID_H) %>%
  select(tab_name, fc_mid = point)

tbl_all <- cells %>%
  left_join(summary_tbl %>% select(tab_name, actual_2026Q1, fc_1yr, fc_5yr),
            by = "tab_name") %>%
  left_join(mid_vals, by = "tab_name") %>%
  mutate(across(c(actual_2026Q1, fc_1yr, fc_mid, fc_5yr), ~replace_na(., 0)),
         cat_no  = match(as.character(asset_cat), CAT_LABELS),
         cat_lab = TPL_CATS[as.character(asset_cat)]) %>%
  arrange(region, cu_type, cat_no)

head(as.data.frame(tbl_all), 10)

combos <- tbl_all %>% distinct(region, cu_type) %>% arrange(region, cu_type)
nrow(combos)   # 6

## ---------------------------------------------------------------------
## [6.3] One chart per tab: grouped bars, four horizons by category
## ---------------------------------------------------------------------
COLS4 <- c("#1F3B63", "#4F81BD", "#E9A13B", "#C0392B")
names(COLS4) <- FC_LABS

theme_sum <- theme_minimal(base_size = 11, base_family = "sans") +
  theme(plot.title       = element_text(face = "bold", size = 12.5),
        plot.subtitle    = element_text(size = 9.5, colour = "grey30"),
        plot.caption     = element_text(size = 8, colour = "grey45", hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position  = "top",
        legend.title     = element_blank(),
        axis.text.x      = element_text(size = 8.5),
        plot.margin      = margin(10, 14, 8, 10))

for (j in seq_len(nrow(combos))) {

  rg <- combos$region[j]; tp <- combos$cu_type[j]
  d  <- tbl_all %>% filter(region == rg, cu_type == tp)

  dl <- d %>%
    select(cat_no, cat_lab, Current = actual_2026Q1, fc_1yr, fc_mid, fc_5yr) %>%
    rename(!!FC_LABS[2] := fc_1yr, !!FC_LABS[3] := fc_mid, !!FC_LABS[4] := fc_5yr) %>%
    pivot_longer(-c(cat_no, cat_lab), names_to = "horizon", values_to = "count") %>%
    mutate(horizon = factor(horizon, levels = FC_LABS),
           cat_lab = factor(cat_lab, levels = TPL_CATS))

  p <- ggplot(dl, aes(cat_lab, count, fill = horizon)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    geom_text(aes(label = ifelse(count > 0, comma(count), "")),
              position = position_dodge(width = 0.8),
              vjust = -0.35, size = 2.6, colour = "grey20", family = "sans") +
    scale_fill_manual(values = COLS4) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.14))) +
    labs(title = paste0("Region ", rg, ", CU Type ", tp,
                        " - credit union counts by asset category"),
         subtitle = paste0("Actual 2026Q1 and forecast counts at ",
                           paste(FC_LABS[-1], collapse = ", ")),
         x = NULL, y = "Number of credit unions",
         caption = paste0("ARIMA forecasts selected by expanding-window cross-validation. ",
                          "Asset categories are nominal dollars.\n",
                          "Source: NCUA 5300 Call Report panel (OCE combined file, 2026Q1).")) +
    theme_sum

  ggsave(file.path(CHART_DIR, sprintf("SUMMARY_R%d_CU%d.png", rg, tp)), p,
         width = 9, height = 4.6, dpi = 300, bg = "white")
  cat(sprintf("chart %d/%d: Region%dCU%d\n", j, nrow(combos), rg, tp))
}

## Check one before building the workbook
p

## ---------------------------------------------------------------------
## [6.4] Build the tabs, matching the template's cell layout
##   B1 = region, C1 = CU type
##   row 3 = header, rows 4-10 = the seven categories
##   chart anchored at row 16 (0-based row 15), column A
## ---------------------------------------------------------------------
SH <- list()

for (j in seq_len(nrow(combos))) {

  rg <- combos$region[j]; tp <- combos$cu_type[j]
  d  <- tbl_all %>% filter(region == rg, cu_type == tp) %>% arrange(cat_no)

  body <- data.frame(
    `Cat No`            = d$cat_no,
    `Asset Categories`  = unname(d$cat_lab),
    `Current Count`     = d$actual_2026Q1,
    `Count 1 Year Out`  = d$fc_1yr,
    MID                 = d$fc_mid,
    `Count 5 Year Out`  = d$fc_5yr,
    check.names = FALSE, stringsAsFactors = FALSE)
  names(body)[5] <- MID_LAB

  rows <- c(
    xl_line(c(paste("Region", rg), paste("CU Type", tp)), row = 1,
            style = c(S_TITLE, S_TITLE), col = 2),
    xl_block(body, start_row = 3,
             col_styles = c(S_INT, S_NORM, S_INT, S_INT, S_INT, S_INT))$xml,
    ## total row, directly under the seven categories
    xl_line(c("", "Total", sum(d$actual_2026Q1), sum(d$fc_1yr),
              sum(d$fc_mid), sum(d$fc_5yr)), row = 11,
            style = c(S_NORM, S_BOLD, rep(S_INTBOLD, 4)), col = 1),
    xl_line(paste0("Sample 2005Q1-2026Q1. ARIMA models selected by expanding-window ",
                   "cross-validation; counts floored at zero and rounded."),
            row = 13, style = S_NORM),
    xl_line(paste0("Cells where a constant CV winner was overridden in favour of a ",
                   "non-flat model are listed on the Summary tab of the main workbook."),
            row = 14, style = S_NORM))

  png_j <- file.path(CHART_DIR, sprintf("SUMMARY_R%d_CU%d.png", rg, tp))

  SH[[j]] <- list(
    name = sprintf("Region%dCU%d", rg, tp),
    rows = rows,
    cols = col_widths(list(c(1, 1, 8), c(2, 2, 18), c(3, 6, 18))),
    freeze = NULL, autofilter = NULL,
    images = if (file.exists(png_j))
      list(list(file = png_j, col = 0, row = 15, w = 9, h = 4.6)) else list())
}

vapply(SH, function(s) s$name, "")

## ---------------------------------------------------------------------
## [6.5] Write it
## ---------------------------------------------------------------------
xlsx_write(SH, OUT_XLSX)
