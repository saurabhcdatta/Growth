## =====================================================================
## 4_plots.R  --  Forecast charts for all 56 cells
## Produces: one PNG per cell, a multi-page PDF, and 8 overview panels.
## Run BEFORE 5_export_excel.R -- that script embeds these PNGs into the workbook.
## =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

## prep <- readRDS("cu_count_prep.rds"); list2env(prep, .GlobalEnv)
## cvr  <- readRDS("cu_count_cv.rds");   list2env(cvr,  .GlobalEnv)
## fitr <- readRDS("cu_count_fits.rds"); list2env(fitr, .GlobalEnv)

PLOT_DIR <- file.path(getwd(), "plots")
dir.create(PLOT_DIR, showWarnings = FALSE)

COL_HIST <- "#1F3B63"   # navy
COL_FC   <- "#C0392B"   # brick
COL_80   <- "#E8A29A"
COL_95   <- "#F5D7D2"

theme_cu <- theme_minimal(base_size = 11, base_family = "sans") +
  theme(plot.title       = element_text(face = "bold", size = 13, margin = margin(b = 2)),
        plot.subtitle    = element_text(size = 9.5, colour = "grey30", margin = margin(b = 8)),
        plot.caption     = element_text(size = 8, colour = "grey45", hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey92"),
        panel.grid.major.y = element_line(colour = "grey90"),
        axis.title       = element_text(size = 9.5, colour = "grey25"),
        plot.margin      = margin(12, 16, 10, 12))

## ---------------------------------------------------------------------
## [4.1] Long data frames: history and forecast, on a real date axis
## ---------------------------------------------------------------------
hist_all <- counts %>%
  mutate(date = as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01")),
         tab_name = paste0("R", region, "_T", cu_type, "_", asset_cat)) %>%
  select(tab_name, region, cu_type, asset_cat, date, q_label, value = n_cu)

fc_all <- bind_rows(fc_tables[sapply(fc_tables, nrow) > 0], .id = "tab_name") %>%
  mutate(date = as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01")))

FC_CUT <- max(hist_all$date)   # 2026Q1

## ---------------------------------------------------------------------
## [4.2] One chart per cell
## ---------------------------------------------------------------------
plot_list <- vector("list", nrow(cells))
names(plot_list) <- cells$tab_name

for (i in seq_len(nrow(cells))) {

  tab <- cells$tab_name[i]
  h   <- hist_all %>% filter(tab_name == tab) %>% arrange(date)
  f   <- fc_all   %>% filter(tab_name == tab) %>% arrange(date)
  d   <- diagnostics %>% filter(tab_name == tab)

  ## Bridge the gap so history and forecast lines connect
  if (nrow(f) > 0) {
    bridge <- tibble::tibble(date = tail(h$date, 1), point = tail(h$value, 1),
                             lo80 = tail(h$value, 1), hi80 = tail(h$value, 1),
                             lo95 = tail(h$value, 1), hi95 = tail(h$value, 1))
    f_path <- bind_rows(bridge, f %>% select(date, point, lo80, hi80, lo95, hi95))
    keypts <- f %>% filter(horizon_q %in% c(4, 12, 20)) %>%
      mutate(lab = paste0(c("1 yr", "3 yr", "5 yr")[match(horizon_q, c(4, 12, 20))],
                          " (", q_label, ")\n", comma(point)))
  }

  y_top <- max(c(h$value, if (nrow(f)) f$hi95 else 0), na.rm = TRUE)

  p <- ggplot()

  if (nrow(f) > 0) {
    p <- p +
      geom_ribbon(data = f_path, aes(date, ymin = lo95, ymax = hi95),
                  fill = COL_95, alpha = 0.85) +
      geom_ribbon(data = f_path, aes(date, ymin = lo80, ymax = hi80),
                  fill = COL_80, alpha = 0.85) +
      geom_vline(xintercept = FC_CUT, linetype = "dotted", colour = "grey45")
  }

  p <- p + geom_line(data = h, aes(date, value), colour = COL_HIST, linewidth = 0.7)

  if (nrow(f) > 0) {
    p <- p +
      geom_line(data = f_path, aes(date, point), colour = COL_FC,
                linewidth = 0.8, linetype = "22") +
      geom_segment(data = keypts, aes(x = date, xend = date, y = 0, yend = point),
                   colour = "grey60", linetype = "dotted", linewidth = 0.3) +
      geom_point(data = keypts, aes(date, point), colour = COL_FC,
                 size = 2.4, shape = 21, fill = "white", stroke = 1.1) +
      geom_label(data = keypts, aes(date, point, label = lab),
                 vjust = -0.35, hjust = 0.5, size = 3.05, lineheight = 0.95,
                 label.size = 0.25, label.padding = unit(0.16, "lines"),
                 colour = COL_FC, fill = "white", family = "sans",
                 fontface = "bold")
  }

  p <- p +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.06))) +
    scale_y_continuous(labels = comma,
                       limits = c(0, y_top * 1.28),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = cells$label[i],
         subtitle = paste0(d$spec, "  |  ",
                           ifelse(d$status == "MODEL",
                                  "selected by expanding-window cross-validation",
                                  d$method)),
         x = NULL, y = "Number of credit unions",
         caption = paste0("Solid navy: actual counts, 2000Q1-2026Q1. Dashed red: forecast. ",
                          "Shaded bands: 80% and 95% prediction intervals.\n",
                          "Source: NCUA 5300 Call Report panel (OCE combined file, 2026Q1). ",
                          "Asset categories are nominal dollars.")) +
    theme_cu

  if (nrow(f) == 0) {
    p <- p + annotate("text", x = mean(range(h$date)), y = 0.5,
                      label = "Cell is structurally empty - no forecast",
                      colour = "grey40", size = 4, family = "sans")
  }

  plot_list[[tab]] <- p
  ggsave(file.path(PLOT_DIR, paste0(tab, ".png")), p,
         width = 9, height = 5, dpi = 300, bg = "white")
  cat(sprintf("plotted %2d/56: %s\n", i, tab))
}

## Look at one before committing to all of them
plot_list[["R1_T1_A4_100to500M"]]

## ---------------------------------------------------------------------
## [4.3] Multi-page PDF, all 56 in order
## ---------------------------------------------------------------------
pdf(file.path(PLOT_DIR, "CU_Count_Forecasts_AllCells.pdf"),
    width = 9, height = 5, onefile = TRUE)
for (i in seq_len(nrow(cells))) print(plot_list[[cells$tab_name[i]]])
dev.off()

## ---------------------------------------------------------------------
## [4.4] Overview panels: one page per region x charter type,
##       seven asset categories faceted with free y scales
## ---------------------------------------------------------------------
panel_df <- bind_rows(
  hist_all %>% mutate(series = "Actual") %>% select(tab_name, date, value, series),
  fc_all   %>% mutate(series = "Forecast") %>% select(tab_name, date, value = point, series)) %>%
  left_join(cells %>% select(tab_name, region, cu_type, asset_cat, cell_id), by = "tab_name") %>%
  mutate(cat_pretty = factor(CAT_PRETTY[as.character(asset_cat)], levels = CAT_PRETTY))

key_df <- fc_all %>% filter(horizon_q %in% c(4, 12, 20)) %>%
  left_join(cells %>% select(tab_name, region, cu_type, asset_cat), by = "tab_name") %>%
  mutate(cat_pretty = factor(CAT_PRETTY[as.character(asset_cat)], levels = CAT_PRETTY))

overview_list <- list()
combos <- cells %>% distinct(region, cu_type) %>% arrange(region, cu_type)

for (j in seq_len(nrow(combos))) {
  rg <- combos$region[j]; tp <- combos$cu_type[j]
  pd <- panel_df %>% filter(region == rg, cu_type == tp)
  kd <- key_df   %>% filter(region == rg, cu_type == tp)

  po <- ggplot(pd, aes(date, value, colour = series, linetype = series)) +
    geom_vline(xintercept = FC_CUT, linetype = "dotted", colour = "grey60") +
    geom_line(linewidth = 0.6) +
    geom_point(data = kd, aes(date, point), inherit.aes = FALSE,
               colour = COL_FC, size = 1.6) +
    geom_text(data = kd, aes(date, point, label = comma(point)), inherit.aes = FALSE,
              vjust = -0.9, size = 2.7, colour = COL_FC, fontface = "bold",
              family = "sans") +
    facet_wrap(~ cat_pretty, scales = "free_y", ncol = 3) +
    scale_colour_manual(values = c(Actual = COL_HIST, Forecast = COL_FC), name = NULL) +
    scale_linetype_manual(values = c(Actual = "solid", Forecast = "22"), name = NULL) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.20))) +
    labs(title = paste0("Region ", rg, ", charter type ", tp,
                        " - credit union counts by asset category"),
         subtitle = "Actual 2000Q1-2026Q1 with forecasts to 2031Q1; labels mark the 1-, 3-, and 5-year points",
         x = NULL, y = "Number of credit unions") +
    theme_cu +
    theme(legend.position = "top",
          strip.text = element_text(face = "bold", size = 9.5))

  overview_list[[paste0("R", rg, "_T", tp)]] <- po
  ggsave(file.path(PLOT_DIR, paste0("OVERVIEW_R", rg, "_T", tp, ".png")), po,
         width = 12, height = 7.5, dpi = 300, bg = "white")
}

pdf(file.path(PLOT_DIR, "CU_Count_Forecasts_Overview.pdf"), width = 12, height = 7.5)
for (p in overview_list) print(p)
dev.off()

cat("\nPlots written to:\n", PLOT_DIR,
    "\nNow run 5_export_excel.R to build the workbook and embed them.\n")
