## =====================================================================
## 1_prep.R  --  Credit Union Growth Forecast
## Build quarterly counts of credit unions by region x cu_type x asset category
## Run block by block in RStudio.  Objects are left in the environment.
## =====================================================================

library(haven)
library(dplyr)
library(tidyr)
library(forecast)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

DTA <- "S:/Data/OCE Data/oce do file archive/2026/2026q1/processing datasets/oce current/OCE_combined_2026q1_2000tocurrent.dta"

## ---------------------------------------------------------------------
## [1.1] Load only the columns we need (the full .dta is ~873 vars)
## ---------------------------------------------------------------------
## join_number is the reliable credit union identifier in this panel; cu_number
## is not stable enough to key on. Everything downstream uses join_number.
oce <- read_dta(DTA, col_select = c(join_number, year, quarter, cu_type,
                                    region, assets_tot, assets_cat))

dim(oce)
head(as.data.frame(oce))

## Strip Stata value labels so filters behave predictably
raw <- oce %>%
  mutate(join_number = as.numeric(zap_labels(join_number)),
         year       = as.numeric(zap_labels(year)),
         quarter    = as.numeric(zap_labels(quarter)),
         cu_type    = as.numeric(zap_labels(cu_type)),
         region     = as.numeric(zap_labels(region)),
         assets_tot = as.numeric(zap_labels(assets_tot)))

table(raw$region,  useNA = "ifany")
table(raw$cu_type, useNA = "ifany")
range(raw$year, na.rm = TRUE)

## ---------------------------------------------------------------------
## [1.2] *** CHECK THE UNITS OF assets_tot BEFORE GOING FURTHER ***
## ---------------------------------------------------------------------
print(quantile(raw$assets_tot, c(0, .25, .5, .75, .99, 1), na.rm = TRUE))

## ASSET_SCALE = number of DOLLARS per one unit of assets_tot.
##   assets_tot in dollars   -> 1
##   assets_tot in thousands -> 1e3
##   assets_tot in millions  -> 1e6
## The median CU is roughly $30-40M, so eyeball the median above and set this.
ASSET_SCALE <- 1

## ---------------------------------------------------------------------
## [1.3] Asset categories (NOMINAL dollars, per spec)
## Convention: left-closed, right-open.  "<10M" means assets_tot < $10,000,000.
## ---------------------------------------------------------------------
BREAKS_USD <- c(-Inf, 10e6, 50e6, 100e6, 500e6, 1e9, 10e9, Inf)
BREAKS     <- BREAKS_USD / ASSET_SCALE

CAT_LABELS <- c("A1_LT10M", "A2_10to50M", "A3_50to100M", "A4_100to500M",
                "A5_500Mto1B", "A6_1Bto10B", "A7_GE10B")
CAT_PRETTY <- c("Under $10M", "$10M - $50M", "$50M - $100M", "$100M - $500M",
                "$500M - $1B", "$1B - $10B", "$10B and over")
names(CAT_PRETTY) <- CAT_LABELS

## ---------------------------------------------------------------------
## [1.4] Filter to the analysis universe and assign categories
## ---------------------------------------------------------------------
panel <- raw %>%
  filter(cu_type %in% c(1, 2),
         region  %in% c(1, 2, 3, 8),
         !is.na(assets_tot),
         !is.na(join_number),
         year >= 2000, year <= 2026,
         !(year == 2026 & quarter > 1)) %>%
  mutate(q_index   = (year - 2000) * 4 + quarter,
         asset_cat = cut(assets_tot, breaks = BREAKS, labels = CAT_LABELS,
                         right = FALSE))

sum(is.na(panel$asset_cat))          # should be 0
nrow(panel)

## Guard against duplicate join_number-quarter rows (amended filings etc.)
dupes <- panel %>% count(join_number, q_index) %>% filter(n > 1)
nrow(dupes)
panel <- panel %>% arrange(join_number, q_index) %>%
  distinct(join_number, q_index, .keep_all = TRUE)

## Optional sanity check against the pre-built assets_cat in the .dta
## table(panel$asset_cat, zap_labels(panel$assets_cat))

## ---------------------------------------------------------------------
## [1.5] Counts per cell per quarter, with structural zeros filled in
## ---------------------------------------------------------------------
Q_MIN <- 1
Q_MAX <- max(panel$q_index)          # 2026q1 -> 105
N_Q   <- Q_MAX - Q_MIN + 1

counts_obs <- panel %>%
  count(q_index, region, cu_type, asset_cat, name = "n_cu")

grid <- expand_grid(q_index   = Q_MIN:Q_MAX,
                    region    = c(1, 2, 3, 8),
                    cu_type   = c(1, 2),
                    asset_cat = factor(CAT_LABELS, levels = CAT_LABELS))

counts <- grid %>%
  left_join(counts_obs, by = c("q_index", "region", "cu_type", "asset_cat")) %>%
  replace_na(list(n_cu = 0)) %>%
  mutate(year    = 2000 + (q_index - 1) %/% 4,
         quarter = (q_index - 1) %% 4 + 1,
         q_label = paste0(year, "Q", quarter)) %>%
  arrange(region, cu_type, asset_cat, q_index)

## Quick look: national totals by category, first and last quarter
counts %>% filter(q_index %in% c(1, Q_MAX)) %>%
  group_by(q_label, asset_cat) %>% summarise(n = sum(n_cu), .groups = "drop") %>%
  pivot_wider(names_from = q_label, values_from = n)

## ---------------------------------------------------------------------
## [1.6] The 56 cells, their tab names, and one ts object each
## ---------------------------------------------------------------------
cells <- counts %>%
  distinct(region, cu_type, asset_cat) %>%
  arrange(region, cu_type, asset_cat) %>%
  mutate(cell_id  = row_number(),
         tab_name = paste0("R", region, "_T", cu_type, "_", asset_cat),
         label    = paste0("Region ", region, " | cu_type ", cu_type, " | ",
                           CAT_PRETTY[as.character(asset_cat)]))

nrow(cells)                          # 56
substr(cells$tab_name, 1, 31)        # all <= 31 chars, Excel's limit

ts_list <- vector("list", nrow(cells))
names(ts_list) <- cells$tab_name

for (i in seq_len(nrow(cells))) {
  y_i <- counts %>%
    filter(region    == cells$region[i],
           cu_type   == cells$cu_type[i],
           asset_cat == cells$asset_cat[i]) %>%
    arrange(q_index) %>% pull(n_cu)
  ts_list[[i]] <- ts(y_i, start = c(2000, 1), frequency = 4)
}

## Total across all 56 cells -- used later for the coherence check
ts_total <- ts(counts %>% group_by(q_index) %>% summarise(n = sum(n_cu)) %>%
                 arrange(q_index) %>% pull(n),
               start = c(2000, 1), frequency = 4)

## ---------------------------------------------------------------------
## [1.7] Cell diagnostics -- which of the 56 are actually modelable
## Region 8 is ONES, not a geography, so its small-asset cells are
## structurally empty; several region 1/2/3 x $10B+ cells are near-empty too.
## ---------------------------------------------------------------------
MIN_TRAIN <- 40                      # quarters required before the first CV origin

cell_diag <- cells %>%
  mutate(n_obs        = N_Q,
         n_nonzero    = sapply(ts_list, function(y) sum(y > 0)),
         mean_count   = round(sapply(ts_list, mean), 2),
         max_count    = sapply(ts_list, max),
         first_count  = sapply(ts_list, function(y) as.numeric(y)[1]),
         last_count   = sapply(ts_list, function(y) as.numeric(y)[N_Q]),
         n_distinct   = sapply(ts_list, function(y) length(unique(as.numeric(y)))),
         status       = case_when(
           n_nonzero == 0                     ~ "EMPTY - no model",
           last_count == 0 & max_count <= 2    ~ "EMPTY - no model",
           max_count < 5 | n_distinct < 4      ~ "SPARSE - naive fallback",
           n_nonzero < MIN_TRAIN               ~ "SPARSE - naive fallback",
           TRUE                                ~ "MODEL"))

print(as.data.frame(cell_diag[, c("tab_name", "n_nonzero", "mean_count",
                                  "first_count", "last_count", "status")]),
      row.names = FALSE)

table(cell_diag$status)

## Carry forward
saveRDS(list(counts = counts, cells = cells, ts_list = ts_list,
             ts_total = ts_total, cell_diag = cell_diag,
             CAT_LABELS = CAT_LABELS, CAT_PRETTY = CAT_PRETTY,
             N_Q = N_Q, ASSET_SCALE = ASSET_SCALE),
        file = "cu_count_prep.rds")
