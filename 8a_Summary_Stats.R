############################################################
# PART 8a — SIMPLE ASSET SUMMARY BY CATEGORY & HORIZON
############################################################

library(data.table)

DATA_DIR <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
setwd(DATA_DIR)
options(scipen = 999)

ASSET_LABELS <- c("1_Less_10M","2_10M_50M","3_50M_100M",
                  "4_100M_500M","5_500M_1B","6_1B_10B","7_10B_Plus")

# Load Part 8 results if not in memory
if (!exists("fc_fcu") || !exists("fc_fiscu")) {
  fc_fcu   <- fread("results_8_disagg/fcu_forecasts.csv")
  fc_fiscu <- fread("results_8_disagg/fiscu_forecasts.csv")
  nm_map <- c("Current Assets ($)"="assets_now",
              "Asset Category (Now)"="bucket_now",
              "Projected Assets 1Yr"="assets_1yr", "Category 1Yr"="bucket_1yr",
              "Projected Assets 3Yr"="assets_3yr", "Category 3Yr"="bucket_3yr",
              "Projected Assets 5Yr"="assets_5yr", "Category 5Yr"="bucket_5yr")
  for (old_nm in names(nm_map)) {
    if (old_nm %in% names(fc_fcu))   setnames(fc_fcu, old_nm, nm_map[old_nm])
    if (old_nm %in% names(fc_fiscu)) setnames(fc_fiscu, old_nm, nm_map[old_nm])
  }
}

# ── Helper: summarise total assets by category ───────────
summarise_assets <- function(dt, bucket_col, asset_col) {
  out <- dt[, .(total_assets = sum(get(asset_col), na.rm = TRUE),
                n_cus = .N),
            by = .(category = get(bucket_col))]
  all_cats <- data.table(category = ASSET_LABELS)
  out <- merge(all_cats, out, by = "category", all.x = TRUE)
  out[is.na(total_assets), total_assets := 0]
  out[is.na(n_cus), n_cus := 0L]
  setorderv(out, "category")
  out
}

fmt <- function(x) {
  ifelse(x >= 1e9, sprintf("$%.2fB", x / 1e9),
  ifelse(x >= 1e6, sprintf("$%.1fM", x / 1e6),
         sprintf("$%.0f", x)))
}

# ── Print summary ────────────────────────────────────────
print_summary <- function(dt, label) {
  horizons <- list(
    list(bucket = "bucket_now", asset = "assets_now", label = "Current"),
    list(bucket = "bucket_1yr", asset = "assets_1yr", label = "1 Year Out"),
    list(bucket = "bucket_3yr", asset = "assets_3yr", label = "3 Years Out"),
    list(bucket = "bucket_5yr", asset = "assets_5yr", label = "5 Years Out")
  )

  message(sprintf("\n========== %s ==========", label))

  for (hz in horizons) {
    s <- summarise_assets(dt, hz$bucket, hz$asset)
    message(sprintf("\n  --- %s (%s) ---", hz$label, label))
    for (i in 1:nrow(s)) {
      message(sprintf("    %-15s  %s  (%d CUs)", s$category[i], fmt(s$total_assets[i]), s$n_cus[i]))
    }
    message(sprintf("    %-15s  %s  (%d CUs)", "TOTAL", fmt(sum(s$total_assets)), sum(s$n_cus)))
  }
}

# ── Run ──────────────────────────────────────────────────
print_summary(fc_fcu,   "FCU")
print_summary(fc_fiscu, "FISCU")

fc_combined <- rbindlist(list(fc_fcu, fc_fiscu), fill = TRUE)
print_summary(fc_combined, "FCU + FISCU COMBINED")

# ── Save to CSV ──────────────────────────────────────────
dir.create("results_8a_summary", showWarnings = FALSE)

save_table <- function(dt) {
  horizons <- list(
    list(bucket = "bucket_now", asset = "assets_now", label = "Current"),
    list(bucket = "bucket_1yr", asset = "assets_1yr", label = "1Yr"),
    list(bucket = "bucket_3yr", asset = "assets_3yr", label = "3Yr"),
    list(bucket = "bucket_5yr", asset = "assets_5yr", label = "5Yr")
  )
  all_hz <- list()
  for (hz in horizons) {
    s <- summarise_assets(dt, hz$bucket, hz$asset)
    setnames(s, c("total_assets","n_cus"),
             c(paste0("assets_",hz$label), paste0("n_cus_",hz$label)))
    all_hz[[hz$label]] <- s
  }
  Reduce(function(a, b) merge(a, b, by = "category", all = TRUE), all_hz)
}

fwrite(save_table(fc_fcu),      "results_8a_summary/fcu_assets_by_category.csv")
fwrite(save_table(fc_fiscu),    "results_8a_summary/fiscu_assets_by_category.csv")
fwrite(save_table(fc_combined), "results_8a_summary/combined_assets_by_category.csv")

message("\nCSVs saved to results_8a_summary/")
