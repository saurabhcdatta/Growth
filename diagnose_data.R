############################################################
# DIAGNOSTIC SCRIPT — Run this BEFORE Part 2
# Takes < 30 seconds. Copy ALL output and share it.
############################################################
library(data.table)
library(zoo)

cat("\n========================================\n")
cat("DIAGNOSTIC — qtrly_enriched.rds\n")
cat("========================================\n\n")

qtrly <- readRDS("qtrly_enriched.rds")
setDT(qtrly)

cat("1) DIMENSIONS:\n")
cat(sprintf("   %d rows  x  %d columns\n\n", nrow(qtrly), ncol(qtrly)))

cat("2) CATEGORIES COLUMN:\n")
cat(sprintf("   class     : %s\n", paste(class(qtrly$categories), collapse="/")))
cat(sprintf("   typeof    : %s\n", typeof(qtrly$categories)))
cat("   unique values:\n")
print(sort(unique(qtrly$categories)))

cat("\n3) DATE COLUMN:\n")
cat(sprintf("   class     : %s\n", paste(class(qtrly$date), collapse="/")))
cat(sprintf("   range     : %s  to  %s\n",
            as.character(min(qtrly$date, na.rm=TRUE)),
            as.character(max(qtrly$date, na.rm=TRUE))))

cat("\n4) TARGET (yoy_ficu_count):\n")
cat(sprintf("   present   : %s\n", "yoy_ficu_count" %in% names(qtrly)))
if ("yoy_ficu_count" %in% names(qtrly)) {
  cat(sprintf("   class     : %s\n", class(qtrly$yoy_ficu_count)))
  cat(sprintf("   NAs       : %d / %d\n",
              sum(is.na(qtrly$yoy_ficu_count)), nrow(qtrly)))
  cat(sprintf("   range     : %.4f  to  %.4f\n",
              min(qtrly$yoy_ficu_count, na.rm=TRUE),
              max(qtrly$yoy_ficu_count, na.rm=TRUE)))
}

cat("\n5) ROW COUNTS AFTER categories -> as.character():\n")
qtrly[, cat_char := as.character(categories)]
for (v in sort(unique(qtrly$cat_char))) {
  n <- sum(qtrly$cat_char == v)
  cat(sprintf("   %-30s : %d rows\n", v, n))
}

cat("\n6) WHAT base::split() PRODUCES:\n")
test_split <- split(qtrly, f = qtrly$cat_char)
cat(sprintf("   names: %s\n", paste(sort(names(test_split)), collapse=" | ")))

cat("\n7) FEATURES AVAILABLE (sample yoy/qoq cols):\n")
yoy_cols <- grep("^yoy_", names(qtrly), value=TRUE)
qoq_cols <- grep("^qoq_", names(qtrly), value=TRUE)
lag_cols  <- grep("_lag[0-9]", names(qtrly), value=TRUE)
rmean_cols<- grep("_rmean", names(qtrly), value=TRUE)
cat(sprintf("   yoy_ cols : %d\n", length(yoy_cols)))
cat(sprintf("   qoq_ cols : %d\n", length(qoq_cols)))
cat(sprintf("   _lag  cols: %d\n", length(lag_cols)))
cat(sprintf("   _rmean cols:%d\n", length(rmean_cols)))

cat("\n8) QUICK FEATURE TEST (will prep_xy work?):\n")
tryCatch({
  TARGET <- "yoy_ficu_count"
  # Take first category's data
  first_cat <- sort(unique(qtrly$cat_char))[1]
  sub <- qtrly[cat_char == first_cat]
  sub <- sub[!is.na(get(TARGET))]

  num_cols <- names(sub)[vapply(sub, is.numeric, logical(1))]
  # Keep only engineered features (yoy_, qoq_, _lag, _rmean, _rsd, regime_, time_idx etc.)
  keep_pattern <- "^(yoy_|qoq_|fedfunds|gs10|gs2|mortgage30|hy_spread|
                    unrate|housing_starts|cpi_yoy|gdp_yoy|payroll_yoy|
                    deposit_yoy|loan_yoy|umich_sent|yield_curve|
                    regime_|time_idx|qtrs_from_|fedfunds_cycle|yield_curve_inv)"
  x_cols <- grep(keep_pattern, num_cols, value=TRUE, perl=TRUE)
  x_cols <- setdiff(x_cols, TARGET)

  cat(sprintf("   First cat '%s': %d rows, %d potential features\n",
              first_cat, nrow(sub), length(x_cols)))
  cat(sprintf("   Train rows (<=2020 Q4): %d\n",
              sum(as.yearqtr(sub$date) <= as.yearqtr("2020 Q4"))))
  cat(sprintf("   Test  rows (> 2020 Q4): %d\n",
              sum(as.yearqtr(sub$date) > as.yearqtr("2020 Q4"))))
  cat("   STATUS: OK - data looks usable\n")
}, error = function(e) {
  cat(sprintf("   ERROR: %s\n", e$message))
})

cat("\n========================================\n")
cat("DIAGNOSTIC COMPLETE — share all output above\n")
cat("========================================\n")
