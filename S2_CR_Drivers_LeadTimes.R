############################################################
# SLIDE GRAPHIC 2 — Top Call Report Drivers with Lead Times
#
# Mirror of S1 but for call report variables. Computes lead
# time per call report variable as the lag (0..MAX_LAG quarters)
# at which average correlation with future CU growth is highest.
#
# Inputs : results_5_callreport/cr_ensemble_ranking.csv
#          qtrly_enriched_v3.rds  (panel with all CR vars + targets)
#
# Output : plots_slides/S2_cr_drivers_leadtimes.pdf/png
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

DATA_DIR <- "S:/Projects/Credit_Union_Growth_Forecast/Data"
PLOT_DIR <- "plots_slides"
setwd(DATA_DIR)
dir.create(PLOT_DIR, showWarnings = FALSE)

MAX_LAG <- 8L  # quarters to test for lead time

# ════════════════════════════════════════════════════════════
# 1. LOAD INPUTS
# ════════════════════════════════════════════════════════════
imp_path   <- "results_5_callreport/cr_ensemble_ranking.csv"
panel_path <- "qtrly_enriched_v3.rds"

if (!file.exists(imp_path))
  stop(sprintf("Run Part 5 (PATCHED) first — missing %s", imp_path))
if (!file.exists(panel_path))
  stop(sprintf("Missing panel file %s — run Part 1 first", panel_path))

imp   <- fread(imp_path)
panel <- as.data.table(readRDS(panel_path))

message(sprintf("Loaded ranking : %d rows | columns: %s",
                nrow(imp), paste(names(imp), collapse=", ")))
message(sprintf("Loaded panel   : %d rows | %d columns",
                nrow(panel), ncol(panel)))

# ════════════════════════════════════════════════════════════
# 2. COMPUTE LEAD TIME — three competing methods, side-by-side
# ════════════════════════════════════════════════════════════
# Method A: lag at MAXIMUM correlation (current approach — usually picks 0)
# Method B: lag at maximum correlation, but FORCE lag >= 1
#           (asks: among LEADING relationships, which lag is strongest?)
# Method C: PERSISTENCE LAG — longest lag at which |corr| is still
#           >= 0.8 * max(|corr|). Asks: how far ahead does this signal
#           remain materially predictive?

target_vars <- c("yoy_fcu_pct", "yoy_fiscu_pct",
                 "yoy_fcu_assets_pct", "yoy_fiscu_assets_pct")

# Aggregate panel to system-level means for correlation analysis
sys_means <- panel[, lapply(.SD, function(x) {
                if (is.numeric(x)) mean(x, na.rm = TRUE) else x[1]
              }),
              by = date,
              .SDcols = setdiff(names(panel),
                                c("date", "categories", "cat_label",
                                  "q1", "q2", "q3", "q4", "qtr"))]
setorder(sys_means, date)

imp_vars <- intersect(imp$variable, names(sys_means))
message(sprintf("Computing lead times (3 methods) for %d call report variables...",
                length(imp_vars)))

# Compute the full lag profile for one variable (returns lag→avg|corr|)
compute_lag_profile <- function(cr_var) {
  x <- as.numeric(sys_means[[cr_var]])
  out <- numeric(MAX_LAG + 1L)
  names(out) <- as.character(0:MAX_LAG)
  for (lg in 0:MAX_LAG) {
    rs <- numeric(0)
    for (tv in target_vars) {
      if (!tv %in% names(sys_means)) next
      y <- as.numeric(sys_means[[tv]])
      n <- length(x)
      if (n - lg < 15L) next
      x_now <- x[1:(n - lg)]
      y_fut <- y[(1 + lg):n]
      ok <- !is.na(x_now) & !is.na(y_fut)
      if (sum(ok) < 15L) next
      r <- suppressWarnings(cor(x_now[ok], y_fut[ok]))
      if (!is.na(r)) rs <- c(rs, abs(r))
    }
    out[as.character(lg)] <- if (length(rs) > 0) mean(rs) else NA_real_
  }
  out
}

# Method A: argmax (allow any lag including 0)
method_A_lag <- function(p) {
  if (all(is.na(p))) return(NA_integer_)
  as.integer(names(which.max(p)))
}

# Method B: argmax constrained to lag >= 1
method_B_lag <- function(p) {
  if (length(p) <= 1 || all(is.na(p[-1]))) return(NA_integer_)
  p2 <- p[-1]  # drop lag 0
  as.integer(names(which.max(p2)))
}

# Method C: longest lag where |corr| >= 0.8 * max(|corr|)
method_C_lag <- function(p) {
  if (all(is.na(p))) return(NA_integer_)
  threshold <- 0.8 * max(p, na.rm = TRUE)
  qualified <- which(p >= threshold)
  if (length(qualified) == 0) return(NA_integer_)
  as.integer(names(p)[max(qualified)])
}

# Compute all three for every variable
diag <- rbindlist(lapply(imp_vars, function(v) {
  prof <- compute_lag_profile(v)
  data.table(
    variable      = v,
    lag_A_argmax  = method_A_lag(prof),
    lag_B_lead    = method_B_lag(prof),
    lag_C_persist = method_C_lag(prof),
    corr_at_0     = round(prof["0"], 3),
    corr_max      = round(max(prof, na.rm = TRUE), 3),
    corr_at_lag_B = if (!is.na(method_B_lag(prof)))
                       round(prof[as.character(method_B_lag(prof))], 3)
                    else NA_real_,
    corr_at_lag_C = if (!is.na(method_C_lag(prof)))
                       round(prof[as.character(method_C_lag(prof))], 3)
                    else NA_real_
  )
}))

# Merge importance rank for sorting
diag <- merge(diag, imp[, .(variable, mean_importance)], by = "variable")
setorderv(diag, "mean_importance", order = -1L)

# ── Print three-method comparison for top 10 ──────────────
top10_diag <- head(diag, 10)

message("\n", strrep("═", 110))
message("LEAD TIME COMPARISON — Top 10 Call Report Drivers")
message(strrep("═", 110))
message(sprintf("%-32s | %5s | %5s | %5s | %6s | %6s | %6s | %6s",
                "Variable", "A", "B", "C", "|r|@0", "|r|max", "|r|@B", "|r|@C"))
message(strrep("─", 110))
for (i in 1:nrow(top10_diag)) {
  message(sprintf("%-32s | %5s | %5s | %5s | %6.3f | %6.3f | %6s | %6s",
                  substr(top10_diag$variable[i], 1, 32),
                  ifelse(is.na(top10_diag$lag_A_argmax[i]),  "NA", paste0("+", top10_diag$lag_A_argmax[i], "Q")),
                  ifelse(is.na(top10_diag$lag_B_lead[i]),    "NA", paste0("+", top10_diag$lag_B_lead[i],   "Q")),
                  ifelse(is.na(top10_diag$lag_C_persist[i]), "NA", paste0("+", top10_diag$lag_C_persist[i],"Q")),
                  top10_diag$corr_at_0[i],
                  top10_diag$corr_max[i],
                  ifelse(is.na(top10_diag$corr_at_lag_B[i]), "  NA",
                         sprintf("%.3f", top10_diag$corr_at_lag_B[i])),
                  ifelse(is.na(top10_diag$corr_at_lag_C[i]), "  NA",
                         sprintf("%.3f", top10_diag$corr_at_lag_C[i]))))
}
message(strrep("═", 110))
message("Method A: argmax over lag 0..", MAX_LAG, "  (current approach)")
message("Method B: argmax over lag 1..", MAX_LAG, "  (forces a leading signal)")
message("Method C: longest lag where |r| >= 0.8 * max  (signal persistence)")
message(strrep("═", 110))

# Use Method B (forced lag >= 1) for the chart — call report variables
# are mostly coincident with growth at lag 0, so the leading lag is
# the economically interesting answer. Change to lag_A_argmax or
# lag_C_persist to switch methods.
leads <- diag[, .(variable,
                   best_lead_q   = lag_B_lead,
                   best_abs_corr = corr_at_lag_B)]

# Merge importance with lead time
imp <- merge(imp, leads, by = "variable", all.x = TRUE)
imp[is.na(best_lead_q), best_lead_q := 0L]

# ════════════════════════════════════════════════════════════
# 3. LABEL & THEME — NCUA CALL REPORT DICTIONARY
# ════════════════════════════════════════════════════════════
# Maps known NCUA 5300 Call Report base names + acct codes to
# human-readable descriptions and themes. This is the CR
# counterpart to S1's FRB Variable Data Dictionary lookup.

# (a) Specific NCUA Call Report fields & ratios
CR_DICT <- list(
  # ── Membership & Acquisition ──
  members              = list(desc = "Total Members",                          theme = "Membership"),
  member_count         = list(desc = "Total Members",                          theme = "Membership"),
  potential_members    = list(desc = "Potential Members (FOM)",                theme = "Membership"),
  acquisition_rate     = list(desc = "Member Acquisition Rate (Level)",       theme = "Membership"),
  fom_size             = list(desc = "Field-of-Membership Size",               theme = "Membership"),

  # ── Asset Levels & Composition (non-endogenous metrics) ──
  assets_mil           = list(desc = "Total Assets ($M)",                      theme = "Asset Composition"),
  assets_pct           = list(desc = "Asset Mix Composition (%)",              theme = "Asset Composition"),
  cash_pct             = list(desc = "Cash & Equivalents (% of Assets)",       theme = "Asset Composition"),
  investments_pct      = list(desc = "Investments (% of Assets)",              theme = "Asset Composition"),
  loans_pct            = list(desc = "Loans (% of Assets)",                    theme = "Asset Composition"),
  fixed_assets_pct     = list(desc = "Fixed Assets (% of Assets)",             theme = "Asset Composition"),
  loan_to_share        = list(desc = "Loan-to-Share Ratio",                    theme = "Asset Composition"),
  loan_to_asset        = list(desc = "Loan-to-Asset Ratio",                    theme = "Asset Composition"),
  investment_to_asset  = list(desc = "Investment-to-Asset Ratio",              theme = "Asset Composition"),

  # ── Capital & Solvency ──
  net_worth            = list(desc = "Net Worth",                              theme = "Capital & Solvency"),
  net_worth_ratio      = list(desc = "Net Worth Ratio",                        theme = "Capital & Solvency"),
  capital_ratio        = list(desc = "Capital Ratio",                          theme = "Capital & Solvency"),
  cap_adeq             = list(desc = "Capital Adequacy",                       theme = "Capital & Solvency"),
  leverage_ratio       = list(desc = "Leverage Ratio",                         theme = "Capital & Solvency"),
  risk_based_cap       = list(desc = "Risk-Based Capital Ratio",               theme = "Capital & Solvency"),

  # ── Funding (Deposits / Shares) ──
  dep_tot              = list(desc = "Total Deposits & Shares",                theme = "Funding"),
  dep_regshr           = list(desc = "Regular Shares (Savings Accounts)",      theme = "Funding"),
  dep_share_drft       = list(desc = "Share Drafts (Checking Accounts)",       theme = "Funding"),
  dep_money_mkt        = list(desc = "Money Market Shares",                    theme = "Funding"),
  dep_share_cert       = list(desc = "Share Certificates (CDs)",               theme = "Funding"),
  dep_ira_keogh        = list(desc = "IRA / Keogh Accounts",                   theme = "Funding"),
  dep_oth_nonshrreg    = list(desc = "Other Non-Regular Share Deposits",       theme = "Funding"),
  insured_tot          = list(desc = "Total NCUSIF-Insured Shares",            theme = "Funding"),
  insured_pct          = list(desc = "Insured Shares (% of Total)",            theme = "Funding"),
  borrowings           = list(desc = "Total Borrowings",                       theme = "Funding"),

  # ── Lending Activity ──
  loans_tot            = list(desc = "Total Loans Outstanding",                theme = "Lending Activity"),
  loans_real_estate    = list(desc = "Real Estate Loans",                      theme = "Lending Activity"),
  loans_auto           = list(desc = "Auto Loans",                             theme = "Lending Activity"),
  loans_credit_card    = list(desc = "Credit Card Loans",                      theme = "Lending Activity"),
  loans_unsecured      = list(desc = "Unsecured Personal Loans",               theme = "Lending Activity"),
  loans_business       = list(desc = "Member Business Loans",                  theme = "Lending Activity"),
  loans_first_mtg      = list(desc = "First Mortgages",                        theme = "Lending Activity"),
  loans_originated     = list(desc = "Loans Originated (Period)",              theme = "Lending Activity"),
  loan_growth          = list(desc = "Loan Origination Volume",                theme = "Lending Activity"),
  # Loan portfolio totals (NCUA "lns_" prefix variants)
  lns_tot              = list(desc = "Total Loans Outstanding",                theme = "Lending Activity"),
  lns_auto_new         = list(desc = "New Auto Loans Outstanding",             theme = "Lending Activity"),
  lns_auto_used        = list(desc = "Used Auto Loans Outstanding",            theme = "Lending Activity"),
  lns_auto             = list(desc = "Total Auto Loans",                       theme = "Lending Activity"),
  lns_re_1             = list(desc = "1st Lien Real Estate Loans",             theme = "Lending Activity"),
  lns_re_2             = list(desc = "2nd Lien Real Estate Loans",             theme = "Lending Activity"),
  lns_re_oth           = list(desc = "Other Real Estate Loans",                theme = "Lending Activity"),
  lns_re_oth_ar        = list(desc = "Other RE Loans (Adj. Rate)",             theme = "Lending Activity"),
  lns_re_oth_fr        = list(desc = "Other RE Loans (Fixed Rate)",            theme = "Lending Activity"),
  lns_re_1_tot         = list(desc = "1st Lien RE Loans (Total)",              theme = "Lending Activity"),
  lns_unsecured        = list(desc = "Unsecured Personal Loans",               theme = "Lending Activity"),
  lns_credit_card      = list(desc = "Credit Card Loans",                      theme = "Lending Activity"),
  lns_business         = list(desc = "Member Business Loans",                  theme = "Lending Activity"),
  lns_mbl              = list(desc = "Member Business Loans",                  theme = "Lending Activity"),
  lns_cc               = list(desc = "Credit Card Loans",                      theme = "Lending Activity"),
  lns_re_1_tot_cyc     = list(desc = "1st Lien RE Loans (Cyclical)",           theme = "Lending Activity"),
  networth_tot         = list(desc = "Total Net Worth",                        theme = "Capital & Solvency"),
  dq_rate              = list(desc = "Total Loan Delinquency Rate",            theme = "Credit Risk"),
  lns_re_1_fr_shr      = list(desc = "1st Lien RE Loans (Share of Loans)",     theme = "Asset Composition"),
  lns_re_2_fr_shr      = list(desc = "2nd Lien RE Loans (Share of Loans)",     theme = "Asset Composition"),
  lns_auto_fr_shr      = list(desc = "Auto Loans (Share of Loans)",            theme = "Asset Composition"),
  lns_unsecured_fr_shr = list(desc = "Unsecured Loans (Share of Loans)",       theme = "Asset Composition"),
  lns_credit_card_fr_shr = list(desc = "Credit Cards (Share of Loans)",        theme = "Asset Composition"),
  lns_business_fr_shr  = list(desc = "Business Loans (Share of Loans)",        theme = "Asset Composition"),
  lns_auto_new_accel   = list(desc = "New Auto Loan Origination Acceleration", theme = "Lending Activity"),
  lns_auto_used_accel  = list(desc = "Used Auto Loan Origination Acceleration",theme = "Lending Activity"),
  lns_re_accel         = list(desc = "RE Loan Origination Acceleration",       theme = "Lending Activity"),
  lns_re_1_fr_accel    = list(desc = "1st Lien RE Share Acceleration",         theme = "Asset Composition"),
  lns_tot_accel        = list(desc = "Total Loan Origination Acceleration",    theme = "Lending Activity"),
  lns_auto_avg         = list(desc = "Average Auto Loan Size",                 theme = "Lending Activity"),
  lns_re_avg           = list(desc = "Average RE Loan Size",                   theme = "Lending Activity"),
  lns_unsecured_avg    = list(desc = "Average Unsecured Loan Size",            theme = "Lending Activity"),
  lns_mbl_shr          = list(desc = "Member Business Loans (Share)",          theme = "Asset Composition"),
  lns_mbl_pct          = list(desc = "Member Business Loans (% of Loans)",     theme = "Asset Composition"),
  dep_mmarket          = list(desc = "Money Market Deposits",                  theme = "Funding"),
  dep_certificates     = list(desc = "Share Certificates / CDs",               theme = "Funding"),

  # ── Credit Risk ──
  delinq_loans         = list(desc = "Delinquent Loans",                       theme = "Credit Risk"),
  delinq_ratio         = list(desc = "Delinquency Ratio (60+ Days)",           theme = "Credit Risk"),
  charge_off_ratio     = list(desc = "Net Charge-Off Ratio",                   theme = "Credit Risk"),
  net_chargeoffs       = list(desc = "Net Charge-Offs",                        theme = "Credit Risk"),
  allowance_ratio      = list(desc = "Allowance for Loan Losses Ratio",        theme = "Credit Risk"),
  troubled_debt        = list(desc = "Troubled Debt Restructurings",           theme = "Credit Risk"),
  dq_auto_new_rate     = list(desc = "New Auto Loan Delinquency Rate",         theme = "Credit Risk"),
  dq_auto_used_rate    = list(desc = "Used Auto Loan Delinquency Rate",        theme = "Credit Risk"),
  dq_credit_card_rate  = list(desc = "Credit Card Delinquency Rate",           theme = "Credit Risk"),
  dq_re_rate           = list(desc = "Real Estate Loan Delinquency Rate",      theme = "Credit Risk"),
  dq_unsecured_rate    = list(desc = "Unsecured Loan Delinquency Rate",        theme = "Credit Risk"),
  dq_first_mtg_rate    = list(desc = "First Mortgage Delinquency Rate",        theme = "Credit Risk"),
  dq_total_rate        = list(desc = "Total Loan Delinquency Rate",            theme = "Credit Risk"),

  # ── Capital ratios (alternative names) ──
  pcanetworth          = list(desc = "PCA Net Worth Ratio",                    theme = "Capital & Solvency"),
  net_worth_pct        = list(desc = "Net Worth (% of Assets)",                theme = "Capital & Solvency"),
  pca_class            = list(desc = "PCA Capital Classification",             theme = "Capital & Solvency"),

  # ── Profitability ──
  roa                  = list(desc = "Return on Assets (ROA)",                 theme = "Profitability"),
  roe                  = list(desc = "Return on Equity (ROE)",                 theme = "Profitability"),
  net_income           = list(desc = "Net Income",                             theme = "Profitability"),
  earnings             = list(desc = "Earnings",                               theme = "Profitability"),

  # ── Net Interest Margin / Income ──
  nim                  = list(desc = "Net Interest Margin",                    theme = "Net Interest Margin"),
  net_interest_inc     = list(desc = "Net Interest Income",                    theme = "Net Interest Margin"),
  interest_inc         = list(desc = "Total Interest Income",                  theme = "Net Interest Margin"),
  interest_exp         = list(desc = "Total Interest Expense",                 theme = "Net Interest Margin"),
  inc_netp             = list(desc = "Net Interest Income (Periodic)",         theme = "Net Interest Margin"),
  inc_net              = list(desc = "Net Income",                             theme = "Profitability"),
  inc_int              = list(desc = "Interest Income",                        theme = "Net Interest Margin"),
  inc_nint             = list(desc = "Non-Interest Income",                    theme = "Non-Interest Income"),

  # ── Non-Interest Income ──
  non_int_inc          = list(desc = "Non-Interest Income",                    theme = "Non-Interest Income"),
  fee_inc              = list(desc = "Fee Income",                             theme = "Non-Interest Income"),
  service_charge_inc   = list(desc = "Service Charge Income",                  theme = "Non-Interest Income"),

  # ── Operating Efficiency ──
  op_exp               = list(desc = "Operating Expenses",                     theme = "Operating Efficiency"),
  op_exp_ratio         = list(desc = "Operating Expense Ratio",                theme = "Operating Efficiency"),
  efficiency_ratio     = list(desc = "Efficiency Ratio",                       theme = "Operating Efficiency"),
  overhead             = list(desc = "Overhead Expenses",                      theme = "Operating Efficiency"),
  exp_comp_per_empl    = list(desc = "Compensation Expense per Employee",      theme = "Operating Efficiency"),
  exp_office           = list(desc = "Office Operations Expense",              theme = "Operating Efficiency"),
  exp_office_ratio     = list(desc = "Office Expense Ratio",                   theme = "Operating Efficiency"),
  exp_education        = list(desc = "Education & Promotional Expense",        theme = "Operating Efficiency"),
  comp_per_empl        = list(desc = "Compensation per Employee",              theme = "Operating Efficiency"),
  ftes                 = list(desc = "Full-Time Equivalent Employees",         theme = "Operating Efficiency"),
  branches             = list(desc = "Number of Branches",                     theme = "Operating Efficiency"),

  # ── Exit / Entry Dynamics ──
  merger_rate          = list(desc = "Merger Rate (Annualised)",               theme = "Exit Dynamics"),
  liquidation_rate     = list(desc = "Liquidation Rate",                       theme = "Exit Dynamics"),
  net_entry_rate       = list(desc = "Net New-Charter Entry Rate",             theme = "New Entry"),
  new_charters         = list(desc = "New Charters Granted",                   theme = "New Entry")
)

# (b) NCUA Call Report numeric account codes (selected critical ones).
# These are the standard 5300 account numbers — extend as needed.
NCUA_ACCT_DICT <- list(
  "001" = list(desc = "Cash on Deposit",                                  theme = "Asset Composition"),
  "002" = list(desc = "Cash on Hand",                                     theme = "Asset Composition"),
  "007" = list(desc = "Cash & Cash Equivalents",                          theme = "Asset Composition"),
  "010" = list(desc = "Total Investments",                                theme = "Asset Composition"),
  "013" = list(desc = "Loans to Members",                                 theme = "Lending Activity"),
  "025" = list(desc = "Allowance for Loan Losses",                        theme = "Credit Risk"),
  "041" = list(desc = "Land & Building",                                  theme = "Asset Composition"),
  "042" = list(desc = "Other Fixed Assets",                               theme = "Asset Composition"),
  "658" = list(desc = "Real Estate Loans Granted YTD",                    theme = "Lending Activity"),
  "697" = list(desc = "Total Members",                                    theme = "Membership"),
  "722" = list(desc = "Other Real Estate Owned",                          theme = "Credit Risk"),
  "730" = list(desc = "Reserves & Undivided Earnings",                    theme = "Capital & Solvency"),
  "799" = list(desc = "Net Worth (Total)",                                theme = "Capital & Solvency"),
  "902" = list(desc = "Total Investment Income",                          theme = "Net Interest Margin"),
  "997" = list(desc = "Total Loan Income",                                theme = "Net Interest Margin")
)

# (c) Resolver — strips transformations, looks up base name, then acct code
resolve_cr_label <- function(v) {
  if (is.na(v) || v == "") return(list(desc = NA_character_, theme = "Other Operational"))
  vl <- tolower(v)

  # Strip transformation prefixes/suffixes to recover base name
  base <- vl
  base <- gsub("^yoy_", "", base)
  base <- gsub("^qoq_", "", base)
  base <- gsub("_lag[0-9]+$", "", base)
  base <- gsub("_rmean[0-9]+$", "", base)
  base <- gsub("_rsd[0-9]+$", "", base)
  base <- gsub("_cyc$", "", base)
  base <- gsub("_chg$", "", base)
  base <- gsub("_accel$", "", base)

  # Detect transformation prefix for label
  prefix <- ""
  if (grepl("^yoy_",  vl)) prefix <- "YoY "
  if (grepl("^qoq_",  vl)) prefix <- "QoQ "

  # 1) Try CR_DICT (full name match)
  if (base %in% names(CR_DICT)) {
    hit <- CR_DICT[[base]]
    return(list(desc = paste0(prefix, hit$desc), theme = hit$theme))
  }

  # 2) Try NCUA account code match — extract digits if pattern is acct_NNN
  m <- regmatches(base, regexpr("(?:^|_)acct_?([0-9]{3,4})", base, perl = TRUE))
  if (length(m) > 0) {
    code <- gsub("[^0-9]", "", m)
    if (code %in% names(NCUA_ACCT_DICT)) {
      hit <- NCUA_ACCT_DICT[[code]]
      return(list(desc = paste0(prefix, hit$desc, " (Acct ", code, ")"),
                  theme = hit$theme))
    }
    # Unknown account code — show as Acct NNN with a note
    return(list(desc = paste0(prefix, "NCUA Account ", code, " (uncatalogued)"),
                theme = "Other Operational"))
  }

  # 3) Heuristic theme assignment for uncatalogued names
  theme <- classify_cr_theme(base)
  pretty <- gsub("_", " ", base)
  pretty <- tools::toTitleCase(pretty)
  return(list(desc = paste0(prefix, pretty), theme = theme))
}

# Heuristic theme classifier (fallback only)
classify_cr_theme <- function(v) {
  vl <- tolower(v)
  if (grepl("delinq|chargeoff|charge_off|nonperf|loss|allowance|^dq_|_dq_",   vl)) return("Credit Risk")
  if (grepl("net_worth|networth|capital|cap_adeq|leverage_ratio|risk_based|reserves|pca", vl)) return("Capital & Solvency")
  if (grepl("loan_to_share|loan_to_asset|investment|securit|liquid|cash|fixed_asset|_pct$|_shr$|_share$|fr_shr|fr_accel", vl)) return("Asset Composition")
  if (grepl("nim|net_interest|interest_inc|interest_exp|inc_netp|inc_int",  vl)) return("Net Interest Margin")
  if (grepl("non_int_inc|fee_inc|service_charge|inc_nint",                   vl)) return("Non-Interest Income")
  if (grepl("op_exp|efficiency|overhead|expense_ratio|compensation|empl|ftes|branch", vl)) return("Operating Efficiency")
  if (grepl("roa|roe|return_on|earnings|profit|net_income|inc_net$",         vl)) return("Profitability")
  if (grepl("members?|membership|fom_|acquisition|potential",                vl)) return("Membership")
  if (grepl("merger|liquid|acquis|exit",                                     vl)) return("Exit Dynamics")
  if (grepl("net_entry|new_charter",                                         vl)) return("New Entry")
  if (grepl("^lns_|^loans?_|loan_origin|loan_growth|_avg$|_avg_",            vl)) return("Lending Activity")
  if (grepl("share|deposit|insured|borrowing|^dep_",                         vl)) return("Funding")
  return("Other Operational")
}

# Apply resolver to every variable in the importance ranking
imp[, c("desc","theme") := {
  res <- lapply(variable, resolve_cr_label)
  list(vapply(res, function(x) x$desc,  character(1)),
       vapply(res, function(x) x$theme, character(1)))
}]

imp[, label := desc]

# Diagnostic
n_resolved <- sum(imp$theme != "Other Operational")
message(sprintf("\nResolved %d / %d call report variables to known themes (%.0f%%)",
                n_resolved, nrow(imp), 100 * n_resolved / nrow(imp)))

unresolved <- imp[theme == "Other Operational" & mean_importance > 0,
                  .(variable, mean_importance)]
if (nrow(unresolved) > 0) {
  message("\nUnresolved (theme = Other Operational) — top 10 by importance:")
  print(head(unresolved[order(-mean_importance)], 10))
  message("Add these to CR_DICT or NCUA_ACCT_DICT for proper labels.")
}

# ════════════════════════════════════════════════════════════
# 4. PICK TOP 10 — UNIQUE FACTORS ONLY
# ════════════════════════════════════════════════════════════
# Group variables by their underlying economic factor: yoy_X,
# qoq_X, X_lag2, X_accel, etc. all map to the same "factor" X.
# Keep only the highest-importance variant per factor so the
# chart shows 10 distinct economic drivers, not 10 transformations
# of 5 drivers.

strip_to_factor <- function(v) {
  vl <- tolower(v)
  vl <- gsub("^yoy_", "", vl)
  vl <- gsub("^qoq_", "", vl)
  vl <- gsub("^d_", "", vl)
  vl <- gsub("_lag[0-9]+$", "", vl)
  vl <- gsub("_rmean[0-9]+$", "", vl)
  vl <- gsub("_rsd[0-9]+$", "", vl)
  vl <- gsub("_cyc$", "", vl)
  vl <- gsub("_chg$", "", vl)
  vl <- gsub("_accel$", "", vl)
  vl <- gsub("_trail[0-9]+$", "", vl)
  vl
}

imp[, factor_base := vapply(variable, strip_to_factor, character(1))]

setorderv(imp, "mean_importance", order = -1L)

# Within each factor group, keep only the row with highest importance
imp_unique <- imp[!duplicated(factor_base)]

# Now take the top 10 unique factors
top10 <- head(imp_unique, 10)

message(sprintf("\nCollapsed %d total variables to %d unique factors. Showing top 10.",
                nrow(imp), nrow(imp_unique)))

# Detect importance scale
imp_max_val <- max(top10$mean_importance, na.rm = TRUE)
top10[, imp_pct := if (imp_max_val <= 1) mean_importance * 100 else mean_importance]

# Truncate long labels
top10[, label_short := ifelse(nchar(label) > 48,
                               paste0(substr(label, 1, 46), "…"),
                               label)]
# Safety net for duplicates (shouldn't happen now, but defensive)
dup_mask <- duplicated(top10$label_short) | duplicated(top10$label_short, fromLast = TRUE)
if (any(dup_mask)) {
  top10[dup_mask, label_short := paste0(label_short, "  [", variable, "]")]
}
top10[, label_short := factor(label_short, levels = rev(label_short))]
top10[, annotation := sprintf("%.0f%% • +%dQ lead", imp_pct, best_lead_q)]

# Console output
message("\nTop 10 Call Report Drivers:")
for (i in 1:nrow(top10)) {
  message(sprintf("  %2d. %-40s  %5.1f%%  +%dQ lead  (%s)",
                  i,
                  as.character(top10$label_short[i]),
                  top10$imp_pct[i],
                  top10$best_lead_q[i],
                  top10$theme[i]))
}

# ════════════════════════════════════════════════════════════
# 5. BUILD CHART (matches S1 styling)
# ════════════════════════════════════════════════════════════
theme_colors <- c(
  "Credit Risk"           = "#E76F51",
  "Capital & Solvency"    = "#5B9BD5",
  "Asset Composition"     = "#3D6FBF",
  "Net Interest Margin"   = "#52B788",
  "Non-Interest Income"   = "#90C088",
  "Operating Efficiency"  = "#F4A261",
  "Profitability"         = "#E8A838",
  "Membership"            = "#2EC4B6",
  "Lending Activity"      = "#7B68A8",
  "Funding"               = "#C77DFF",
  "Exit Dynamics"         = "#B5651D",
  "New Entry"             = "#6FAE6F",
  "Other Operational"     = "#999999"
)

# Only keep themes that appear (legend tidiness)
present_themes <- unique(top10$theme)
theme_colors_used <- theme_colors[names(theme_colors) %in% present_themes]
# Add fallback for any new themes
unmapped <- setdiff(present_themes, names(theme_colors))
if (length(unmapped) > 0) {
  fallback_palette <- c("#9C6644", "#6A994E", "#386641", "#BC4749", "#A7C957")
  for (i in seq_along(unmapped)) {
    theme_colors_used[unmapped[i]] <- fallback_palette[(i - 1) %% length(fallback_palette) + 1]
  }
}

p <- ggplot(top10, aes(x = imp_pct, y = label_short, fill = theme)) +
  geom_col(width = 0.65, alpha = 0.92) +
  geom_text(aes(label = annotation), hjust = -0.05,
            size = 3.4, fontface = "bold", color = "#333333") +
  scale_fill_manual(values = theme_colors_used, name = "Theme") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.32))) +
  labs(
    title    = "Top Call Report Drivers by Importance and Lead Time",
    subtitle = "How well each CU-specific operational variable explains future CU growth, and how far ahead the signal arrives\nLead time = quarters the call report variable leads CU response",
    x        = "Importance Score (R² × 100)",
    y        = NULL,
    caption  = "Source: Ensemble ML (Ridge, LASSO, Elastic Net, Random Forest) across 14 forecast targets  |  Endogenous variables (lagged growth, market share, count/asset levels) excluded"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#0B1D3A"),
    plot.subtitle = element_text(size = 11, color = "#666666", margin = margin(b = 12)),
    plot.caption  = element_text(size = 8, color = "#999999", hjust = 0),
    axis.text.y   = element_text(face = "bold", size = 10, color = "#333333"),
    axis.text.x   = element_text(size = 9, color = "#666666"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.4),
    legend.position = "bottom",
    legend.text  = element_text(size = 9),
    legend.title = element_text(size = 9, face = "bold"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 25, 15, 15)
  )

# ── Save ─────────────────────────────────────────────────
pdf_path <- file.path(PLOT_DIR, "S2_cr_drivers_leadtimes.pdf")
png_path <- file.path(PLOT_DIR, "S2_cr_drivers_leadtimes.png")

tryCatch({
  grDevices::cairo_pdf(pdf_path, width = 13, height = 7.5)
  print(p); grDevices::dev.off()
}, error = function(e) {
  try(grDevices::dev.off(), silent = TRUE)
  grDevices::pdf(pdf_path, width = 13, height = 7.5)
  print(p); grDevices::dev.off()
})

ggsave(png_path, p, width = 13, height = 7.5, dpi = 200, bg = "white")

message(sprintf("\nSaved: %s", pdf_path))
message(sprintf("Saved: %s", png_path))
