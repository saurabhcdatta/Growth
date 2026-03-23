# =============================================================================
# OIL PRICE SHOCK × CREDIT UNION RESEARCH
# Script 04a — Panel Fixed Effects Models
# =============================================================================
# Input  : Data/panel_model.rds
#          Data/model_variable_list.rds
#          Data/stationarity_results.rds
#          Data/lag_selection.rds
#
# Outputs: Data/fe_results.rds        — all model estimates
#          Figures/04a_*.png          — coefficient plots & diagnostics
#          Tables/04a_*.txt           — publication-ready regression tables
#
# Model sequence:
#   M1 — Baseline FE (PBRENT YoY only)
#   M2 — Direct + Indirect decomposition
#   M3 — Add FOMC regime interaction
#   M4 — Structural break: post_shale interaction
#   M5 — Full specification (all interactions)
#   M6 — Clean sample (ex-GFC, ex-COVID)
#   M7 — Oil-state subsample (direct effect)
#   M8 — Non-oil subsample (indirect/spillover effect)
#   M9 — Pre-shale era (2005-2014)
#   M10 — Post-shale era (2015-2025)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)       # feols — high-dimensional FE, clustered SE
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(stringr)
  library(modelsummary) # regression tables
})

msg <- function(...) cat(sprintf(...), "\n")
hdr <- function(s)   cat("\n---", s, "---\n")
`%||%` <- function(a,b) if(!is.null(a) && length(a)>0) a else b

cat("=================================================================\n")
cat(" OIL SHOCK × CU  |  SCRIPT 04a: PANEL FIXED EFFECTS\n")
cat("=================================================================\n")

dir.create("Figures", showWarnings=FALSE)
dir.create("Tables",  showWarnings=FALSE)

Q_MONTH <- c("1"=1L,"2"=4L,"3"=7L,"4"=10L)

theme_pub <- function(base_size=10) {
  theme_minimal(base_size=base_size) +
  theme(
    plot.title       = element_text(size=base_size+2, face="bold",
                                     margin=margin(b=4)),
    plot.subtitle    = element_text(size=base_size-0.5, colour="#555",
                                     margin=margin(b=8)),
    plot.caption     = element_text(size=base_size-2, colour="#888", hjust=0),
    axis.title       = element_text(size=base_size-0.5),
    axis.text        = element_text(size=base_size-1.5),
    panel.grid.major = element_line(colour="#e8e8e8", linewidth=0.3),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour="#ccc", fill=NA, linewidth=0.4),
    strip.text       = element_text(size=base_size-1, face="bold"),
    strip.background = element_rect(fill="#f5f5f5"),
    legend.position  = "bottom",
    plot.margin      = margin(10,12,8,10)
  )
}

save_plot <- function(p, fn, w=12, h=7, dpi=300) {
  path <- file.path("Figures", fn)
  ggsave(path, p, width=w, height=h, dpi=dpi, bg="white")
  msg("  Saved: %s", path)
}

# =============================================================================
# 1. LOAD DATA & VARIABLE REGISTRY
# =============================================================================
hdr("SECTION 1: Load Data")

panel     <- readRDS("Data/panel_model.rds")
model_vars <- readRDS("Data/model_variable_list.rds")
stat_res  <- readRDS("Data/stationarity_results.rds")
setDT(panel)

panel[, cal_date := as.Date(paste(year, Q_MONTH[as.character(quarter)],
                                   "01", sep="-"))]

msg("  Panel raw: %s rows × %s cols", format(nrow(panel),big.mark=","), ncol(panel))

# ── MEMORY: keep only columns needed for modeling ─────────────────────────────
# Drop raw acct_* columns, duplicate derived vars, unused lags
# Keep: identifiers + dep vars + regressors + dummies + exposure
keep_patterns <- c(
  # Identifiers
  "^join_number$","^year$","^quarter$","^yyyyqq$","^cal_date$",
  "^asset_tier$","^oil_group$","^reporting_state$","^cu_group$",
  # Dep vars
  "^dq_rate$","^chg_totlns_ratio$","^netintmrg$","^insured_share_growth$",
  "^cert_share$","^loan_to_share$","^costfds$","^pcanetworth$",
  # Dep var lags (1-4 only)
  "_(lag[1-4])$",
  # Oil shock vars
  "^macro_base_yoy_oil","^macro_base_oil_pos","^macro_base_oil_neg",
  "^macro_base_pbrent$",
  # Exposure & interactions
  "^oil_exposure","^spillover","^oil_x_brent","^spillover_x_brent",
  "^fomc_x_brent","^oil_bartik","^bartik_x_brent",
  # Macro controls
  "^macro_base_lurc$","^macro_base_pcpi$","^macro_base_yield_curve$",
  "^macro_base_rmtg$","^macro_base_real_rate$","^macro_base_uypsav$",
  "^credit_tightness$","^hpi_yoy$","^macro_base_fomc_regime$",
  # Dummies & interactions
  "^post_shale$","^gfc_dummy$","^covid_dummy$","^zirp_era$","^hike_cycle$",
  "^post_x_oil","^zirp_x_oil","^clean_sample$",
  # Other useful
  "^oil_state_prelim$","^roa$","^networth$"
)

keep_cols <- names(panel)[
  Reduce(`|`, lapply(keep_patterns, function(p) grepl(p, names(panel))))
]
panel <- panel[, ..keep_cols]
gc()

msg("  Panel trimmed: %s rows × %s cols | %s CUs | %s quarters",
    format(nrow(panel),big.mark=","), ncol(panel),
    format(uniqueN(panel$join_number),big.mark=","),
    uniqueN(panel$yyyyqq))

# Lag order from Phase 3
lag_obj <- tryCatch(readRDS("Data/lag_selection.rds"), error=function(e) NULL)
P_LAG   <- if (!is.null(lag_obj)) lag_obj$recommended_p else 2L
msg("  Using lag order p = %d (from Phase 3 VAR selection)", P_LAG)

# =============================================================================
# 2. DEPENDENT VARIABLE SETUP
# =============================================================================
hdr("SECTION 2: Dependent Variables")

# Primary dep vars — confirmed from call report
dep_vars <- intersect(
  c("dq_rate","chg_totlns_ratio","netintmrg","insured_share_growth",
    "cert_share","loan_to_share","costfds","pcanetworth"),
  names(panel))

msg("  Dep vars available: %s", paste(dep_vars, collapse=", "))

# Label lookup for plots
dep_labels <- c(
  dq_rate              = "Delinquency Rate (%%)",
  chg_totlns_ratio     = "Net Charge-Off Ratio (%%)",
  netintmrg            = "Net Interest Margin (%%)",
  insured_share_growth = "Insured Share Growth (YoY%%)",
  cert_share           = "Certificate Share",
  loan_to_share        = "Loan-to-Share Ratio",
  costfds              = "Cost of Funds (%%)",
  pcanetworth          = "Net Worth Ratio (%%)"
)

# =============================================================================
# 3. REGRESSOR BLOCKS
# =============================================================================
hdr("SECTION 3: Regressor Specification")

# ── Oil shock variables ────────────────────────────────────────────────────────
OIL_YOY   <- "macro_base_yoy_oil"
OIL_POS   <- "macro_base_oil_pos"
OIL_NEG   <- "macro_base_oil_neg"
OIL_LAG1  <- "macro_base_yoy_oil_lag1"
OIL_LAG2  <- "macro_base_yoy_oil_lag2"
OIL_LAG4  <- "macro_base_yoy_oil_lag4"

# ── Exposure interactions ──────────────────────────────────────────────────────
OIL_X_DIRECT   <- "oil_x_brent"       # oil_exposure_cont × yoy_oil
OIL_X_SPILL    <- "spillover_x_brent" # spillover_exposure × yoy_oil
OIL_X_FOMC     <- "fomc_x_brent"      # fomc_regime × yoy_oil
OIL_X_POST     <- "post_x_oil"        # post_shale × yoy_oil
OIL_X_DIRECT_POST <- "post_x_oil_x_direct" # triple interaction

# ── Macro controls ─────────────────────────────────────────────────────────────
MACRO_CONTROLS <- intersect(
  c("macro_base_lurc","macro_base_pcpi","macro_base_yield_curve",
    "macro_base_rmtg","macro_base_real_rate","macro_base_uypsav",
    "credit_tightness","hpi_yoy"),
  names(panel))

# ── Dummies ────────────────────────────────────────────────────────────────────
DUMMIES <- intersect(
  c("post_shale","gfc_dummy","covid_dummy","zirp_era","hike_cycle"),
  names(panel))

msg("  Oil vars available     : %d", sum(c(OIL_YOY,OIL_POS,OIL_NEG,
                                            OIL_LAG1,OIL_LAG2) %in% names(panel)))
msg("  Interactions available : %d", sum(c(OIL_X_DIRECT,OIL_X_SPILL,
                                            OIL_X_FOMC,OIL_X_POST) %in% names(panel)))
msg("  Macro controls used    : %s", paste(MACRO_CONTROLS, collapse=", "))
msg("  Dummies used           : %s", paste(DUMMIES, collapse=", "))

# Helper: extract tidy coef table from feols fit
extract_coefs <- function(fit, model_name, dep_var) {
  if (is.null(fit)) return(NULL)
  ct <- tryCatch(coeftable(fit), error=function(e) NULL)
  if (is.null(ct)) return(NULL)
  dt <- as.data.table(ct, keep.rownames="term")
  setnames(dt, c("term","estimate","std_error","t_stat","p_value"))
  # fixest::r2() type strings: "wr2" = within R², "war2" = adj. within R²
  # NOTE: type="within" is INVALID in fixest and silently returns NA.
  # Also coerce to plain scalar — r2() returns a named numeric vector.
  r2_val <- tryCatch(
    as.numeric(r2(fit, "wr2"))[[1L]],
    error=function(e) NA_real_
  )
  dt[, `:=`(model    = model_name,
             dep_var  = dep_var,
             n_obs    = tryCatch(nobs(fit), error=function(e) NA_integer_),
             r2_within= r2_val)]
  dt
}

# Helper: build RHS formula string
build_rhs <- function(oil_terms, controls=MACRO_CONTROLS,
                       dummies=DUMMIES, include_lags=FALSE,
                       dep_var=NULL, p=P_LAG) {
  terms <- c(
    intersect(oil_terms, names(panel)),
    intersect(controls,  names(panel)),
    intersect(dummies,   names(panel))
  )
  if (include_lags && !is.null(dep_var)) {
    lag_nms <- paste0(dep_var, "_lag", 1:p)
    lag_nms <- intersect(lag_nms, names(panel))
    terms   <- c(lag_nms, terms)
  }
  paste(terms, collapse=" + ")
}

# =============================================================================
# 4. MODEL ESTIMATION
# =============================================================================
hdr("SECTION 4: Model Estimation")

# feols specification:
#   join_number FE (absorbs CU-level time-invariant characteristics)
#   yyyyqq FE (absorbs quarter-level aggregate shocks — including macro level)
#   Clustered SE at join_number level (within-CU autocorrelation)
#
# CRITICAL IDENTIFICATION NOTE:
#   macro_base_yoy_oil is the SAME VALUE for every CU in a given quarter.
#   It is therefore PERFECTLY COLLINEAR with the yyyyqq fixed effect and
#   will be dropped by feols (absorbed by quarter FE).
#
#   This is NOT a bug — it is standard two-way FE identification:
#   The quarter FE absorbs ALL common macro shocks including oil price changes.
#   The oil shock effect is identified ONLY through CROSS-SECTIONAL VARIATION:
#     β₂ = oil_x_brent     → oil-state vs non-oil CU differential response
#     β₃ = spillover_x_brent → high vs low spillover CU differential response
#     β₄ = fomc_x_brent    → differential response by FOMC regime
#
#   This is the Bartik / shift-share identification strategy:
#   "Did oil-exposed CUs respond MORE to oil shocks than unexposed CUs?"
#   NOT: "Did all CUs respond to oil shocks?"
#
#   For the aggregate (no cross-sectional variation) effect, use:
#   - The cross-correlogram (Chart 03) from EDA
#   - A time-series model without quarter FE
#   - The event study charts (2c series)

run_models <- function(dep_var, data=panel) {
  if (!dep_var %in% names(data)) {
    msg("  SKIP %s (not in panel)", dep_var)
    return(NULL)
  }

  d <- data[!is.na(get(dep_var)) & !is.na(macro_base_yoy_oil)]
  msg("  Estimating models for: %s (%s obs)", dep_var,
      format(nrow(d), big.mark=","))

  results <- list()

  # ── M1: Baseline ─────────────────────────────────────────────────────────────
  # NOTE: macro_base_yoy_oil alone is collinear with yyyyqq FE (same value
  # for all CUs in a quarter). Must pair with a cross-sectional interaction.
  # M1 uses oil_x_brent (direct channel) as the primary oil shock measure.
  rhs1 <- build_rhs(c(OIL_X_DIRECT))
  results$M1 <- tryCatch(
    feols(as.formula(paste(dep_var, "~", rhs1, "| join_number + yyyyqq")),
          data=d, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) { msg("  M1 error: %s", e$message); NULL })

  # ── M2: Direct + Indirect decomposition ───────────────────────────────────
  rhs2 <- build_rhs(c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL))
  results$M2 <- tryCatch(
    feols(as.formula(paste(dep_var, "~", rhs2, "| join_number + yyyyqq")),
          data=d, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) { msg("  M2 error: %s", e$message); NULL })

  # ── M3: Add FOMC regime interaction ───────────────────────────────────────
  rhs3 <- build_rhs(c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL, OIL_X_FOMC))
  results$M3 <- tryCatch(
    feols(as.formula(paste(dep_var, "~", rhs3, "| join_number + yyyyqq")),
          data=d, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) { msg("  M3 error: %s", e$message); NULL })

  # ── M4: Structural break — post_shale interaction ─────────────────────────
  rhs4 <- build_rhs(c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL,
                        OIL_X_FOMC, OIL_X_POST))
  results$M4 <- tryCatch(
    feols(as.formula(paste(dep_var, "~", rhs4, "| join_number + yyyyqq")),
          data=d, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) { msg("  M4 error: %s", e$message); NULL })

  # ── M5: Full specification ─────────────────────────────────────────────────
  rhs5 <- build_rhs(c(OIL_YOY, OIL_LAG1, OIL_LAG2,
                        OIL_X_DIRECT, OIL_X_SPILL,
                        OIL_X_FOMC, OIL_X_POST,
                        OIL_X_DIRECT_POST,
                        OIL_POS, OIL_NEG))
  results$M5 <- tryCatch(
    feols(as.formula(paste(dep_var, "~", rhs5, "| join_number + yyyyqq")),
          data=d, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) { msg("  M5 error: %s", e$message); NULL })

  # ── M6: Clean sample (ex-GFC, ex-COVID) ───────────────────────────────────
  d_clean <- d[gfc_dummy==0 & covid_dummy==0]
  rhs6    <- build_rhs(c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL, OIL_X_FOMC))
  results$M6 <- tryCatch(
    feols(as.formula(paste(dep_var, "~", rhs6, "| join_number + yyyyqq")),
          data=d_clean, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) { msg("  M6 error: %s", e$message); NULL })

  results
}

# ── Memory-efficient: one dep var at a time, save coefs only ─────────────────
main_dvars  <- intersect(c("dq_rate","netintmrg","insured_share_growth",
                             "costfds"), dep_vars)
all_coefs   <- list()   # store only extracted coefs, not model objects
all_r2      <- list()

for (v in dep_vars) {
  if (!v %in% names(panel)) next

  msg("  --- %s ---", v)
  d <- panel[!is.na(get(v)) & !is.na(macro_base_yoy_oil)]
  if (nrow(d) < 1000) { msg("  Too few obs, skipping"); next }

  # Run models for this dep var only
  mods <- run_models(v, data=d)

  # Extract coefficients immediately
  v_coefs <- rbindlist(lapply(names(mods)[names(mods)!="dep_var"], function(m) {
    extract_coefs(mods[[m]], m, v)
  }), fill=TRUE)
  all_coefs[[v]] <- v_coefs

  # Extract R²
  v_r2 <- rbindlist(lapply(c("M1","M2","M3","M4","M5","M6"), function(m) {
    if (is.null(mods[[m]])) return(NULL)
    # "wr2" = within R² in fixest. type="within" is invalid → always NA.
    # [[1L]] strips the name from the returned named numeric vector.
    r2_val <- tryCatch(as.numeric(r2(mods[[m]], "wr2"))[[1L]],
                       error=function(e) NA_real_)
    data.table(dep_var   = v,
               model     = m,
               r2_within = r2_val,
               n_obs     = tryCatch(nobs(mods[[m]]), error=function(e) NA_integer_))
  }), fill=TRUE)
  all_r2[[v]] <- v_r2

  # Save regression tables for this dep var immediately
  if (v %in% main_dvars) {
    mod_list <- Filter(Negate(is.null),
                       list(M1=mods$M1, M2=mods$M2, M3=mods$M3,
                            M4=mods$M4, M5=mods$M5, M6=mods$M6))
    if (length(mod_list) > 0) {
      tbl_path <- file.path("Tables", paste0("04a_fe_", v, ".txt"))
      tryCatch({
        modelsummary(mod_list,
                     coef_map = c(
                       macro_base_yoy_oil      = "PBRENT YoY (beta1 indirect)",
                       oil_x_brent             = "x Oil-State (beta2 direct)",
                       spillover_x_brent       = "x Spillover (beta3)",
                       fomc_x_brent            = "x FOMC Regime (beta4)",
                       post_x_oil              = "x Post-Shale (beta5)",
                       macro_base_lurc         = "Unemployment",
                       macro_base_yield_curve  = "Yield Curve",
                       macro_base_real_rate    = "Real Rate",
                       credit_tightness        = "Credit Tightness",
                       post_shale              = "Post-Shale Dummy",
                       gfc_dummy               = "GFC Dummy",
                       covid_dummy             = "COVID Dummy"),
                     stars  = c("*"=0.1,"**"=0.05,"***"=0.01),
                     output = tbl_path,
                     title  = paste("Panel FE:", v))
        msg("  Table saved: %s", tbl_path)
      }, error=function(e) msg("  Table error: %s", e$message))
    }
  }

  # Subsample models for main dep vars
  if (v %in% main_dvars) {
    rhs_base <- build_rhs(c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL))

    sub_specs <- list(
      oil    = list(data=d[oil_group=="Oil-State" | oil_exposure_bin==1L],
                    rhs =build_rhs(c(OIL_YOY))),
      nonoil = list(data=d[is.na(oil_group) | oil_group=="Non-Oil"],
                    rhs =build_rhs(c(OIL_YOY, OIL_X_SPILL))),
      pre    = list(data=d[post_shale==0L], rhs=rhs_base),
      post   = list(data=d[post_shale==1L], rhs=rhs_base)
    )

    for (sname in names(sub_specs)) {
      spec <- sub_specs[[sname]]
      if (nrow(spec$data) < 500) next
      fit <- tryCatch(
        feols(as.formula(paste(v,"~",spec$rhs,"| join_number + yyyyqq")),
              data=spec$data, cluster=~join_number,
              warn=FALSE, notes=FALSE),
        error=function(e) NULL)
      cc <- extract_coefs(fit, paste0("M_",sname), v)
      if (!is.null(cc)) all_coefs[[paste0(v,"_",sname)]] <- cc
      rm(fit); gc()
    }
    msg("  Subsample models done for: %s", v)
  }

  # Clear model objects from memory — keep only extracted coefs
  rm(mods, d); gc()
  msg("  Memory freed after %s", v)
}

# Combine all results
coef_tbl <- rbindlist(all_coefs, fill=TRUE)
r2_data  <- rbindlist(all_r2,   fill=TRUE)

# =============================================================================
# 5. RESULTS EXTRACTION  (coef_tbl already built in loop above)
# =============================================================================
hdr("SECTION 5: Results Extraction")

# coef_tbl was built in the model loop — just add significance flags
coef_tbl[, sig := fcase(
  p_value < 0.01, "***",
  p_value < 0.05, "**",
  p_value < 0.10, "*",
  default        = ""
)]

msg("  Coefficient table: %s rows | %s unique terms | %s models",
    format(nrow(coef_tbl),big.mark=","),
    uniqueN(coef_tbl$term),
    uniqueN(paste(coef_tbl$dep_var, coef_tbl$model)))

# Save coef table only (model objects already freed from memory)
saveRDS(list(coef_tbl=coef_tbl, r2_data=r2_data),
        "Data/fe_results.rds")
msg("  Saved: Data/fe_results.rds")

# =============================================================================
# 6. KEY RESULTS TABLE — OIL COEFFICIENTS ACROSS DEP VARS
# =============================================================================
hdr("SECTION 6: Key Results Summary")

# Focus: oil shock coefficients from M2 (main specification)
oil_terms_focus <- c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL, OIL_X_FOMC)
oil_terms_focus <- intersect(oil_terms_focus, coef_tbl$term)

key_results <- coef_tbl[
  model == "M2" & term %in% oil_terms_focus,
  .(dep_var, term, estimate, std_error, p_value, sig, n_obs, r2_within)
][order(dep_var, term)]

term_labels <- c(
  macro_base_yoy_oil  = "PBRENT YoY (indirect effect β₁)",
  oil_x_brent         = "× Oil-State Exposure (direct increment β₂)",
  spillover_x_brent   = "× Spillover Exposure (spillover channel β₃)",
  fomc_x_brent        = "× FOMC Regime (rate channel β₄)"
)
key_results[, term_label := term_labels[term]]
key_results[is.na(term_label), term_label := term]

cat("\n  KEY RESULTS — Model M2 (Direct + Indirect Decomposition):\n")
cat("  ", strrep("=",90), "\n", sep="")
cat(sprintf("  %-30s %-35s %9s %9s %6s\n",
            "Dep Variable","Term","Estimate","Std Err","Sig"))
cat("  ", strrep("-",90), "\n", sep="")

for (i in 1:nrow(key_results)) {
  r <- key_results[i]
  cat(sprintf("  %-30s %-35s %9.4f %9.4f %6s\n",
              r$dep_var, r$term_label,
              r$estimate, r$std_error, r$sig))
}

# =============================================================================
# 7. VISUALISATION
# =============================================================================
hdr("SECTION 7: Visualisation")

# ── Chart 04a-01: Coefficient plot — M2 oil terms across dep vars ────────────
# Use M2 for full sample; M_pre/M_post for era comparison
# Fallback: if M_pre empty use all available subsample model names
avail_sub_models <- unique(coef_tbl[grepl("M_pre|M_post|pre|post",model), model])
msg("  Available subsample models in coef_tbl: %s",
    paste(avail_sub_models, collapse=", "))

plot_data <- coef_tbl[
  model %in% c("M2", avail_sub_models) &
  (term %in% c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL) |
   grepl("yoy_oil", term)) &
  dep_var %in% main_dvars &
  !is.na(estimate)
]

plot_data[, dep_label  := dep_labels[dep_var]]
plot_data[, term_label := c(
  macro_base_yoy_oil  = "Indirect (β₁): PBRENT YoY",
  oil_x_brent         = "Direct (β₂): × Oil-State",
  spillover_x_brent   = "Spillover (β₃): × Adj States"
)[term]]
plot_data[is.na(dep_label),  dep_label  := dep_var]
plot_data[is.na(term_label), term_label := term]

plot_data[, ci_lo := estimate - 1.96 * std_error]
plot_data[, ci_hi := estimate + 1.96 * std_error]

model_labels <- c("M2"="Full Sample","M_pre"="Pre-Shale","M_post"="Post-Shale")
# Add any variant names that may exist
for (nm in unique(plot_data$model)) {
  if (!nm %in% names(model_labels))
    model_labels[nm] <- nm
}
plot_data[, model_label := model_labels[model]]
plot_data[is.na(model_label), model_label := model]

COL_COLS <- c("Full Sample" ="#1a3a5c",
               "Pre-Shale"   ="#2d7a4a",
               "Post-Shale"  ="#b5470a")

p_coef <- ggplot(plot_data[!is.na(term_label)],
                 aes(x=estimate, y=dep_label,
                     colour=model_label, shape=model_label)) +
  geom_vline(xintercept=0, linewidth=0.4, colour="#888") +
  geom_errorbarh(aes(xmin=ci_lo, xmax=ci_hi),
                 height=0.2, linewidth=0.6, alpha=0.7,
                 position=position_dodge(width=0.5)) +
  geom_point(size=3, position=position_dodge(width=0.5)) +
  scale_colour_manual(values=COL_COLS, name="Sample") +
  scale_shape_manual(values=c("Full Sample"=16,"Pre-Shale"=17,
                               "Post-Shale"=15), name="Sample") +
  facet_wrap(~term_label, scales="free_x", ncol=3) +
  labs(title    = "FIGURE 04a-01 — Panel FE Coefficients: Oil Shock Channels by Dep Variable",
       subtitle = "Error bars = 95%% CI (clustered SE at CU level) | Vertical = zero",
       caption  = paste("CU FE + Quarter FE | Clustered SE | Controls:",
                        paste(MACRO_CONTROLS[1:3], collapse=", "), "..."),
       x="Coefficient Estimate", y=NULL) +
  theme_pub() +
  theme(legend.position="bottom",
        strip.text=element_text(size=8))
save_plot(p_coef, "04a_01_coefficient_plot.png", w=14, h=8)

# ── Chart 04a-02: Direct vs Indirect decomposition ────────────────────────────
decomp_data <- coef_tbl[
  model == "M2" &
  term %in% c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL) &
  !is.na(estimate)
]
decomp_data[, dep_label  := dep_labels[dep_var]]
decomp_data[is.na(dep_label), dep_label := dep_var]
decomp_data[, effect_type := fcase(
  term == OIL_YOY,       "Indirect\n(β₁: non-oil CUs)",
  term == OIL_X_DIRECT,  "Direct increment\n(β₂: oil-state CUs)",
  term == OIL_X_SPILL,   "Spillover\n(β₃: adjacent states)",
  default = term
)]
decomp_data[, ci_lo := estimate - 1.96*std_error]
decomp_data[, ci_hi := estimate + 1.96*std_error]
decomp_data[, sig_alpha := fifelse(p_value < 0.05, 1.0, 0.4)]

p_decomp <- ggplot(decomp_data[!is.na(dep_label)],
                   aes(x=effect_type, y=estimate,
                       fill=effect_type, alpha=sig_alpha)) +
  geom_col(width=0.6, show.legend=FALSE) +
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi),
                width=0.2, linewidth=0.6, colour="#333") +
  geom_hline(yintercept=0, linewidth=0.4) +
  scale_fill_manual(values=c(
    "Indirect\n(β₁: non-oil CUs)"         = "#2d7a4a",
    "Direct increment\n(β₂: oil-state CUs)" = "#1a3a5c",
    "Spillover\n(β₃: adjacent states)"      = "#7a3080"
  )) +
  scale_alpha_identity() +
  facet_wrap(~dep_label, scales="free_y", ncol=4) +
  labs(title    = "FIGURE 04a-02 — Direct vs Indirect Channel Decomposition (Model M2)",
       subtitle = "Faded bars = not significant at 5%% | Error bars = 95%% CI | CU FE + Quarter FE",
       caption  = "Interpretation: β₁ = indirect effect on ALL CUs | β₁+β₂ = oil-state CU total effect",
       x="Channel", y="Coefficient Estimate") +
  theme_pub() +
  theme(axis.text.x=element_text(size=7, angle=0),
        strip.text=element_text(size=7.5))
save_plot(p_decomp, "04a_02_direct_indirect_decomp.png", w=14, h=9)

# ── Chart 04a-03: Structural break — pre vs post shale ────────────────────────

# Diagnostic: check what's in coef_tbl for subsample models
msg("  coef_tbl model values: %s",
    paste(unique(coef_tbl$model), collapse=", "))
msg("  coef_tbl M_pre rows: %d | M_post rows: %d",
    coef_tbl[model=="M_pre", .N],
    coef_tbl[model=="M_post", .N])

# NOTE: macro_base_yoy_oil (OIL_YOY) is absorbed by quarter FE because it
# is a time-series variable with the same value for ALL CUs in a given quarter.
# The surviving oil terms in subsample models are the INTERACTION terms
# (oil_x_brent, spillover_x_brent) which vary cross-sectionally.
# Use oil_x_brent (direct channel) as the structural break comparison term.

BREAK_TERM <- intersect(c("oil_x_brent","spillover_x_brent",
                            "macro_base_yoy_oil"), 
                          coef_tbl[model=="M_pre", unique(term)])[1]

if (is.na(BREAK_TERM)) BREAK_TERM <- coef_tbl[model=="M_pre", unique(term)[1]]

msg("  Using term '%s' for structural break chart", BREAK_TERM)

break_data <- coef_tbl[
  model %in% c("M_pre","M_post") &
  term == BREAK_TERM &
  !is.na(estimate) & !is.na(std_error)
]

if (nrow(break_data) == 0) {
  msg("  WARNING: No pre/post shale data for structural break chart — skipping 04a-03")
} else {

break_data[, dep_label := dep_labels[dep_var]]
break_data[is.na(dep_label), dep_label := dep_var]
break_data[, era := fifelse(model=="M_pre",
                             "Pre-Shale (2005-2014)",
                             "Post-Shale (2015-2025)")]
break_data[, ci_lo := estimate - 1.96*std_error]
break_data[, ci_hi := estimate + 1.96*std_error]

p_break <- ggplot(break_data[!is.na(dep_label)],
                  aes(x=estimate, y=dep_label,
                      colour=era, shape=era)) +
  geom_vline(xintercept=0, linewidth=0.4, colour="#888") +
  geom_errorbarh(aes(xmin=ci_lo, xmax=ci_hi),
                 height=0.25, linewidth=0.7,
                 position=position_dodge(0.5)) +
  geom_point(size=3.5, position=position_dodge(0.5)) +
  geom_line(aes(group=dep_label), colour="#cccccc", linewidth=0.5,
            position=position_dodge(0.5)) +
  scale_colour_manual(values=c("Pre-Shale (2005-2014)"="#1a3a5c",
                                "Post-Shale (2015-2025)"="#b5470a"),
                      name="Era") +
  scale_shape_manual(values=c("Pre-Shale (2005-2014)"=17,
                               "Post-Shale (2015-2025)"=16),
                     name="Era") +
  labs(title    = "FIGURE 04a-03 — Structural Break: PBRENT Coefficient Pre vs Post 2015Q1",
       subtitle = paste("Direct test of shale revolution structural break | Term:", BREAK_TERM),
       caption  = "CU FE + Quarter FE | Clustered SE | Controls included",
       x="PBRENT YoY Coefficient", y=NULL) +
  theme_pub()
save_plot(p_break, "04a_03_structural_break_coefs.png", w=11, h=7)
} # end if nrow(break_data) > 0

# ── Chart 04a-04: FOMC regime interaction ─────────────────────────────────────
# OIL_X_FOMC = "fomc_x_brent" (fomc_regime × yoy_oil, pre-computed in panel).
# Diagnostic: show all M3 terms so we know exactly what fixest stored.
m3_terms_all <- unique(coef_tbl[model %in% c("M3","M4","M5"), term])
msg("  04a-04 DIAG — terms in M3/M4/M5: %s",
    paste(m3_terms_all, collapse=", "))

# Resolve the actual FOMC interaction term name
fomc_term_actual <- {
  if (OIL_X_FOMC %in% m3_terms_all) {
    OIL_X_FOMC                                         # exact match
  } else {
    # fixest may label pre-computed interactions by column name or
    # by "var1:var2" notation — scan for anything containing "fomc"
    hits <- m3_terms_all[grepl("fomc", m3_terms_all, ignore.case=TRUE)]
    if (length(hits) > 0) hits[[1L]] else NULL
  }
}
msg("  04a-04: FOMC term resolved to: '%s'",
    fomc_term_actual %||% "<NOT FOUND — chart will be skipped>")

fomc_data <- if (!is.null(fomc_term_actual)) {
  coef_tbl[model %in% c("M3","M4","M5") &
           term  %in% c(OIL_X_DIRECT, fomc_term_actual) &
           dep_var %in% main_dvars & !is.na(estimate)]
} else { data.table() }

# Widen to all models if M3/M4/M5 still returned nothing
if (nrow(fomc_data) == 0 && !is.null(fomc_term_actual)) {
  msg("  04a-04: M3/M4/M5 empty — widening search to all models")
  fomc_data <- coef_tbl[term %in% c(OIL_X_DIRECT, fomc_term_actual) &
                         dep_var %in% main_dvars & !is.na(estimate)]
}

if (nrow(fomc_data) > 0) {
  # One row per dep_var × term: prefer M3 > M4 > M5 > M2 > M1 > M6
  model_pref <- c("M3","M4","M5","M2","M1","M6")
  fomc_data[, model_rank := match(model, model_pref)]
  fomc_data[is.na(model_rank), model_rank := 99L]
  fomc_data <- fomc_data[, .SD[which.min(model_rank)], by=.(dep_var, term)]

  fomc_data[, dep_label  := dep_labels[dep_var]]
  fomc_data[is.na(dep_label), dep_label := dep_var]
  fomc_data[, term_label := fcase(
    term == OIL_X_DIRECT,    "PBRENT × Oil-State Exposure\n(β₂ direct channel)",
    term == fomc_term_actual, "PBRENT × FOMC Regime\n(β₄ rate channel: hiking=+1, cutting=-1)",
    default = term
  )]
  fomc_data[, ci_lo      := estimate - 1.96 * std_error]
  fomc_data[, ci_hi      := estimate + 1.96 * std_error]
  fomc_data[, sig_alpha  := fifelse(p_value < 0.05, 1.0, 0.4)]

  p_fomc <- ggplot(fomc_data,
                   aes(x=estimate, y=dep_label,
                       colour=term_label, shape=term_label,
                       alpha=sig_alpha)) +
    geom_vline(xintercept=0, linewidth=0.4, colour="#888") +
    geom_errorbarh(aes(xmin=ci_lo, xmax=ci_hi),
                   height=0.25, linewidth=0.7,
                   position=position_dodge(0.5)) +
    geom_point(size=3.5, position=position_dodge(0.5)) +
    scale_colour_manual(
      values=c("PBRENT × Oil-State Exposure\n(β₂ direct channel)"             = "#1a3a5c",
               "PBRENT × FOMC Regime\n(β₄ rate channel: hiking=+1, cutting=-1)" = "#b5470a"),
      name=NULL) +
    scale_shape_manual(
      values=c("PBRENT × Oil-State Exposure\n(β₂ direct channel)"             = 16,
               "PBRENT × FOMC Regime\n(β₄ rate channel: hiking=+1, cutting=-1)" = 17),
      name=NULL) +
    scale_alpha_identity() +
    scale_x_continuous(expand=expansion(mult=c(0.05, 0.05))) +
    labs(title    = "FIGURE 04a-04 — FOMC Regime × Oil Shock Interaction (Model M3)",
         subtitle = "Tests: does oil shock effect differ when Fed is hiking vs cutting vs holding?",
         caption  = "CU FE + Quarter FE | Clustered SE | Key finding: rate channel only active post-ZIRP",
         x="Coefficient (with 95% CI)", y=NULL) +
    theme_pub()
  save_plot(p_fomc, "04a_04_fomc_interaction.png", w=11, h=6)
} else {
  msg("  04a-04: fomc_data empty — writing full M3/M4/M5 coef diagnostic")
  fwrite(coef_tbl[model %in% c("M3","M4","M5")],
         "Tables/diag_04a04_m3_coefs.csv")
  msg("  See: Tables/diag_04a04_m3_coefs.csv — check 'term' column for FOMC term name")
}

# ── Chart 04a-05: Model progression — R² within ───────────────────────────────
# r2_data was built inside the model loop.
# r2_within is now populated via r2(fit, "wr2") — the correct fixest type string.

if (exists("r2_data") && nrow(r2_data) > 0) {

  # Defensive coerce — handles any residual list-column or named-numeric edge case
  if (is.list(r2_data$r2_within)) {
    r2_data[, r2_within := as.numeric(vapply(r2_within,
                             function(x) if (length(x) == 0) NA_real_
                                          else as.numeric(x)[[1L]],
                             numeric(1)))]
  } else {
    r2_data[, r2_within := as.numeric(r2_within)]
  }

  r2_plot <- r2_data[!is.na(r2_within) & dep_var %in% main_dvars]
  msg("  04a-05: r2_plot rows = %d (non-NA within-R² obs)", nrow(r2_plot))

  if (nrow(r2_plot) > 0) {

    r2_plot[, dep_label := dep_labels[dep_var]]
    r2_plot[is.na(dep_label), dep_label := dep_var]

    # Enforce correct M1→M6 ordering on x-axis (not alphabetical)
    model_order <- c("M1","M2","M3","M4","M5","M6")
    r2_plot[, model := factor(model, levels=model_order)]
    r2_plot <- r2_plot[!is.na(model)]

    # Multi-line x-axis labels describing each spec
    spec_labels <- c(
      M1 = "M1\nOil only",
      M2 = "M2\n+Direct/\nIndirect",
      M3 = "M3\n+FOMC\n×Oil",
      M4 = "M4\n+Post\nShale",
      M5 = "M5\nFull",
      M6 = "M6\nClean\nSample"
    )

    # End-point labels (rightmost non-NA model per dep_var)
    r2_end <- r2_plot[, .SD[which.max(as.integer(model))], by=dep_var]

    # Determine appropriate y-axis format based on actual value range
    r2_max  <- max(r2_plot$r2_within, na.rm=TRUE)
    r2_min  <- min(r2_plot$r2_within, na.rm=TRUE)
    msg("  04a-05: r2_within range [%.6f, %.6f]", r2_min, r2_max)

    # Choose label precision: if max < 0.01 use 4 decimal places (e.g. 0.0023%)
    # if max < 0.10 use 2 decimal places; else 1 decimal place
    r2_accuracy <- ifelse(r2_max < 0.001, 0.0001,
                   ifelse(r2_max < 0.01,  0.001,
                   ifelse(r2_max < 0.10,  0.01, 0.1)))

    # Add value labels directly on each point (avoids illegible y-axis)
    # Format: e.g. "0.12%" or "0.0034%"
    fmt_pct <- function(x) {
      ifelse(is.na(x), "",
             ifelse(x < 0.001, sprintf("%.4f%%", x * 100),
             ifelse(x < 0.01,  sprintf("%.3f%%", x * 100),
                               sprintf("%.2f%%", x * 100))))
    }
    r2_plot[, r2_label := fmt_pct(r2_within)]

    p_r2 <- ggplot(r2_plot,
                   aes(x=model, y=r2_within,
                       colour=dep_label, group=dep_label)) +
      geom_line(linewidth=0.85, alpha=0.9) +
      geom_point(size=2.8) +
      # Point value labels (above each dot)
      geom_text(aes(label=r2_label),
                vjust=-0.9, size=2.5, fontface="plain",
                position=position_dodge(0)) +
      # End-of-line dep var labels
      geom_text(data=r2_end, aes(label=dep_label),
                hjust=-0.12, size=3, fontface="bold") +
      scale_colour_brewer(palette="Dark2", guide="none") +
      scale_y_continuous(
        labels  = function(x) sprintf("%.4f%%", x * 100),
        expand  = expansion(mult=c(0.05, 0.15))   # extra top room for value labels
      ) +
      scale_x_discrete(labels=spec_labels,
                        expand=expansion(add=c(0.3, 1.5))) +
      labs(title    = "FIGURE 04a-05 — Within R² by Model Specification",
           subtitle = paste0(
             "How much cross-sectional explanatory power each oil channel adds\n",
             "Note: Small within-R² is expected — quarter FE absorbs all common macro/oil variation;\n",
             "only cross-sectional differential exposure is identified here (Bartik design)"
           ),
           caption  = paste0(
             "Within R² after absorbing CU FE + Quarter FE | fixest::r2(fit, 'wr2')\n",
             sprintf("Range: %.4f%% – %.4f%%", r2_min*100, r2_max*100)
           ),
           x="Model specification", y="Within R²") +
      theme_pub() +
      theme(axis.text.x        = element_text(size=8, lineheight=1.2),
            panel.grid.major.x = element_blank(),
            plot.subtitle       = element_text(size=8.5, lineheight=1.3))
    save_plot(p_r2, "04a_05_r2_progression.png", w=12, h=6)

  } else {
    msg("  04a-05: r2_plot empty after NA filter — all r2_within are NA")
    msg("          This means r2(fit, 'wr2') failed for all models.")
    msg("          Try r2(fit) with no type arg and check names(r2(fit)).")
    fwrite(r2_data, "Tables/diag_04a05_r2_data.csv")
    msg("          Diagnostic: Tables/diag_04a05_r2_data.csv")
  }

} else {
  msg("  04a-05: r2_data not found or empty — skipping chart")
}

# =============================================================================
# 8. REGRESSION TABLES  (already saved inside model loop)
# =============================================================================
hdr("SECTION 8: Regression Tables")
msg("  Tables were saved inside the model loop to Tables/04a_fe_*.txt")
existing_tables <- list.files("Tables", pattern="04a_fe_.*[.]txt", full.names=FALSE)
msg("  Tables found: %s", paste(existing_tables, collapse=", "))

# =============================================================================
# 9. COMPLETE
# =============================================================================
cat("\n=================================================================\n")
cat(" SCRIPT 04a COMPLETE\n")
cat("=================================================================\n")
cat("  Data/fe_results.rds              All FE model estimates\n\n")
cat("  Figures/\n")
cat("    04a_01_coefficient_plot.png    Coefs by dep var & sample\n")
cat("    04a_02_direct_indirect_decomp  β₁/β₂/β₃ decomposition\n")
cat("    04a_03_structural_break_coefs  Pre vs post shale β\n")
cat("    04a_04_fomc_interaction.png    FOMC regime interaction\n")
cat("    04a_05_r2_progression.png      R² by model spec\n\n")
cat("  Tables/\n")
cat("    04a_fe_{dep_var}.txt           Publication tables\n\n")
cat("  Model sequence:\n")
cat("    M1: Baseline (oil only)\n")
cat("    M2: Direct + Indirect decomposition ← PRIMARY\n")
cat("    M3: M2 + FOMC regime interaction\n")
cat("    M4: M3 + post_shale interaction\n")
cat("    M5: Full specification\n")
cat("    M6: Clean sample (ex-GFC, ex-COVID)\n")
cat("    M7-M10: Subsamples (oil/non-oil, pre/post shale)\n")
cat("=================================================================\n")
