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

msg("  Panel: %s rows × %s cols | %s CUs | %s quarters",
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
#   yyyyqq FE (absorbs quarter-level aggregate shocks)
#   Clustered SE at join_number level (within-CU autocorrelation)

run_models <- function(dep_var, data=panel) {
  if (!dep_var %in% names(data)) {
    msg("  SKIP %s (not in panel)", dep_var)
    return(NULL)
  }

  d <- data[!is.na(get(dep_var)) & !is.na(macro_base_yoy_oil)]
  msg("  Estimating models for: %s (%s obs)", dep_var,
      format(nrow(d), big.mark=","))

  results <- list()

  # ── M1: Baseline — oil only ────────────────────────────────────────────────
  rhs1 <- build_rhs(c(OIL_YOY))
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

# ── Run for each dep var ───────────────────────────────────────────────────────
all_models <- lapply(dep_vars, function(v) {
  res <- run_models(v)
  if (!is.null(res)) res$dep_var <- v
  res
})
names(all_models) <- dep_vars

# ── Subsample models (for main dep vars) ──────────────────────────────────────
main_dvars <- intersect(c("dq_rate","netintmrg","insured_share_growth",
                            "costfds"), dep_vars)

subsample_models <- list()
for (v in main_dvars) {
  d <- panel[!is.na(get(v)) & !is.na(macro_base_yoy_oil)]
  rhs_sub <- build_rhs(c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL, OIL_X_FOMC))

  # M7: Oil-state CUs only (direct channel)
  d_oil <- d[oil_group == "Oil-State" | oil_exposure_bin == 1L]
  subsample_models[[paste0(v,"_oil")]] <- tryCatch(
    feols(as.formula(paste(v, "~", build_rhs(c(OIL_YOY)), "| join_number + yyyyqq")),
          data=d_oil, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) NULL)

  # M8: Non-oil CUs only (indirect channel)
  d_nonoil <- d[is.na(oil_group) | oil_group == "Non-Oil"]
  subsample_models[[paste0(v,"_nonoil")]] <- tryCatch(
    feols(as.formula(paste(v, "~", build_rhs(c(OIL_YOY, OIL_X_SPILL)),
                           "| join_number + yyyyqq")),
          data=d_nonoil, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) NULL)

  # M9: Pre-shale (2005-2014)
  d_pre <- d[post_shale == 0L]
  subsample_models[[paste0(v,"_pre")]] <- tryCatch(
    feols(as.formula(paste(v, "~", build_rhs(c(OIL_YOY, OIL_X_DIRECT,
                                                OIL_X_SPILL)),
                           "| join_number + yyyyqq")),
          data=d_pre, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) NULL)

  # M10: Post-shale (2015-2025)
  d_post <- d[post_shale == 1L]
  subsample_models[[paste0(v,"_post")]] <- tryCatch(
    feols(as.formula(paste(v, "~", build_rhs(c(OIL_YOY, OIL_X_DIRECT,
                                                OIL_X_SPILL)),
                           "| join_number + yyyyqq")),
          data=d_post, cluster=~join_number, warn=FALSE, notes=FALSE),
    error=function(e) NULL)

  msg("  Subsample models estimated for: %s", v)
}

# =============================================================================
# 5. RESULTS EXTRACTION
# =============================================================================
hdr("SECTION 5: Results Extraction")

# Extract coefficients into tidy table
extract_coefs <- function(fit, model_name, dep_var) {
  if (is.null(fit)) return(NULL)
  ct <- tryCatch(coeftable(fit), error=function(e) NULL)
  if (is.null(ct)) return(NULL)
  dt <- as.data.table(ct, keep.rownames="term")
  setnames(dt, c("term","estimate","std_error","t_stat","p_value"))
  dt[, `:=`(model=model_name, dep_var=dep_var,
             n_obs    = nobs(fit),
             r2_within = r2(fit, type="within") %||% NA,
             n_cu     = uniqueN(fit$obs_selection$obsCluster))]
  dt
}

coef_tbl <- rbindlist(lapply(dep_vars, function(v) {
  mods <- all_models[[v]]
  if (is.null(mods)) return(NULL)
  rbindlist(lapply(names(mods)[names(mods)!="dep_var"], function(m) {
    extract_coefs(mods[[m]], m, v)
  }), fill=TRUE)
}), fill=TRUE)

# Add subsample coefs
sub_coefs <- rbindlist(lapply(names(subsample_models), function(nm) {
  parts  <- str_split(nm, "_(?=(oil|nonoil|pre|post)$)")[[1]]
  dep_v  <- parts[1]
  samp   <- parts[2]
  extract_coefs(subsample_models[[nm]],
                paste0("M_", samp), dep_v)
}), fill=TRUE)

coef_tbl <- rbindlist(list(coef_tbl, sub_coefs), fill=TRUE)

# Significance flags
coef_tbl[, sig := fcase(
  p_value < 0.01, "***",
  p_value < 0.05, "**",
  p_value < 0.10, "*",
  default        = ""
)]

msg("  Coefficient table: %s rows | %s unique terms",
    nrow(coef_tbl), uniqueN(coef_tbl$term))

saveRDS(list(models=all_models, subsample=subsample_models,
              coef_tbl=coef_tbl),
        "Data/fe_results.rds")

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
plot_data <- coef_tbl[
  model %in% c("M2","M_pre","M_post") &
  term %in% c(OIL_YOY, OIL_X_DIRECT, OIL_X_SPILL) &
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
plot_data[, model_label := model_labels[model]]

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
break_data <- coef_tbl[
  model %in% c("M_pre","M_post") &
  term == OIL_YOY &
  !is.na(estimate)
]
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
       subtitle = "Direct test of shale revolution structural break | β = ∂outcome/∂PBRENT YoY",
       caption  = "CU FE + Quarter FE | Clustered SE | Controls included",
       x="PBRENT YoY Coefficient", y=NULL) +
  theme_pub()
save_plot(p_break, "04a_03_structural_break_coefs.png", w=11, h=7)

# ── Chart 04a-04: FOMC regime interaction ─────────────────────────────────────
fomc_data <- coef_tbl[
  model == "M3" &
  term %in% c(OIL_YOY, OIL_X_FOMC) &
  dep_var %in% main_dvars &
  !is.na(estimate)
]
fomc_data[, dep_label := dep_labels[dep_var]]
fomc_data[is.na(dep_label), dep_label := dep_var]
fomc_data[, term_label := fifelse(
  term == OIL_YOY,
  "PBRENT YoY\n(FOMC hold baseline)",
  "PBRENT × FOMC Regime\n(hiking=+1, cutting=-1)"
)]
fomc_data[, ci_lo := estimate - 1.96*std_error]
fomc_data[, ci_hi := estimate + 1.96*std_error]

if (nrow(fomc_data) > 0) {
  p_fomc <- ggplot(fomc_data,
                   aes(x=estimate, y=dep_label,
                       colour=term_label, shape=term_label)) +
    geom_vline(xintercept=0, linewidth=0.4, colour="#888") +
    geom_errorbarh(aes(xmin=ci_lo, xmax=ci_hi),
                   height=0.25, linewidth=0.7,
                   position=position_dodge(0.5)) +
    geom_point(size=3.5, position=position_dodge(0.5)) +
    scale_colour_manual(
      values=c("PBRENT YoY\n(FOMC hold baseline)"        = "#1a3a5c",
               "PBRENT × FOMC Regime\n(hiking=+1, cutting=-1)" = "#b5470a"),
      name=NULL) +
    scale_shape_manual(
      values=c("PBRENT YoY\n(FOMC hold baseline)"        = 16,
               "PBRENT × FOMC Regime\n(hiking=+1, cutting=-1)" = 17),
      name=NULL) +
    labs(title    = "FIGURE 04a-04 — FOMC Regime × Oil Shock Interaction (Model M3)",
         subtitle = "Tests: does oil shock effect differ when Fed is hiking vs cutting vs holding?",
         caption  = "CU FE + Quarter FE | Clustered SE | Key finding: rate channel only active post-ZIRP",
         x="Coefficient", y=NULL) +
    theme_pub()
  save_plot(p_fomc, "04a_04_fomc_interaction.png", w=11, h=6)
}

# ── Chart 04a-05: Model progression — R² within ───────────────────────────────
r2_data <- rbindlist(lapply(dep_vars, function(v) {
  mods <- all_models[[v]]
  if (is.null(mods)) return(NULL)
  rbindlist(lapply(c("M1","M2","M3","M4","M5","M6"), function(m) {
    if (is.null(mods[[m]])) return(NULL)
    data.table(
      dep_var    = v,
      model      = m,
      r2_within  = tryCatch(r2(mods[[m]],type="within"), error=function(e) NA),
      n_obs      = tryCatch(nobs(mods[[m]]), error=function(e) NA)
    )
  }))
}))

if (nrow(r2_data[!is.na(r2_within)]) > 0) {
  r2_data[, dep_label := dep_labels[dep_var]]
  r2_data[is.na(dep_label), dep_label := dep_var]

  p_r2 <- ggplot(r2_data[!is.na(r2_within) & dep_var %in% main_dvars],
                 aes(x=model, y=r2_within, colour=dep_label,
                     group=dep_label)) +
    geom_line(linewidth=0.8) +
    geom_point(size=2.5) +
    scale_colour_brewer(palette="Dark2", name="Dep Variable") +
    scale_y_continuous(labels=percent_format(accuracy=0.1)) +
    labs(title    = "FIGURE 04a-05 — Within R² by Model Specification",
         subtitle = "M1=baseline | M2=direct+indirect | M3=+FOMC | M4=+post_shale | M5=full | M6=clean sample",
         caption  = "Within R² after absorbing CU FE + Quarter FE",
         x="Model", y="Within R²") +
    theme_pub()
  save_plot(p_r2, "04a_05_r2_progression.png", w=10, h=6)
}

# =============================================================================
# 8. REGRESSION TABLES
# =============================================================================
hdr("SECTION 8: Regression Tables")

for (v in main_dvars) {
  mods <- all_models[[v]]
  if (is.null(mods)) next

  mod_list <- Filter(Negate(is.null),
                     list(M1=mods$M1, M2=mods$M2, M3=mods$M3,
                          M4=mods$M4, M5=mods$M5, M6=mods$M6))
  if (length(mod_list) == 0) next

  # Coefficient labels
  coef_map <- c(
    macro_base_yoy_oil       = "PBRENT YoY (β₁ indirect)",
    oil_x_brent              = "× Oil-State (β₂ direct)",
    spillover_x_brent        = "× Spillover (β₃)",
    fomc_x_brent             = "× FOMC Regime (β₄)",
    post_x_oil               = "× Post-Shale (β₅)",
    post_x_oil_x_direct      = "× Post×Direct (β₆)",
    macro_base_yoy_oil_lag1  = "PBRENT YoY Lag1",
    macro_base_yoy_oil_lag2  = "PBRENT YoY Lag2",
    macro_base_lurc          = "Unemployment",
    macro_base_yield_curve   = "Yield Curve",
    macro_base_real_rate     = "Real Rate",
    credit_tightness         = "Credit Tightness",
    post_shale               = "Post-Shale Dummy",
    gfc_dummy                = "GFC Dummy",
    covid_dummy              = "COVID Dummy"
  )

  tbl_path <- file.path("Tables", paste0("04a_fe_", v, ".txt"))
  tryCatch({
    modelsummary(
      mod_list,
      coef_map    = coef_map,
      stars       = c("*"=0.1, "**"=0.05, "***"=0.01),
      gof_map     = c("nobs","r.squared","adj.r.squared"),
      output      = tbl_path,
      title       = paste("Panel FE Results:", dep_labels[v] %||% v),
      notes       = "CU FE + Quarter FE | Clustered SE at CU level"
    )
    msg("  Table saved: %s", tbl_path)
  }, error=function(e) {
    msg("  Table error for %s: %s", v, e$message)
  })
}

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
