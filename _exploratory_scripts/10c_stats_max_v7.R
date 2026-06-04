## --------------------------------------------------------------- ##
# CAGED Stats and Analyses — v7
## --------------------------------------------------------------- ##
# PURPOSE:
#   Systematic beta regression models testing how consumer exclusion
#   affects beta dispersion, individually and moderated by ecological
#   predictors. Addresses four project hypotheses (McDevitt-Irwin et al.).
#
# MODEL FAMILY:  beta_family(link = "probit") — glmmTMB
# TRANSFORM:     Smithson-Verkuilen: y* = (y*(n-1) + 0.5) / n
# RANDOM FX:     (1 | var_upper.source / exp.name) throughout
# COVARIATES:    scale(gamma.richness_exp.name) + scale(betadisp.sample.size)
# DISPFORMULA:   Only if DHARMa reveals meaningful heterogeneity AND ΔAIC > 2
#
# MODELS
#   Mod 1 — Overall caging effect
#   Mod 2  — Aquatic vs. terrestrial × caging (with gamma richness covariate)
#   Mod 2b — Aquatic vs. terrestrial × caging (gamma richness dropped; sensitivity check)
#   Mod 3  — Ecosystem type (var_ecotype1) × caging
#   Mod 4 — Successional stage × caging
#   Mod 5 — Exclusion duration × caging      [complete-case subset]
#   Mod 6 — Exclosure area × caging          [complete-case subset; log area]
#   Mod 7  — Consumer metabolism × caging    [known-metabolism subset]
#   Mod 7b — Absolute latitude × caging      [full dataset]
#   Mod 8  — Combined multi-predictor        [complete-case: metabolism + duration + area]
#
# HELPER FUNCTIONS: sourced from 10c_helpers.R
#   summarise_model()       — Type II ANOVA + AIC + R²
#   validate_dharma()       — DHARMa diagnostic plots (overall, dispersion, per-predictor)
#   plot_partial_with_data()— model predictions overlaid on raw data
#   plot_covariate_effects()— covariate partial effects (gamma richness, sample size)

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

librarian::shelf(
  tidyverse, glmmTMB, DHARMa, performance,
  car, broom.mixed, MuMIn, ggeffects,
  emmeans, patchwork, njlyon0/supportR
)

source(file.path("00_setup.R"))

dir.create(file.path("results"),                     showWarnings = FALSE)
dir.create(file.path("graphs", "model_validation"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("graphs", "model_predictions"), showWarnings = FALSE, recursive = TRUE)

rm(list = ls()); gc()

options(scipen = 1)  # fixed notation for ≥1e-4; scientific for ≤1e-5

source(file.path("_exploratory_scripts/10c_helpers.R"))

## ------------------------------------------- ##
# Load Data ----
## ------------------------------------------- ##

caged_beta_raw   <- read.csv(file.path("data", "08_caged_w.meta-beta-disp_finest-scales.csv"))
caged_effectsize <- read.csv(file.path("data", "08_caged_prepped-effect-size.csv"))

dplyr::glimpse(caged_beta_raw)
cat("Raw rows:", nrow(caged_beta_raw), "\n")

## ------------------------------------------- ##
# Prepare Base Dataset ----
## ------------------------------------------- ##

caged_beta <- caged_beta_raw %>%
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>%
  dplyr::filter(var_aq.or.terr != "" & !is.na(var_aq.or.terr)) %>%
  dplyr::filter(!is.na(lat)) %>%
  droplevels() %>%
  dplyr::mutate(
    exclusion.duration = suppressWarnings(as.numeric(var_exclusion.duration.continuousyears)),
    exclosure.area     = suppressWarnings(as.numeric(var_exclosure.area.m2)),
    abs.lat            = abs(lat),
    log.area           = log(exclosure.area)
  ) %>%
  dplyr::mutate(
    cage.treatment_std      = factor(cage.treatment_std, levels = c("uncaged", "caged")),
    var_aq.or.terr          = factor(var_aq.or.terr),
    var_ecotype1            = factor(var_ecotype1),
    var_succ.vs.late        = factor(var_succ.vs.late, levels = c("late", "early")),
    var_consumer.metabolism = factor(var_consumer.metabolism)
  )

# Smithson-Verkuilen transform: y* = (y*(n-1) + 0.5) / n
n_all      <- nrow(caged_beta)
caged_beta <- caged_beta %>%
  dplyr::mutate(betadisp_t = sv_transform(betadisp.comm.dist, n_all))

stopifnot(all(caged_beta$betadisp_t > 0 & caged_beta$betadisp_t < 1))

cat("Base dataset rows:", nrow(caged_beta), "\n")
cat("Sources:",   length(unique(caged_beta$source)),   "\n")
cat("exp.names:", length(unique(caged_beta$exp.name)), "\n")
cat("SV zeros (should be 0):", sum(caged_beta$betadisp_t == 0), "\n")

## ------------------------------------------- ##
# Complete-Case Subsets ----
## ------------------------------------------- ##

# Mod 5 — exclusion duration (drop experiments with unknown duration)
caged_beta_dur <- caged_beta %>%
  dplyr::filter(!is.na(exclusion.duration)) %>%
  droplevels()

# Mod 6 — exclosure area (log-transform; drop Inf from log(0) and unknowns)
caged_beta_area <- caged_beta %>%
  dplyr::filter(!is.na(log.area) & is.finite(log.area)) %>%
  droplevels()

# Mods 7 & 8 — consumer metabolism (drop blank, "both", and NA)
caged_beta_met <- caged_beta %>%
  dplyr::filter(!is.na(var_consumer.metabolism),
                !var_consumer.metabolism %in% c("", "both")) %>%
  dplyr::mutate(var_consumer.metabolism = droplevels(var_consumer.metabolism)) %>%
  droplevels()

cat("Duration subset:   ", nrow(caged_beta_dur),  "rows\n")
cat("Area subset:       ", nrow(caged_beta_area), "rows\n")
cat("Metabolism subset: ", nrow(caged_beta_met),  "rows\n")
cat("Metabolism levels: ", levels(caged_beta_met$var_consumer.metabolism), "\n")

# Mod 8 — complete cases across all predictors in the combined model
# (R drops NAs automatically, but explicit subset ensures DHARMa and
#  partial plots use the same rows as the fitted model)
caged_beta_mod8 <- caged_beta_met %>%
  dplyr::filter(
    !is.na(exclusion.duration),
    !is.na(log.area) & is.finite(log.area),
    !is.na(var_succ.vs.late) & var_succ.vs.late != "",
    !is.na(gamma.richness_exp.name),
    !is.na(betadisp.sample.size)
  ) %>%
  droplevels()
cat("Mod 8 complete-case subset:", nrow(caged_beta_mod8), "rows,",
    length(unique(caged_beta_mod8$exp.name)), "experiments\n")

## ------------------------------------------- ##
# Shared Settings ----
## ------------------------------------------- ##

ctrl <- glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))

## ======================================================== ##
# INDIVIDUAL MODELS ----
## ======================================================== ##

## ------------------------------------------- ##
# Mod 1 — Overall Caging Effect ----
## ------------------------------------------- ##
# H1: Consumer loss increases beta diversity (community variability).

mod1 <- glmmTMB(
  betadisp_t ~
    cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

summarise_model(mod1, "Mod1_Overall_Caging")
validate_dharma(mod1, caged_beta, "mod1_overall")
plot_partial_with_data(mod1, caged_beta,
  terms  = "cage.treatment_std",
  prefix = "mod1_overall",
  x_lab  = "Cage treatment")
plot_covariate_effects(mod1, caged_beta, "mod1_overall")

## ------------------------------------------- ##
# Mod 2 — Aquatic vs. Terrestrial × Caging ----
## ------------------------------------------- ##
# H1 extension: caging effect stronger in aquatic (stronger top-down control).

mod2 <- glmmTMB(
  betadisp_t ~
    var_aq.or.terr * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

# If DHARMa shows heterogeneity by ecosystem type, consider refitting with
#   dispformula = ~ var_aq.or.terr — only add if ΔAIC > 2

summarise_model(mod2, "Mod2_AqTerr_x_Caging")
validate_dharma(mod2, caged_beta, "mod2_aqterr",
  pred_vars = "var_aq.or.terr")
plot_partial_with_data(mod2, caged_beta,
  terms  = c("cage.treatment_std", "var_aq.or.terr"),
  prefix = "mod2_aqterr",
  x_lab  = "Cage treatment")
plot_covariate_effects(mod2, caged_beta, "mod2_aqterr")

## ------------------------------------------- ##
# Mod 2b — AqTerr × Caging (gamma richness dropped) ----
## ------------------------------------------- ##
# Gamma richness is substantially higher in terrestrial than aquatic systems
# (~100 vs ~50 species per experiment). Because species pool size co-varies
# with ecosystem type, including scale(gamma.richness_exp.name) as a covariate
# in Mod 2 may partial out real aq/terr signal. Mod 2b removes it to show
# whether the aq/terr × caging effect is suppressed or mediated by gamma richness.

# Summarise the confound before fitting
cat("\n--- Gamma richness by ecosystem type (motivating Mod 2b) ---\n")
caged_beta %>%
  dplyr::filter(!is.na(gamma.richness_exp.name)) %>%
  dplyr::group_by(var_aq.or.terr) %>%
  dplyr::summarise(
    n_rows        = dplyr::n(),
    n_experiments = dplyr::n_distinct(exp.name),
    mean_gamma    = round(mean(gamma.richness_exp.name),   1),
    median_gamma  = round(median(gamma.richness_exp.name), 1),
    .groups = "drop"
  ) %>%
  print()

mod2b <- glmmTMB(
  betadisp_t ~
    var_aq.or.terr * cage.treatment_std +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

summarise_model(mod2b, "Mod2b_AqTerr_x_Caging_NoGamma")
validate_dharma(mod2b, caged_beta, "mod2b_aqterr",
  pred_vars = "var_aq.or.terr")
plot_partial_with_data(mod2b, caged_beta,
  terms  = c("cage.treatment_std", "var_aq.or.terr"),
  prefix = "mod2b_aqterr",
  x_lab  = "Cage treatment")
# plot_covariate_effects not called — gamma richness excluded by design

## ------------------------------------------- ##
# Mod 3 — Ecosystem Type (ecotype1) × Caging ----
## ------------------------------------------- ##
# H1 extension: finer ecosystem classification resolves variation in caging effect.
# Note: ecotype on x-axis (13 levels), cage treatment as color grouping.

mod3 <- glmmTMB(
  betadisp_t ~
    var_ecotype1 * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

summarise_model(mod3, "Mod3_Ecotype_x_Caging")
validate_dharma(mod3, caged_beta, "mod3_ecotype",
  pred_vars = "var_ecotype1")
# x = ecotype (13 levels on x-axis), group = cage treatment (2 colors)
plot_partial_with_data(mod3, caged_beta,
  terms  = c("var_ecotype1", "cage.treatment_std"),
  prefix = "mod3_ecotype",
  x_lab  = "Ecosystem type",
  width  = 13, height = 6)
plot_covariate_effects(mod3, caged_beta, "mod3_ecotype")

## ------------------------------------------- ##
# Mod 4 — Successional Stage × Caging ----
## ------------------------------------------- ##
# H4: Consumers have stronger effect after disturbance; early-successional
#     communities more susceptible to top-down control.
# Reference level = "late" (undisturbed baseline).

mod4 <- glmmTMB(
  betadisp_t ~
    var_succ.vs.late * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

summarise_model(mod4, "Mod4_Succession_x_Caging")
validate_dharma(mod4, caged_beta, "mod4_succ",
  pred_vars = "var_succ.vs.late")
plot_partial_with_data(mod4, caged_beta,
  terms  = c("cage.treatment_std", "var_succ.vs.late"),
  prefix = "mod4_succ",
  x_lab  = "Cage treatment")
plot_covariate_effects(mod4, caged_beta, "mod4_succ")

## ------------------------------------------- ##
# Mod 5 — Exclusion Duration × Caging ----
## ------------------------------------------- ##
# H4: Longer exclusion → more stochastic assembly → stronger caging effect.
# Complete-case subset (known duration only).

mod5 <- glmmTMB(
  betadisp_t ~
    scale(exclusion.duration) * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_dur
)

summarise_model(mod5, "Mod5_Duration_x_Caging")
validate_dharma(mod5, caged_beta_dur, "mod5_duration",
  pred_vars = "exclusion.duration")
plot_partial_with_data(mod5, caged_beta_dur,
  terms  = c("exclusion.duration [n=200]", "cage.treatment_std"),
  prefix = "mod5_duration",
  x_lab  = "Exclusion duration (years)")
plot_covariate_effects(mod5, caged_beta_dur, "mod5_duration")

## ------------------------------------------- ##
# Mod 6 — Exclosure Area × Caging ----
## ------------------------------------------- ##
# H3: Spatial scale moderates caging effect.
# Log-transformed area; complete-case subset.

mod6 <- glmmTMB(
  betadisp_t ~
    scale(log.area) * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_area
)

summarise_model(mod6, "Mod6_Area_x_Caging")
validate_dharma(mod6, caged_beta_area, "mod6_area",
  pred_vars = "log.area")
plot_partial_with_data(mod6, caged_beta_area,
  terms  = c("log.area [n=200]", "cage.treatment_std"),
  prefix = "mod6_area",
  x_lab  = "Log exclosure area (log m²)")
plot_covariate_effects(mod6, caged_beta_area, "mod6_area")

## ------------------------------------------- ##
# Mod 7 — Consumer Metabolism × Caging ----
## ------------------------------------------- ##
# H2: Endotherms vs. ectotherms differ in top-down control strength.
# Known-metabolism subset (blank and "both" excluded).

mod7 <- glmmTMB(
  betadisp_t ~
    var_consumer.metabolism * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_met
)

summarise_model(mod7, "Mod7_Metabolism_x_Caging")
validate_dharma(mod7, caged_beta_met, "mod7_metabolism",
  pred_vars = "var_consumer.metabolism")
plot_partial_with_data(mod7, caged_beta_met,
  terms  = c("cage.treatment_std", "var_consumer.metabolism"),
  prefix = "mod7_metabolism",
  x_lab  = "Cage treatment")
plot_covariate_effects(mod7, caged_beta_met, "mod7_metabolism")

## ------------------------------------------- ##
# Mod 7b — Absolute Latitude × Caging ----
## ------------------------------------------- ##
# Latitudinal gradient in caging effect: does consumer influence on beta
# dispersion change with distance from the equator?
# abs.lat is complete for all rows (lat already filtered at data prep).

mod7b <- glmmTMB(
  betadisp_t ~
    scale(abs.lat) * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

summarise_model(mod7b, "Mod7b_Latitude_x_Caging")
validate_dharma(mod7b, caged_beta, "mod7b_latitude",
  pred_vars = "abs.lat")
plot_partial_with_data(mod7b, caged_beta,
  terms  = c("abs.lat [n=200]", "cage.treatment_std"),
  prefix = "mod7b_latitude",
  x_lab  = "Absolute latitude (°)")
plot_covariate_effects(mod7b, caged_beta, "mod7b_latitude")

## ======================================================== ##
# MULTI-PREDICTOR MODEL ----
## ======================================================== ##

## ------------------------------------------- ##
# Mod 8 — Combined Multi-Predictor ----
## ------------------------------------------- ##
# All individual moderators × caging in one model.
# Covariates (gamma richness, sample size) as main effects only.
# Data: complete cases from caged_beta_met (known metabolism +
#       known duration + known area) — caged_beta_mod8.
# VIF checked on a main-effects-only version of Mod 8 (interactions inflate VIF
# artifactually). All predictors < 5 — no collinearity concerns.

mod8 <- glmmTMB(
  betadisp_t ~
    cage.treatment_std * var_aq.or.terr +
    cage.treatment_std * var_succ.vs.late +
    cage.treatment_std * var_consumer.metabolism +
    cage.treatment_std * scale(exclusion.duration) +
    cage.treatment_std * scale(abs.lat) +
    cage.treatment_std * scale(log.area) +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  #dispformula = ~ abs.lat + var_aq.or.terr,
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_mod8
)


mod8 <- glmmTMB(
  betadisp_t ~
    cage.treatment_std * var_aq.or.terr +
    cage.treatment_std * var_succ.vs.late +
    cage.treatment_std * var_consumer.metabolism +
    cage.treatment_std * scale(exclusion.duration) +
    cage.treatment_std * scale(abs.lat) +
    cage.treatment_std * scale(log.area) +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  #dispformula = ~ abs.lat + var_aq.or.terr,
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_mod8
)

summarise_model(mod8, "Mod8_Combined_All")

mod8.for.vif <- glmmTMB(
  betadisp_t ~
    var_aq.or.terr +
    var_succ.vs.late +
    var_consumer.metabolism +
    scale(exclusion.duration) +
    scale(abs.lat) +
    scale(log.area) +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_mod8
)

# VIF with automatic flagging
cat("\n--- VIF (Mod 8 — NO INTERACTION TERMS) ---\n")
vif_tbl <- tryCatch(as.data.frame(check_collinearity(mod8.for.vif)), error = function(e) NULL)
if (!is.null(vif_tbl)) {
  print(vif_tbl, digits = 3)
  vif_col   <- names(vif_tbl)[grep("^VIF$", names(vif_tbl))[1]]
  term_col  <- if ("Term" %in% names(vif_tbl)) "Term" else names(vif_tbl)[1]
  vif_vals  <- vif_tbl[[vif_col]]
  vif_terms <- vif_tbl[[term_col]]
  high <- vif_terms[!is.na(vif_vals) & vif_vals >= 10]
  mod  <- vif_terms[!is.na(vif_vals) & vif_vals >= 5 & vif_vals < 10]
  if (length(high) > 0) cat("\n!! HIGH VIF (>=10):", paste(high, collapse = ", "), "\n")
  if (length(mod)  > 0) cat("\n!  MODERATE VIF (5-10):", paste(mod,  collapse = ", "), "\n")
  if (length(c(high, mod)) == 0) cat("\nAll VIF < 5 — no collinearity concerns.\n")
}

validate_dharma(mod8, caged_beta_mod8, "mod8_combined",
  pred_vars = c("var_aq.or.terr", "var_succ.vs.late", "var_consumer.metabolism",
                "exclusion.duration", "abs.lat", "log.area"))

# Mod 8 partial effects — collect panels, then assemble multipanel figures.
# save = FALSE suppresses individual PNGs; combined figures are saved below.
p8_aqterr <- plot_partial_with_data(mod8, caged_beta_mod8,
  terms  = c("cage.treatment_std", "var_aq.or.terr"),
  prefix = "mod8_aqterr", x_lab = "Cage treatment", save = FALSE)
p8_succ <- plot_partial_with_data(mod8, caged_beta_mod8,
  terms  = c("cage.treatment_std", "var_succ.vs.late"),
  prefix = "mod8_succ", x_lab = "Cage treatment", save = FALSE)
p8_metab <- plot_partial_with_data(mod8, caged_beta_mod8,
  terms  = c("cage.treatment_std", "var_consumer.metabolism"),
  prefix = "mod8_metabolism", x_lab = "Cage treatment", save = FALSE)
p8_dur <- plot_partial_with_data(mod8, caged_beta_mod8,
  terms  = c("exclusion.duration [n=200]", "cage.treatment_std"),
  prefix = "mod8_duration", x_lab = "Exclusion duration (years)", save = FALSE)
p8_lat <- plot_partial_with_data(mod8, caged_beta_mod8,
  terms  = c("abs.lat [n=200]", "cage.treatment_std"),
  prefix = "mod8_latitude", x_lab = "Absolute latitude (°)", save = FALSE)
p8_area <- plot_partial_with_data(mod8, caged_beta_mod8,
  terms  = c("log.area [n=200]", "cage.treatment_std"),
  prefix = "mod8_area", x_lab = "Log exclosure area (log m²)", save = FALSE)

# Multipanel: categorical moderators × caging (3-panel row)
p8_cat <- (p8_aqterr | p8_succ | p8_metab) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave(file.path("graphs/model_predictions", "mod8_categorical_partial.png"),
       p8_cat, width = 15, height = 5, dpi = 300)

# Multipanel: continuous moderators × caging (3-panel row, shared cage legend)
p8_cont <- (p8_dur | p8_lat | p8_area) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave(file.path("graphs/model_predictions", "mod8_continuous_partial.png"),
       p8_cont, width = 15, height = 5, dpi = 300)

# Covariates panel (gamma richness + sample size)
plot_covariate_effects(mod8, caged_beta_mod8, "mod8_combined")

## ======================================================== ##
# Model Comparison Summary ----
## ======================================================== ##

# AIC and R² for full-dataset models only (fitted on caged_beta).
# Mods 5, 6, 7, 7b use different subsets and are noted separately.
# Mod 8 uses caged_beta_mod8 (complete cases) and is not AIC-comparable.

base_models <- list(
  Mod1_Overall       = mod1,
  Mod2_AqTerr        = mod2,
  Mod2b_AqTerr_NoGam = mod2b,
  Mod3_Ecotype       = mod3,
  Mod4_Succession    = mod4,
  Mod7b_Latitude     = mod7b
)

model_summary_tbl <- purrr::map_dfr(names(base_models), function(nm) {
  m  <- base_models[[nm]]
  r2 <- tryCatch(r.squaredGLMM(m),
                 error = function(e) matrix(NA, 1, 2,
                   dimnames = list(NULL, c("R2m", "R2c"))))
  data.frame(
    model          = nm,
    AIC            = round(AIC(m), 2),
    R2_marginal    = round(r2[1, "R2m"], 4),
    R2_conditional = round(r2[1, "R2c"], 4)
  )
})

print(model_summary_tbl)
write.csv(model_summary_tbl, row.names = FALSE,
  file = file.path("results", "model_comparison_summary.csv"))

# End ----
