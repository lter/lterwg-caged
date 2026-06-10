## --------------------------------------------------------------- ##
# CAGED Stats and Analyses — v10
## --------------------------------------------------------------- ##
# PURPOSE:
#   Systematic beta regression models testing how consumer exclusion
#   affects beta dispersion, moderated by ecological predictors.
#   Addresses project hypotheses (McDevitt-Irwin et al.).
#
# CHANGES FROM v9:
#   - Data file switched to 08-A_caged_w.meta-beta-disp_fine-scales.csv
#   - var_exclosure.area.m2 replaced by var_plot.size throughout;
#     derived variable renamed log.plot.size
#   - var_aq.or.terr factor reordered: aquatic → transitional → terrestrial
#   - caged_beta_aq renamed caged_beta_aq_trans (now aquatic + transitional)
#   - New caged_beta_trans subset (transitional only) for Mod 7
#   - Mod 5 (taxonomy) uses caged_beta_aq_trans_tax (aquatic + transitional)
#   - Mod 7 (metabolism) uses caged_beta_trans_met (transitional only)
#
# MODEL FAMILY:  beta_family(link = "probit") — glmmTMB
# TRANSFORM:     Smithson-Verkuilen: y* = (y*(n-1) + 0.5) / n
# RANDOM FX:     (1 | var_upper.source / exp.name) for full-dataset models;
#                (1 | exp.name) for aquatic/transitional subsets (too few
#                upper-level groups for nested structure)
#
# STANDARD COVARIATES (all models):
#   gamma_rich_s
#   betadisp_ss_s
#   excl_dur_s       [from var_exclusion.duration.continuousyears]
#   abs_lat_s
#   log_plot_s            [log(var_plot.size)]
#
# MODELS
#   Mod 1 — Overall caging effect                             [full dataset]
#   Mod 2 — Aquatic vs. terrestrial × caging                 [full dataset]
#   Mod 3a — Ecosystem type × caging                         [aquatic only]
#   Mod 3b — Ecosystem type × caging                      [transitional only]
#   Mod 3c — Ecosystem type × caging                      [terrestrial only]
#   Mod 4 — Successional stage × caging            [aquatic + transitional]
#   Mod 5 — Consumer taxonomy × caging          [aquatic + transitional only]
#   Mod 6 — Consumer richness × caging                       [full dataset]
#   Mod 7 — Consumer metabolism × caging             [transitional only]
#
# HELPER FUNCTIONS: sourced from 10c_helpers.R

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

librarian::shelf(
  tidyverse, glmmTMB, DHARMa, performance,
  car, broom.mixed, MuMIn, ggeffects,
  emmeans, patchwork, lme4, njlyon0/supportR
)

source("00_setup.R")

dir.create("results",                                        showWarnings = FALSE)
dir.create(file.path("graphs", "model_validation"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("graphs", "model_predictions"), showWarnings = FALSE, recursive = TRUE)

rm(list = ls()); gc()

options(scipen = 1)

source(file.path("_exploratory_scripts", "10c_helpers.R"))

## ------------------------------------------- ##
# Load Data ----
## ------------------------------------------- ##

caged_beta_raw <- read.csv(file.path("data", "08-A_caged_w.meta-beta-disp_fine-scales.csv"))

dplyr::glimpse(caged_beta_raw)
cat("Raw rows:", nrow(caged_beta_raw), "\n")

## ------------------------------------------- ##
# Prepare Base Dataset ----
## ------------------------------------------- ##

caged_beta_all <- caged_beta_raw %>%
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>%
  dplyr::filter(var_aq.or.terr != "" & !is.na(var_aq.or.terr)) %>%
  dplyr::filter(!is.na(lat)) %>%
  droplevels() %>%
  dplyr::mutate(
    exclusion.duration           = suppressWarnings(as.numeric(var_exclusion.duration.continuousyears)),
    plot.size                    = suppressWarnings(as.numeric(var_plot.size)),
    var_consumer.richness.number = suppressWarnings(as.numeric(var_consumer.richness.number)),
    abs.lat                      = abs(lat),
    log.plot.size                = log(plot.size)
  ) %>%
  dplyr::mutate(
    cage.treatment_std      = factor(cage.treatment_std, levels = c("uncaged", "caged")),
    var_aq.or.terr          = factor(var_aq.or.terr,
                                     levels = c("aquatic", "transitional", "terrestrial")),
    var_ecotype1            = factor(var_ecotype1),
    var_succ.vs.late        = factor(var_succ.vs.late, levels = c("late", "early")),
    var_consumer.metabolism = factor(var_consumer.metabolism),
    var_consumer.taxonomy   = factor(var_consumer.taxonomy)
  )

## ------------------------------------------- ##
# Complete-Case Base Dataset ----
## ------------------------------------------- ##
# All models include excl_dur_s and log_plot_s, so rows
# missing either are dropped here once rather than silently per model.

caged_beta <- caged_beta_all %>%
  dplyr::filter(
    !is.na(exclusion.duration),
    !is.na(log.plot.size) & is.finite(log.plot.size)
  ) %>%
  droplevels()

n_cc       <- nrow(caged_beta)
caged_beta <- caged_beta %>%
  dplyr::mutate(betadisp_t = sv_transform(betadisp.comm.dist, n_cc))

stopifnot(all(caged_beta$betadisp_t > 0 & caged_beta$betadisp_t < 1))

# Pre-compute Gelman 2-SD scaled predictors on the full complete-case dataset.
# Using pre-computed columns (rather than inline scale2() in formulas) ensures
# ggpredict can generate newdata grids without NA issues.
# Scaling is always relative to the full caged_beta dataset so coefficients
# remain comparable across full-dataset and subset models.
caged_beta <- caged_beta %>%
  dplyr::mutate(
    gamma_rich_s    = scale2(gamma.richness_exp.name),
    betadisp_ss_s   = scale2(betadisp.sample.size),
    excl_dur_s      = scale2(exclusion.duration),
    abs_lat_s       = scale2(abs.lat),
    log_plot_s      = scale2(log.plot.size),
    consumer_rich_s = scale2(var_consumer.richness.number)
  )

cat("\n--- Dataset summary ---\n")
cat("All rows (caged/uncaged, lat + aq/terr present):", nrow(caged_beta_all), "\n")
cat("Complete-case rows (+ known duration + known plot size):", nrow(caged_beta), "\n")
cat("  Sources:    ", length(unique(caged_beta$source)),   "\n")
cat("  exp.names:  ", length(unique(caged_beta$exp.name)), "\n")
cat("  SV zeros (should be 0):", sum(caged_beta$betadisp_t == 0), "\n")

## ------------------------------------------- ##
# Ecosystem Subsets ----
## ------------------------------------------- ##

# Helper: re-scale covariates within a subset so the 2-SD scaling reflects the
# variation that actually exists in that data. Avoids inflated coefficients and
# SEs caused by a predictor having near-zero within-subset variance when scaled
# to the full-dataset SD (e.g., exclusion.duration: full SD = 23 yrs but aquatic
# studies max out at 4 yrs, so full-dataset scaling compresses them to ~0.08 units).
rescale_covariates <- function(df) {
  df %>%
    dplyr::mutate(
      gamma_rich_s    = scale2(gamma.richness_exp.name),
      betadisp_ss_s   = scale2(betadisp.sample.size),
      excl_dur_s      = scale2(exclusion.duration),
      abs_lat_s       = scale2(abs.lat),
      log_plot_s      = scale2(log.plot.size),
      consumer_rich_s = scale2(var_consumer.richness.number)
    )
}

# Aquatic only — used for Mod 3a
caged_beta_aq <- caged_beta %>%
  dplyr::filter(var_aq.or.terr == "aquatic") %>%
  droplevels() %>%
  rescale_covariates()

cat("\nAquatic-only subset:", nrow(caged_beta_aq), "rows,",
    length(unique(caged_beta_aq$exp.name)), "experiments,",
    length(unique(caged_beta_aq$var_upper.source)), "sources\n")

# Transitional only — used for Mods 3b and 7
caged_beta_trans <- caged_beta %>%
  dplyr::filter(var_aq.or.terr == "transitional") %>%
  droplevels() %>%
  rescale_covariates()

cat("Transitional-only subset:", nrow(caged_beta_trans), "rows,",
    length(unique(caged_beta_trans$exp.name)), "experiments,",
    length(unique(caged_beta_trans$var_upper.source)), "sources\n")

# Terrestrial only — used for Mod 3c
caged_beta_terr <- caged_beta %>%
  dplyr::filter(var_aq.or.terr == "terrestrial") %>%
  droplevels() %>%
  rescale_covariates()

cat("Terrestrial-only subset:", nrow(caged_beta_terr), "rows,",
    length(unique(caged_beta_terr$exp.name)), "experiments,",
    length(unique(caged_beta_terr$var_upper.source)), "sources\n")

# Aquatic + transitional — used for Mods 4 and 5
caged_beta_aq_trans <- caged_beta %>%
  dplyr::filter(var_aq.or.terr %in% c("aquatic", "transitional")) %>%
  droplevels() %>%
  rescale_covariates()

cat("Aquatic + transitional subset:", nrow(caged_beta_aq_trans), "rows,",
    length(unique(caged_beta_aq_trans$exp.name)), "experiments\n")

# Mod 5: taxonomy subset from aquatic + transitional (drop blank/NA)
caged_beta_aq_trans_tax <- caged_beta_aq_trans %>%
  dplyr::filter(!is.na(var_consumer.taxonomy),
                var_consumer.taxonomy != "") %>%
  dplyr::mutate(var_consumer.taxonomy = droplevels(var_consumer.taxonomy)) %>%
  droplevels()

cat("Aq+trans taxonomy subset:", nrow(caged_beta_aq_trans_tax), "rows,",
    length(unique(caged_beta_aq_trans_tax$exp.name)), "experiments\n")
cat("Taxonomy levels:", levels(caged_beta_aq_trans_tax$var_consumer.taxonomy), "\n")

# Mod 7: metabolism subset from transitional only (drop blank, "both", NA)
caged_beta_trans_met <- caged_beta_trans %>%
  dplyr::filter(!is.na(var_consumer.metabolism),
                !var_consumer.metabolism %in% c("", "both")) %>%
  dplyr::mutate(var_consumer.metabolism = droplevels(var_consumer.metabolism)) %>%
  droplevels()

cat("Trans metabolism subset:", nrow(caged_beta_trans_met), "rows,",
    length(unique(caged_beta_trans_met$exp.name)), "experiments\n")
cat("Metabolism levels:", levels(caged_beta_trans_met$var_consumer.metabolism), "\n")

## ------------------------------------------- ##
# Shared Settings ----
## ------------------------------------------- ##

ctrl <- glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))

COVARIATE_TERMS <- list(
  list(term = "gamma_rich_s [n=150]",
       xlab = "Gamma richness (exp.name)",           color = "#0072B2"),
  list(term = "betadisp_ss_s [n=150]",
       xlab = "Betadisp sample size (# replicates)", color = "#D55E00"),
  list(term = "excl_dur_s [n=150]",
       xlab = "Exclusion duration (years)",          color = "#009E73"),
  list(term = "abs_lat_s [n=150]",
       xlab = "Absolute latitude (°)",               color = "#CC79A7"),
  list(term = "log_plot_s [n=150]",
       xlab = "Log plot size (log m²)",              color = "#E69F00")
)

make_covariate_terms <- function(anova_tbl) {
  lapply(COVARIATE_TERMS, function(ct) {
    raw_var    <- trimws(sub("\\s*\\[.*\\]", "", ct$term))
    ct$p_value <- lookup_p(anova_tbl, raw_var)
    ct
  })
}

## ======================================================== ##
# MODELS ----
## ======================================================== ##

## ------------------------------------------- ##
# Mod 1 — Overall Caging Effect ----
## ------------------------------------------- ##

mod1 <- glmmTMB(
  betadisp_t ~
    cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

at1 <- summarise_model(mod1, "Mod1_Overall_Caging")
validate_dharma(mod1, caged_beta, "mod1_overall")
check_model_vif(mod1, caged_beta, "Mod1_Overall_Caging")
plot_partial_with_data(mod1, caged_beta,
  terms   = "cage.treatment_std",
  prefix  = "mod1_overall",
  x_lab   = "Cage treatment",
  p_value = lookup_p(at1, "cage.treatment_std"))
plot_covariate_effects(mod1, caged_beta, "mod1_overall",
  covariate_terms = make_covariate_terms(at1))

## ------------------------------------------- ##
# Mod 2 — Aquatic vs. Terrestrial × Caging ----
## ------------------------------------------- ##

mod2 <- glmmTMB(
  betadisp_t ~
    var_aq.or.terr * cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

at2 <- summarise_model(mod2, "Mod2_AqTerr_x_Caging")
validate_dharma(mod2, caged_beta, "mod2_aqterr",
  pred_vars = "var_aq.or.terr")
check_model_vif(mod2, caged_beta, "Mod2_AqTerr_x_Caging")
plot_partial_with_data(mod2, caged_beta,
  terms   = c("var_aq.or.terr", "cage.treatment_std"),
  prefix  = "mod2_aqterr",
  x_lab   = "Ecosystem type",
  p_value = lookup_p(at2, "var_aq.or.terr:cage.treatment_std"))
plot_covariate_effects(mod2, caged_beta, "mod2_aqterr",
  covariate_terms = make_covariate_terms(at2))

## ------------------------------------------- ##
# Mod 3a — Ecotype × Caging (aquatic only) ----
## ------------------------------------------- ##

mod3a <- glmmTMB(
  betadisp_t ~
    var_ecotype1 * cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_aq
)

at3a <- summarise_model(mod3a, "Mod3a_Ecotype_x_Caging_AqOnly")
validate_dharma(mod3a, caged_beta_aq, "mod3a_ecotype_aq",
  pred_vars = "var_ecotype1")
check_model_vif(mod3a, caged_beta_aq, "Mod3a_Ecotype_x_Caging_AqOnly")
plot_partial_with_data(mod3a, caged_beta_aq,
  terms   = c("var_ecotype1", "cage.treatment_std"),
  prefix  = "mod3a_ecotype_aq",
  x_lab   = "Ecosystem type (aquatic)",
  width   = 11, height = 6,
  p_value = lookup_p(at3a, "var_ecotype1:cage.treatment_std"))
plot_covariate_effects(mod3a, caged_beta_aq, "mod3a_ecotype_aq",
  covariate_terms = make_covariate_terms(at3a))

## ------------------------------------------- ##
# Mod 3b — Ecotype × Caging (transitional only) ----
## ------------------------------------------- ##
# Simplified RE: (1 | exp.name) — too few upper-level sources in this subset.

mod3b <- glmmTMB(
  betadisp_t ~
    var_ecotype1 * cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_trans
)

at3b <- summarise_model(mod3b, "Mod3b_Ecotype_x_Caging_TransOnly")
validate_dharma(mod3b, caged_beta_trans, "mod3b_ecotype_trans",
  pred_vars = "var_ecotype1")
check_model_vif(mod3b, caged_beta_trans, "Mod3b_Ecotype_x_Caging_TransOnly")
plot_partial_with_data(mod3b, caged_beta_trans,
  terms   = c("var_ecotype1", "cage.treatment_std"),
  prefix  = "mod3b_ecotype_trans",
  x_lab   = "Ecosystem type (transitional)",
  width   = 11, height = 6,
  p_value = lookup_p(at3b, "var_ecotype1:cage.treatment_std"))
plot_covariate_effects(mod3b, caged_beta_trans, "mod3b_ecotype_trans",
  covariate_terms = make_covariate_terms(at3b))

## ------------------------------------------- ##
# Mod 3c — Ecotype × Caging (terrestrial only) ----
## ------------------------------------------- ##

mod3c <- glmmTMB(
  betadisp_t ~
    var_ecotype1 * cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_terr
)

at3c <- summarise_model(mod3c, "Mod3c_Ecotype_x_Caging_TerrOnly")
validate_dharma(mod3c, caged_beta_terr, "mod3c_ecotype_terr",
  pred_vars = "var_ecotype1")
check_model_vif(mod3c, caged_beta_terr, "Mod3c_Ecotype_x_Caging_TerrOnly")
plot_partial_with_data(mod3c, caged_beta_terr,
  terms   = c("var_ecotype1", "cage.treatment_std"),
  prefix  = "mod3c_ecotype_terr",
  x_lab   = "Ecosystem type (terrestrial)",
  width   = 11, height = 6,
  p_value = lookup_p(at3c, "var_ecotype1:cage.treatment_std"))
plot_covariate_effects(mod3c, caged_beta_terr, "mod3c_ecotype_terr",
  covariate_terms = make_covariate_terms(at3c))

## ------------------------------------------- ##
# Mod 4 — Successional Stage × Caging (aquatic + transitional) ----
## ------------------------------------------- ##
# Restricted to aquatic + transitional; terrestrial excluded.

mod4 <- glmmTMB(
  betadisp_t ~
    var_succ.vs.late * cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_aq_trans
)

at4 <- summarise_model(mod4, "Mod4_Succession_x_Caging")
validate_dharma(mod4, caged_beta_aq_trans, "mod4_succ",
  pred_vars = "var_succ.vs.late")
check_model_vif(mod4, caged_beta_aq_trans, "Mod4_Succession_x_Caging")
plot_partial_with_data(mod4, caged_beta_aq_trans,
  terms   = c("cage.treatment_std", "var_succ.vs.late"),
  prefix  = "mod4_succ",
  x_lab   = "Cage treatment",
  p_value = lookup_p(at4, "var_succ.vs.late:cage.treatment_std"))
plot_covariate_effects(mod4, caged_beta_aq_trans, "mod4_succ",
  covariate_terms = make_covariate_terms(at4))

## ------------------------------------------- ##
# Mod 5 — Consumer Taxonomy × Caging (aquatic + transitional) ----
## ------------------------------------------- ##
# Simplified RE: (1 | exp.name) — too few upper-level sources in this subset
# for the full nested structure.

mod5 <- glmmTMB(
  betadisp_t ~
    var_consumer.taxonomy * cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_aq_trans_tax
)

at5 <- summarise_model(mod5, "Mod5_Taxonomy_x_Caging_AqTransOnly")
validate_dharma(mod5, caged_beta_aq_trans_tax, "mod5_taxonomy",
  pred_vars = "var_consumer.taxonomy")
check_model_vif(mod5, caged_beta_aq_trans_tax, "Mod5_Taxonomy_x_Caging_AqTransOnly")
plot_partial_with_data(mod5, caged_beta_aq_trans_tax,
  terms   = c("var_consumer.taxonomy", "cage.treatment_std"),
  prefix  = "mod5_taxonomy",
  x_lab   = "Consumer taxonomy",
  width   = 11, height = 6,
  p_value = lookup_p(at5, "var_consumer.taxonomy:cage.treatment_std"))
plot_covariate_effects(mod5, caged_beta_aq_trans_tax, "mod5_taxonomy",
  covariate_terms = make_covariate_terms(at5))

## ------------------------------------------- ##
# Mod 6 — Consumer Richness × Caging ----
## ------------------------------------------- ##

mod6 <- glmmTMB(
  betadisp_t ~
    consumer_rich_s * cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

at6 <- summarise_model(mod6, "Mod6_ConsumerRichness_x_Caging")
validate_dharma(mod6, caged_beta, "mod6_consumer_richness",
  pred_vars = "consumer_rich_s")
check_model_vif(mod6, caged_beta, "Mod6_ConsumerRichness_x_Caging")
plot_partial_with_data(mod6, caged_beta,
  terms   = c("consumer_rich_s [n=200]", "cage.treatment_std"),
  prefix  = "mod6_consumer_richness",
  x_lab   = "Consumer richness (2-SD scaled)",
  p_value = lookup_p(at6, "consumer_rich_s:cage.treatment_std"))
plot_covariate_effects(mod6, caged_beta, "mod6_consumer_richness",
  covariate_terms = make_covariate_terms(at6))

## ------------------------------------------- ##
# Mod 7 — Consumer Metabolism × Caging (transitional only) ----
## ------------------------------------------- ##
# Simplified RE: (1 | exp.name) — too few upper-level sources in this subset
# for the full nested structure.

mod7 <- glmmTMB(
  betadisp_t ~
    var_consumer.metabolism * cage.treatment_std +
    gamma_rich_s +
    betadisp_ss_s +
    excl_dur_s +
    abs_lat_s +
    log_plot_s +
    (1 | exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_trans_met
)

at7 <- summarise_model(mod7, "Mod7_Metabolism_x_Caging_TransOnly")
validate_dharma(mod7, caged_beta_trans_met, "mod7_metabolism",
  pred_vars = "var_consumer.metabolism")
check_model_vif(mod7, caged_beta_trans_met, "Mod7_Metabolism_x_Caging_TransOnly")
plot_partial_with_data(mod7, caged_beta_trans_met,
  terms   = c("cage.treatment_std", "var_consumer.metabolism"),
  prefix  = "mod7_metabolism",
  x_lab   = "Cage treatment",
  p_value = lookup_p(at7, "var_consumer.metabolism:cage.treatment_std"))
plot_covariate_effects(mod7, caged_beta_trans_met, "mod7_metabolism",
  covariate_terms = make_covariate_terms(at7))

## ======================================================== ##
# Figure 1: Standardized Coefficient Plot (Variable-Centered) ----
## ======================================================== ##
# Y-axis = predictor variable; color = model.
# Covariates appear in all models so each has 9 colored points.
# Focal variable / interaction terms are model-specific.
# Two panels: (A) Caging + covariates; (B) Focal variable + interaction.

model_order <- c("Mod 1","Mod 2","Mod 3a","Mod 3b","Mod 3c",
                 "Mod 4","Mod 5","Mod 6","Mod 7")

# Extract all fixed-effect coefficients from all models
coef_specs_fig1 <- list(
  list(model = mod1,  label = "Mod 1",  focal_re = NA_character_),
  list(model = mod2,  label = "Mod 2",  focal_re = "^var_aq\\.or\\.terr"),
  list(model = mod3a, label = "Mod 3a", focal_re = "^var_ecotype1"),
  list(model = mod3b, label = "Mod 3b", focal_re = "^var_ecotype1"),
  list(model = mod3c, label = "Mod 3c", focal_re = "^var_ecotype1"),
  list(model = mod4,  label = "Mod 4",  focal_re = "^var_succ\\.vs\\.late"),
  list(model = mod5,  label = "Mod 5",  focal_re = "^var_consumer\\.taxonomy"),
  list(model = mod6,  label = "Mod 6",  focal_re = "^consumer_rich_s"),
  list(model = mod7,  label = "Mod 7",  focal_re = "^var_consumer\\.metabolism")
)

coef_df <- purrr::map_dfr(coef_specs_fig1, function(s) {
  fp <- s$focal_re
  broom.mixed::tidy(s$model, effects = "fixed", conf.int = TRUE) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      model_label = s$label,
      role = dplyr::case_when(
        term == "cage.treatment_stdcaged"                          ~ "Caging",
        !is.na(fp) & grepl(fp, term) & !grepl(":", term)          ~ "Focal variable",
        !is.na(fp) & grepl(fp, term) &  grepl(":", term)          ~ "Interaction",
        TRUE                                                        ~ "Covariate"
      )
    )
})

# Clean human-readable label for each term
make_term_label <- function(term) {
  simple <- c(
    "cage.treatment_stdcaged" = "Caging",
    "gamma_rich_s"            = "Gamma richness",
    "betadisp_ss_s"           = "Sample size",
    "excl_dur_s"              = "Excl. duration",
    "abs_lat_s"               = "Abs. latitude",
    "log_plot_s"              = "Log plot size",
    "consumer_rich_s"         = "Consumer richness"
  )
  if (term %in% names(simple)) return(unname(simple[term]))
  lbl <- gsub(":cage\\.treatment_stdcaged$", " × Caging", term)
  lbl <- gsub("^var_aq\\.or\\.terr",       "Biome: ",       lbl)
  lbl <- gsub("^var_ecotype1",              "Ecotype: ",     lbl)
  lbl <- gsub("^var_succ\\.vs\\.late",      "Succession: ",  lbl)
  lbl <- gsub("^var_consumer\\.taxonomy",   "Taxonomy: ",    lbl)
  lbl <- gsub("^var_consumer\\.metabolism", "Metabolism: ",  lbl)
  trimws(lbl)
}

coef_df <- coef_df %>%
  dplyr::mutate(
    term_label  = vapply(term, make_term_label, character(1)),
    model_label = factor(model_label, levels = model_order),
    panel       = dplyr::if_else(
      role %in% c("Caging", "Covariate"),
      "A: Caging & covariates",
      "B: Focal variable & interaction"
    )
  )

# Fixed y-axis order for panel A (caging first, then covariates)
cov_y_order <- rev(c(
  "Caging",
  "Gamma richness", "Sample size", "Excl. duration",
  "Abs. latitude",  "Log plot size"
))

# Y-axis order for panel B: focal levels then interaction levels, grouped by model
focal_y_order <- coef_df %>%
  dplyr::filter(panel == "B: Focal variable & interaction") %>%
  dplyr::mutate(role_ord = dplyr::if_else(role == "Focal variable", 1L, 2L)) %>%
  dplyr::arrange(as.integer(model_label), role_ord, term_label) %>%
  dplyr::distinct(term_label) %>%
  dplyr::pull(term_label) %>%
  rev()

# 9-color palette — one per model
model_pal <- c(
  "Mod 1"  = "#E41A1C",
  "Mod 2"  = "#377EB8",
  "Mod 3a" = "#4DAF4A",
  "Mod 3b" = "#984EA3",
  "Mod 3c" = "#FF7F00",
  "Mod 4"  = "#A65628",
  "Mod 5"  = "#F781BF",
  "Mod 6"  = "#888888",
  "Mod 7"  = "#00CED1"
)

# ---- Panel A: Caging & covariates ----
pA_df <- coef_df %>%
  dplyr::filter(panel == "A: Caging & covariates") %>%
  dplyr::mutate(term_label = factor(term_label, levels = cov_y_order))

pA <- ggplot(pA_df, aes(x = estimate, y = term_label, color = model_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.7),
    size = 0.35, linewidth = 0.55, na.rm = TRUE
  ) +
  scale_color_manual(values = model_pal, name = "Model") +
  labs(x = "Standardized coefficient (probit, ±95% CI)",
       y = NULL, title = "A: Caging & covariates") +
  theme_classic(base_size = 11) +
  theme(
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text.y     = element_text(size = 10),
    legend.position = "none",
    plot.title      = element_text(face = "bold", size = 11)
  )

# ---- Panel B: Focal variable & interaction ----
pB_df <- coef_df %>%
  dplyr::filter(panel == "B: Focal variable & interaction") %>%
  dplyr::mutate(term_label = factor(term_label, levels = focal_y_order))

pB <- ggplot(pB_df, aes(x = estimate, y = term_label, color = model_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.7),
    size = 0.35, linewidth = 0.55, na.rm = TRUE
  ) +
  scale_color_manual(values = model_pal, name = "Model") +
  labs(x = "Standardized coefficient (probit, ±95% CI)",
       y = NULL, title = "B: Focal variable & interaction") +
  theme_classic(base_size = 11) +
  theme(
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text.y     = element_text(size = 9),
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 11)
  )

# Combine: A on top (fixed height for 6 rows), B below (scaled to its rows)
nB <- dplyr::n_distinct(pB_df$term_label)
fig1 <- pA / pB +
  plot_layout(heights = c(6, nB))

ggsave(file.path("graphs", "model_predictions", "fig1_coefficient_plot.png"),
       fig1,
       width  = 10,
       height = max(6, 0.4 * (6 + nB)),
       dpi    = 300)

cat("\nFig 1 saved — panel A: 6 rows; panel B:", nB, "rows\n")

## ======================================================== ##
# Model Comparison Summary ----
## ======================================================== ##

full_models <- list(
  Mod1_Overall          = mod1,
  Mod2_AqTerr           = mod2,
  Mod6_ConsumerRichness = mod6
)

subset_models <- list(
  Mod3a_Ecotype_AqOnly    = mod3a,
  Mod3b_Ecotype_TransOnly = mod3b,
  Mod3c_Ecotype_TerrOnly  = mod3c,
  Mod4_Succession_AqTrans = mod4,
  Mod5_Taxonomy_AqTrans   = mod5,
  Mod7_Metabolism_Trans   = mod7
)

summarise_model_set <- function(model_list) {
  purrr::map_dfr(names(model_list), function(nm) {
    m  <- model_list[[nm]]
    r2 <- tryCatch({
      re_bars  <- lme4::findbars(formula(m))
      re_str   <- paste(vapply(re_bars, function(b) paste0("(", deparse(b), ")"), character(1)),
                        collapse = " + ")
      lhs      <- deparse(lme4::nobars(formula(m))[[2]])
      null_f   <- as.formula(paste(lhs, "~ 1 +", re_str))
      null_mod <- update(m, formula = null_f, data = model.frame(m))
      r.squaredLR(m, null = null_mod)
    }, error = function(e) NULL)
    data.frame(
      model     = nm,
      AIC       = round(AIC(m), 2),
      R2_LR     = if (!is.null(r2)) round(r2[[1]], 4)                   else NA_real_,
      R2_LR_adj = if (!is.null(r2)) round(attr(r2, "adj.r.squared"), 4) else NA_real_
    )
  })
}

cat("\n=== Model comparison: full dataset ===\n")
tbl_full <- summarise_model_set(full_models)
print(tbl_full)

cat("\n=== Model comparison: ecosystem subsets ===\n")
tbl_sub <- summarise_model_set(subset_models)
print(tbl_sub)

write.csv(tbl_full, row.names = FALSE,
  file = file.path("results", "model_comparison_full_dataset.csv"))
write.csv(tbl_sub, row.names = FALSE,
  file = file.path("results", "model_comparison_subset_models.csv"))

## ======================================================== ##
# Consolidated Results Table ----
## ======================================================== ##
# One row per model. Significance symbols derived from car::Anova Type II
# p-values already stored in at1–at7 / at3a–at3c.
#   ***  p < 0.001
#   **   p < 0.01
#   *    p < 0.05
#   †    p <= 0.10  (marginal)
#   NS   p > 0.10
#   —    term not in model

sig_sym <- function(p) {
  if (is.null(p) || is.na(p)) return("—")
  if (p < 0.001)      "***"
  else if (p < 0.01)  "**"
  else if (p < 0.05)  "*"
  else if (p <= 0.10) "†"
  else                "NS"
}

# Per-model metadata: label, biome/dataset, focal variable, ANOVA term strings,
# RE structure string, and the pre-computed ANOVA table object.
model_specs <- list(
  list(label = "Mod 1",  dataset = "Full",         focal = "—",
       f_term = NA,  int_term = NA,
       re = "(1 | source/exp.name)",  at = at1),
  list(label = "Mod 2",  dataset = "Full",         focal = "Biome",
       f_term = "var_aq.or.terr",
       int_term = "var_aq.or.terr:cage.treatment_std",
       re = "(1 | source/exp.name)",  at = at2),
  list(label = "Mod 3a", dataset = "Aquatic",      focal = "Ecotype",
       f_term = "var_ecotype1",
       int_term = "var_ecotype1:cage.treatment_std",
       re = "(1 | source/exp.name)",  at = at3a),
  list(label = "Mod 3b", dataset = "Transitional", focal = "Ecotype",
       f_term = "var_ecotype1",
       int_term = "var_ecotype1:cage.treatment_std",
       re = "(1 | exp.name)",         at = at3b),
  list(label = "Mod 3c", dataset = "Terrestrial",  focal = "Ecotype",
       f_term = "var_ecotype1",
       int_term = "var_ecotype1:cage.treatment_std",
       re = "(1 | source/exp.name)",  at = at3c),
  list(label = "Mod 4",  dataset = "Aq + Trans",   focal = "Successional stage",
       f_term = "var_succ.vs.late",
       int_term = "var_succ.vs.late:cage.treatment_std",
       re = "(1 | source/exp.name)",  at = at4),
  list(label = "Mod 5",  dataset = "Aq + Trans",   focal = "Consumer taxonomy",
       f_term = "var_consumer.taxonomy",
       int_term = "var_consumer.taxonomy:cage.treatment_std",
       re = "(1 | exp.name)",         at = at5),
  list(label = "Mod 6",  dataset = "Full",         focal = "Consumer richness",
       f_term = "consumer_rich_s",
       int_term = "consumer_rich_s:cage.treatment_std",
       re = "(1 | source/exp.name)",  at = at6),
  list(label = "Mod 7",  dataset = "Transitional", focal = "Consumer metabolism",
       f_term = "var_consumer.metabolism",
       int_term = "var_consumer.metabolism:cage.treatment_std",
       re = "(1 | exp.name)",         at = at7)
)

results_tbl <- purrr::map_dfr(model_specs, function(s) {
  at <- s$at

  # Df for the focal × caging interaction; fall back to caging df for Mod 1
  df_term <- if (!is.na(s$int_term)) s$int_term else "cage.treatment_std"
  df_idx  <- which(at$term == df_term)
  df_val  <- if (length(df_idx)) at$Df[df_idx[1]] else NA_integer_

  data.frame(
    Model          = s$label,
    Dataset        = s$dataset,
    Focal_variable = s$focal,
    abs_latitude   = sig_sym(lookup_p(at, "abs_lat_s")),
    gamma_richness = sig_sym(lookup_p(at, "gamma_rich_s")),
    plot_size      = sig_sym(lookup_p(at, "log_plot_s")),
    sample_size    = sig_sym(lookup_p(at, "betadisp_ss_s")),
    excl_duration  = sig_sym(lookup_p(at, "excl_dur_s")),
    caging         = sig_sym(lookup_p(at, "cage.treatment_std")),
    focal_var      = if (!is.na(s$f_term))   sig_sym(lookup_p(at, s$f_term))   else "—",
    interaction    = if (!is.na(s$int_term)) sig_sym(lookup_p(at, s$int_term)) else "—",
    df             = df_val,
    random_effects = s$re,
    stringsAsFactors = FALSE
  )
})

cat("\n=== Consolidated Results Table ===\n")
print(results_tbl, row.names = FALSE)
write.csv(results_tbl, row.names = FALSE,
  file = file.path("results", "consolidated_results_table.csv"))

## ======================================================== ##
# POST-HOC: Within-Group Caging Contrasts ----
## ======================================================== ##
# Within-group caging contrasts for models with significant categorical ×
# caging interactions. Inference on probit link scale; means reported on
# response scale. Mod 2 excluded (no significant interaction).

posthoc_within_caging <- function(model, moderator, label) {
  cat("\n", strrep("=", 60), "\n", label, "\n", strrep("=", 60), "\n\n", sep = "")

  f <- as.formula(paste("~ cage.treatment_std |", moderator))

  emm <- tryCatch(emmeans(model, f), error = function(e) { message(e$message); NULL })
  if (is.null(emm)) return(invisible(NULL))

  contrasts <- pairs(emm, adjust = "tukey")
  cat("--- Contrasts (link scale, Tukey-adjusted) ---\n")
  print(summary(contrasts))

  emm_resp <- emmeans(model, f, type = "response")
  cat("\n--- Marginal means (response scale) ---\n")
  print(summary(emm_resp))

  safe_label <- gsub("[^A-Za-z0-9_]", "_", label)
  write.csv(as.data.frame(summary(contrasts)), row.names = FALSE,
    file = file.path("results", paste0(safe_label, "_posthoc_contrasts.csv")))
  write.csv(as.data.frame(summary(emm_resp)), row.names = FALSE,
    file = file.path("results", paste0(safe_label, "_posthoc_means.csv")))

  invisible(list(emm = emm, contrasts = contrasts))
}

posthoc_within_caging(mod3a, "var_ecotype1", "Mod3a_Ecotype_x_Caging_AqOnly")
posthoc_within_caging(mod3b, "var_ecotype1", "Mod3b_Ecotype_x_Caging_TransOnly")
posthoc_within_caging(mod3c, "var_ecotype1", "Mod3c_Ecotype_x_Caging_TerrOnly")
posthoc_within_caging(mod5, "var_consumer.taxonomy",   "Mod5_Taxonomy_x_Caging_AqTransOnly")
posthoc_within_caging(mod7, "var_consumer.metabolism", "Mod7_Metabolism_x_Caging_TransOnly")

# End ----
