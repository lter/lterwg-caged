## --------------------------------------------------------------- ##
# CAGED Stats and Analyses — v9
## --------------------------------------------------------------- ##
# PURPOSE:
#   Systematic beta regression models testing how consumer exclusion
#   affects beta dispersion, moderated by ecological predictors.
#   Addresses project hypotheses (McDevitt-Irwin et al.).
#
# CHANGES FROM v7:
#   - Mod 2b (gamma richness sensitivity check) removed
#   - Mods 5 (duration) and 6 (area) dropped; both variables folded
#     into the standard covariate set for ALL models
#   - Mod 7b (latitude) dropped; abs.lat folded into covariate set
#   - Mod 8 (combined complete-case) removed
#   - Mod 7 (metabolism) restricted to aquatic studies only
#   - Added Mod 5 (consumer taxonomy × caging, aquatic only)
#   - Added Mod 6 (consumer richness × caging, full dataset)
#   - VIF check (check_model_vif) added to all model validation blocks
#   - scale(gamma.richness_exp.name) removed from standard covariates
#
# MODEL FAMILY:  beta_family(link = "probit") — glmmTMB
# TRANSFORM:     Smithson-Verkuilen: y* = (y*(n-1) + 0.5) / n
# RANDOM FX:     (1 | var_upper.source / exp.name) throughout
#
# STANDARD COVARIATES (all models):
#   scale(gamma.richness_exp.name)
#   scale(betadisp.sample.size)
#   scale(exclusion.duration)       [from var_exclusion.duration.continuousyears]
#   scale(abs.lat)
#   scale(log.area)                 [log(var_exclosure.area.m2)]
#
# NOTE: Including exclusion.duration and log.area in all models requires
#   a complete-case base dataset. See "Complete-Case Base Dataset" section.
#
# MODELS
#   Mod 1 — Overall caging effect                             [full dataset]
#   Mod 2 — Aquatic vs. terrestrial × caging                 [full dataset]
#   Mod 3 — Ecosystem type (var_ecotype1) × caging           [full dataset]
#   Mod 4 — Successional stage × caging                      [full dataset]
#   Mod 5 — Consumer taxonomy × caging                       [aquatic only]
#   Mod 6 — Consumer richness (var_consumer.richness.number) × caging
#                                                             [full dataset]
#   Mod 7 — Consumer metabolism × caging                     [aquatic only]
#
# HELPER FUNCTIONS: sourced from 10c_helpers.R
#   summarise_model()       — Type II ANOVA + AIC + R²
#   validate_dharma()       — DHARMa diagnostic plots
#   check_model_vif()       — VIF on main-effects-only refit
#   plot_partial_with_data()— model predictions overlaid on raw data
#   plot_covariate_effects()— covariate partial effects plots

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

librarian::shelf(
  tidyverse, glmmTMB, DHARMa, performance,
  car, broom.mixed, MuMIn, ggeffects,
  emmeans, patchwork, lme4, njlyon0/supportR
)

source("00_setup.R")

dir.create("results",                     showWarnings = FALSE)
dir.create(file.path("graphs", "model_validation"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("graphs", "model_predictions"), showWarnings = FALSE, recursive = TRUE)

rm(list = ls()); gc()

options(scipen = 1)

source(file.path("_exploratory_scripts", "10c_helpers.R"))

## ------------------------------------------- ##
# Load Data ----
## ------------------------------------------- ##

caged_beta_raw <- read.csv(file.path("data", "08-A_caged_w.meta-beta-disp_finest-scales.csv"))

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
    exclosure.area               = suppressWarnings(as.numeric(var_exclosure.area.m2)),
    var_consumer.richness.number = suppressWarnings(as.numeric(var_consumer.richness.number)),
    abs.lat                      = abs(lat),
    log.area                     = log(exclosure.area)
  ) %>%
  dplyr::mutate(
    cage.treatment_std      = factor(cage.treatment_std, levels = c("uncaged", "caged")),
    var_aq.or.terr          = factor(var_aq.or.terr),
    var_ecotype1            = factor(var_ecotype1),
    var_succ.vs.late        = factor(var_succ.vs.late, levels = c("late", "early")),
    var_consumer.metabolism = factor(var_consumer.metabolism),
    var_consumer.taxonomy   = factor(var_consumer.taxonomy)
  )

## ------------------------------------------- ##
# Complete-Case Base Dataset ----
## ------------------------------------------- ##
# All models include scale(exclusion.duration) and scale(log.area), so rows
# missing either are dropped here once rather than silently per model.
# abs.lat is complete by construction (lat already filtered above).
# betadisp.sample.size is assumed complete in the source data.

caged_beta <- caged_beta_all %>%
  dplyr::filter(
    !is.na(exclusion.duration),
    !is.na(log.area) & is.finite(log.area)
  ) %>%
  droplevels()

# Smithson-Verkuilen transform applied to the complete-case dataset
n_cc       <- nrow(caged_beta)
caged_beta <- caged_beta %>%
  dplyr::mutate(betadisp_t = sv_transform(betadisp.comm.dist, n_cc))

stopifnot(all(caged_beta$betadisp_t > 0 & caged_beta$betadisp_t < 1))

cat("\n--- Dataset summary ---\n")
cat("All rows (caged/uncaged, lat + aq/terr present):", nrow(caged_beta_all), "\n")
cat("Complete-case rows (+ known duration + known area):", nrow(caged_beta), "\n")
cat("  Sources:    ", length(unique(caged_beta$source)),   "\n")
cat("  exp.names:  ", length(unique(caged_beta$exp.name)), "\n")
cat("  SV zeros (should be 0):", sum(caged_beta$betadisp_t == 0), "\n")

## ------------------------------------------- ##
# Aquatic-Only Subset ----
## ------------------------------------------- ##
# Used for Mods 5 (taxonomy) and 7 (metabolism).
# Drop blank/NA consumer.metabolism and consumer.taxonomy before factoring.

caged_beta_aq <- caged_beta %>%
  dplyr::filter(var_aq.or.terr == "aquatic") %>%
  droplevels()

cat("\nAquatic complete-case subset:", nrow(caged_beta_aq), "rows,",
    length(unique(caged_beta_aq$exp.name)), "experiments\n")

# Aquatic subset for metabolism (drop blank, "both")
caged_beta_aq_met <- caged_beta_aq %>%
  dplyr::filter(!is.na(var_consumer.metabolism),
                !var_consumer.metabolism %in% c("", "both")) %>%
  dplyr::mutate(var_consumer.metabolism = droplevels(var_consumer.metabolism)) %>%
  droplevels()

cat("Aquatic metabolism subset:", nrow(caged_beta_aq_met), "rows,",
    length(unique(caged_beta_aq_met$exp.name)), "experiments\n")
cat("Metabolism levels:", levels(caged_beta_aq_met$var_consumer.metabolism), "\n")

# Aquatic subset for taxonomy (drop blank/NA)
caged_beta_aq_tax <- caged_beta_aq %>%
  dplyr::filter(!is.na(var_consumer.taxonomy),
                var_consumer.taxonomy != "") %>%
  dplyr::mutate(var_consumer.taxonomy = droplevels(var_consumer.taxonomy)) %>%
  droplevels()

cat("Aquatic taxonomy subset:", nrow(caged_beta_aq_tax), "rows,",
    length(unique(caged_beta_aq_tax$exp.name)), "experiments\n")
cat("Taxonomy levels:", levels(caged_beta_aq_tax$var_consumer.taxonomy), "\n")

## ------------------------------------------- ##
# Shared Settings ----
## ------------------------------------------- ##

ctrl <- glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))

# Standard covariate block used in all model formulas (for reference in comments)
# scale(gamma.richness_exp.name) + scale(betadisp.sample.size) +
# scale(exclusion.duration) + scale(abs.lat) + scale(log.area)

# Standard covariate terms for plot_covariate_effects()
COVARIATE_TERMS <- list(
  list(term = "gamma.richness_exp.name [n=150]",
       xlab = "Gamma richness (exp.name)",           color = "#0072B2"),
  list(term = "betadisp.sample.size [n=150]",
       xlab = "Betadisp sample size (# replicates)", color = "#D55E00"),
  list(term = "exclusion.duration [n=150]",
       xlab = "Exclusion duration (years)",          color = "#009E73"),
  list(term = "abs.lat [n=150]",
       xlab = "Absolute latitude (°)",               color = "#CC79A7"),
  list(term = "log.area [n=150]",
       xlab = "Log exclosure area (log m²)",         color = "#E69F00")
)

# Build covariate_terms list with per-model p-values (for linetype annotation).
# Looks up scale(<raw_var>) in the ANOVA table returned by summarise_model().
make_covariate_terms <- function(anova_tbl) {
  lapply(COVARIATE_TERMS, function(ct) {
    raw_var    <- trimws(sub("\\s*\\[.*\\]", "", ct$term))
    ct$p_value <- lookup_p(anova_tbl, paste0("scale(", raw_var, ")"))
    ct
  })
}

## ======================================================== ##
# MODELS ----
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
    scale(exclusion.duration) +
    scale(abs.lat) +
    scale(log.area) +
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
# H1 extension: caging effect stronger in aquatic systems (stronger top-down control).

mod2 <- glmmTMB(
  betadisp_t ~
    var_aq.or.terr * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    scale(exclusion.duration) +
    scale(abs.lat) +
    scale(log.area) +
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
  terms   = c("cage.treatment_std", "var_aq.or.terr"),
  prefix  = "mod2_aqterr",
  x_lab   = "Cage treatment",
  p_value = lookup_p(at2, "var_aq.or.terr:cage.treatment_std"))
plot_covariate_effects(mod2, caged_beta, "mod2_aqterr",
  covariate_terms = make_covariate_terms(at2))

## ------------------------------------------- ##
# Mod 3 — Ecosystem Type (ecotype1) × Caging ----
## ------------------------------------------- ##
# H1 extension: finer ecosystem classification resolves variation in caging effect.
# Note: ecotype on x-axis (many levels), cage treatment as color grouping.

mod3 <- glmmTMB(
  betadisp_t ~
    var_ecotype1 * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    scale(exclusion.duration) +
    scale(abs.lat) +
    scale(log.area) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

at3 <- summarise_model(mod3, "Mod3_Ecotype_x_Caging")
validate_dharma(mod3, caged_beta, "mod3_ecotype",
  pred_vars = "var_ecotype1")
check_model_vif(mod3, caged_beta, "Mod3_Ecotype_x_Caging")
plot_partial_with_data(mod3, caged_beta,
  terms   = c("var_ecotype1", "cage.treatment_std"),
  prefix  = "mod3_ecotype",
  x_lab   = "Ecosystem type",
  width   = 13, height = 6,
  p_value = lookup_p(at3, "var_ecotype1:cage.treatment_std"))
plot_covariate_effects(mod3, caged_beta, "mod3_ecotype",
  covariate_terms = make_covariate_terms(at3))

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
    scale(exclusion.duration) +
    scale(abs.lat) +
    scale(log.area) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

at4 <- summarise_model(mod4, "Mod4_Succession_x_Caging")
validate_dharma(mod4, caged_beta, "mod4_succ",
  pred_vars = "var_succ.vs.late")
check_model_vif(mod4, caged_beta, "Mod4_Succession_x_Caging")
plot_partial_with_data(mod4, caged_beta,
  terms   = c("cage.treatment_std", "var_succ.vs.late"),
  prefix  = "mod4_succ",
  x_lab   = "Cage treatment",
  p_value = lookup_p(at4, "var_succ.vs.late:cage.treatment_std"))
plot_covariate_effects(mod4, caged_beta, "mod4_succ",
  covariate_terms = make_covariate_terms(at4))

## ------------------------------------------- ##
# Mod 5 — Consumer Taxonomy × Caging (aquatic only) ----
## ------------------------------------------- ##
# H2 extension: consumer taxonomic identity (e.g., fish vs. invertebrate)
# determines strength of top-down control on community variability.
# Restricted to aquatic systems where consumer taxonomy is most clearly
# defined and sample sizes per taxon are adequate.

mod5 <- glmmTMB(
  betadisp_t ~
    var_consumer.taxonomy * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    scale(exclusion.duration) +
    scale(abs.lat) +
    scale(log.area) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_aq_tax
)

at5 <- summarise_model(mod5, "Mod5_Taxonomy_x_Caging_AqOnly")
validate_dharma(mod5, caged_beta_aq_tax, "mod5_taxonomy",
  pred_vars = "var_consumer.taxonomy")
check_model_vif(mod5, caged_beta_aq_tax, "Mod5_Taxonomy_x_Caging_AqOnly")
plot_partial_with_data(mod5, caged_beta_aq_tax,
  terms   = c("var_consumer.taxonomy", "cage.treatment_std"),
  prefix  = "mod5_taxonomy",
  x_lab   = "Consumer taxonomy",
  width   = 11, height = 6,
  p_value = lookup_p(at5, "var_consumer.taxonomy:cage.treatment_std"))
plot_covariate_effects(mod5, caged_beta_aq_tax, "mod5_taxonomy",
  covariate_terms = make_covariate_terms(at5))

## ------------------------------------------- ##
# Mod 6 — Consumer Richness × Caging ----
## ------------------------------------------- ##
# H2 extension: consumer richness (numeric; higher = more diverse consumer
# assemblage) moderates the caging effect.
# Full complete-case dataset.

mod6 <- glmmTMB(
  betadisp_t ~
    scale(var_consumer.richness.number) * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    scale(exclusion.duration) +
    scale(abs.lat) +
    scale(log.area) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

at6 <- summarise_model(mod6, "Mod6_ConsumerRichness_x_Caging")
validate_dharma(mod6, caged_beta, "mod6_consumer_richness",
  pred_vars = "var_consumer.richness.number")
check_model_vif(mod6, caged_beta, "Mod6_ConsumerRichness_x_Caging")
plot_partial_with_data(mod6, caged_beta,
  terms   = c("var_consumer.richness.number [n=200]", "cage.treatment_std"),
  prefix  = "mod6_consumer_richness",
  x_lab   = "Consumer richness (# species)",
  p_value = lookup_p(at6, "scale(var_consumer.richness.number):cage.treatment_std"))
plot_covariate_effects(mod6, caged_beta, "mod6_consumer_richness",
  covariate_terms = make_covariate_terms(at6))

## ------------------------------------------- ##
# Mod 7 — Consumer Metabolism × Caging (aquatic only) ----
## ------------------------------------------- ##
# H2: Endotherms vs. ectotherms differ in top-down control strength.
# Restricted to aquatic systems to reduce confounding with ecosystem type
# (endotherms dominate terrestrial systems, ectotherms dominate aquatic).

mod7 <- glmmTMB(
  betadisp_t ~
    var_consumer.metabolism * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    scale(exclusion.duration) +
    scale(abs.lat) +
    scale(log.area) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta_aq_met
)

at7 <- summarise_model(mod7, "Mod7_Metabolism_x_Caging_AqOnly")
validate_dharma(mod7, caged_beta_aq_met, "mod7_metabolism",
  pred_vars = "var_consumer.metabolism")
check_model_vif(mod7, caged_beta_aq_met, "Mod7_Metabolism_x_Caging_AqOnly")
plot_partial_with_data(mod7, caged_beta_aq_met,
  terms   = c("cage.treatment_std", "var_consumer.metabolism"),
  prefix  = "mod7_metabolism",
  x_lab   = "Cage treatment",
  p_value = lookup_p(at7, "var_consumer.metabolism:cage.treatment_std"))
plot_covariate_effects(mod7, caged_beta_aq_met, "mod7_metabolism",
  covariate_terms = make_covariate_terms(at7))

## ======================================================== ##
# Model Comparison Summary ----
## ======================================================== ##
# AIC and R² reported separately for full-dataset and aquatic-only models;
# the two sets are fitted on different data and are not AIC-comparable.

full_models <- list(
  Mod1_Overall            = mod1,
  Mod2_AqTerr             = mod2,
  Mod3_Ecotype            = mod3,
  Mod4_Succession         = mod4,
  Mod6_ConsumerRichness   = mod6
)

aq_models <- list(
  Mod5_Taxonomy_AqOnly    = mod5,
  Mod7_Metabolism_AqOnly  = mod7
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
      R2_LR     = if (!is.null(r2)) round(r2[[1]], 4)                    else NA_real_,
      R2_LR_adj = if (!is.null(r2)) round(attr(r2, "adj.r.squared"), 4)  else NA_real_
    )
  })
}

cat("\n=== Model comparison: full dataset ===\n")
tbl_full <- summarise_model_set(full_models)
print(tbl_full)

cat("\n=== Model comparison: aquatic only ===\n")
tbl_aq <- summarise_model_set(aq_models)
print(tbl_aq)

write.csv(tbl_full, row.names = FALSE,
  file = file.path("results","model_comparison_full_dataset.csv"))
write.csv(tbl_aq, row.names = FALSE,
  file = file.path("results","model_comparison_aquatic_only.csv"))

## ======================================================== ##
# POST-HOC: Within-Group Caging Contrasts ----
## ======================================================== ##
# For models with a significant categorical × caging interaction, test whether
# the caging effect (uncaged vs. caged) is significant within each level of the
# moderator. Inference on the link (probit) scale; means also reported on the
# response scale for interpretability.
# Mods 3, 5, 7 only — Mod 2 excluded (no significant interaction or main effect
# of var_aq.or.terr). Mod 4 excluded (2-level moderator; ANOVA p-value is the
# complete test).

posthoc_within_caging <- function(model, moderator, label) {
  cat("\n", strrep("=", 60), "\n", label, "\n", strrep("=", 60), "\n\n", sep = "")

  f <- as.formula(paste("~ cage.treatment_std |", moderator))

  # Marginal means on link scale
  emm <- tryCatch(emmeans(model, f), error = function(e) { message(e$message); NULL })
  if (is.null(emm)) return(invisible(NULL))

  # Caged vs. uncaged contrast within each moderator level (Tukey-adjusted)
  contrasts <- pairs(emm, adjust = "tukey")
  cat("--- Contrasts (link scale, Tukey-adjusted) ---\n")
  print(summary(contrasts))

  # Marginal means back-transformed to response scale
  emm_resp <- emmeans(model, f, type = "response")
  cat("\n--- Marginal means (response scale) ---\n")
  print(summary(emm_resp))

  # Save contrast table
  contr_df <- as.data.frame(summary(contrasts))
  safe_label <- gsub("[^A-Za-z0-9_]", "_", label)
  write.csv(contr_df, row.names = FALSE,
    file = file.path("results", paste0(safe_label, "_posthoc_contrasts.csv")))

  # Save marginal means table
  means_df <- as.data.frame(summary(emm_resp))
  write.csv(means_df, row.names = FALSE,
    file = file.path("results", paste0(safe_label, "_posthoc_means.csv")))

  invisible(list(emm = emm, contrasts = contrasts))
}

posthoc_within_caging(mod3, "var_ecotype1",            "Mod3_Ecotype_x_Caging")
posthoc_within_caging(mod5, "var_consumer.taxonomy",   "Mod5_Taxonomy_x_Caging_AqOnly")
posthoc_within_caging(mod7, "var_consumer.metabolism", "Mod7_Metabolism_x_Caging_AqOnly")

# End ----
