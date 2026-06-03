## --------------------------------------------------------------- ##
# CAGED Stats and Analyses — v2
## --------------------------------------------------------------- ##
# PURPOSE:
#   Systematic beta regression models testing how consumer exclusion
#   affects beta dispersion, individually and moderated by ecological
#   predictors. Addresses four project hypotheses.
#
# MODEL FAMILY:  beta_family(link = "probit") — glmmTMB
# TRANSFORM:     Smithson-Verkuilen: y* = (y*(n-1) + 0.5) / n
# RANDOM FX:     (1 | var_upper.source / exp.name) throughout
# COVARIATES:    scale(gamma.richness_exp.name) + scale(betadisp.sample.size)
# DISPFORMULA:   Only if DHARMa reveals meaningful heterogeneity AND ΔAIC > 2
#
# MODELS
#   Mod 1 — Overall caging effect (intercept-only moderator)
#   Mod 2 — Aquatic vs. terrestrial × caging
#   Mod 3 — Ecosystem type (var_ecotype1) × caging
#   Mod 4 — Successional stage × caging
#   Mod 5 — Exclusion duration × caging      [complete-case subset]
#   Mod 6 — Exclosure area × caging          [complete-case subset; log area]
#   Mod 7 — Consumer metabolism × caging
#   Mod 8 — Combined categorical moderators  [multi-predictor]
#
# OUTPUTS (per model)
#   results/  : Type II ANOVA table (.csv)
#   graphs/model_validation/ : DHARMa plots (.png)
#   graphs/model_predictions/: obs-vs-pred + partial effects (.png)

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

librarian::shelf(
  tidyverse, glmmTMB, DHARMa, performance,
  car, broom.mixed, MuMIn, ggeffects,
  emmeans, patchwork, njlyon0/supportR
)

source(file.path("00_setup.R"))

dir.create(file.path("results"),                        showWarnings = FALSE)
dir.create(file.path("graphs", "model_validation"),     showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("graphs", "model_predictions"),    showWarnings = FALSE, recursive = TRUE)

rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ----
## ------------------------------------------- ##

caged_beta_raw  <- read.csv(file.path("data", "08_caged_w.meta-beta-disp_finest-scales.csv"))
caged_effectsize <- read.csv(file.path("data", "08_caged_prepped-effect-size.csv"))

# Quick structure checks
dplyr::glimpse(caged_beta_raw)
dim(caged_beta_raw)

## ------------------------------------------- ##
# Prepare Base Dataset ----
## ------------------------------------------- ##

caged_beta <- caged_beta_raw %>%
  # Keep only primary treatment arms
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>%
  # Require ecosystem type and latitude
  dplyr::filter(var_aq.or.terr != "" & !is.na(var_aq.or.terr)) %>%
  dplyr::filter(!is.na(lat)) %>%
  droplevels() %>%
  # Derive numeric moderators
  dplyr::mutate(
    exclusion.duration = suppressWarnings(as.numeric(var_exclusion.duration.continuousyears)),
    exclosure.area     = suppressWarnings(as.numeric(var_exclosure.area.m2)),
    abs.lat            = abs(lat),
    log.area           = log(exclosure.area)
  ) %>%
  # Set factor reference levels
  dplyr::mutate(
    cage.treatment_std      = factor(cage.treatment_std, levels = c("uncaged", "caged")),
    var_aq.or.terr          = factor(var_aq.or.terr),
    var_ecotype1            = factor(var_ecotype1),
    var_succ.vs.late        = factor(var_succ.vs.late, levels = c("late", "early")),
    var_consumer.metabolism = factor(var_consumer.metabolism)
  )

# Smithson-Verkuilen transform (applied once to full dataset)
# y* = (y*(n-1) + 0.5) / n  — nudges boundary zeros off [0,1)
n_all <- nrow(caged_beta)
sv_transform <- function(y, n) { (y * (n - 1) + 0.5) / n }

caged_beta <- caged_beta %>%
  dplyr::mutate(betadisp_t = sv_transform(betadisp.comm.dist, n_all))

# Verify transform produced strict (0,1) support
stopifnot(all(caged_beta$betadisp_t > 0 & caged_beta$betadisp_t < 1))

cat("Base dataset rows:", nrow(caged_beta), "\n")
cat("Sources:", length(unique(caged_beta$source)), "\n")
cat("exp.names:", length(unique(caged_beta$exp.name)), "\n")
cat("Zeros in betadisp_t:", sum(caged_beta$betadisp_t == 0), "\n")

## ------------------------------------------- ##
# Complete-Case Subsets ----
## ------------------------------------------- ##

# Mod 5 — exclusion duration (continuous; numeric coercion above)
caged_beta_dur <- caged_beta %>%
  dplyr::filter(!is.na(exclusion.duration)) %>%
  droplevels()

# Mod 6 — exclosure area (log-transformed; remove Inf from log(0))
caged_beta_area <- caged_beta %>%
  dplyr::filter(!is.na(log.area) & is.finite(log.area)) %>%
  droplevels()

cat("Duration subset rows:", nrow(caged_beta_dur), "\n")
cat("Area subset rows:", nrow(caged_beta_area), "\n")

## ------------------------------------------- ##
# Shared Settings ----
## ------------------------------------------- ##

ctrl <- glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))

## ------------------------------------------- ##
# Helper Functions ----
## ------------------------------------------- ##

# Print + export Type II ANOVA table; return invisibly
summarise_model <- function(model, label, outdir = "results") {

  cat("\n", strrep("=", 60), "\n", label, "\n", strrep("=", 60), "\n\n", sep = "")

  # Convergence
  conv <- tryCatch(check_convergence(model), error = function(e) NA)
  cat("Convergence:", if (isTRUE(conv)) "OK" else "CHECK WARNINGS", "\n")

  # AIC
  cat("AIC:", round(AIC(model), 2), "\n")

  # R² (Nakagawa & Schielzeth)
  r2 <- tryCatch(r.squaredGLMM(model), error = function(e) NULL)
  if (!is.null(r2))
    cat(sprintf("R2 marginal = %.4f  |  R2 conditional = %.4f\n",
                r2[1, "R2m"], r2[1, "R2c"]))

  # Type II ANOVA
  anova_tbl <- car::Anova(model, type = "II") %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::mutate(sig = dplyr::case_when(
      `Pr(>Chisq)` < 0.001 ~ "***",
      `Pr(>Chisq)` < 0.01  ~ "**",
      `Pr(>Chisq)` < 0.05  ~ "*",
      TRUE                  ~ ""))

  print(anova_tbl, digits = 4)

  write.csv(anova_tbl, row.names = FALSE,
    file = file.path(outdir, paste0(gsub("[^A-Za-z0-9_]", "_", label), "_anova.csv")))

  invisible(anova_tbl)
}

# DHARMa simulated residual plots
validate_dharma <- function(model, data, prefix,
                             outdir = "graphs/model_validation",
                             nsim = 500, seed = 42) {
  set.seed(seed)
  sim <- simulateResiduals(fittedModel = model, n = nsim, plot = FALSE)

  # Overall QQ + residual vs. predicted
  png(file.path(outdir, paste0(prefix, "_dharma_overall.png")),
      width = 900, height = 600, res = 150)
  plot(sim, main = prefix)
  dev.off()

  # Residuals by caging treatment
  png(file.path(outdir, paste0(prefix, "_dharma_cage.png")),
      width = 700, height = 500, res = 150)
  plotResiduals(sim, form = data$cage.treatment_std,
                main = paste(prefix, "| by cage treatment"))
  dev.off()

  invisible(sim)
}

# Observed vs. predicted scatter
plot_obs_pred <- function(model, data, prefix, outdir = "graphs/model_predictions") {
  df <- data.frame(obs = data$betadisp_t, pred = fitted(model))

  p <- ggplot(df, aes(x = obs, y = pred)) +
    geom_point(alpha = 0.12, size = 0.7, color = "grey30") +
    geom_abline(slope = 1, intercept = 0, color = "#e63946", linewidth = 0.9) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = "Observed (SV-transformed)", y = "Predicted") +
    theme_classic(base_size = 12) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
          axis.text = element_text(color = "black"))

  ggsave(file.path(outdir, paste0(prefix, "_obs_pred.png")),
         plot = p, width = 4, height = 4, dpi = 300)
  invisible(p)
}

# ggeffects partial effects plot (returns gg object + saves)
plot_partial <- function(model, terms, prefix,
                          outdir = "graphs/model_predictions",
                          ylab = "Beta dispersion (SV-transformed)") {
  preds <- ggpredict(model, terms = terms)

  p <- plot(preds, show_data = FALSE, colors = "metro") +
    labs(y = ylab, title = NULL, x = NULL) +
    theme_classic(base_size = 12) +
    theme(panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.7),
          axis.text     = element_text(color = "black"),
          legend.position = "right")

  ggsave(file.path(outdir, paste0(prefix, "_partial.png")),
         plot = p, width = 6, height = 4, dpi = 300)
  invisible(p)
}

## ======================================================== ##
# INDIVIDUAL MODELS ----
## ======================================================== ##

## ------------------------------------------- ##
# Mod 1 — Overall Caging Effect ----
## ------------------------------------------- ##
# H1: Consumer loss increases beta diversity.
# Intercept-only moderator; controls for gamma richness + sample size.

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
plot_obs_pred(mod1, caged_beta, "mod1_overall")
plot_partial(mod1, terms = "cage.treatment_std", prefix = "mod1_overall")

## ------------------------------------------- ##
# Mod 2 — Aquatic vs. Terrestrial × Caging ----
## ------------------------------------------- ##
# H1 extension: caging effect stronger in aquatic systems.

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

# Note: if DHARMa shows heterogeneity by ecosystem type, consider refitting with
#   dispformula = ~ var_aq.or.terr  and comparing AIC (only add if ΔAIC > 2)

summarise_model(mod2, "Mod2_AqTerr_x_Caging")
validate_dharma(mod2, caged_beta, "mod2_aqterr")
plot_obs_pred(mod2, caged_beta, "mod2_aqterr")
plot_partial(mod2,
  terms  = c("cage.treatment_std", "var_aq.or.terr"),
  prefix = "mod2_aqterr")

## ------------------------------------------- ##
# Mod 3 — Ecosystem Type × Caging ----
## ------------------------------------------- ##
# H1 extension: finer ecosystem classification may reveal stronger/weaker effects.

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
validate_dharma(mod3, caged_beta, "mod3_ecotype")
plot_obs_pred(mod3, caged_beta, "mod3_ecotype")
plot_partial(mod3,
  terms  = c("cage.treatment_std", "var_ecotype1"),
  prefix = "mod3_ecotype")

## ------------------------------------------- ##
# Mod 4 — Successional Stage × Caging ----
## ------------------------------------------- ##
# H4: Consumers have stronger effect on variability after disturbance
#     (early-successional communities more susceptible to top-down control).
# Reference level = "late" (late-successional, undisturbed baseline).

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
validate_dharma(mod4, caged_beta, "mod4_succ")
plot_obs_pred(mod4, caged_beta, "mod4_succ")
plot_partial(mod4,
  terms  = c("cage.treatment_std", "var_succ.vs.late"),
  prefix = "mod4_succ")

## ------------------------------------------- ##
# Mod 5 — Exclusion Duration × Caging ----
## ------------------------------------------- ##
# H4: Longer exclusion → more time for stochastic assembly → stronger caging effect.
# Complete-case subset (experiments with known duration only).

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
validate_dharma(mod5, caged_beta_dur, "mod5_duration")
plot_obs_pred(mod5, caged_beta_dur, "mod5_duration")
plot_partial(mod5,
  terms  = c("exclusion.duration [all]", "cage.treatment_std"),
  prefix = "mod5_duration")

## ------------------------------------------- ##
# Mod 6 — Exclosure Area × Caging ----
## ------------------------------------------- ##
# H3: Spatial scale moderates the caging effect.
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
validate_dharma(mod6, caged_beta_area, "mod6_area")
plot_obs_pred(mod6, caged_beta_area, "mod6_area")
plot_partial(mod6,
  terms  = c("log.area [all]", "cage.treatment_std"),
  prefix = "mod6_area")

## ------------------------------------------- ##
# Mod 7 — Consumer Metabolism × Caging ----
## ------------------------------------------- ##
# H2: Consumer metabolism (endotherm vs. ectotherm) modulates top-down control strength.

mod7 <- glmmTMB(
  betadisp_t ~
    var_consumer.metabolism * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

summarise_model(mod7, "Mod7_Metabolism_x_Caging")
validate_dharma(mod7, caged_beta, "mod7_metabolism")
plot_obs_pred(mod7, caged_beta, "mod7_metabolism")
plot_partial(mod7,
  terms  = c("cage.treatment_std", "var_consumer.metabolism"),
  prefix = "mod7_metabolism")

## ======================================================== ##
# MULTI-PREDICTOR MODEL ----
## ======================================================== ##

## ------------------------------------------- ##
# Mod 8 — Combined Categorical Moderators ----
## ------------------------------------------- ##
# Combines the strongest categorical moderators identified in Mods 1-7.
# Each moderator interacts with cage.treatment_std independently.
# VIF checked post-fit to flag collinearity (threshold: moderate > 5, high > 10).

mod8 <- glmmTMB(
  betadisp_t ~
    cage.treatment_std * var_aq.or.terr +
    cage.treatment_std * var_succ.vs.late +
    cage.treatment_std * var_consumer.metabolism +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = caged_beta
)

summarise_model(mod8, "Mod8_Combined_Categorical")

# VIF — flag any term > 5 (moderate concern) or > 10 (high concern)
cat("\n--- VIF (Mod 8) ---\n")
check_collinearity(mod8) %>% print()

validate_dharma(mod8, caged_beta, "mod8_combined")
plot_obs_pred(mod8, caged_beta, "mod8_combined")

# Partial plots for each moderator in Mod 8
plot_partial(mod8,
  terms  = c("cage.treatment_std", "var_aq.or.terr"),
  prefix = "mod8_aqterr")
plot_partial(mod8,
  terms  = c("cage.treatment_std", "var_succ.vs.late"),
  prefix = "mod8_succ")
plot_partial(mod8,
  terms  = c("cage.treatment_std", "var_consumer.metabolism"),
  prefix = "mod8_metabolism")

## ======================================================== ##
# Model Comparison Summary ----
## ======================================================== ##

# Collect AIC and R² for all models using the same dataset (base only)
base_models <- list(
  Mod1_Overall    = mod1,
  Mod2_AqTerr     = mod2,
  Mod3_Ecotype    = mod3,
  Mod4_Succession = mod4,
  Mod7_Metabolism = mod7,
  Mod8_Combined   = mod8
)

model_summary_tbl <- purrr::map_dfr(names(base_models), function(nm) {
  m <- base_models[[nm]]
  r2 <- tryCatch(r.squaredGLMM(m), error = function(e) matrix(NA, 1, 2,
    dimnames = list(NULL, c("R2m", "R2c"))))
  data.frame(
    model      = nm,
    AIC        = round(AIC(m), 2),
    R2_marginal   = round(r2[1, "R2m"], 4),
    R2_conditional = round(r2[1, "R2c"], 4)
  )
})

print(model_summary_tbl)
write.csv(model_summary_tbl, row.names = FALSE,
  file = file.path("results", "model_comparison_summary.csv"))

# Note: Mods 5 & 6 use subsets so are not directly AIC-comparable to base models

# End ----
