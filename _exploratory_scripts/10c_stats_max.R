## --------------------------------------------------------------- ##
# CAGED Stats and Analyses 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, Max Castorani, 
# Jamie McDevitt-Irwin, Kelly E Speare ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, glmmTMB, DHARMa,
                 performance, lubridate, car, broom.mixed, #easystats, 
                 njlyon0/supportR, MuMIn, visreg, grid, gridExtra,
                 emmeans, tidymodels, qqplotr, sjPlot, effects) #, update_all= TRUE) 

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ----
# these dfs were created in script 08 script
## ------------------------------------------- ##
caged_effectsize <- read.csv(file.path("data", "08_caged_prepped-effect-size.csv"))
caged_beta <- read.csv(file.path("data", "08_caged_w.meta-beta-disp_finest-scales.csv"))

dim(caged_effectsize) # 417 rows
dim(caged_beta) # 13,964  rows

# Check number of sources
unique(caged_effectsize$exp.name) # 346
unique(caged_beta$exp.name) #347
dim(caged_effectsize) #417


# Drop partial and uncertain levels for column 'cage.treatment_std'
caged_beta <- caged_beta %>%
  dplyr::filter(cage.treatment_std != 'partial') %>%
  dplyr::filter(cage.treatment_std != 'uncertain') %>%
  droplevels()

## ------------------------------------------- ##
# Latitude Models  ----
## ------------------------------------------- ##

glimpse(caged_beta) #12,905 rows

# Check distribution of values in response variable, betadisp.comm.dist
hist(caged_beta$betadisp.comm.dist)  

# Check range of values in response variable, betadisp.comm.dist, with increased precision
options(digits = 22) # 22 is maximum value allowable
range(caged_beta$betadisp.comm.dist) 

# Check relative frequencies of true ones
table(caged_beta$betadisp.comm.dist == 1)
paste0("Frequency of true ones is ",
       round(100 * table(caged_beta$betadisp.comm.dist == 1)[2] / nrow(caged_beta), 2), 
       "%"
)
# Check relative frequencies of true zeroes
table(caged_beta$betadisp.comm.dist == 0) 
paste0("Frequency of true zeroes is ",
       round(100 * table(caged_beta$betadisp.comm.dist == 0)[2] / nrow(caged_beta), 2), 
       "%"
       )

# Reset number of significant digits to the default value (7)
options(digits = 7)

# Due to the possible presence of true zeroes and ones, which are invalid in beta regression (i.e., we have [0,1) but the model assumes (0,1)), we transform values slightly inward such that they are above 0 and below 1 by a small amount, epsilon. There is a boundary distortion, but there are sound reasons for taking this approach:
# (1) There are no true ones, but some true zeroes
# (2) True zeroes are rare, representing ~1% of the data
# (3) True zeroes and ones are not structurally different from values 0 < y < 1
# (4) Model results are readily interpretable and process is simple to understand and implement
# (4) Zero-inflated beta model could be considered, but this would require fitting separate binary models for the probability of zeroes and ones, which does not make sense in our application as again the process generating true zeroes and ones should be the same process as that generating 0 < y < 1
# Reference: Smithson & Verkuilen (2006), "A better lemon squeezer? Maximum-likelihood regression with beta-distributed dependent variables."

# Data are in the semi-closed interval [0,1), i.e., they include true 0s but no 1s. Hence, we apply a transformation that only adjusts the zeroes while leaving the interior values nearly unchanged
# This nudges zeros to a small positive value but barely affects values near 1
n <- nrow(caged_beta)
transform.fun <- function(y){(y * (n - 1) + 0.5 ) / n}

# Alternative transformation if you have true zeroes and true ones, [0, 1] - DO NOT NEED TO USE
#transform.fun <- function(y){y * (1 - epsilon) + (epsilon / 2)}
#epsilon <- 1e-6

# Apply transformation
caged_beta$betadisp.comm.dist_transform <- sapply(X = caged_beta$betadisp.comm.dist, FUN = transform.fun)

# Validate (this should look like a 1:1)
plot(x = caged_beta$betadisp.comm.dist, y =  caged_beta$betadisp.comm.dist_transform)


## ------------------------------------------- ##
## Create new dataframes for models to follow ----
## ------------------------------------------- ##
# Drop rows for which there is no terrestrial or aquatic categorization
table(caged_beta$var_aq.or.terr)

caged_beta2 <- caged_beta %>% 
  dplyr::filter(var_aq.or.terr != "") %>% 
  droplevels()
unique(caged_beta2$exp.name) # 347, none are dropped

table(caged_beta2$var_aq.or.terr) #Good!

# Drop rows for which latitude is missing
table(is.na(caged_beta2$lat))

# which exp names are missing latitude
caged_beta2$exp.name[is.na(caged_beta2$lat)]

caged_beta2 <- caged_beta2[!is.na(caged_beta2$lat), ]

table(is.na(caged_beta2$lat)) #Good!

unique(caged_beta2$exp.name) # 347

# Create a column for absolute value of latitude
caged_beta2 <- caged_beta2 %>%
  mutate(abs.lat = abs(lat))

# Make sure there are no NAs for the experiment name
table(is.na(caged_beta2$exp.name)) #Good!

# Make aquatic/terrestrial a factor
caged_beta2$var_aq.or.terr <- factor(caged_beta2$var_aq.or.terr)

# Subset datasets for uncaged plots only or caged plots only
uncaged.df <- caged_beta2 %>%
  filter(cage.treatment_std == "uncaged") %>%
  droplevels()

caged.df <- caged_beta2 %>%
  filter(cage.treatment_std == "caged") %>%
  droplevels()

# Check data: General relationships between y and x1 or x2
ggplot(data = caged_beta2, aes(x = lat, y = betadisp.comm.dist_transform)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2)) +
  facet_grid(cage.treatment_std ~ var_aq.or.terr) 

ggplot(data = caged_beta2, aes(x = cage.treatment_std, y = betadisp.comm.dist_transform)) +
  geom_boxplot()

ggplot(data = caged_beta2, aes(x = var_aq.or.terr, y = betadisp.comm.dist_transform)) +
  geom_boxplot()

ggplot(data = caged_beta2, aes(x = cage.treatment_std, y = betadisp.comm.dist_transform)) +
  geom_boxplot() +
  facet_wrap( ~ var_aq.or.terr) 

ggplot(data = uncaged.df, aes(x = lat, y = betadisp.comm.dist_transform)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2)) 

ggplot(data = uncaged.df, aes(x = abs.lat, y = betadisp.comm.dist_transform)) +
  geom_point() +
  geom_smooth(method = 'lm') 

ggplot(data = uncaged.df, aes(x = var_aq.or.terr, y = betadisp.comm.dist_transform)) +
  geom_boxplot()

ggplot(data = uncaged.df, aes(x = lat, y = betadisp.comm.dist_transform)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2)) +
  facet_wrap(~var_aq.or.terr)

ggplot(data = caged.df, aes(x = lat, y = betadisp.comm.dist_transform)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2)) 

ggplot(data = caged.df, aes(x = abs.lat, y = betadisp.comm.dist_transform)) +
  geom_point() +
  geom_smooth(method = 'lm')

ggplot(data = caged.df, aes(x = var_aq.or.terr, y = betadisp.comm.dist_transform)) +
  geom_boxplot()



## ------------------------------------------- ##
## Fit Beta Regression Models ----
## ------------------------------------------- ## 

# ------------------------------------------
# A Preamble: Common Link Functions for Beta Regression
# ------------------------------------------
# Link       | Range of μ | Notes
# -----------|-------------|-----------------------------------------------------
# "logit"    |   (0,1)     | Most common; interpretable on odds scale; symmetric
# "probit"   |   (0,1)     | Similar to logit; assumes normal errors; symmetric
# "cloglog"  |   (0,1)     | Asymmetric; good when changes happen near μ = 0

# Practical note: After fitting the logit link function, there was significant underdispersion. So, I went back and tested all other possible links as a sensitivity analysis. The results did not change in a meaningful way (relatively minor changes to the p-values), but I recorded the AIC values and iteratively checked the DHARMa simulated model residuals. 

# In the end, the probit model had the lowest AIC by at least 10-20 units and was best behaved in DHARMa diagnostics.

# check sample size
dim(caged_beta2) # 12889    45
unique(caged_beta2$exp.name) # 347 
unique(caged_beta2$source) # 116
# this is the sample size after dropping missing metadata 

caged_beta2 %>%
  group_by(var_aq.or.terr,var_succ.vs.late) %>%
  summarize(n())

# Caged vs Uncaged Model: Ecotype
caging.betamod.habitat <- glmmTMB(betadisp.comm.dist  ~ 
                                    cage.treatment_std * var_ecotype1 +
                                    scale(gamma.richness_exp.name) + 
                                    scale(betadisp.sample.size) +
                                    (1 | var_upper.source / exp.name), 
                                  dispformula = ~ cage.treatment_std * var_ecotype1 + abs.lat,
                                  family = ordbeta(link = 'probit'),
                                  control = glmmTMBControl(optimizer = optim, 
                                                           optArgs = list(method = "BFGS")), 
                                  data = caged_beta2) 
summary(caging.betamod.habitat)    
car::Anova(caging.betamod.habitat, type = "II")

## ------------------------------------------- ##
# DHARMa Validation: caging.betamod.habitat ----
## ------------------------------------------- ##

sim_out.habitat <- simulateResiduals(fittedModel = caging.betamod.habitat, n = 1000, plot = FALSE)
plot(sim_out.habitat)
testUniformity(sim_out.habitat)
testDispersion(sim_out.habitat)
testZeroInflation(sim_out.habitat)
plotResiduals(sim_out.habitat, form = caged_beta2$cage.treatment_std)
plotResiduals(sim_out.habitat, form = caged_beta2$var_ecotype1)
plotResiduals(sim_out.habitat, form = caged_beta2$gamma.richness_exp.name)
plotResiduals(sim_out.habitat, form = caged_beta2$betadisp.sample.size)
plotResiduals(sim_out.habitat, form = caged_beta2$abs.lat)
plotResiduals(sim_out.habitat, form = as.factor(caged_beta2$var_upper.source))
plotResiduals(sim_out.habitat, form = as.factor(caged_beta2$exp.name))

## ------------------------------------------- ##
# Plot 1: Model predictions over raw data ----
## ------------------------------------------- ##
emm.habitat <- emmeans(caging.betamod.habitat,
                       specs  = ~ cage.treatment_std * var_ecotype1,
                       type   = "response")
emm_df.habitat <- as.data.frame(emm.habitat)

ggplot() +
  geom_jitter(data = caged_beta2,
              aes(x = var_ecotype1,
                  y = betadisp.comm.dist,
                  color = cage.treatment_std),
              alpha = 0.15, size = 0.8,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)) +
  geom_pointrange(data = emm_df.habitat,
                  aes(x = var_ecotype1,
                      y = response,
                      ymin = asymp.LCL,
                      ymax = asymp.UCL,
                      color = cage.treatment_std),
                  position = position_dodge(width = 0.6),
                  size = 0.8) +
  scale_color_manual(values = c("caged" = "royalblue", "uncaged" = "darkorange"),
                     name = "Treatment") +
  labs(x = "Ecotype", y = "Beta dispersion") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## ------------------------------------------- ##
# Plot 2: Partial regression plots ----
## ------------------------------------------- ##
ggplot(emm_df.habitat, aes(x = cage.treatment_std,
                           y = response,
                           ymin = asymp.LCL,
                           ymax = asymp.UCL,
                           color = cage.treatment_std,
                           group = var_ecotype1)) +
  geom_pointrange(size = 0.7) +
  geom_line(color = "grey50") +
  facet_wrap(~ var_ecotype1, scales = "free_y") +
  scale_color_manual(values = c("caged" = "royalblue", "uncaged" = "darkorange"),
                     name = "Treatment") +
  labs(x = NULL, y = "Predicted beta dispersion\n(± 95% CI)") +
  theme_classic(base_size = 12) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))

caged_beta2$fitted_vals.habitat   <- predict(caging.betamod.habitat, type = "response")
caged_beta2$partial_resid.habitat <- caged_beta2$betadisp.comm.dist -
  caged_beta2$fitted_vals.habitat

ggplot(caged_beta2, aes(x = gamma.richness_exp.name,
                        y = partial_resid.habitat + fitted_vals.habitat,
                        color = cage.treatment_std)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ var_ecotype1) +
  scale_color_manual(values = c("caged" = "royalblue", "uncaged" = "darkorange"),
                     name = "Treatment") +
  labs(x = "Gamma richness (experiment-level)", y = "Beta dispersion (partial)") +
  theme_classic(base_size = 12) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))

ggplot(caged_beta2, aes(x = betadisp.sample.size,
                        y = partial_resid.habitat + fitted_vals.habitat,
                        color = cage.treatment_std)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ var_ecotype1) +
  scale_color_manual(values = c("caged" = "royalblue", "uncaged" = "darkorange"),
                     name = "Treatment") +
  labs(x = "Sample size (replicates per treatment)", y = "Beta dispersion (partial)") +
  theme_classic(base_size = 12) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))


## ============================================================== ##
# Caged vs Uncaged Model: Aquatic vs. Terrestrial
## ============================================================== ##
caging.betamod.aqterr <- glmmTMB(betadisp.comm.dist  ~ 
                                   cage.treatment_std * var_aq.or.terr +
                                   scale(gamma.richness_exp.name) + 
                                   scale(betadisp.sample.size) +
                                   (1 | var_upper.source / exp.name), 
                                 dispformula = ~ cage.treatment_std * var_aq.or.terr + abs.lat,
                                 family = ordbeta(link = 'probit'),
                                 control = glmmTMBControl(optimizer = optim, 
                                                          optArgs = list(method = "BFGS")), 
                                 data = caged_beta2) 
summary(caging.betamod.aqterr)    
car::Anova(caging.betamod.aqterr, type = "II")

## ------------------------------------------- ##
# DHARMa Validation: caging.betamod.aqterr ----
## ------------------------------------------- ##
sim_out.aqterr <- simulateResiduals(fittedModel = caging.betamod.aqterr, n = 1000, plot = FALSE)
plot(sim_out.aqterr)
testUniformity(sim_out.aqterr)
testDispersion(sim_out.aqterr)
testZeroInflation(sim_out.aqterr)
plotResiduals(sim_out.aqterr, form = caged_beta2$cage.treatment_std)
plotResiduals(sim_out.aqterr, form = caged_beta2$var_aq.or.terr)
plotResiduals(sim_out.aqterr, form = caged_beta2$gamma.richness_exp.name)
plotResiduals(sim_out.aqterr, form = caged_beta2$betadisp.sample.size)
plotResiduals(sim_out.aqterr, form = caged_beta2$abs.lat)
plotResiduals(sim_out.aqterr, form = as.factor(caged_beta2$var_upper.source))
plotResiduals(sim_out.aqterr, form = as.factor(caged_beta2$exp.name))

## ------------------------------------------- ##
# Plot 1: Model predictions over raw data ----
## ------------------------------------------- ##
emm.aqterr <- emmeans(caging.betamod.aqterr,
                      specs  = ~ cage.treatment_std * var_aq.or.terr,
                      type   = "response")
emm_df.aqterr <- as.data.frame(emm.aqterr)

ggplot() +
  geom_jitter(data = caged_beta2,
              aes(x = var_aq.or.terr,
                  y = betadisp.comm.dist,
                  color = cage.treatment_std),
              alpha = 0.15, size = 0.8,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)) +
  geom_pointrange(data = emm_df.aqterr,
                  aes(x = var_aq.or.terr,
                      y = response,
                      ymin = asymp.LCL,
                      ymax = asymp.UCL,
                      color = cage.treatment_std),
                  position = position_dodge(width = 0.6),
                  size = 0.8) +
  scale_color_manual(values = c("caged" = "royalblue", "uncaged" = "darkorange"),
                     name = "Treatment") +
  labs(x = "Ecosystem type", y = "Beta dispersion") +
  theme_classic(base_size = 13)

## ------------------------------------------- ##
# Plot 2: Partial regression plots ----
## ------------------------------------------- ##
ggplot(emm_df.aqterr, aes(x = cage.treatment_std,
                          y = response,
                          ymin = asymp.LCL,
                          ymax = asymp.UCL,
                          color = cage.treatment_std,
                          group = var_aq.or.terr)) +
  geom_pointrange(size = 0.7) +
  geom_line(color = "grey50") +
  facet_wrap(~ var_aq.or.terr) +
  scale_color_manual(values = c("caged" = "royalblue", "uncaged" = "darkorange"),
                     name = "Treatment") +
  labs(x = NULL, y = "Predicted beta dispersion\n(± 95% CI)") +
  theme_classic(base_size = 12) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))

caged_beta2$fitted_vals.aqterr   <- predict(caging.betamod.aqterr, type = "response")
caged_beta2$partial_resid.aqterr <- caged_beta2$betadisp.comm.dist -
  caged_beta2$fitted_vals.aqterr

ggplot(caged_beta2, aes(x = gamma.richness_exp.name,
                        y = partial_resid.aqterr + fitted_vals.aqterr,
                        color = cage.treatment_std)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ var_aq.or.terr) +
  scale_color_manual(values = c("caged" = "royalblue", "uncaged" = "darkorange"),
                     name = "Treatment") +
  labs(x = "Gamma richness (experiment-level)", y = "Beta dispersion (partial)") +
  theme_classic(base_size = 12) +
  theme(strip.background = element_blank())

ggplot(caged_beta2, aes(x = betadisp.sample.size,
                        y = partial_resid.aqterr + fitted_vals.aqterr,
                        color = cage.treatment_std)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ var_aq.or.terr) +
  scale_color_manual(values = c("caged" = "royalblue", "uncaged" = "darkorange"),
                     name = "Treatment") +
  labs(x = "Sample size (replicates per treatment)", y = "Beta dispersion (partial)") +
  theme_classic(base_size = 12) +
  theme(strip.background = element_blank())


# MAX STOPPED HERE 6/2/26



# MODEL 1: beta dispersion ~ aqu.terr * abslat + gamma + samplesize + (1/exp.name) 
# this is only on the uncaged data 

# Uncaged data 
# uncaged.beta2 <- caged_beta2 %>%
#   dplyr::filter(cage.treatment_std == "uncaged") %>%
#   filter(!abs.lat > 75) %>%
#   droplevels()

caged_beta2 %>%
  group_by(var_aq.or.terr) %>%
  summarize(n())

# try running with just late succession
uncaged.late <- caged_beta2 %>%
  filter(cage.treatment_std == "uncaged") %>%
  filter(var_succ.vs.late == "late")
  

uncaged.late.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                              var_aq.or.terr * scale(abs.lat)  +
                             # accounting for gamma richness and sample size
                             scale(gamma.richness) + scale(betadisp.sample.size) +
                              (1|exp.name), 
                            dispformula = ~ var_aq.or.terr, #+ abs.lat,
                            family = beta_family(link = "probit"),
                            control = glmmTMBControl(optimizer = optim, 
                                                     optArgs = list(method = "BFGS")), 
                            # Or use nlminb, or bobyqa via nloptr
                            data = uncaged.late) 
# scaling the continuous variables reduced multicollinearity to low from moderate

AIC(uncaged.late.betamod)
summary(uncaged.late.betamod)    
car::Anova(uncaged.late.betamod, type = "II") # interaction is significant

library(effects)
plot(allEffects(uncaged.late.betamod))
# beta dispresion increases with gamma richness and sample size
# beta dispersion increases with latitude for aquatic, but decreases with latitude for terrestrial

check_model(uncaged.betamod) # NOTE -- THIS DOESN'T RUN ON MAX'S MACHINE
# JMI: works for me other than homogeneity not printing 

check_collinearity(uncaged.betamod)


# MODEL 2: beta.disp ~ caging * abs(latitude) * ecosystem type + (1/exp.name)
# this is on the caged and uncaged data 
three.way.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                               var_aq.or.terr * abs.lat * cage.treatment_std  +
                               gamma.richness + betadisp.sample.size +
                               (1|exp.name), 
                             dispformula = ~ var_aq.or.terr, #+ abs.lat,
                             family = beta_family(link = "probit"),
                             control = glmmTMBControl(optimizer = optim, 
                                                      optArgs = list(method = "BFGS")), 
                             # Or use nlminb, or bobyqa via nloptr
                             data = caged_beta2) 
AIC(three.way.betamod)
summary(three.way.betamod)    
car::Anova(three.way.betamod, type = "II")
# update april 3, now the interaction is sig (0.05) - we've had some fluctuations here depending on data going in

plot(allEffects(three.way.betamod))
check_model(three.way.betamod)


# Rerun without the threeway interaction- only keep the significant interactions from above
two.way.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                             # scaling continuous predictors does not affect results but does help with multicollinearity
                               var_aq.or.terr * scale(abs.lat)  + 
                               scale(abs.lat) *cage.treatment_std  +
                               scale(gamma.richness) + 
                               scale(betadisp.sample.size) +
                               (1|exp.name), 
                             dispformula = ~ var_aq.or.terr, #+ abs.lat,
                             family = beta_family(link = "probit"),
                             control = glmmTMBControl(optimizer = optim, 
                                                      optArgs = list(method = "BFGS")), 
                             # Or use nlminb, or bobyqa via nloptr
                             data = caged_beta2) 
AIC(two.way.betamod)
summary(two.way.betamod)    
car::Anova(two.way.betamod, type = "II")

library(effects)
plot(allEffects(two.way.betamod))
# terrestrial beta diversity decreaes with latitude, aquatic slightly increases (regardless of caging)
# caged (No consumers) has a stronger decline with latitude than uncaged (regardless of aquatic vs terrestrial)

check_model(two.way.betamod) # NOTE -- THIS DOESN'T RUN ON MAX'S MACHINE, JMI- if you wait >5mins it works :)
# homogeneity doesn't print but everything else does!


# Try the model with caged data only to see if this helps us understand why the three way interaction is not significant


# Try running model on just late successional data
late.data <- caged_beta2 %>%
  filter(var_succ.vs.late == "late")

late.data %>%
  group_by(var_aq.or.terr) %>%
  summarize(n()) # aquatic is only ~20% of our data

three.way.betamod.late <- glmmTMB(betadisp.comm.dist_transform ~ 
                               var_aq.or.terr * abs.lat * cage.treatment_std  +
                               gamma.richness + betadisp.sample.size +
                               (1|exp.name), 
                             dispformula = ~ var_aq.or.terr, #+ abs.lat,
                             family = beta_family(link = "probit"),
                             control = glmmTMBControl(optimizer = optim, 
                                                      optArgs = list(method = "BFGS")), 
                             # Or use nlminb, or bobyqa via nloptr
                             data = late.data) 
AIC(three.way.betamod.late)
summary(three.way.betamod.late)    
car::Anova(three.way.betamod.late, type = "II") # three way interaction is no longer sig 


# Reduced model 
two.way.betamod.late <- glmmTMB(betadisp.comm.dist_transform ~ 
                                    var_aq.or.terr + 
                                  abs.lat * cage.treatment_std  +
                                    gamma.richness + betadisp.sample.size +
                                    (1|exp.name), 
                                  dispformula = ~ var_aq.or.terr, #+ abs.lat,
                                  family = beta_family(link = "probit"),
                                  control = glmmTMBControl(optimizer = optim, 
                                                           optArgs = list(method = "BFGS")), 
                                  # Or use nlminb, or bobyqa via nloptr
                                  data = late.data) 
AIC(two.way.betamod.late)
summary(two.way.betamod.late)    
car::Anova(two.way.betamod.late, type = "II") # three way interaction is no longer sig 

plot(allEffects(two.way.betamod.late))




# Caged data
# try running with just late succession
caged.late <- caged_beta2 %>%
  filter(cage.treatment_std == "caged") %>%
  filter(var_succ.vs.late == "late")


caged.late.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                                  var_aq.or.terr * scale(abs.lat)  +
                                  # accounting for gamma richness and sample size
                                  scale(gamma.richness) + scale(betadisp.sample.size) +
                                  (1|exp.name), 
                                dispformula = ~ var_aq.or.terr, #+ abs.lat,
                                family = beta_family(link = "probit"),
                                control = glmmTMBControl(optimizer = optim, 
                                                         optArgs = list(method = "BFGS")), 
                                # Or use nlminb, or bobyqa via nloptr
                                data = caged.late) 
# scaling the continuous variables reduced multicollinearity to low from moderate

AIC(uncaged.late.betamod)
summary(uncaged.late.betamod)    
car::Anova(caged.late.betamod, type = "II") # interaction is significant


plot(allEffects(caged.late.betamod))


caged.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                             var_aq.or.terr * abs.lat  +
                             # accounting for gamma richness and sample size
                             gamma.richness + betadisp.sample.size +
                             (1|exp.name), 
                           dispformula = ~ var_aq.or.terr, #+ abs.lat,
                           family = beta_family(link = "probit"),
                           control = glmmTMBControl(optimizer = optim, 
                                                    optArgs = list(method = "BFGS")), 
                           # Or use nlminb, or bobyqa via nloptr
                           data = caged.beta2) 
AIC(caged.betamod)
summary(caged.betamod)    
car::Anova(caged.betamod, type = "II") # interaction is significant

plot(allEffects(caged.betamod))
# difference from the uncaged model is the aquatic slope is now negative, terrestrial looks the same




# Can we run separate models for aquatic and terrestrial?
caged_beta2 %>%
  group_by(var_aq.or.terr) %>%
  summarize(n()) # aquatic is only ~20% of our data
2124/10402

unique(aquatic.beta$exp.name) # 88
unique(terrestrial.beta$exp.name) # 212
# ~29% is aquatic 

unique(aquatic.beta$source) # 32
unique(terrestrial.beta$source) # 77
# ~29% is aquatic

caged_beta2 %>%
  group_by(var_aq.or.terr, var_ecotype1) %>%
  summarize(n()) 

5809/10380 # 56% are grassland





aquatic.beta <- caged_beta2 %>%
  dplyr::filter(var_aq.or.terr == "aquatic") %>%
  filter(!abs.lat > 75) %>%
  droplevels()


aquatic.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                               scale(abs.lat) * cage.treatment_std  +
                               scale(gamma.richness) + scale(betadisp.sample.size) +
                               (1|exp.name), 
                             #dispformula = ~ var_aq.or.terr, #+ abs.lat,
                             family = beta_family(link = "probit"),
                             control = glmmTMBControl(optimizer = optim, 
                                                      optArgs = list(method = "BFGS")), 
                             # Or use nlminb, or bobyqa via nloptr
                             data = aquatic.beta) 

car::Anova(aquatic.betamod, type = "II") # interaction is significant
plot(allEffects(aquatic.betamod))

check_model(aquatic.betamod)
check_collinearity(aquatic.betamod)



terrestrial.beta <- caged_beta2 %>%
  dplyr::filter(var_aq.or.terr == "terrestrial") %>%
  droplevels()


terrestrial.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                             scale(abs.lat) * cage.treatment_std  +
                             scale(gamma.richness) + scale(betadisp.sample.size) +
                             (1|exp.name), 
                           #dispformula = ~ var_aq.or.terr, #+ abs.lat,
                           family = beta_family(link = "probit"),
                           control = glmmTMBControl(optimizer = optim, 
                                                    optArgs = list(method = "BFGS")), 
                           # Or use nlminb, or bobyqa via nloptr
                           data = terrestrial.beta) 

car::Anova(terrestrial.betamod, type = "II") # interaction is significant
plot(allEffects(terrestrial.betamod))

check_model(terrestrial.betamod)
check_collinearity(terrestrial.betamod) 



# Code that needs to be cleaned is below 



# Assign the best model
uncaged.betamod 

# Summarize model output and statistics 
anova_table <- Anova(uncaged.betamod) %>%
  tidy()

# === 1. Format the ANOVA table ===
# Convert p-values to numeric and format them in decimal notation
anova_table <- anova_table %>%
  mutate(significance = case_when(
    p.value < 0.0001 ~ "****",
    p.value < 0.001  ~ "***",
    p.value < 0.01   ~ "**",
    p.value < 0.05   ~ "*",
    p.value < 0.1    ~ ".",
    TRUE             ~ "NA"
  )) %>%
  mutate(
    p.value = ifelse(p.value < 0.0001,
                     "< 0.0001",
                     formatC(p.value, format = "f", digits = 4)),
    statistic = round(statistic, 1)
  )

# === 2. Define model and file output ===
# Define output file path
outfile <- "results/uncaged_plots_beta_regression_anova_table_output.txt"

# Add a custom header with model formula
model_formula <- "ANOVA Table for Beta Regression Model\n
                  Probit link function and log-link dispersion parameter varying with Ecosystem Type\n
                  Beta_dispersion_distance ~ |Latitude| * Ecosystem_Type + (1|Experiment)"

# === 3. Capture model summary ===
model_summary_text <- capture.output(summary(uncaged.betamod))

# === 4. Capture header and formatted ANOVA table ===
anova_text <- capture.output({
  cat("\n")
  cat("====================================\n")
  cat(model_formula, "\n")
  cat("====================================\n")
  print(anova_table)
})

# === 5. Write everything to file ===
writeLines(c(model_summary_text, "", anova_text), con = outfile)

# === 6. Also print to console ===
cat(paste(model_summary_text, collapse = "\n"))
cat("\n\n")
print(anova_table)


## ------------------------------------------- ##
## Validate Beta Regression Model ----
## ------------------------------------------- ##

# Validate with DHARMa

# === Inputs ===
dat <- caged_beta3
fittedModel <- uncaged.betamod
dirloc <- "graphs/model_validation/"
outfile <- "model_diagnostics_summary.txt"
prefix <- "uncaged_plots_beta_regression_"

# === 1. Run simulation ===
simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = FALSE)

# === 2. Produce and save figures ===

# Plot 1: general residuals
png(paste0(dirloc, prefix, "residuals_overall.png"), width = 800, height = 600)
plot(simulationOutput)
dev.off()

# Note that there is "significant" deviation, but it is easy to get because n > 5,000!
# The deviation does not look troubling in its magnitude

# Plot 2: residuals vs. var_aq.or.terr
png(paste0(dirloc, prefix, "residuals_vs_var_aq_or_terr.png"), width = 800, height = 600)
plotResiduals(simulationOutput, form = dat$var_aq.or.terr)
dev.off()

# Plot 3: residuals vs. abs.lat
png(paste0(dirloc, prefix, "residuals_vs_abs_lat.png"), width = 800, height = 600)
plotResiduals(simulationOutput, form = dat$abs.lat)
dev.off()

# Plot 4: residuals vs. study
png(paste0(dirloc, prefix, "residuals_vs_source.png"), width = 2000, height = 600)
plotResiduals(simulationOutput, form = factor(dat$source))
dev.off()

# === 3. Run and capture diagnostics ===
dispersion_test_output <- capture.output(testDispersion(simulationOutput))
resid_variance <- mean(simulationOutput$scaledResiduals^2)

# === 4. Compose commentary and write to file ===
diagnostic_notes <- c(
  "=== DHARMa Model Validation Summary ===",
  "",
  "Note: Due to the large sample size (n > 5,000), statistically significant tests of dispersion",
  "can arise from very small deviations. This does not necessarily indicate poor model fit.",
  "",
  ">>> Output of testDispersion():",
  dispersion_test_output,
  "",
  paste0(">>> Mean of scaled residuals squared: ", round(resid_variance, 6)),
  "",
  "Interpretation:",
  "The DHARMa test reports underdispersion (dispersion < 1), but residual plots show no visible structure,",
  "and the mean squared scaled residuals (~", round(resid_variance, 6), ") are close to the theoretical expectation of ~0.333.",
  "This supports the conclusion that any underdispersion is minor and likely not of practical concern.",
  "",
  "See saved PNG files for residual plots:",
  paste0("- ", prefix, "residuals_overall.png"),
  paste0("- ", prefix, "residuals_vs_var_aq_or_terr.png"),
  paste0("- ", prefix, "residuals_vs_abs_lat.png")
)

# === 5. Write everything to TXT file ===
writeLines(diagnostic_notes, con = paste0(dirloc, outfile))

# FOR METHODS SECTION OF PAPER:
# We modeled the beta dispersion distance from uncaged (control) replicates/plots using a beta regression with a probit link function, fitted via the glmmTMB package in R. The beta dispersion data had a few values (~1% of data) equal to exactly zero; to conform with the assumptions of beta regression, we applied a minor transform to bring such values slightly above zero: y * (n - 1) + 0.5 (Smithson & Verkuilen, 2006). We selected the probit link based on substantially improved model fit (ΔAIC > 10) and slightly better-behaved residuals. Diagnostic checks using the DHARMa package showed minor underdispersion, with no evidence of residual patterns or misspecification.


## ------------------------------------------- ##
## Generate Predictions for Beta Regression Model ----
## ------------------------------------------- ##
# Generate prediction grid
newdata <- expand.grid(
  var_aq.or.terr     = unique(dat$var_aq.or.terr),
  abs.lat            = seq(min(dat$abs.lat), max(dat$abs.lat), length.out = 100)
)

# Dummy grouping vars for random effect prediction
newdata$exp.name <- NA

# Get predictions with SE
preds <- predict(fittedModel, newdata = newdata, type = "response", se.fit = TRUE)
newdata$fit   <- preds$fit
newdata$se    <- preds$se.fit
newdata$lower <- newdata$fit - 1.96 * newdata$se
newdata$upper <- newdata$fit + 1.96 * newdata$se

# Plot: points = raw data; line = prediction; ribbon = 95% CI; facet = two key interactions

dat$var_aq.or.terr <- as.character(dat$var_aq.or.terr)
newdata$var_aq.or.terr <- as.character(newdata$var_aq.or.terr)

dat$var_aq.or.terr[dat$var_aq.or.terr == "terrestrial"] <- "Terrestrial"
dat$var_aq.or.terr[dat$var_aq.or.terr == "aquatic"] <- "Aquatic/Marine"

newdata$var_aq.or.terr[newdata$var_aq.or.terr == "terrestrial"] <- "Terrestrial"
newdata$var_aq.or.terr[newdata$var_aq.or.terr == "aquatic"] <- "Aquatic/Marine"

my_colors <- c("Aquatic/Marine" = "#1f78b4",      # blue
               "Terrestrial" = "#33a02c")  # green

my_shapes <- c("Aquatic/Marine" = 16,    # solid circle
               "Terrestrial" = 17)  # solid triangle

p1 <- ggplot(dat, aes(x = abs.lat, y = betadisp.comm.dist_transform)) +
  geom_ribbon(data = newdata, aes(x = abs.lat, ymin = lower, ymax = upper, fill = var_aq.or.terr),
              alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = newdata, aes(y = fit, color = var_aq.or.terr), size = 1, alpha = 1) +
  geom_point(aes(color = var_aq.or.terr, shape = var_aq.or.terr), alpha = 0.4) +
  facet_grid( ~ var_aq.or.terr) +
  labs(y = "Beta Dispersion Distance",
       x = "| Latitude |",
       title = "Uncaged plots by ecosystem type and latitude: data and model fits") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        aspect.ratio = 1,
        axis.text = element_text(color = 'black')) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.2)) +
  scale_x_continuous(breaks = seq(-60, 100, by = 20))+
  theme(legend.position="none")
p1
ggsave("graphs/model_predictions/uncaged_plots_beta_regression_data.and.model.preds.pdf", height = 4, width = 6)
ggsave("graphs/model_predictions/uncaged_plots_beta_regression_data.and.model.preds.jpg", height = 4, width = 6)

p2 <- ggplot(dat, aes(x = var_aq.or.terr, y = betadisp.comm.dist_transform)) +
  geom_boxplot(aes(fill = var_aq.or.terr)) +
  labs(y = "",
       x = "\n\n",
       title = "\n\n") +
  theme_classic() +
  theme(aspect.ratio = 3,
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        title = element_text(size = 9)) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.2)) +
  scale_x_discrete(labels = NULL) +
  theme(legend.position="none")
p2

# Combine figures

# Shrink internal margins
tight_theme <- theme(
  plot.margin = margin(2, 2, 2, 2),  # top, right, bottom, left
  panel.spacing = unit(0.2, "lines")  # space between facet panels
  #strip.text = element_text(margin = margin(0, 0, 0, 0))  # remove facet label padding
)

# Apply to plots
p1_tight <- p1 + tight_theme
p2_tight <- p2 + tight_theme

# Convert to grobs
g1 <- ggplotGrob(p1_tight)
g2 <- ggplotGrob(p2_tight)

# Combine via grid.arrange with manual width tuning
combined_plot <- grid.arrange(
  g1, g2,
  ncol = 2,
  widths = unit.c(unit(3, "null"), unit(0.7, "null"))  # adjust right-side width as needed
)

# Save
ggsave("graphs/model_predictions/uncaged_plots_beta_regression_combined.pdf",
       plot = combined_plot, width = 8, height = 4, device = "pdf")
ggsave("graphs/model_predictions/uncaged_plots_beta_regression_combined.jpg",
       plot = combined_plot, width = 8, height = 4, device = "jpeg")


# ------------------------------------------------------------------------------------------------------



# - Following code is from Kelly -----------------------------------------------

# fitting and validating 3 models from our Data Analysis Team meeting on 8/14/25

## - Model 1: ------------------------------------------------------------------
#Three way interaction: beta.disp ~ caging * abs(latitude) * ecosystem type
# beta regression

require(effects)

# use dataframe caged_beta2
# has beta dispersion data from caged and uncaged 

three.way.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                             var_aq.or.terr * abs.lat* cage.treatment_std  +
                             (1|exp.name), 
                           dispformula = ~ var_aq.or.terr, #+ abs.lat,
                           family = beta_family(link = "probit"),
                           control = glmmTMBControl(optimizer = optim, 
                                                    optArgs = list(method = "BFGS")), 
                           # Or use nlminb, or bobyqa via nloptr
                           data = caged_beta2) 
AIC(three.way.betamod)
summary(three.way.betamod)    
car::Anova(three.way.betamod, type = "II")

library(effects)
plot(allEffects(three.way.betamod))

check_model(three.way.betamod) # NOTE -- THIS DOESN'T RUN ON MAX'S MACHINE

# Validate with DHARMa
dat <- caged_beta2
fittedModel <- three.way.betamod
 
simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F)

plot(simulationOutput)
# # Note that there is "significant" deviation, but there are so many observations that this may not be meaningful

# In addition to plotting the residuals vs. fitted, we must plot residuals vs. individual predictors
plotResiduals(simulationOutput, form = dat$cage.treatment_std)
plotResiduals(simulationOutput, form = dat$var_aq.or.terr)
plotResiduals(simulationOutput, form = dat$abs.lat)

testDispersion(simulationOutput)

# plot raw data and model predictions

three.way.pred<-data.frame(emmeans(three.way.betamod, ~ var_aq.or.terr *cage.treatment_std | abs.lat,
                                    at = list(abs.lat = c(0,10,20,30,40,50,60,70)), type = "response"))
## - Model 2: ------------------------------------------------------------------
#Two way interaction: abs(diff(beta.disp)) ~ abs(latitude) * ecosystem type
# beta regression
# directly copied max's code to put all 3 models in one place
# Fit model on only uncaged data
caged_beta3 <- caged_beta2 %>%
  dplyr::filter(cage.treatment_std == "uncaged") %>%
  droplevels()

uncaged.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                             var_aq.or.terr * abs.lat  +
                             (1|exp.name), 
                           dispformula = ~ var_aq.or.terr, #+ abs.lat,
                           family = beta_family(link = "probit"),
                           control = glmmTMBControl(optimizer = optim, 
                                                    optArgs = list(method = "BFGS")), 
                           # Or use nlminb, or bobyqa via nloptr
                           data = caged_beta3) 
AIC(uncaged.betamod)
summary(uncaged.betamod)    
car::Anova(uncaged.betamod, type = "II")

plot(allEffects(uncaged.betamod))

## - Model 3: ------------------------------------------------------------------
#Two way interaction: abs(diff(beta.disp)) ~ abs(latitude) * ecosystem type
# beta regression

# absolute value of difference
caged_effectsize_abs <- caged_effectsize %>% 
  mutate(ablat = abs(lat), 
         abdiff = abs(within.cage.treat_betadisp.mean.diff) )

range(caged_effectsize_abs$abdiff) #  0.0002819578 0.4272029784
hist(caged_effectsize_abs$abdiff) # try a beta regression?


Baediff.betamod1 <- glmmTMB(abdiff ~ 
                              var_aq.or.terr*ablat, #+
                            
                            # (1|source), 
                            #    dispformula = ~ var_aq.or.terr + abs.lat,
                            family = beta_family(link = "logit"),
                            control = glmmTMBControl(optimizer = optim, 
                                                     optArgs = list(method = "BFGS")), 
                            # Or use nlminb, or bobyqa via nloptr
                            data = caged_effectsize_abs) 

summary(Baediff.betamod1)    
car::Anova(Baediff.betamod1, type = "II")
# not significant when you use a beta regression

plot(allEffects(Baediff.betamod1))

check_model(Baediff.betamod1) # looks like it fits well? 



# Max stopped checking code here 10/14/2025
# All code seems to be running well, more or less, except "check_model(three.way.betamod)"


## ------------------------------------------- ##
# Exploratory Models ----
## ------------------------------------------- ##

#RVs
#1. Beta dispersion, per plot, for each experiment (right?? or per-site?) 
#betadisp.comm.dist = from caged_beta and caged_v1, average community distance 
#2. Beta dispersion Effect Size (Uncaged - Caged)
#within.cage.treat_betadisp.mean.diff = from caged_beta and caged_v1, mean beta difference bt uncaged and caged. negative = consumers decrease B dispersion, positive = consumers increase B dispersion
#3. Beta dispersion ES absolute value

#IVs
#cage.treatment_std + ecotype1 + latitude + consumer richness + experiment age + gamma diversity + excl size size + successional stage + max consumer size + B sample size + B design level 
#2way Int IVs: cage.treatment_std*var_ecotype1 OR cage.treatment_std*lat
#3way Int IVs: cage.treatment_std*var_ecotype1*lat
# betadisp.comm.dist ~ cage.treatment_std + ecotype1 + latitude + consumer richness + experiment age + gamma diversity + excl size size + successional stage + max consumer size + B sample size + B design level 

#REs
#(1|expname) or (1| source/expname)

#1. Beta dispersion (raw) models 
#Leave name of best fitting/most ecological sense model up here for reference (will delete when final models selected)
#BaeDisp.lmer

#caged_beta <- read.csv(file.path("data", "caged_beta.csv"))

## ------------------------------------------- ##
# Models I.  ----
# Beta Dispersion (raw)
## ------------------------------------------- ##

#I.i LMERs: RE = 1|exp.name ----
#Cage*Habitat interaction + latitude
#No interaction detected, main effect of habitat and latitude
BaeDisp_lat.lmer <- lmer(betadisp.comm.dist ~ 
                           cage.treatment_std*var_aq.or.terr  +  
                           abs(lat) + 
                           (1|exp.name), data = caged_beta)

check_model(BaeDisp_lat.lmer)
summary(BaeDisp_lat.lmer)
car::Anova(BaeDisp_lat.lmer, test.statistic = "F") 
performance::r2(BaeDisp_lat.lmer)

emmip(BaeDisp_lat.lmer, ~ cage.treatment_std |var_ecotype1)
emmip(BaeDisp_lat.lmer, ~ var_consumer.richness.category)

plot_model(BaeDisp_lat.lmer)

#Cage*Habitat w richness predictors
#cool interactions here!
BaeDisp_richsamp.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_aq.or.terr  + 
                       var_consumer.richness.category + 
                       gamma.richness + abs(lat) + 
                       #exp.age + 
                       betadisp.sample.size + 
                       (1|exp.name), data = caged_beta)

check_model(BaeDisp_richsamp.lmer)
summary(BaeDisp_richsamp.lmer)
car::Anova(BaeDisp_richsamp.lmer, test.statistic = "F") 

emmip(BaeDisp_richsamp.lmer, ~ cage.treatment_std |var_aq.or.terr)
emmip(BaeDisp_richsamp.lmer, ~ var_consumer.richness.category)

plot_model(BaeDisp_richsamp.lmer)

#Cool double 2 way model with richness and ecotype interactions
BaeDisp2Way.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_aq.or.terr + 
                       cage.treatment_std:var_consumer.richness.category + 
                       gamma.richness + 
                       abs(lat) + 
                       #exp.age + 
                       betadisp.sample.size + 
                       (1|exp.name), data = caged_beta)

check_model(BaeDisp2Way.lmer, panel = F) %>% plot()
check_collinearity(BaeDisp2Way.lmer)
summary(BaeDisp2Way.lmer)
car::Anova(BaeDisp2Way.lmer, test.statistic = "F")
performance::r2(BaeDisp2Way.lmer)

#3way interactions 
#"rank deficient" but bolker says that is ok. 
BaeDisp3way.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_ecotype1*var_consumer.richness.category + gamma.richness + #lat + exp.age + 
                           betadisp.sample.size + 
                           (1|exp.name), data = caged_beta)

check_model(BaeDisp3way.lmer, panel = F) |> plot() #resid normality is wack
summary(BaeDisp3way.lmer)
car::Anova(BaeDisp3way.lmer, test.statistic = "F")
performance::r2(BaeDisp3way.lmer)

emmip(BaeDisp3way.lmer, ~ cage.treatment_std | consumer.richness.category)

AIC(BaeDisp.lmer, BaeDispsimp.lmer, BaeDisp3way.lmer)

#LMER: nested RE = 1|source/exp.name ----
#Cage*Habitat interaction + latitude
#No interaction detected, main effect of habitat and latitude
BaeDisp_lat.nestlmer <- lmer(betadisp.comm.dist ~ 
                           cage.treatment_std*var_aq.or.terr  +  
                           abs(lat) + 
                           (1|source/exp.name), data = caged_beta)

check_model(BaeDisp_lat.nestlmer, panel = F) |> plot() #plot them all 
summary(BaeDisp_lat.nestlmer)
car::Anova(BaeDisp_lat.nestlmer, test.statistic = "F") 

emmip(BaeDisp_lat.nestlmer, ~ cage.treatment_std |var_aq.or.terr)

plot_model(BaeDisp_lat.lmer)

#LMER: random slope RE = cage.treatment_std|source/exp.name ----
#Cage*Habitat interaction + latitude
#No interaction detected, main effect of habitat and latitude

#Bdisp ME random slope----
#3way cage eco richness interaction
#Singular
#BaeDisp3way_slop.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1*consumer.richness.category + gamma.richness + #lat + exp.age + 
#                                betadisp.sample.size + 
#                                (cage.treatment_std|source/exp.name), data = caged_beta)

#check_model(BaeDisp3way_slop.lmer, panel = F) |> plot() #plot them all 
#summary(BaeDisp3way_slop.lmer)
#car::Anova(BaeDisp3way_slop.lmer, test.statistic = "F") #takes 1 million minutes to run
#performance::r2(BaeDisp3way_slop.lmer)

#I.ii GLMER: Max is going to try some Beta regressions----




## ------------------------------------------- ##
# Models II. Consumer Effect Size (Difference in Beta Dispersion) ----
## ------------------------------------------- ##

glimpse(caged_effectsize)
#II.i LMER:  RE = 1|source ----
#Habitat * latitude... no effects

BaeES_lat.lmer <- lmer(within.cage.treat_betadisp.mean.diff ~ 
                     var_aq.or.terr * abs(lat) +
                     (1|source), data = caged_effectsize)

check_model(BaeES_lat.lmer, panel = F) %>% plot()
check_collinearity(BaeES_lat.lmer)
summary(BaeES_lat.lmer)
car::Anova(BaeES_lat.lmer, test.statistic = "F")
performance::r2(BaeES_lat.lmer)

plot_model(BaeES_lat.lmer)

#Lat and Habitat Type
#cool interactions here!
BaeES_richsamp.lmer <- lmer(within.cage.treat_betadisp.mean.diff ~ 
                              var_aq.or.terr  + 
                              var_consumer.richness.category + 
                              gamma.richness + 
                              abs(lat) + 
                              betadisp.sample.size + 
                              (1|source), data = caged_effectsize)

check_model(BaeES_richsamp.lmer)
summary(BaeES_richsamp.lmer)
car::Anova(BaeES_richsamp.lmer, test.statistic = "F") 

emmip(BaeES_richsamp.lmer, ~ var_consumer.richness.category)

plot_model(BaeES_richsamp.lmer)




## ------------------------------------------- ##
# Models III. Absolute Value Consumer Effect Size (Difference in Beta Dispersion) ----
## ------------------------------------------- ##
#I dont like this response variable

caged_effectsize_lat = caged_effectsize %>% 
  mutate(ablat = abs(lat), abdiff = abs(within.cage.treat_betadisp.mean.diff) )

LatHabIntES.lmer <- lmer(abdiff ~ 
                     ablat*var_aq.or.terr + 
                       (1|source), 
                   data = caged_effectsize_lat)

LatHabES.lmer <- lmer(abdiff ~ 
                        ablat+var_aq.or.terr + 
                        (1|source), 
                      data = caged_effectsize_lat)
AIC(LatHabIntES.lmer, LatHabES.lmer)

check_model(LatHabIntES.lmer, panel = F) %>% plot()
summary(LatHabIntES.lmer)
car::Anova(LatHabIntES.lmer, test.statistic = "F")
performance::r2(LatHabES.lmer)

emmip(LatHabIntES.lmer, var_aq.or.terr ~ ablat, cov.reduce = range)
emtrends(LatHabIntES.lmer, "var_aq.or.terr", var = "ablat")



# # MAX'S OLD CODE BELOW -- PLEASE DO NOT DELETE YET
# 
# 
# ## ------------------------------------------- ##
# ## Fit Beta Regression Models ----
# ## ------------------------------------------- ##
# 
# # The scatterplots above suggested that trends using an absolute value of latitude might yield different results than latitude. This is perhaps because of the distribution of studies along the latitudinal gradient.
# 
# # Hence modeling latitude using a parabolic function (y ~ poly(x, 2)). Tried these models using an absolute latitude and found the same result (no change in significance)
# 
# # Nesting within source is generally preferable because it better reflects study-level heterogeneity.
# # (1 | exp.name) assumes all experiments are independent, even if they come from the same paper. This underestimates variance and overstates precision, inflating test statistics, treating intra-paper correlation as noise or ignoring it.
# # (1 | source / exp.name) models the hierarchical structure in which experiments within a paper are not fully independent. This increases the estimated uncertainty (increasing p-values vs. not including source), especially if some papers contribute many experiments. 
# # Hence, using random effects as "(1|source/exp.name)"
# 
# # In preliminiary model fitting, DHARMa indicated a bit of problem with dispersion, so adding a dispersion term to model not only the mean response but the variance of the response. Essentially this is allowing the dispersion to differ between aquatic and terrestrial studies, and with latitude. This is pretty apparent from the exploratory plots above, too.
# 
# # Fit model 1 on all data
# caged.uncaged.betamod1 <- glmmTMB(betadisp.comm.dist_transform ~ 
#                                     cage.treatment_std +
#                                     var_aq.or.terr     +
#                                     poly(lat, 2)       +
#                                     cage.treatment_std * var_aq.or.terr +
#                                     cage.treatment_std * poly(lat, 2)   +
#                                     var_aq.or.terr     * poly(lat, 2)   +
#                                     cage.treatment_std * var_aq.or.terr * poly(lat, 2) + 
#                                     (1|source/exp.name), 
#                                   dispformula = ~ var_aq.or.terr + poly(lat, 2),
#                                   family = beta_family(link = "logit"),
#                                   control = glmmTMBControl(optimizer = optim, 
#                                                            optArgs = list(method = "BFGS")), 
#                                   # Or use nlminb, or bobyqa via nloptr
#                                   data = caged_beta2) 
# summary(caged.uncaged.betamod1)    
# car::Anova(caged.uncaged.betamod1, type = "II")
# 
# # Drop 3-way interaction
# caged.uncaged.betamod1 <- glmmTMB(betadisp.comm.dist_transform ~ 
#                                     cage.treatment_std +
#                                     var_aq.or.terr     +
#                                     poly(lat, 2)       +
#                                     cage.treatment_std * var_aq.or.terr +
#                                     cage.treatment_std * poly(lat, 2)   +
#                                     var_aq.or.terr     * poly(lat, 2)   +
#                                     (1|source/exp.name), 
#                                   dispformula = ~ var_aq.or.terr + poly(lat, 2),
#                                   family = beta_family(link = "logit"),
#                                   control = glmmTMBControl(optimizer = optim, 
#                                                            optArgs = list(method = "BFGS")), 
#                                   # Or use nlminb, or bobyqa via nloptr
#                                   data = caged_beta2) 
# summary(caged.uncaged.betamod1)    
# car::Anova(caged.uncaged.betamod1, type = "II")
# 
# # Drop 2-way interaction of cage treatment * aquatic/terrestrial
# caged.uncaged.betamod1 <- glmmTMB(betadisp.comm.dist_transform ~ 
#                                     cage.treatment_std +
#                                     var_aq.or.terr     +
#                                     poly(lat, 2)       +
#                                     cage.treatment_std * poly(lat, 2)   +
#                                     var_aq.or.terr     * poly(lat, 2)   +
#                                     (1|source/exp.name), 
#                                   dispformula = ~ var_aq.or.terr + poly(lat, 2),
#                                   family = beta_family(link = "logit"),
#                                   control = glmmTMBControl(optimizer = optim, 
#                                                            optArgs = list(method = "BFGS")), 
#                                   # Or use nlminb, or bobyqa via nloptr
#                                   data = caged_beta2) 
# summary(caged.uncaged.betamod1)    
# car::Anova(caged.uncaged.betamod1, type = "III")
# 
# # Summarize model output and statistics - Type II
# anova_table <- Anova(caged.uncaged.betamod1, type = "II") %>%
#   tidy()
# 
# # Convert p-values to numeric and format them in decimal notation
# anova_table <- anova_table %>%
#   mutate(
#     p.value = ifelse(p.value < 0.0001,
#                      "< 0.0001",
#                      formatC(p.value, format = "f", digits = 4)),
#     statistic = round(statistic, 1)
#   )
# print("Type II Sums-of-squares")
# anova_table
# 
# # Summarize model output and statistics - Type III
# anova_table <- Anova(caged.uncaged.betamod1, type = "III") %>%
#   tidy()
# 
# # Convert p-values to numeric and format them in decimal notation
# anova_table <- anova_table %>%
#   mutate(
#     p.value = ifelse(p.value < 0.0001,
#                      "< 0.0001",
#                      formatC(p.value, format = "f", digits = 4)),
#     statistic = round(statistic, 1)
#   )
# print("Type III Sums-of-squares")
# anova_table
# 
# ## ------------------------------------------- ##
# ## Validate Beta Regression Models ----
# ## ------------------------------------------- ##
# 
# # Validate with DHARMa
# dat <- caged_beta2
# fittedModel <- caged.uncaged.betamod1
# 
# simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F)
# 
# plot(simulationOutput)
# # Note that there is "significant" deviation, but it is easy to get because n > 9,000!
# # The deviation does not look troubling in its magnitude
# 
# # In addition to plotting the residuals vs. fitted, we must plot residuals vs. individual predictors
# plotResiduals(simulationOutput, form = dat$cage.treatment_std)
# plotResiduals(simulationOutput, form = dat$var_aq.or.terr)
# plotResiduals(simulationOutput, form = dat$abs.lat)
# 
# testDispersion(simulationOutput)
# # Dispersion looks fine
# 
# ## ------------------------------------------- ##
# ## Generate Predictions for Beta Regression Models ----
# ## ------------------------------------------- ##
# 
# # Save original polynomial transformation
# lat_poly <- poly(caged_beta2$lat, 2)
# 
# # Generate prediction grid
# newdata <- expand.grid(
#   cage.treatment_std = unique(caged_beta2$cage.treatment_std),
#   var_aq.or.terr     = unique(caged_beta2$var_aq.or.terr),
#   lat                = seq(min(caged_beta2$lat), max(caged_beta2$lat), length.out = 100)
# )
# 
# # Create poly terms using same basis
# poly_lat_new <- predict(lat_poly, newdata$lat)
# newdata$`poly(lat, 2)1` <- poly_lat_new[, 1]
# newdata$`poly(lat, 2)2` <- poly_lat_new[, 2]
# 
# # Dummy grouping vars for prediction
# newdata$source <- NA
# newdata$exp.name <- NA
# 
# # Get predictions with SE
# preds <- predict(caged.uncaged.betamod1, newdata = newdata, type = "response", se.fit = TRUE)
# newdata$fit   <- preds$fit
# newdata$se    <- preds$se.fit
# newdata$lower <- newdata$fit - 1.96 * newdata$se
# newdata$upper <- newdata$fit + 1.96 * newdata$se
# 
# # Plot: points = raw data; line = prediction; ribbon = 95% CI; facet = two key interactions
# 
# my_colors <- c("aquatic" = "#1f78b4",      # blue
#                "terrestrial" = "#33a02c")  # green
# 
# my_shapes <- c("caged" = 16,    # solid circle
#                "uncaged" = 17)  # solid triangle
# 
# ggplot(caged_beta2, aes(x = lat, y = betadisp.comm.dist_transform)) +
#   geom_point(aes(color = var_aq.or.terr, shape = cage.treatment_std), alpha = 0.4) +
#   geom_line(data = newdata, aes(y = fit, color = var_aq.or.terr), size = 1) +
#   geom_ribbon(data = newdata, 
#               aes(x = lat, ymin = lower, ymax = upper, fill = var_aq.or.terr), 
#               alpha = 0.2, inherit.aes = FALSE) +
#   facet_grid(cage.treatment_std ~ var_aq.or.terr) +
#   labs(y = "Beta Dispersion Distance (± 95% CI)",
#        x = "Latitude",
#        title = "Raw data with marginal model predictions by treatment and ecosystem type") +
#   theme_classic() +
#   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
#         aspect.ratio = 1) +
#   scale_color_manual(values = my_colors) +
#   scale_fill_manual(values = my_colors) +
#   scale_shape_manual(values = my_shapes) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.2)) +
#   scale_x_continuous(breaks = seq(-60, 100, by = 20))
# ggsave("graphs/beta.reg.output1-raw.data.pdf", height = 8, width = 8)
# 
# # Compute study-level means
# study_means <- caged_beta2 %>%
#   group_by(source, cage.treatment_std, var_aq.or.terr) %>%
#   summarise(
#     lat  = mean(lat, na.rm = T),
#     mean_response = mean(betadisp.comm.dist_transform),
#     se_response   = sd(betadisp.comm.dist_transform) / sqrt(n()),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     lower_ci = mean_response - 1.96 * se_response,
#     upper_ci = mean_response + 1.96 * se_response
#   )
# 
# # Use study-level points instead of raw experiment-level points
# ggplot(study_means, aes(x = lat, y = mean_response)) +
#   geom_point(aes(color = var_aq.or.terr, shape = cage.treatment_std), size = 2.5, alpha = 0.7) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = var_aq.or.terr), width = 0.4) +
#   geom_line(data = newdata,
#             aes(x = lat, y = fit, color = var_aq.or.terr, group = cage.treatment_std), 
#             size = 1) +
#   geom_ribbon(data = newdata, 
#               aes(x = lat, ymin = lower, ymax = upper, fill = var_aq.or.terr), 
#               alpha = 0.2, inherit.aes = FALSE) +
#   facet_grid(cage.treatment_std ~ var_aq.or.terr) +
#   labs(y = "Beta Dispersion Distance (± 95% CI)",
#        x = "Latitude",
#        title = "Study-level mean data with marginal model predictions by treatment and ecosystem type") +
#   theme_classic() +
#   theme(
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
#     aspect.ratio = 1
#   ) +
#   scale_color_manual(values = my_colors) +
#   scale_fill_manual(values = my_colors) +
#   scale_shape_manual(values = my_shapes) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.2)) +
#   scale_x_continuous(breaks = seq(-60, 100, by = 20))
# ggsave("graphs/beta.reg.output2-study.means.pdf", height = 8, width = 8)



