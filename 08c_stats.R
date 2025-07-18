## --------------------------------------------------------------- ##
# CAGED Stats and Analyses 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, Max Castorani, Jamie McDevitt-Irwin ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, glmmTMB, DHARMa,
                 performance, easystats, lubridate, car, broom.mixed,
                 njlyon0/supportR, MuMIn, visreg, 
                 emmeans, tidymodels, qqplotr, sjPlot) #, update_all= TRUE) 

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("results"), showWarnings = F)
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()


## ------------------------------------------- ##
# Load Data ----
# these dfs were created in script 08a script
## ------------------------------------------- ##
BaeDiff.df <- read.csv("data/BaeDiff.df.csv")
BaeDisp.df <- read.csv("data/BaeDisp.df.csv")


# View(BaeDiff.df) # 235 rows
# View(BaeDisp.df) # 10875 rows


## ------------------------------------------- ##
# Models for Paper 1 ----
## ------------------------------------------- ##

glimpse(BaeDisp.df) #10,881 rows

# Check distribution of values in response variable, betadisp.comm.dist
hist(BaeDisp.df$betadisp.comm.dist)  

# Check range of values in response variable, betadisp.comm.dist, with increased precision
options(digits = 22) # 22 is maximum value allowable
range(BaeDisp.df$betadisp.comm.dist) 

# Check relative frequencies of true ones
table(BaeDisp.df$betadisp.comm.dist == 1)

# Check relative frequencies of true zeroes
table(BaeDisp.df$betadisp.comm.dist == 0) 
paste0("Frequency of true zeroes is ",
       round(100 * table(BaeDisp.df$betadisp.comm.dist == 0)[2] / nrow(BaeDisp.df), 2), 
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
n <- nrow(BaeDisp.df)
transform.fun <- function(y){(y * (n - 1) + 0.5 ) / n}

# Alternative transformation if you have true zeroes and true ones, [0, 1] - DO NOT NEED TO USE
#transform.fun <- function(y){y * (1 - epsilon) + (epsilon / 2)}
#epsilon <- 1e-6

# Apply transformation
BaeDisp.df$betadisp.comm.dist_transform <- sapply(X = BaeDisp.df$betadisp.comm.dist, FUN = transform.fun)

# Validate (this should look like a 1:1)
plot(x = BaeDisp.df$betadisp.comm.dist, y =  BaeDisp.df$betadisp.comm.dist_transform)


## ------------------------------------------- ##
## Create new dataframes for models to follow ----
## ------------------------------------------- ##
# Drop rows for which there is no terrestrial or aquatic categorization
table(BaeDisp.df$var_aq.or.terr)

BaeDisp.df2 <- BaeDisp.df %>% 
  dplyr::filter(var_aq.or.terr != "") %>% 
  droplevels()

table(BaeDisp.df2$var_aq.or.terr)

# Drop rows for which latitude is missing
table(is.na(BaeDisp.df2$lat))

BaeDisp.df2 <- BaeDisp.df2[!is.na(BaeDisp.df2$lat), ]

table(is.na(BaeDisp.df2$lat))

# Create a column for absolute value of latitude
BaeDisp.df2 <- BaeDisp.df2 %>%
  mutate(abs.lat = abs(lat))

# Make sure there are no NAs for the experiment name
table(is.na(BaeDisp.df2$exp.name))

# Make aquatic/terrestrial a factor
BaeDisp.df2$var_aq.or.terr <- factor(BaeDisp.df2$var_aq.or.terr)

# Subset datasets for uncaged plots only or caged plots only
uncaged.df <- BaeDisp.df2 %>%
  filter(cage.treatment_std == "uncaged") %>%
  droplevels()

caged.df <- BaeDisp.df2 %>%
  filter(cage.treatment_std == "caged") %>%
  droplevels()

# Check data: Collinearity between x1 and x2?
ggplot(data = uncaged.df, aes(x = var_aq.or.terr, y = abs.lat)) +
  geom_boxplot()

ggplot(data = caged.df, aes(x = var_aq.or.terr, y = abs.lat)) +
  geom_boxplot()

# Check data: General relationships between y and x1 or x2
ggplot(data = BaeDisp.df2, aes(x = lat, y = betadisp.comm.dist_transform)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2)) +
  facet_grid(cage.treatment_std ~ var_aq.or.terr) 
ggplot(data = BaeDisp.df2, aes(x = cage.treatment_std, y = betadisp.comm.dist_transform)) +
  geom_boxplot()
ggplot(data = BaeDisp.df2, aes(x = var_aq.or.terr, y = betadisp.comm.dist_transform)) +
  geom_boxplot()
ggplot(data = BaeDisp.df2, aes(x = cage.treatment_std, y = betadisp.comm.dist_transform)) +
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
## ------------------------------------------- ##
# REPEAT ABOVE WORKFLOW FOR ONLY UNCAGED PLOTS
## ------------------------------------------- ##
## ------------------------------------------- ##

## ------------------------------------------- ##
## Fit Beta Regression Models ----
## ------------------------------------------- ## 

# Fit model on only uncaged data
BaeDisp.df3 <- BaeDisp.df2 %>%
  dplyr::filter(cage.treatment_std == "uncaged") %>%
  droplevels()
 
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

uncaged.betamod <- glmmTMB(betadisp.comm.dist_transform ~ 
                              var_aq.or.terr * abs.lat  +
                              (1|exp.name), 
                            dispformula = ~ var_aq.or.terr, #+ abs.lat,
                            family = beta_family(link = "probit"),
                            control = glmmTMBControl(optimizer = optim, 
                                                     optArgs = list(method = "BFGS")), 
                            # Or use nlminb, or bobyqa via nloptr
                            data = BaeDisp.df3) 
AIC(uncaged.betamod)
summary(uncaged.betamod)    
car::Anova(uncaged.betamod, type = "II")

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
dat <- BaeDisp.df3
fittedModel <- uncaged.betamod

simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F)

plot(simulationOutput)
# Note that there is "significant" deviation, but it is easy to get because n > 5,000!
# The deviation does not look troubling in its magnitude

# In addition to plotting the residuals vs. fitted, we must plot residuals vs. individual predictors
plotResiduals(simulationOutput, form = dat$var_aq.or.terr)
plotResiduals(simulationOutput, form = dat$abs.lat)

# Check dispersion
testDispersion(simulationOutput)
# The model residuals are underdispersed but...
# The significant underdispersion DHARMa test result from  is probably not a symptom of serious model misfit but rather a statistically detectable but practically minor deviation, amplified by the large sample size. Beta regression residuals are bounded by (0,1), so they can be quite tightly distributed when the model fits well. With a well-fitting model and strong signal, the observed residuals might be slightly less variable than the simulated ones, especially if the residual variance is well captured; the model is regularizing the residuals more than expected under the simulation framework; and/or DHARMa's simulation procedure relies on distributional assumptions, and even subtle mismatches (e.g., from how it simulates dispersion around beta-distributed outcomes) can become statistically significant at N = 5,000.

# Another sanity check on dispersion:
mean(simulationOutput$scaledResiduals^2)
# For a well-fitted model, this should be ≈ 1/3 (the theoretical variance of uniform(0,1) scaled residuals).
# Much lower than 0.33 suggests underdispersion, but again, the magnitude matters.
# In this case, our value of 0.366 is about 1/3. This supports the idea that the significant DHARMa test result is not practically meaningful, and the model is likely well-behaved.

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
       x = "|Latitude|",
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
ggsave("graphs/UNCAGED.beta.reg.output1-raw.data.pdf", height = 8, width = 8)

p

# MAX STOPPED HERE JULY 18, 2025

# ------------------------------------------------------------------------------------------------------

# Following code is from Jamie

## ------------------------------------------- ##
## Figure 2 ----
## ------------------------------------------- ##
# Beta regression using Max's code above (but only on uncaged)
# Fit model 1 on all data
uncaged.betamod1 <- glmmTMB(betadisp.comm.dist_transform ~ 
                              var_aq.or.terr*abs.lat +

                              (1|exp.name), 
                            # check if we need this if we use abslat and uncaged only
                            # still sig without dispersion formula
                            dispformula = ~ var_aq.or.terr + abs.lat,
                            family = beta_family(link = "logit"),
                            control = glmmTMBControl(optimizer = optim, 
                                                     optArgs = list(method = "BFGS")), 
                            # Or use nlminb, or bobyqa via nloptr
                            data = uncaged.df) 
summary(uncaged.betamod1)    
car::Anova(uncaged.betamod1, type = "II")
# interaction is not sig when you use a beta regression and soruce as a nested random effect
# also would be sig if source wasn't a RE
# need to try with the new data

check_model(uncaged.betamod1)


# lmer for uncaged only (Figure 2 model)
uncaged.mod1 <- lmer(betadisp.comm.dist ~ 
                           var_aq.or.terr*abs.lat + 
                           (1|exp.name), 
                     data = uncaged.df)
check_model(uncaged.mod1)
summary(uncaged.mod1)
car::Anova(uncaged.mod1, test.statistic = "F") 
# marginally significant with source as a RE 

performance::r2(uncaged.mod1)

emmip(uncaged.mod1, var_aq.or.terr ~ 
        abs.lat, cov.reduce= range)

plot_model(uncaged.mod1)


# lmer for caged only (Supplement to figure 2)
caged.mod1 <- lmer(betadisp.comm.dist ~ 
                       var_aq.or.terr*abs.lat + 
                       (1|exp.name), data = caged.df)
check_model(caged.mod1)
summary(caged.mod1)
car::Anova(caged.mod1, test.statistic = "F") # significant interaction
performance::r2(caged.mod1)

emmip(caged.mod1, var_aq.or.terr ~ abs.lat, cov.reduce= range)

plot_model(caged.mod1)



## ------------------------------------------- ##
## Figure 3 ----
## ------------------------------------------- ##

# absolute value of difference
BaeDiff.df_abs <- BaeDiff.df %>% 
  mutate(ablat = abs(lat), 
         abdiff = abs(within.cage.treat_betadisp.mean.diff) )

range(BaeDiff.df_abs$abdiff) #  0.0002819578 0.4272029784
hist(BaeDiff.df_abs$abdiff) # try a beta regression?


Baediff.betamod1 <- glmmTMB(abdiff ~ 
                              var_aq.or.terr*ablat, #+
                              
                             # (1|source), 
                        #    dispformula = ~ var_aq.or.terr + abs.lat,
                            family = beta_family(link = "logit"),
                            control = glmmTMBControl(optimizer = optim, 
                                                     optArgs = list(method = "BFGS")), 
                            # Or use nlminb, or bobyqa via nloptr
                            data = BaeDiff.df_abs) 

summary(Baediff.betamod1)    
car::Anova(Baediff.betamod1, type = "II")
# not significant when you use a beta regression

check_model(Baediff.betamod1) # looks like it fits well? 

abs.diff.mod1 <- lm(abdiff ~ 
                           ablat*var_aq.or.terr, #+ 
                        #   (1|source), 
                         data = BaeDiff.df_abs)
check_model(abs.diff.mod1)
summary(abs.diff.mod1)
car::Anova(abs.diff.mod1, test.statistic = "F") 
# not significant anymore? maybe because we have updated data?
# sitll need to run with all the new datasets added

performance::r2(abs.diff.mod1)

emmip(abs.diff.mod1, var_aq.or.terr ~ 
        ablat, cov.reduce= range)

plot_model(abs.diff.mod1)


# true difference
diff.mod1 <- lmer(within.cage.treat_betadisp.mean.diff ~ 
                        ablat*var_aq.or.terr + 
                        (1|source), 
                      data = BaeDiff.df_abs)
check_model(diff.mod1)
summary(diff.mod1)
car::Anova(diff.mod1, test.statistic = "F") 
# not significant, this is why the absolute value is helpful
performance::r2(diff.mod1)









## ------------------------------------------- ##
# Exploratory Models ----
## ------------------------------------------- ##

#RVs
#1. Beta dispersion, per plot, for each experiment (right?? or per-site?) 
#betadisp.comm.dist = from BaeDisp.df and caged_v1, average community distance 
#2. Beta dispersion Effect Size (Uncaged - Caged)
#within.cage.treat_betadisp.mean.diff = from BaeDisp.df and caged_v1, mean beta difference bt uncaged and caged. negative = consumers decrease B dispersion, positive = consumers increase B dispersion
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

#BaeDisp.df <- read.csv(file.path("data", "BaeDisp.df.csv"))

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
                           (1|exp.name), data = BaeDisp.df)

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
                       (1|exp.name), data = BaeDisp.df)

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
                       (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp2Way.lmer, panel = F) %>% plot()
check_collinearity(BaeDisp2Way.lmer)
summary(BaeDisp2Way.lmer)
car::Anova(BaeDisp2Way.lmer, test.statistic = "F")
performance::r2(BaeDisp2Way.lmer)

#3way interactions 
#"rank deficient" but bolker says that is ok. 
BaeDisp3way.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_ecotype1*var_consumer.richness.category + gamma.richness + #lat + exp.age + 
                           betadisp.sample.size + 
                           (1|exp.name), data = BaeDisp.df)

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
                           (1|source/exp.name), data = BaeDisp.df)

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
#                                (cage.treatment_std|source/exp.name), data = BaeDisp.df)

#check_model(BaeDisp3way_slop.lmer, panel = F) |> plot() #plot them all 
#summary(BaeDisp3way_slop.lmer)
#car::Anova(BaeDisp3way_slop.lmer, test.statistic = "F") #takes 1 million minutes to run
#performance::r2(BaeDisp3way_slop.lmer)

#I.ii GLMER: Max is going to try some Beta regressions----




## ------------------------------------------- ##
# Models II. Consumer Effect Size (Difference in Beta Dispersion) ----
## ------------------------------------------- ##

glimpse(BaeDiff.df)
#II.i LMER:  RE = 1|source ----
#Habitat * latitude... no effects

BaeES_lat.lmer <- lmer(within.cage.treat_betadisp.mean.diff ~ 
                     var_aq.or.terr * abs(lat) +
                     (1|source), data = BaeDiff.df)

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
                              (1|source), data = BaeDiff.df)

check_model(BaeES_richsamp.lmer)
summary(BaeES_richsamp.lmer)
car::Anova(BaeES_richsamp.lmer, test.statistic = "F") 

emmip(BaeES_richsamp.lmer, ~ var_consumer.richness.category)

plot_model(BaeES_richsamp.lmer)




## ------------------------------------------- ##
# Models III. Absolute Value Consumer Effect Size (Difference in Beta Dispersion) ----
## ------------------------------------------- ##
#I dont like this response variable

BaeDiff.df_lat = BaeDiff.df %>% 
  mutate(ablat = abs(lat), abdiff = abs(within.cage.treat_betadisp.mean.diff) )

LatHabIntES.lmer <- lmer(abdiff ~ 
                     ablat*var_aq.or.terr + 
                       (1|source), 
                   data = BaeDiff.df_lat)

LatHabES.lmer <- lmer(abdiff ~ 
                        ablat+var_aq.or.terr + 
                        (1|source), 
                      data = BaeDiff.df_lat)
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
#                                   data = BaeDisp.df2) 
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
#                                   data = BaeDisp.df2) 
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
#                                   data = BaeDisp.df2) 
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
# dat <- BaeDisp.df2
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
# lat_poly <- poly(BaeDisp.df2$lat, 2)
# 
# # Generate prediction grid
# newdata <- expand.grid(
#   cage.treatment_std = unique(BaeDisp.df2$cage.treatment_std),
#   var_aq.or.terr     = unique(BaeDisp.df2$var_aq.or.terr),
#   lat                = seq(min(BaeDisp.df2$lat), max(BaeDisp.df2$lat), length.out = 100)
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
# ggplot(BaeDisp.df2, aes(x = lat, y = betadisp.comm.dist_transform)) +
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
# study_means <- BaeDisp.df2 %>%
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



