## --------------------------------------------------------------- ##
# CAGED Stats and Analyses 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, 
                 performance, easystats, lubridate, car, 
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
hist(BaeDisp.df$betadisp.comm.dist)
range(BaeDisp.df$betadisp.comm.dist)

## ------------------------------------------- ##
## Figure 2 ----
## ------------------------------------------- ##
# lmer for uncaged only (Figure 2 model)
uncaged.df <- BaeDisp.df %>%
  filter(cage.treatment_std == "uncaged") %>%
  mutate(abs.lat = abs(lat))

uncaged.mod1 <- lmer(betadisp.comm.dist ~ 
                           var_aq.or.terr*abs.lat + 
                           (1|exp.name), data = uncaged.df)
check_model(uncaged.mod1)
summary(uncaged.mod1)
car::Anova(uncaged.mod1, test.statistic = "F") 
# significant interaction!
performance::r2(uncaged.mod1)

emmip(uncaged.mod1, var_aq.or.terr ~ 
        abs.lat, cov.reduce= range)

plot_model(uncaged.mod1)


# lmer for caged only (Supplement to figure 2)
caged.df <- BaeDisp.df %>%
  filter(cage.treatment_std == "caged")%>%
  mutate(abs.lat = abs(lat))

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

abs.diff.mod1 <- lmer(abdiff ~ 
                           ablat*var_aq.or.terr + 
                           (1|source), 
                         data = BaeDiff.df_abs)
check_model(abs.diff.mod1)
summary(abs.diff.mod1)
car::Anova(abs.diff.mod1, test.statistic = "F") 
# marginally significant
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



