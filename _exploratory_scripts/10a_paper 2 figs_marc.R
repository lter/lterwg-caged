## --------------------------------------------------------------- ##
# CAGED Paper 2 Figures and some Analyses
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, glmmTMB, DHARMa, ggpubr,
                 performance, easystats, lubridate, car, broom.mixed,
                 njlyon0/supportR, MuMIn, visreg, grid, gridExtra,
                 emmeans, tidymodels, qqplotr, sjPlot) #, update_all= TRUE) 

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()


## ------------------------------------------- ##
# Load Data ----
# these dfs were created in script 08 script
## ------------------------------------------- ##
caged_effectsize <- read.csv(file.path("data", "08_caged_prepped-effect-size.csv"))

caged_beta <- read.csv(file.path("data", "08-A_caged_w.meta-beta-disp_finest-scales.csv"))


###PAPER 2 FIGURES####
#Caged vs Uncaged Figures

caged_beta %>% 
  filter(var_ecotype1 != "") %>% 
  filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  ggplot(aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  geom_jitter(width = 0.05, alpha = .2, size = 1) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 1.5, color = "hotpink") + 
  theme_pubr(base_size=16) +
  labs(x= "Treatment",
       y="B Dispersion") +
  facet_wrap(~var_ecotype1)

#ES by habitat
caged_effectsize %>% 
  filter(var_ecotype1 != "") %>% 
  ggplot(aes(x = var_ecotype1, y = within.cage.treat_betadisp.mean.diff)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 2) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = .8, color = "hotpink") + 
  theme_pubr(base_size=12) +
  labs(x= "Habitat",
       y="B Dispersion") 

newecotype_ES
newecotype_B

#ES by habitat
newecotype_B %>% 
  filter(var_ecotype1 != "") %>% 
  filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  ggplot(aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 2) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = .8, color = "hotpink") + 
  theme_pubr(base_size=12) +
  labs(x= "Habitat",
       y="B Dispersion") +
  facet_wrap(~lat_ecotype)

caged_effectsize %>% 
  filter(var_ecotype1 != "") %>% 
  filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  ggplot(aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 2) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = .8, color = "hotpink") + 
  theme_pubr(base_size=12) +
  labs(x= "Habitat",
       y="B Dispersion") +
  facet_wrap(~var_ecotype1)
