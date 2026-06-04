## --------------------------------------------------------------- ##
# CAGED Raw data figures
## --------------------------------------------------------------- ##
# Written by: Jamie McDevitt-Irwin

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, njlyon0/supportR,
                 ggpubr) #, update_all= TRUE) 

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


# View(caged_effectsize) 
# View(caged_beta) 
dim(caged_effectsize)# 347 rows
dim(caged_beta)# 13964  rows

## ------------------------------------------- ##
# Plots ---- 
## ------------------------------------------- ##
colnames(caged_beta)
colnames(caged_effectsize)

##NEW ECOTYPES####
newecotype_ES = caged_effectsize %>% 
  mutate(lat_ecotype = 
           case_when(var_ecotype1 == "grassland" ~ "grassy systems", 
                     var_ecotype1 == "savanna" ~ "grassy systems",
                     var_ecotype1 == "tundra" ~ "grassy systems",
                     var_ecotype1 == "subtidal" ~ "subtidal systems",
                     var_ecotype1 == "coral reef" ~ "subtidal systems",
                     #var_ecotype1 == "forest" ~ "trees", 
                     .default = NA)) %>% 
  filter(lat_ecotype != "") 

newecotype_ES.nogex = caged_effectsize %>% 
  mutate(lat_ecotype = 
           case_when(var_ecotype1 == "grassland" ~ "grassy systems", 
                     var_ecotype1 == "savanna" ~ "grassy systems",
                     var_ecotype1 == "tundra" ~ "grassy systems",
                     var_ecotype1 == "subtidal" ~ "subtidal systems",
                     var_ecotype1 == "coral reef" ~ "subtidal systems",
                     #var_ecotype1 == "forest" ~ "trees", 
                     .default = NA)) %>% 
  filter(lat_ecotype != "") %>% 
  filter(!source == "gex_canada1-46_ex-rainfall-gradient_2008_grazers_plants.csv" )

newecotype_ES %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs(lat), y = within.cage.treat_betadisp.mean.diff)) + 
 # stat_smooth(aes(color = lat_ecotype), 
  #            method = "lm", geom = "smooth", linewidth = 2) + 
  # stat_smooth(method = "lm") +
  geom_jitter(width = 0.05) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.3) +
  theme_pubr(base_size=10) +
  facet_wrap(~lat_ecotype) +
  labs(x= "Absolute Latitude",
       y="B Effect Size",
       fill = "Ecotype")

newecotype_ES.grassy = newecotype_ES %>% 
  filter(lat_ecotype == "grassy systems")
glimpse(newecotype_ES.grassy)

newecotype_ES.subtidal = newecotype_ES %>% 
  filter(lat_ecotype == "subtidal systems")
glimpse(newecotype_ES.subtidal)

ESmod_new.me <- glmmTMB(
  within.cage.treat_betadisp.mean.diff ~
    abs(lat) + 
    lat_ecotype +
    scale(gamma.richness) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source ),
  # family  = beta_family(link = "probit"),
  # control = ctrl,
  data    = newecotype_ES 
)

glance(ESmod_new.me)
summary(ESmod_new.me)
car::Anova(ESmod_new.me) 

ESmod_grassy.me <- glmmTMB(
  within.cage.treat_betadisp.mean.diff ~
    abs(lat) +
    scale(gamma.richness) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source ),
  # family  = beta_family(link = "probit"),
  # control = ctrl,
  data    = newecotype_ES.grassy 
)

glance(ESmod_grassy.me)
summary(ESmod_grassy.me)
car::Anova(ESmod_grassy.me) 

ESmod_subtidal.me <- glmmTMB(
  within.cage.treat_betadisp.mean.diff ~
    abs(lat) +
    scale(gamma.richness) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source ),
  # family  = beta_family(link = "probit"),
  # control = ctrl,
  data    = newecotype_ES.subtidal 
)

glance(ESmod_subtidal.me)
summary(ESmod_subtidal.me)
car::Anova(ESmod_subtidal.me) 



#B Diff Raw ####
newecotype_B = caged_beta %>% 
  mutate(lat_ecotype = 
           case_when(var_ecotype1 == "grassland" ~ "grassy systems", 
                     var_ecotype1 == "savanna" ~ "grassy systems",
                     var_ecotype1 == "tundra" ~ "grassy systems",
                     var_ecotype1 == "subtidal" ~ "subtidal systems",
                     var_ecotype1 == "coral reef" ~ "subtidal systems",
                     #var_ecotype1 == "forest" ~ "trees", 
                     .default = NA)) %>% 
  filter(lat_ecotype != "")

n_all      <- nrow(newecotype_B)
newecotype_B <- newecotype_B %>%
  dplyr::mutate(betadisp_t = sv_transform(betadisp.comm.dist, n_all))

newecotype_B %>% 
  filter(lat_ecotype != "") %>% 
  filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  ggplot(aes(x = abs(lat), y = betadisp.comm.dist)) + 
  geom_jitter(aes(color = cage.treatment_std), 
              width = 0.05, alpha = .1, size = .5) + 
  stat_smooth(aes(color = cage.treatment_std), 
              method = "lm", geom = "smooth", linewidth = 1.5) + 
  theme_pubr(base_size=16) +
  facet_wrap(~lat_ecotype) +
  labs(x= "Absolute Latitude",
       y="B Dispersion")

newecotype_B.grassy = newecotype_B %>% 
  filter(lat_ecotype == "grassy systems")
glimpse(newecotype_B.grassy)

newecotype_B.subtidal = newecotype_B %>% 
  filter(lat_ecotype == "subtidal systems")
glimpse(newecotype_B.subtidal)

ESmod_grassy.B <- glmmTMB(
  betadisp_t ~
    cage.treatment_std * abs(lat) +
  #  scale(gamma.richness_exp.name) +
  #  scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  #control = ctrl,
  data    = newecotype_B.grassy
)

glance(ESmod_grassy.B)
summary(ESmod_grassy.B)
car::Anova(ESmod_grassy.B) #LAT = .6

ESmod_subtidal.B <- glmmTMB(
  betadisp_t ~
    cage.treatment_std * abs(lat) +
   # scale(gamma.richness_exp.name) +
   # scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  #control = ctrl,
  data    = newecotype_B.subtidal
)

glance(ESmod_subtidal.B)
summary(ESmod_subtidal.B)
car::Anova(ESmod_subtidal.B) #LAT = .6
