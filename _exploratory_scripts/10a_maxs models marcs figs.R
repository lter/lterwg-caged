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

mod1

caged_effectsize %>% 
  filter(var_aq.or.terr != "") %>% 
  ggplot(aes(x = abs(lat), y = within.cage.treat_betadisp.mean.diff)) + 
  geom_jitter(width = 0.05, 
              aes(color = var_succ.vs.late)) + 
  geom_hline(yintercept = 0, color = "black") +
 # stat_summary(geom = "pointrange", fun.data = "mean_se", color = "red") +
  theme_pubr(base_size=16) +
  labs(x= "Latitude",
       y="B disp effect size")

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

newecotype_ES %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs(lat), y = within.cage.treat_betadisp.mean.diff)) + 
  stat_smooth(aes(color = lat_ecotype), 
              method = "lm", geom = "smooth", linewidth = 2) + 
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

ESmod_grassy.me <- lmer(
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
car::Anova(ESmod_grassy.me, test.statistic = "F") 

#lat, but not abs lat, is sig

#B Diff Raw 
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

newecotype_B %>% 
  filter(lat_ecotype != "") %>% 
  filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  ggplot(aes(x = abs(lat), y = betadisp.comm.dist)) + 
  geom_jitter(aes(color = cage.treatment_std), 
              width = 0.05, alpha = .3) + 
  stat_smooth(aes(color = cage.treatment_std), 
              method = "lm", geom = "smooth", linewidth = 2) + 
  theme_pubr(base_size=16) +
  facet_wrap(~lat_ecotype) +
  labs(x= "Absolute Latitude",
       y="B Dispersion")

newecotype_ES.grassy = newecotype_ES %>% 
  filter(lat_ecotype == "grassy systems")
glimpse(newecotype_ES.grassy)

newecotype_ES.subtidal = newecotype_ES %>% 
  filter(lat_ecotype == "subtidal systems")
glimpse(newecotype_ES.subtidal)

ESmod_grassy.B <- glmmTMB(
  within.cage.treat_betadisp.mean.diff ~
    lat +
    scale(gamma.richness) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source),
  #family  = beta_family(link = "probit"),
 # control = ctrl,
  data    = newecotype_ES.grassy
)

glance(ESmod_grassy.B)
check_model(mod2_ES_new)
summary(ESmod_grassy.B)
car::Anova(ESmod_grassy.B) #LAT = .6

ESmod_grassy.me <- lmer(
  within.cage.treat_betadisp.mean.diff ~
    lat +
    scale(gamma.richness) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source ),
  # family  = beta_family(link = "probit"),
  # control = ctrl,
  data    = newecotype_ES.grassy 
)

glance(ESmod_grassy.me)
summary(ESmod_grassy.me)
car::Anova(ESmod_grassy.me, test.statistic = "F") 

#lat, but not abs lat, is sig

ESmod_grassy.B <- glmmTMB(
  within.cage.treat_betadisp.mean.diff ~
    abs(lat) +
    scale(gamma.richness) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source),
  #family  = beta_family(link = "probit"),
  # control = ctrl,
  data    = newecotype_ES
)

glance(ESmod_grassy.B)
check_model(mod2_ES_new)
summary(ESmod_grassy.B)
car::Anova(ESmod_grassy.B) #LAT = .6


mod2_ES_subtidal <- lmer(
  within.cage.treat_betadisp.mean.diff ~
    #lat_ecotype + 
    abs(lat) +
    scale(gamma.richness) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source ),
  # family  = beta_family(link = "probit"),
  # control = ctrl,
  data    = newecotype_ES.subtidal 
)

glance(mod2_ES_subtidal)
check_model(mod2_ES_subtidal)
summary(mod2_ES_subtidal)
car::Anova(mod2_ES_subtidal, test.statistic = "F") 

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

#B disp

newecotype_B %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs(lat), y = betadisp.comm.dist)) + 
  geom_jitter(aes(color = cage.treatment_std), 
              width = 0.05, alpha = 0.2, size = .5) + 
  stat_smooth(aes(color = cage.treatment_std), 
              method = "lm", geom = "smooth", linewidth = 2) + 
  theme_pubr(base_size=10) +
  facet_wrap(~lat_ecotype)

mod2_B <- glmmTMB(
  betadisp.comm.dist ~
    lat_ecotype * cage.treatment_std +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  control = ctrl,
  data    = newecotype_B
)

mod2_ES <- lmer(
  within.cage.treat_betadisp.mean.diff ~
    #lat_ecotype + 
    abs(lat) +
    scale(gamma.richness) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source ),
  # family  = beta_family(link = "probit"),
  # control = ctrl,
  data    = newecotype_ES
)


# Number of experiments in aquatic vs terrestrial
caged_beta %>%
  dplyr::count(exp.name, var_aq.or.terr) %>%
  filter(var_aq.or.terr != "") %>%
  ggplot(aes(x= var_aq.or.terr,
             fill=var_aq.or.terr)) +
  geom_bar() +
  theme_pubr(base_size=16) +
  scale_fill_manual(values= c("turquoise",
                              "darkgreen")) +
  labs(x= "Biome",
       y="Number of experiments",
       fill = "Ecotype")

# Proportion of experiments in each ecosystem type
caged_beta %>%
  dplyr::count(exp.name, var_aq.or.terr, var_ecotype1) %>%
  filter(var_aq.or.terr != "") %>%
  filter(var_ecotype1 != "") %>%
  ggplot(aes(x=var_aq.or.terr, 
             fill=var_ecotype1)) +
  geom_bar(position= "fill") +
  theme_pubr(base_size=16) +
  labs(x= "Biome",
       y="Proportion",
       fill = "Ecotype")

# Proportion of experiments in aquatic and terrestrial that are successional vs late 
caged_beta %>%
  dplyr::count(exp.name, var_aq.or.terr, var_succ.vs.late) %>%
  filter(var_aq.or.terr != "") %>%
  filter(var_succ.vs.late != "") %>%
  ggplot(aes(x=var_aq.or.terr, 
             fill=var_succ.vs.late)) +
  geom_bar(position= "fill") +
  theme_pubr(base_size=16) +
  labs(x= "Biome",
       y="Proportion",
       fill = "Assembly")




## ------------------------------------------- ##
# Paper 1 Raw Data Plots ---- 
## ------------------------------------------- ##

# Effect Size 
caged_effectsize %>%
  filter(var_succ.vs.late == "late") %>%
  ggplot(aes(x=abs(lat), 
             y=within.cage.treat_betadisp.mean.diff)) +
  geom_point() +
  geom_smooth(method="lm") +
  #geom_smooth(method = "loess", se = FALSE, col="red") +
  theme_pubr(base_size=16) +
  # scale_color_manual(values= c("royalblue","darkgreen"))+
  labs(x= "Absolute latitude",
       y= "Effect size\n(uncaged - caged mean)",
       colour = "Biome") +
  geom_hline(yintercept=0)



# Log Response Ratio -beta dispersion
caged_effectsize %>%
  filter(var_succ.vs.late == "late") %>%
  ggplot(aes(x=abs(lat), 
             y=betadisp.mean.lrr)) +
  geom_point() +
  geom_smooth(method="lm") +
  #geom_smooth(method = "loess", se = FALSE, col="red") +
  theme_pubr(base_size=16) +
  # scale_color_manual(values= c("royalblue","darkgreen"))+
  labs(x= "Absolute latitude",
       y= "LRR Beta Dispersion (uncaged/caged)") +
  geom_hline(yintercept=0) #+
# geom_hline(yintercept=0.69)




# Log Response Ratio - alpha diversity
caged_effectsize %>%
  filter(var_succ.vs.late == "late") %>%
  ggplot(aes(x=abs(lat), 
             y=alpha.diversity_cage.treat.lrr)) +
  geom_point() +
  geom_smooth(method="lm") +
  #geom_smooth(method = "loess", se = FALSE, col="red") +
  theme_pubr(base_size=16) +
  # scale_color_manual(values= c("royalblue","darkgreen"))+
  labs(x= "Absolute latitude",
       y= "LRR Alpha Diversity(uncaged/caged)") +
  geom_hline(yintercept=0) #+
# geom_hline(yintercept=0.69)



# Log Response Ratio - dominance
caged_effectsize %>%
  filter(var_succ.vs.late == "late") %>%
  ggplot(aes(x=abs(lat), 
             y=dominance_cage.treat.lrr)) +
  geom_point() +
  geom_smooth(method="lm") +
  #geom_smooth(method = "loess", se = FALSE, col="red") +
  theme_pubr(base_size=16) +
  # scale_color_manual(values= c("royalblue","darkgreen"))+
  labs(x= "Absolute latitude",
       y= "LRR Dominance (uncaged/caged)") +
  geom_hline(yintercept=0) #+
# geom_hline(yintercept=0.69)


# Beta dispersion by caging treatment*latitude
caged_beta %>%
  filter(var_succ.vs.late == "late") %>%
  filter(cage.treatment_std %in% c("caged", "uncaged")) %>%
  ggplot(aes(x=abs(lat), 
             y=betadisp.comm.dist,
             col=cage.treatment_std)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  labs(x= "Absolute latitude",
       y= "Beta dispersion",
       colour = "Biome") 





# Beta dispersion by caging treatment
caged_beta %>%
  filter(var_aq.or.terr != "") %>%
  ggplot(aes(x=cage.treatment_std,
             y=betadisp.comm.dist)) +
  geom_boxplot(size=1.1) +
  geom_point(position= position_jitter(), alpha= 0.05) +
  theme_pubr(base_size=16) +
  # scale_color_manual(values= c("royalblue",
  #                             "darkturquoise")) +
  labs(x= "Caging treatment",
       y= "Beta dispersion") 

# Beta dispersion by caging * aquatic.terrestrial
caged_beta %>%
  filter(var_aq.or.terr != "") %>%
  ggplot(aes(x=var_aq.or.terr, 
             y=betadisp.comm.dist,
             color=cage.treatment_std,)) +
  geom_boxplot(size=1.1) +
  geom_point(position= position_jitterdodge(), alpha= 0.05) +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue",
                               "darkturquoise")) +
  labs(x= "Biome",
       y= "Beta dispersion",
       colour = "Caging treatment") 

# Beta dispersion by caging treatment * succession
caged_beta %>%
  filter(var_aq.or.terr != "") %>%
  filter(var_succ.vs.late != "") %>%
  ggplot(aes(x=cage.treatment_std,
             y=betadisp.comm.dist)) +
  geom_boxplot(size=1.1) +
  geom_point(position= position_jitter(), alpha= 0.05) +
  facet_wrap(~var_succ.vs.late)+
  theme_pubr(base_size=16) +
  # scale_color_manual(values= c("royalblue",
  #                             "darkturquoise")) +
  labs(x= "Caging treatment",
       y= "Beta dispersion") 

# Beta dispersion by caging * ecosystem type
caged_beta %>%
  # filter(var_aq.or.terr != "") %>%
  filter(var_ecotype1 != "") %>%
  ggplot(aes(x=cage.treatment_std, 
             y=betadisp.comm.dist,
             color=cage.treatment_std,)) +
  geom_boxplot(size=1.1) +
  geom_point(position= position_jitterdodge(), alpha= 0.05) +
  facet_wrap(~var_ecotype1)+
  theme_pubr(base_size=20) +
  scale_color_manual(values= c("royalblue",
                               "darkturquoise")) +
  labs(x= "",
       y= "Beta dispersion",
       colour = "Caging treatment") 


# Beta dispersion by caging treatment*latitude
caged_beta %>%
  filter(var_aq.or.terr != "") %>%
  # filter(var_aq.or.terr == c("aquatic")) %>%
  # filter(!abs(lat) > 60) %>%
  ggplot(aes(x=abs(lat), 
             y=betadisp.comm.dist,
             col=cage.treatment_std)) +
  geom_point() +
  facet_wrap(~var_aq.or.terr)+
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue",
                               "darkturquoise")) +
  labs(x= "Absolute latitude",
       y= "Beta dispersion",
       colour = "Biome") 




# Beta dispersion by caging treatment*latitude
caged_beta %>%
  filter(var_aq.or.terr != "") %>%
  filter(var_succ.vs.late != "") %>%
  ggplot(aes(x=abs(lat), 
             y=betadisp.comm.dist,
             col=var_succ.vs.late)) +
  geom_point(alpha=0.2) +
  facet_wrap(~var_aq.or.terr*cage.treatment_std)+
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("purple3",
                               "hotpink")) +
  labs(x= "Absolute latitude",
       y= "Beta dispersion",
       colour = "Succession") 



# Beta dispersion (uncaged only) by latitude
caged_beta %>%
  filter(cage.treatment_std == "uncaged") %>%
  ggplot(aes(x=abs(lat), 
             y=betadisp.comm.dist,
             col=var_aq.or.terr)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  #scale_color_manual(values= c("royalblue",
  #                             "darkgreen")) +
  labs(x= "Absolute latitude",
       y= "Beta dispersion",
       colour = "Biome") +
  ggtitle("Uncaged data only")

# Remove rows for which we have no biome classification
caged_beta <- caged_beta %>%
  filter(var_aq.or.terr != "") %>%
  droplevels()

# Beta dispersion (caged only) by latitude
caged_beta %>%
  filter(cage.treatment_std == "caged") %>%
  ggplot(aes(x=abs(lat), 
             y=betadisp.comm.dist,
             col=var_aq.or.terr)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue",
                               "darkgreen"))+
  labs(x= "Absolute latitude",
       y= "Beta dispersion",
       colour = "Biome") +
  ggtitle("Caged data only")



# Absolute diff by latitude
caged_effectsize %>%
  filter(var_aq.or.terr != "") %>%
  droplevels() %>%
  ggplot(aes(x=abs(lat), 
             y=abs(within.cage.treat_betadisp.mean.diff),
             col=var_aq.or.terr)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue",
                               "darkgreen"))+
  labs(x= "Absolute latitude",
       y= "Absolute value of effect size\n(uncaged - caged mean)",
       colour = "Biome")
# absolute value= doesnt matter which direction, is just showing a big difference betweeen caged and uncaged beta dispersion



# Gamma richness by latitude
caged_effectsize %>%
  filter(var_aq.or.terr != "") %>%
  droplevels() %>%
  ggplot(aes(x=abs(lat), 
             y=gamma.richness,
             col=var_aq.or.terr)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue",
                               "darkgreen")) +
  labs(x="Absolute latitude",
       y="Gamma richness",
       colour="Biome")


# Plot size for aquatic vs terrestrial 
caged_effectsize %>%
  # lots of these values are "unknown"
  filter(!is.na(as.numeric(var_exclosure.area.m2))) %>%
  ggplot(aes(x=var_aq.or.terr, 
             # change from character to numeric 
             y=as.numeric(var_exclosure.area.m2))) +
  geom_boxplot() +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue","darkgreen"))
# seems like too big of differences, makes it hard to evaluate? 

