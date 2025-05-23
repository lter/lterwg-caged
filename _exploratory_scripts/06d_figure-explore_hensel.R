## --------------------------------------------------------------- ##
# CAGED Data Exploring via Figures
## --------------------------------------------------------------- ##
# Written by: Marc Hensel

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, lme4, performance, lubridate, car, njlyon0/supportR, visreg, patchwork) #, update_all= TRUE)

# Create needed folder(s)
#dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data, BaeDisp and BaeDiff are created in 08_stats

BaeDisp.df <- read.csv(file.path("data", "BaeDisp.df.csv"))
BaeDiff.df <- read.csv(file.path("data", "BaeDiff.df.csv"))


uncagedlat.pointfig = 
  BaeDisp.df %>% 
  dplyr::filter(cage.treatment_std == "uncaged", 
                var_aq.or.terr %in% c("aquatic", "terrestrial")) %>% 
  ggplot(data = ., aes(x = abs(lat), y = betadisp.comm.dist, color = var_aq.or.terr)) + 
  geom_point(size = .5, alpha = .3) +
 # stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
 #              size = .6, position = position_dodge(width = .1), alpha = .5) +
  stat_smooth(geom = "smooth", method = "lm") +
  scale_colour_manual(labels = c("Aquatic", "Terrestrial"), values=c("#101ece", "#06952f")) +
  labs(y = expression("Beta dispersion \n(median distance) IN UNCAGED"), x = "absolute value of latitude") +
  theme_bw(base_size=24)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank())

uncagedlat.pointfig

cagedlat.pointfig = 
  BaeDisp.df %>% 
  dplyr::filter(cage.treatment_std == "caged") %>%
  filter(var_aq.or.terr %in% c("aquatic", "terrestrial")) %>% 
  ggplot(data = ., aes(x = abs(lat), y = betadisp.comm.dist, color = var_aq.or.terr)) + 
  geom_point(size = .5, alpha = .3) +
  # stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
  #              size = .6, position = position_dodge(width = .1), alpha = .5) +
  stat_smooth(geom = "smooth", method = "lm") +
  scale_colour_manual(labels = c("Aquatic", "Terrestrial"), values=c("#101ece", "#06952f")) +
  labs(y = expression("Beta dispersion \n(median distance) IN CAGED"), x = "absolute value of latitude") +
  theme_bw(base_size=24)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank())

cagedlat.pointfig

uncagedlat.pointfig + cagedlat.pointfig


lat.Difffig = 
  BaeDiff.df %>% 
  dplyr::filter(var_aq.or.terr %in% c("aquatic", "terrestrial")) %>% 
  ggplot(data = ., aes(x = abs(lat), y = within.cage.treat_betadisp.mean.diff, color = var_aq.or.terr)) + 
  geom_point(size = 2, alpha = .3) +
  # stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
  #              size = .6, position = position_dodge(width = .1), alpha = .5) +
  stat_smooth(geom = "smooth", method = "lm") +
  scale_colour_manual(labels = c("Aquatic", "Terrestrial"), values=c("#101ece", "#06952f")) +
  geom_hline(yintercept = 0, linetype = 2, color = "orange", linewidth = 1.5) +
  labs(y = expression("Beta Eff Size \n(Uncaged - Caged Bdisp) "), x = "absolute value of latitude") +
  annotate(geom="text", x=60, y=.4, label="Consumers Increase Variability", color="blue") +
  annotate(geom="text", x=60, y=-.4, label="Consumers Decrease Variability", color="red")+
  theme_bw(base_size=24)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank())

(uncagedlat.pointfig + cagedlat.pointfig) 
lat.Difffig

lat.Diffabs.fig = 
  BaeDiff.df %>% 
  dplyr::filter(var_aq.or.terr %in% c("aquatic", "terrestrial")) %>% 
  ggplot(data = ., aes(x = abs(lat), y = abs(within.cage.treat_betadisp.mean.diff), color = var_aq.or.terr)) + 
  geom_point(size = 2, alpha = .3) +
  # stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
  #              size = .6, position = position_dodge(width = .1), alpha = .5) +
  stat_smooth(geom = "smooth", method = "lm") +
  scale_colour_manual(labels = c("Aquatic", "Terrestrial"), values=c("#101ece", "#06952f")) +
  #geom_hline(yintercept = 0, linetype = 2, color = "orange", linewidth = 1.5) +
  labs(y = expression("[Beta Eff Size] \n(Uncaged - Caged Bdisp) "), x = "absolute value of latitude") +
  annotate(geom="text", x=20, y=.2, label=expression("Larger #s = \n consumers control B div more"), color="black") +
 # annotate(geom="text", x=60, y=-.4, label="Consumers Decrease Variability", color="red")+
  theme_bw(base_size=24)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank())

lat.Diffabs.fig

habitatlat.pointfig = 
  BaeDisp.df %>% 
  dplyr::filter(var_aq.or.terr %in% c("aquatic", "terrestrial")) %>% 
  ggplot(data = ., aes(x = abs(lat), y = betadisp.comm.dist, color = cage.treatment_std)) + 
  geom_point(size = .5, alpha = .3) +
  # stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
  #              size = .6, position = position_dodge(width = .1), alpha = .5) +
  stat_smooth(geom = "smooth", method = "lm") +
  scale_colour_manual(labels = c("uncaged", "caged"), values=c("black", "purple")) +
  labs(y = expression("Beta dispersion \n(median distance)"), x = "absolute value of latitude") +
  theme_bw(base_size=24)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~var_aq.or.terr)

habitatlat.pointfig

cagelat.pointfig = 
  BaeDisp.df %>% 
  dplyr::filter(var_aq.or.terr %in% c("aquatic", "terrestrial")) %>% 
  ggplot(data = ., aes(x = abs(lat), y = betadisp.comm.dist, color = var_aq.or.terr)) + 
  geom_point(size = .5, alpha = .3) +
  # stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
  #              size = .6, position = position_dodge(width = .1), alpha = .5) +
  stat_smooth(geom = "smooth", method = "lm") +
  scale_colour_manual(labels = c("Aquatic", "Terrestrial"), values=c("#101ece", "#06952f")) +
  labs(y = expression("Beta dispersion \n(median distance)"), x = "absolute value of latitude") +
  theme_bw(base_size=24)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~cage.treatment_std)

cagelat.pointfig


EcoSys.BDisp.fig = 
  BaeDisp.df %>% 
  dplyr::filter(var_aq.or.terr %in% c("aquatic", "terrestrial")) %>% 
  ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  #geom_point(size = 2, alpha = .3) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
                size = 1) +
  stat_summary(geom = "line", method = "lm") +
  scale_colour_manual(labels = c("Aquatic", "Terrestrial"), values=c("#101ece", "#06952f")) +
  #geom_hline(yintercept = 0, linetype = 2, color = "orange", linewidth = 1.5) +
  labs(y = expression("B disp"), x = "") +
 # annotate(geom="text", x=20, y=.2, label=expression("Larger #s = \n consumers control B div more"), color="black") +
  theme_bw(base_size=20)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank()) +
  facet_wrap(~var_ecotype1)

EcoSys.BDisp.fig






#NOTE: since this is still initial explorations, you gotta go run the DFs in 06c_model-explore-hensel.R to get the two DFs I used in these. Will fix this later tho

marc.modeldata_v1 <- read.csv(file.path("data", "marc.modeldata_v1.csv"))
marc.modeldata_ES <- read.csv(file.path("data", "marc.modeldata_ES.csv"))

#alldata_v1 <- read.csv(file.path("data", "06_caged_with-metadata.csv"))

#lets see what we are dealing with
glimpse(marc.modeldata_v1)

#list of figs to look at. go further down for the code

ecotype.violinfig
ecotype.pointfig
climate.pointfig
consdiv.pointfig #kinda interesting
exage.pointfig
lat.pointfig #consumers stronger in tropics?


ecotype.violinfig = 
  marc.modeldata_v1 %>% 
  ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
   geom_violin(stat = "ydensity", position = "dodge", draw_quantiles = c(0.25, 0.5, 0.75)) +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
               size = 1, position = position_dodge(width = .1)) +
  labs(y = expression("Beta dispersion \n(median distance)"), x = "") +
  theme_bw(base_size=12)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank()) +
   facet_wrap(~ecotype1)

ecotype.violinfig
 
ecotype.pointfig = 
   marc.modeldata_v1 %>% 
   ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
   geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
   stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
                size = 1, position = position_dodge(width = .1), color = "red") +
   labs(y = expression("Beta dispersion \n(median distance)"), x = "") +
   theme_bw(base_size=12)  +
   theme(plot.margin = unit(c(1,1,1,1), "cm"), 
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank()) +
   facet_wrap(~ecotype1)
 
ecotype.pointfig

climate.pointfig = 
  marc.modeldata_v1 %>% 
  #filter(!Treatment == "Cage Control") %>% 
  #filter(Species == 'Mud crab') %>%
  ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
               size = 1, position = position_dodge(width = .1), color = "red") +
  labs(y = expression("Beta dispersion \n(median distance)"), x = "") +
  theme_bw(base_size=12)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank()) +
  facet_wrap(~climate.zone)

climate.pointfig

consdiv.pointfig = 
  marc.modeldata_v1 %>% 
  ggplot(data = ., aes(x = consumer.richness.category, y = betadisp.comm.dist, shape = cage.treatment_std)) + 
  #geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
               size = 1, position = position_dodge(width = .1), color = "red") +
  labs(y = expression("Beta dispersion \n(median distance)"), x = "") +
  theme_bw(base_size=16)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) 

consdiv.pointfig

exage.pointfig = 
  marc.modeldata_v1 %>% 
  dplyr::filter(!is.na(exp.age)) %>%
  ggplot(data = ., aes(x = exp.age, y = betadisp.comm.dist, color = cage.treatment_std)) + 
  stat_smooth(geom = "smooth", method = "lm") +
  geom_point(size = .5, alpha = .3) +
  labs(y = expression("Beta dispersion \n(median distance)"), x = "") +
  theme_bw(base_size=12)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) 

exage.pointfig

lat.pointfig = 
  marc.modeldata_v1 %>% 
 # dplyr::filter(!is.na(exp.age)) %>%
  ggplot(data = ., aes(x = lat, y = betadisp.comm.dist, color = cage.treatment_std)) + 
  stat_smooth(geom = "smooth", method = "lm") +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
               size = 1, position = position_dodge(width = .1)) +
  geom_point(size = .5, alpha = .3) +
  labs(y = expression("Beta dispersion \n(median distance)"), x = "") +
  theme_bw(base_size=12)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) 

lat.pointfig

lat.pointfig = 
  marc.modeldata_v1 %>% 
  # dplyr::filter(!is.na(exp.age)) %>%
  ggplot(data = ., aes(x = abs(lat), y = betadisp.comm.dist, color = cage.treatment_std)) + 
  stat_smooth(geom = "smooth", method = "lm") +
 # stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
 #              size = .8, position = position_dodge(width = .1), alpha = .4) +
  geom_point(size = .5, alpha = .3) +
  labs(y = expression("Beta dispersion \n(median distance)"), x = "") +
  theme_bw(base_size=12)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) 

lat.pointfig

## diff figs

marc.modeldata_ES %>% 
  ggplot(data = ., aes(x = diff, y = ecotype1)) + 
 # geom_violin(stat = "ydensity", position = "dodge", draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
               size = 1, position = position_dodge(width = .1)) +
  geom_vline(xintercept = 0, color = "red") +
  labs(y = expression("Consumer effect size \n(Diff in B)"), x = "") +
  theme_bw(base_size=12)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank())

marc.modeldata_ES %>% 
  ggplot(data = ., aes(x = abs(lat), y = diff)) + 
  geom_point(size = .5, alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red") +
  stat_smooth(method = "lm") +
  labs(y = expression("Consumer effect size \n(Diff in B)"), x = "") +
  theme_bw(base_size=18)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank())
