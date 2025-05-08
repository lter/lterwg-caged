## --------------------------------------------------------------- ##
# CAGED Data Exploring via Figures
## --------------------------------------------------------------- ##
# Written by: Marc Hensel

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, lme4, performance, lubridate, car, njlyon0/supportR, visreg) #, update_all= TRUE)

# Create needed folder(s)
#dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
#NOTE: since this is still initial explorations, you gotta go run the DFs in 06c_model-explore-hensel.R to get the two DFs I used in these. Will fix this later tho

marc.modeldata_v1
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
