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
caged_beta <- read.csv(file.path("data", "08_caged_prepped-beta-dispersion.csv"))


# View(caged_effectsize) 
# View(caged_beta) 
dim(caged_effectsize)# 370 rows
dim(caged_beta)# 12339  rows

## ------------------------------------------- ##
# Plots ---- 
## ------------------------------------------- ##
colnames(caged_beta)
colnames(caged_effectsize)


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


caged_beta %>%
 # filter(var_aq.or.terr != "") %>%
  filter(var_ecotype1 != "") %>%
  ggplot(aes(x=cage.treatment_std, 
             y=betadisp.comm.dist,
             color=cage.treatment_std,)) +
  geom_boxplot(size=1.1) +
  geom_point(position= position_jitterdodge(), alpha= 0.05) +
  facet_wrap(~var_ecotype1)+
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue",
                               "darkturquoise")) +
  labs(x= "",
       y= "Beta dispersion",
       colour = "Caging treatment") 



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


# Diff by latitude

caged_beta <- caged_beta %>%
  filter(var_aq.or.terr != "") %>%
  droplevels()


caged_effectsize %>%
  ggplot(aes(x=abs(lat), 
             y=within.cage.treat_betadisp.mean.diff,
             col=var_aq.or.terr)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue","darkgreen"))+
  labs(x= "Absolute latitude",
       y= "Effect size\n(uncaged - caged mean)",
       colour = "Biome")
  
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

