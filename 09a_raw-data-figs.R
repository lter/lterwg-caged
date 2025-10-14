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
BaeDiff.df <- read.csv(file.path("data", "08_caged_prepped-effect-size.csv"))
BaeDisp.df <- read.csv(file.path("data", "08_caged_prepped-beta-dispersion.csv"))


# View(BaeDiff.df) 
# View(BaeDisp.df) 
dim(BaeDiff.df)# 273 rows
dim(BaeDisp.df)# 11281 rows

## ------------------------------------------- ##
# Plots ---- 
## ------------------------------------------- ##
colnames(BaeDisp.df)
colnames(BaeDiff.df)

# Beta dispersion (uncaged only) by latitude
BaeDisp.df %>%
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
BaeDisp.df <- BaeDisp.df %>%
  filter(var_aq.or.terr != "") %>%
  droplevels()

# Beta dispersion (caged only) by latitude
BaeDisp.df %>%
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
BaeDiff.df %>%
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
BaeDiff.df %>%
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
BaeDiff.df %>%
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
BaeDiff.df %>%
  # lots of these values are "unknown"
  filter(!is.na(as.numeric(var_exclosure.area.m2))) %>%
  ggplot(aes(x=var_aq.or.terr, 
             # change from character to numeric 
             y=as.numeric(var_exclosure.area.m2))) +
  geom_boxplot() +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("royalblue","darkgreen"))
# seems like too big of differences, makes it hard to evaluate? 

