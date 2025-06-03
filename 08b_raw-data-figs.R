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
  scale_color_manual(values= c("grey", "royalblue","darkgreen"))

# Beta dispersion (caged only) by latitude
BaeDisp.df %>%
  filter(cage.treatment_std == "caged") %>%
  ggplot(aes(x=abs(lat), 
             y=betadisp.comm.dist,
             col=var_aq.or.terr)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("grey", "royalblue","darkgreen"))


# Diff by latitude
BaeDiff.df %>%
  ggplot(aes(x=abs(lat), 
             y=within.cage.treat_betadisp.mean.diff,
             col=var_aq.or.terr)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("grey", "royalblue","darkgreen"))
  
# Absolute diff by latitude
BaeDiff.df %>%
  ggplot(aes(x=abs(lat), 
             y=abs(within.cage.treat_betadisp.mean.diff),
             col=var_aq.or.terr)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_pubr(base_size=16) +
  scale_color_manual(values= c("grey", "royalblue","darkgreen"))
# absolute value= doesnt matter which direction, is just showing a big difference betweeen caged and uncaged beta dispersion
