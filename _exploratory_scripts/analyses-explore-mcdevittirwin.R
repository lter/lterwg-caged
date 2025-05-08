## --------------------------------------------------------------- ##
# Analyses
## --------------------------------------------------------------- ##
# Written by: Jamie McDevitt-Irwin
# stolen from Marc Hensel's script! 

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, lme4, 
                 performance, lubridate, car, 
                 njlyon0/supportR, sjPlot, emmeans, ggpubr) #, update_all= TRUE)

# Create needed folder(s)
#dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
alldata_v1 <- read.csv(file.path("data", "06_caged_with-metadata.csv"))


## ------------------------------------------- ##
# Data Wrangling
## ------------------------------------------- ##
glimpse(alldata_v1) # 11,875 rows, 51 columns

supportR::num_check(data = alldata_v1, col = "exp.name.spatialextent.category")
sort(unique(alldata_v1$betadisp.design.level))


#get years and richness into number form
alldata_v1$year.start.exclosure <- year(as.Date(as.character(alldata_v1$year.start.exclosure), format = "%Y"))
alldata_v1$year.end.exclosure <- year(as.Date(as.character(alldata_v1$year.end.exclosure), format = "%Y"))
#alldata_v1$consumer.richness <- as.numeric(alldata_v1$consumer.richness) #some stupid shit like ">10" in here, so this gives NA
alldata_v1$lat <- as.numeric(alldata_v1$lat)


## ------------------------------------------- ##
# Exploratory Figures 
## ------------------------------------------- ##
alldata_v1 %>% 
  ggplot(aes(x=cage.treatment_std, 
             y=betadisp.comm.dist)) +
           geom_boxplot() +
  facet_wrap(~aq.or.terr) +
  theme_pubr(base_size=16)


alldata_v1 %>% 
  ggplot(aes(x=consumer.richness.category, 
             y=betadisp.comm.dist)) +
  geom_boxplot() +
  facet_wrap(~aq.or.terr) +
  theme_pubr(base_size=16)


alldata_v1 %>% 
  ggplot(aes(x=cage.treatment_std, 
             y=betadisp.comm.dist)) +
  geom_boxplot() +
  facet_wrap(~ecotype1,
             scale="free_y") +
  theme_pubr(base_size=16)

alldata_v1 %>% 
  ggplot(aes(x=betadisp.sample.size, 
             y=betadisp.comm.dist)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(~cage.treatment_std) +
  theme_pubr(base_size=16)


alldata_v1 %>% 
  ggplot(aes(x=lat, 
             y=betadisp.comm.dist)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(~cage.treatment_std) +
  theme_pubr(base_size=16)


alldata_v1 %>% 
  ggplot(aes(x=betadisp.design.level, 
             y=betadisp.comm.dist)) +
  geom_boxplot() +
  facet_wrap(~cage.treatment_std,
             scale="free_y") +
  theme_pubr(base_size=16)



## ------------------------------------------- ##
# Exploratory Model 
## ------------------------------------------- ##
hist(alldata_v1$betadisp.comm.dist)
range(alldata_v1$betadisp.comm.dist) # 0 1
dim(alldata_v1) #11875 

mod1 <- lmer(betadisp.comm.dist ~ cage.treatment_std + 
                         ecotype1 + 
                         consumer.richness.category + 
                         #gamma diversity + 
                         #exclosure size  + 
                         betadisp.sample.size + 
                         betadisp.design.level + 
                         lat + 
                         #exp.age +
                         (1|source), 
                       data = alldata_v1)
summary(mod1)
performance::r2(mod1) # marginal=fixed effects, conditional= random

# Residuals 
check_model(mod1)


# Type 2 Anova
car::Anova(mod1, type= 2)
# everything except beta disp design level is sig, lat is only almost sig 


mod.data1 <- alldata_v1 %>%
  filter(cage.treatment_std %in% c("caged","uncaged"))

# Try with an interaction
mod2 <- lmer(betadisp.comm.dist ~ cage.treatment_std +
               (1|source), 
             data = mod.data1)
summary(mod2)
performance::r2(mod2) # marginal=fixed effects, conditional= random

# Residuals 
check_model(mod2)


# Type 2 Anova
car::Anova(mod2, type= 2)
# everything except beta disp design level is sig, lat is only almost sig 

sjPlot::plot_model(mod2)

emmeans(mod2, ~ cage.treatment_std)
emmeans(mod2, ~ aq.or.terr) # aquatic is much lower than terrestrial
emmeans(mod2, ~ consumer.richness.category)



# Next steps: dredge() AIC selection
# Maybe also random effects AIC selection? on full model




