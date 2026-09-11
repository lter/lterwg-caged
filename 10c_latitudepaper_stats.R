## --------------------------------------------------------------- ##
# Latitude Paper Statistics
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, Max Castorani, 
# Jamie McDevitt-Irwin, Kelly E Speare ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, glmmTMB, DHARMa,
                 performance, easystats, lubridate, car, broom.mixed,
                 njlyon0/supportR, MuMIn, visreg, grid, gridExtra,
                 emmeans, tidymodels, qqplotr, sjPlot, ggpubr, effects) #, update_all= TRUE) 

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()


## ------------------------------------------- ##
# Load Data ----
# these dfs were created in script 08 and 09 script
## ------------------------------------------- ##
# effect size df
caged_effectsize <- read.csv(file.path("data", "08_caged_prepped-effect-size.csv"))

dim(caged_effectsize) # 404 rows (effect size calculated for each consumer level original treatment)

# Check number of sources
unique(caged_effectsize$source) # 117
unique(caged_effectsize$exp.name) # 347

# raw beta disperison df
caged_raw <- read.csv(file.path("data", "08-A_caged_w.meta-beta-disp_fine-scales.csv"))

dim(caged_raw) # 13964    46

# Check number of sources
unique(caged_raw$source) # 117
unique(caged_raw$exp.name) # 347


## ------------------------------------------- ##
# Data Wrangling
## ------------------------------------------- ##
colnames(caged_effectsize)

range(caged_effectsize$mean.beta.lrr)
# why is there a NA in beta disp mean LRR? - now its royo

# clean up the data
# need to replace
df1 <- caged_effectsize %>%
  # no longer just using late successional because we looking at effect size 
  #filter(var_succ.vs.late == "late") %>%
  # get rid of royo NA
  drop_na(mean.beta.lrr)%>% 
  mutate(abs.lat = abs(lat)) %>%
  # get rid of any resources of mobile animals - are there any? 
  filter(var_resource.type.category != "mobile animals") %>%
  # filter out just grassy vs subtidal for this first paper
  filter(var_grassy_v_stubtidal %in% c("herbaceous", "reef (marine)")) %>%
  # get rid of the crazy outlier
  filter(mean.beta.lrr < 58) 

# Now we have a lower sample size
unique(df1$exp.name) # 245 experiments
unique(df1$source) # 71 sources
dim(df1) # 289 effect sizes

range(df1$mean.beta.lrr) 
hist(df1$mean.beta.lrr) # there is a pretty strong outlier? - its an ashton paper
# outlier is gone now because i filtered at line 67



# clean up the raw values 
raw_df1 <- caged_raw %>%
  mutate(abs.lat = abs(lat)) %>%
  # get rid of any resources of mobile animals - are there any? 
  filter(var_resource.type.category != "mobile animals") %>%
  # filter out just grassy vs subtidal for this first paper
  filter(var_grassy_v_stubtidal %in% c("herbaceous", "reef (marine)")) %>%
  # filter out uncaged only
  filter(cage.treatment_std == "uncaged")


# Convert to factor
df1$var_grassy_v_stubtidal <- as.factor(df1$var_grassy_v_stubtidal)
df1$var_upper.source <- as.factor(df1$var_upper.source)

str(df1$var_grassy_v_stubtidal)
str(df1$abs.lat)
str(df1$var_upper.source) # this should be the random effect 


## ------------------------------------------- ##
# Models 
## ------------------------------------------- ##
# Beta dispersion 
beta.mod1 <- lmer(mean.beta.lrr ~ poly(lat, degree=2) +
                    # trying out poly latitude instead of abslat - still no sig 
                   var_grassy_v_stubtidal +
                   (1|var_upper.source), 
               data = df1) # interaction is not sig, so we removed

summary(beta.mod1) 
glance(beta.mod1)
car::Anova(beta.mod1, type =2) # nothing is significant
check_model(beta.mod1) # haven't check this yet, not showing

plot(allEffects(beta.mod1)) # increasing with abs latitude 
# why are we keeping both late and early successional again? i canʻt find anything in our notes



# Alpha Diversity
alpha.mod1 <- lmer(mean.alpha.lrr ~
                     poly(lat, degree=2) +
                       var_grassy_v_stubtidal +
                       (1|var_upper.source),
                     data = df1)  # interaction is not significant, so removed
glance(alpha.mod1)
summary(alpha.mod1)
car::Anova(alpha.mod1, type = 2) # grassy vs subtidal is sig 
plot(allEffects(alpha.mod1))


# Dominance
dom.mod1 <- lmer(mean.dom.lrr ~
                   poly(lat, degree=2) +
                        var_grassy_v_stubtidal +
                        (1|var_upper.source),
                      data = df1) # interaction not sig
glance(dom.mod1)
summary(dom.mod1)
car::Anova(dom.mod1, type = 2) # nothing is significant
plot(allEffects(dom.mod1))


# Centroid
cent.mod1 <- lmer(mean.cent.lrr ~
                    poly(lat, degree=2) +
                            var_grassy_v_stubtidal +
                            (1|var_upper.source),
                          data = df1) # interaction is not significant
glance(cent.mod1)
summary(cent.mod1)
car::Anova(cent.mod1, type = 2) # not sig
plot(allEffects(cent.mod1)) 
# Our centroid anova values are exactly the same as beta dispersion --> probalby because we are averaging to create an effect size
# should we calculate composition in another way? or use raw values instead of effect size?


# How does dominance influence our beta LRR? 
dom.df.tyler <- df1 %>% 
  mutate(inc.dom = case_when(mean.dom.lrr > 0 ~ "Increases Dom", 
                             mean.dom.lrr < 0 ~ "Decreases Dom"))

beta.dom.mod1 <- lmer(mean.beta.lrr ~ 
                        poly(lat, degree=2) * inc.dom
                      * var_grassy_v_stubtidal +
                        (1|var_upper.source), 
                      data = dom.df.tyler)


summary(beta.dom.mod1)
glance(beta.dom.mod1)
check_model(beta.dom.mod1)
car::Anova(beta.dom.mod1, type =2) # three way interaction is sig 

plot(allEffects(beta.dom.mod1))


beta.dom.mod2 <- lmer(
  mean.beta.lrr ~ 
    poly(lat, degree = 2) + inc.dom + var_grassy_v_stubtidal +
    (1 | var_upper.source), 
  data = dom.df.tyler
) # nothing is sig once we change to poly

summary(beta.dom.mod2)
glance(beta.dom.mod2)
check_model(beta.dom.mod2)
car::Anova(beta.dom.mod2, type =2) # three way interaction is sig 

plot(allEffects(beta.dom.mod2))



# Uncaged Raw Dominance (different dataframe)
raw.dom.mod1 <- lmer(dominance ~ 
                          abs.lat * var_grassy_v_stubtidal +
                          (1|var_upper.source), 
                        data = raw_df1) 
summary(raw.dom.mod1)
glance(raw.dom.mod1)
check_model(raw.dom.mod1)
car::Anova(raw.dom.mod1, type =2) # interaciton is significant

plot(allEffects(raw.dom.mod1))







## ------------------------------------------- ##
# Raw Data Figures
## ------------------------------------------- ##
# Beta LRR Figure
Fig1BLat <- df1 %>% 
  ggplot(aes(x = abs.lat, y = mean.beta.lrr)) + 
  # this is not our model prediction 
  stat_smooth(method = "lm", geom = "smooth", linewidth = 2) + 
  geom_jitter(width = 0.05, aes(color = var_grassy_v_stubtidal)) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.3) +
  theme_pubr(base_size= 18) +
  labs(x= "Absolute Latitude",
       y="Beta Dispersion LRR")
Fig1BLat


# Dominance influence beta LRR 
Fig1BLat_DOM <- dom.df.tyler %>% 
  ggplot(aes(x = abs.lat, y = mean.beta.lrr)) + 
  stat_smooth(method = "lm", geom = "smooth", linewidth = 2,
              aes(color = inc.dom)) + 
  geom_jitter(width = 0.05, aes(color = inc.dom)) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.3) +
  theme_pubr(base_size= 18) +
  labs(x= "Absolute Latitude",
       y="Beta Dispersion LRR") +
  facet_grid(~var_grassy_v_stubtidal)                 

Fig1BLat_DOM



# Alpha Diversity 
Fig1AGrassSub <-  df1 %>%
  ggplot(aes(x = var_grassy_v_stubtidal,
             y = mean.alpha.lrr)) + 
  geom_jitter(width = 0.01, alpha = 0.2) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", 
               geom = "pointrange", size = 1, color = "hotpink") + 
  theme_pubr(base_size= 18) +
  labs(x= "",
       y="Alpha Diversity LRR")
Fig1AGrassSub

Fig1ALat <- df1 %>%
  ggplot(aes(x = abs(lat), 
             y = alpha.mean.lrr)) + 
  geom_jitter(width = 0.05, 
              aes(color = var_grassy_v_stubtidal)) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.3) +
  theme_pubr(base_size= 18) +
  labs(x= "Absolute Latitude",
       y="Alpha Diversity LRR")
Fig1ALat



#Beta Div LRR ~ Dominance LRR Figure
Fig3BetaDom <- df1 %>%
  ggplot(aes(x = mean.dom.lrr, 
             y = mean.beta.lrr)) + 
  geom_jitter(width = 0.01, alpha = 0.8, size = 1,
              aes(color = var_grassy_v_stubtidal)) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black", 
             alpha = 0.3) +
  stat_smooth(method = "lm", 
              aes(color = var_grassy_v_stubtidal)) + 
  theme_pubr(base_size= 18) +
  labs(x= "Dominance LRR",
       y="Beta Disperson LRR")

Fig3BetaDom


# Dominance LRR vs ablat
Fig3.5LatHabDom <- df1 %>%
  ggplot(aes(x = abs.lat, 
             y = mean.dom.lrr)) + 
  geom_jitter(width = 0.01, alpha = 0.8, size = 1,
              aes(color = var_grassy_v_stubtidal)) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.7) +
  #geom_vline(xintercept = 0, color = "black", 
   #          alpha = 0.3) +
  stat_smooth(method = "lm", size = 1,
              aes(color = var_grassy_v_stubtidal)) + 
  theme_pubr(base_size= 18) +
  labs(x= "Absolute Latitude",
       y= "Dominance LRR")

Fig3.5LatHabDom


# Raw Uncaged Dominance
FigLatRawDom <- raw_df1 %>%
  ggplot(aes(x = abs.lat, 
             y = dominance)) + 
  geom_jitter(width = 0.01, alpha = 0.8, size = 1) + 
  theme_pubr(base_size= 14) +
  labs(x= "Absolute Latitude",
       y= "Dominance (raw) in Uncaged")+
  facet_grid(~var_grassy_v_stubtidal)  +
  stat_smooth(method = "lm", geom = "smooth", linewidth = 2) + 
  geom_jitter(width = 0.05, aes(color = var_grassy_v_stubtidal)) 

FigLatRawDom


