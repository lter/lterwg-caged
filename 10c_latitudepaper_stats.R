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
                 emmeans, tidymodels, qqplotr, sjPlot, ggpubr) #, update_all= TRUE) 

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()


## ------------------------------------------- ##
# Load Data ----
# these dfs were created in script 08 script
## ------------------------------------------- ##


caged_effectsize <- read.csv(file.path("data", "08-B_caged_expname-effect-size_fine-scales.csv"))

dim(caged_effectsize) # 620 rows (effect size calculated for each consumer level original treatment)

# Check number of sources
unique(caged_effectsize$exp.name) # 347
dim(caged_effectsize)


## ------------------------------------------- ##
# Paper 1 Effect Size Models
## ------------------------------------------- ##

colnames(caged_effectsize)

range(caged_effectsize$betadisp.mean.lrr)
# why is there a NA in beta disp mean LRR? - its the gilson PA site 

# clean up the data
df1 <- caged_effectsize %>%
  # no longer just using late successional 
  #filter(var_succ.vs.late == "late") %>%
  drop_na(betadisp.mean.lrr)%>%
  mutate(abs.lat = abs(lat)) %>%
  # get rid of any resources of mobile animals 
  filter(var_resource.type.category != "mobile animals") %>%
  # filter out just grassy vs subtidal
  filter(var_grassy_v_stubtidal %in% c("herbaceous", "reef (marine)"))


unique(df1$exp.name) # 246 experiments
unique(df1$source) # 71 sources
dim(df1) #444 effect sizes

range(df1$betadisp.mean.lrr) 
hist(df1$betadisp.mean.lrr) # there is a pretty strong outlier? - its an ashton paper

colnames(df1)
str(df1)



#Beta LRR Models ####
lrr.mod1 <- lmer(betadisp.mean.lrr ~ abs.lat + 
                   #scale(gamma.richness_exp.name) +
                   var_grassy_v_stubtidal +
                   (1|exp.name), 
               data = df1) # singular when you use var_upper_source
summary(lrr.mod1) 
glance(lrr.mod1)
check_model(lrr.mod1)
car::Anova(lrr.mod1, type =2)
#Response: betadisp.mean.lrr
#Chisq Df Pr(>Chisq)  
#abs.lat         3.5727  1    0.05874 .
#var_aq.or.terr2 0.5874  1    0.44344  

coeff(lrr.mod1$lat_ecotype)

car::Anova(lrr.mod1, test.statistic = "F")
#Response: betadisp.mean.lrr
#F Df Df.res  Pr(>F)  
#abs.lat         3.2960  1 81.151 0.07314 .
#var_aq.or.terr2 0.5196  1 20.745 0.47907  

library(effects)
plot(allEffects(lrr.mod1))


# Using GLMMTMB instead - still gaussian
lrr.Bmod1 <- glmmTMB(betadisp.mean.lrr ~
                       abs.lat *
                       var_grassy_v_stubtidal +
                       (1|var_upper.source),
                     data = df1) # this time its not singular, probably glmmTMB has a better model optimizer

glance(lrr.Bmod1)
summary(lrr.Bmod1)
car::Anova(lrr.Bmod1, type = 2) # interaction is now significant 
plot(allEffects(lrr.Bmod1))
# why are we keeping both late and early successional again? i canʻt find anything in our notes



# Alpha Diversity
alpha.mod1 <- glmmTMB(alpha.mean.lrr ~
                       abs.lat +
                       var_grassy_v_stubtidal +
                       (1|var_upper.source),
                     data = df1) # this time its not singular, probably glmmTMB has a better model optimizer
glance(alpha.mod1)
summary(alpha.mod1)
car::Anova(alpha.mod1, type = 2) # no interaction
plot(allEffects(alpha.mod1))


# Dominance
dominance.mod1 <- glmmTMB(dominance.mean.lrr ~
                        abs.lat +
                        var_grassy_v_stubtidal +
                        (1|var_upper.source),
                      data = df1) # this time its not singular, probably glmmTMB has a better model optimizer
glance(dominance.mod1)
summary(dominance.mod1)
car::Anova(dominance.mod1, type = 2) # no interaction
plot(allEffects(dominance.mod1))


# Centroid
cent.mod1 <- glmmTMB(betadisp.centroid.lrr ~
                            abs.lat *
                            var_grassy_v_stubtidal +
                            (1|var_upper.source),
                          data = df1) # this time its not singular, probably glmmTMB has a better model optimizer
glance(cent.mod1)
summary(cent.mod1)
car::Anova(cent.mod1, type = 2) 
plot(allEffects(cent.mod1)) # looks almost the same as beta dispersion







#Beta LRR Figure Draft####
caged_effectsize_neweco
caged_effectsize_grassy 
caged_effectsize_subtidal

Fig1BLat = 
caged_effectsize_neweco %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs(lat), y = betadisp.mean.lrr)) + 
  stat_smooth(method = "lm", geom = "smooth", linewidth = 2) + 
  geom_jitter(width = 0.05, aes(color = lat_ecotype)) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.3) +
  theme_pubr(base_size= 18) +
  labs(x= "Absolute Latitude",
       y="Beta Dispersion LRR") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank())

Fig1BLat

#Tyler's Idea about Dominance ####

dom.df.tyler = caged_effectsize_neweco %>% 
  mutate(inc.dom = case_when(dominance_cage.treat.lrr > 0 ~ "Increases Dom", 
                             dominance_cage.treat.lrr < 0 ~ "Decreases Dom"))

lrrDom.mod2 <- lmer(betadisp.mean.lrr ~ abs.lat * 
                      inc.dom * lat_ecotype +
                   (1|var_upper.source), 
                 data = dom.df.tyler)

summary(lrr.mod2)
glance(lrr.mod2)
check_model(lrr.mod2)
car::Anova(lrrDom.mod2, type =2)

plot(allEffects(lrrDom.mod2))

Fig1BLat_DOM = 
  dom.df.tyler %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs(lat), y = betadisp.mean.lrr)) + 
  stat_smooth(method = "lm", geom = "smooth", linewidth = 2,
              aes(color = inc.dom)) + 
#  stat_smooth(method = "lm", geom = "smooth", linewidth = 2,
#              color = "black") + 
  geom_jitter(width = 0.05, aes(color = inc.dom)) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.3) +
  theme_pubr(base_size= 18) +
  labs(x= "Absolute Latitude",
       y="Beta Dispersion LRR") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right", legend.title = element_blank()) +
  facet_grid(~lat_ecotype)

Fig1BLat_DOM

#Grassy and Subtidal seperate but equal modeling####
lrr_grassy.mod1 <- lmer(betadisp.mean.lrr ~ abs.lat +
                   (1|var_upper.source), data = caged_effectsize_grassy) # 241 samples
summary(lrr_grassy.mod1)
glance(lrr_grassy.mod1)
check_model(lrr_grassy.mod1)
car::Anova(lrr_grassy.mod1, type =2)
#Response: betadisp.mean.lrr
#Chisq Df Pr(>Chisq)
#abs.lat 0.4263  1     0.5138

library(effects)
plot(allEffects(lrr.mod1))

lrr_subtidal.mod1 <- lmer(betadisp.mean.lrr ~ abs.lat +
                          (1|var_upper.source), data = caged_effectsize_subtidal) # 241 samples
summary(lrr_subtidal.mod1)
glance(lrr_subtidal.mod1)
check_model(lrr_subtidal.mod1)
car::Anova(lrr_subtidal.mod1, type =2)
#Response: betadisp.mean.lrr
#Chisq Df Pr(>Chisq)
#abs.lat 1.9446  1     0.1632

#Beta Dispersion (raw) models####
#NOT paper 1
newecotype_B.grassy = newecotype_B %>% 
  filter(lat_ecotype == "grassy systems")
glimpse(newecotype_B.grassy)

newecotype_B.subtidal = newecotype_B %>% 
  filter(lat_ecotype == "subtidal systems")
glimpse(newecotype_B.subtidal)

ESmod_grassy.B <- glmmTMB(
  betadisp_t ~
    cage.treatment_std * abs(lat) +
    scale(gamma.richness_exp.name) +
    scale(betadisp.sample.size) +
    (1 | var_upper.source / exp.name),
  family  = beta_family(link = "probit"),
  #control = ctrl,
  data    = newecotype_B.grassy
)

glance(ESmod_grassy.B)
summary(ESmod_grassy.B)
car::Anova(ESmod_grassy.B) #LAT = .6

#Alpha Diversity LRR ####
range(df1$alpha.diversity_cage.treat.lrr) # -6.671471  6.629928

alphalrr.mod1 <- lmer(alpha.diversity_cage.treat.lrr ~ 
                        abs.lat + 
                        lat_ecotype +
                   (1|var_upper.source), data = caged_effectsize_neweco) 

summary(alphalrr.mod1)
glance(alphalrr.mod1)
check_model(alphalrr.mod1)
car::Anova(alphalrr.mod1, type =2)
#Response: alpha.diversity_cage.treat.lrr
#F Df  Df.res Pr(>F)  
#abs.lat     0.2065  1 118.768 0.6504  
#lat_ecotype 5.8977  1  35.875 0.0203 *

car::Anova(alphalrr.mod1, test.statistic = "F")
#Response: alpha.diversity_cage.treat.lrr
#Chisq Df Pr(>Chisq)  
#abs.lat     0.2149  1     0.6430  
#at_ecotype 6.2101  1     0.0127 *

plot(allEffects(alphalrr.mod1))

alphalrr.mod2 <- lmer(alpha.diversity_cage.treat.lrr ~ 
                        abs.lat * 
                        lat_ecotype +
                        (1|var_upper.source), data = caged_effectsize_neweco)

#alt model structure (beta)
alphalrr.Bmod1 <- glmmTMB(
  alpha.diversity_cage.treat.lrr ~
    abs.lat + 
    lat_ecotype +
    scale(gamma.richness_exp.name) +
    (1 | var_upper.source ),
  # family  = beta_family(link = "probit"),
  # control = ctrl,
  data    = caged_effectsize_neweco 
)

glance(alphalrr.Bmod1)
summary(alphalrr.Bmod1)
car::Anova(alphalrr.Bmod1, type = 2) 

#Response: alpha.diversity_cage.treat.lrr
#Chisq Df Pr(>Chisq)
#abs.lat                        0.3277  1     0.5670
#lat_ecotype                    2.3189  1     0.1278
#scale(gamma.richness_exp.name) 1.3622  1     0.2432 

AICc(alphalrr.mod1, alphalrr.mod2, alphalrr.Bmod1)

#Alpha LRR Figures####

Fig1AGrassSub = 
  caged_effectsize_neweco %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = lat_ecotype, y = alpha.diversity_cage.treat.lrr)) + 
  geom_jitter(width = 0.01, alpha = 0.2) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.3) +
  stat_summary(fun.data = "mean_cl_boot", 
               geom = "pointrange", size = 1, color = "hotpink") + 
  theme_pubr(base_size= 18) +
  labs(x= "",
       y="Alpha Diversity LRR") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank())

Fig1AGrassSub

Fig1ALat = 
  caged_effectsize_neweco %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs(lat), y = alpha.diversity_cage.treat.lrr)) + 
 # stat_smooth(method = "lm", geom = "smooth", linewidth = 2) + 
  geom_jitter(width = 0.05, aes(color = lat_ecotype)) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.3) +
  theme_pubr(base_size= 18) +
  labs(x= "Absolute Latitude",
       y="Alpha Diversity LRR") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank())

Fig1ALat

#Building Figure 1 for the Paper 1 fam####

Fig1BLat / Fig1AGrassSub 

# Paper 1 Raw Dominance Figure
#Beta Div LRR ~ Dominance LRR ####
betadom.mod1 <- lmer(betadisp.mean.lrr ~ 
                       dominance_cage.treat.lrr * 
                       lat_ecotype +
                       (1|var_upper.source), 
                     data = caged_effectsize_neweco) 

summary(betadom.mod1)
glance(betadom.mod1)
check_model(betadom.mod1)
car::Anova(betadom.mod1, type =2)

car::Anova(betadom.mod1, test.statistic = "F")

#Beta Div LRR ~ Dominance LRR Figure####
Fig3BetaDom = 
  caged_effectsize_neweco %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = dominance_cage.treat.lrr, y = betadisp.mean.lrr)) + 
  geom_jitter(width = 0.01, alpha = 0.8, size = 1,
              aes(color = lat_ecotype)) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black", 
             alpha = 0.3) +
  stat_smooth(method = "lm", size = 1,
              aes(color = lat_ecotype)) + 
  theme_pubr(base_size= 18) +
  labs(x= "Dominance LRR",
       y="Beta Disperson LRR") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #legend.position = "", 
    legend.title = element_blank())

Fig3BetaDom


#Building Figure 1 for the Paper 1 fam####

Fig1BLat / Fig1AGrassSub 

#Dominance LRR models ####
#NOTE: they dont want these

domlrr.mod1 <- lmer(dominance_cage.treat.lrr ~ 
                        abs.lat * lat_ecotype +
                        (1|var_upper.source), data = caged_effectsize_neweco) # 241 samples
summary(domlrr.mod1)
glance(domlrr.mod1)
check_model(domlrr.mod1)
car::Anova(domlrr.mod1, type =2)

#Response: dominance_cage.treat.lrr
#Chisq Df Pr(>Chisq)  
#abs.lat             2.6144  1     0.1059  
#lat_ecotype         1.2495  1     0.2636  
#abs.lat:lat_ecotype 4.6254  1     0.0315 *

car::Anova(domlrr.mod1, test.statistic = "F")
#Response: dominance_cage.treat.lrr
#F Df Df.res  Pr(>F)  
#abs.lat             2.4133  1 79.723 0.12427  
#lat_ecotype         1.3381  1 20.469 0.26069  
#abs.lat:lat_ecotype 4.2763  1 87.980 0.04158 *

plot(allEffects(domlrr.mod1))

Fig3.5LatHabDom = 
  caged_effectsize_neweco %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs.lat, y = dominance_cage.treat.lrr)) + 
  geom_jitter(width = 0.01, alpha = 0.8, size = 1,
              aes(color = lat_ecotype)) + 
  geom_hline(yintercept = 0, color = "black", 
             alpha = 0.7) +
  #geom_vline(xintercept = 0, color = "black", 
   #          alpha = 0.3) +
  stat_smooth(method = "lm", size = 1,
              aes(color = lat_ecotype)) + 
  theme_pubr(base_size= 18) +
  labs(x= "Absolute Latitude",
       y= "Dominance LRR") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #legend.position = "", 
    legend.title = element_blank())

Fig3.5LatHabDom

#Raw Dominance#####
rawdom.df = rawbetas_v1 %>% 
  mutate(lat_ecotype = 
           case_when(var_ecotype1 == "grassland" ~ "grassy systems", 
                     var_ecotype1 == "savanna" ~ "grassy systems",
                     var_ecotype1 == "tundra" ~ "grassy systems",
                     var_ecotype1 == "desert" ~ "grassy systems",
                     var_ecotype1 == "subtidal" ~ "subtidal systems",
                     var_ecotype1 == "coral reef" ~ "subtidal systems",
                     #var_ecotype1 == "forest" ~ "trees", 
                     .default = NA)) %>% 
  filter(lat_ecotype != "")

domuncaged.mod1 <- lmer(dominance.uncaged ~ 
                      abs(lat) * lat_ecotype +
                      (1|var_upper.source), data = rawdom.df) # 241 samples
summary(domuncaged.mod1)
glance(domuncaged.mod1)
check_model(domuncaged.mod1)
car::Anova(domuncaged.mod1, type =2)

#Response: dominance.uncaged
#F Df  Df.res  Pr(>F)  
#abs(lat)             0.0071  1 172.961 0.93310  
#lat_ecotype          3.0958  1  52.616 0.08431 .
#abs(lat):lat_ecotype 1.0074  1 146.202 0.31718  

car::Anova(domuncaged.mod1, test.statistic = "F")

FigLatRawDom = 
  rawdom.df %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs(lat), y = dominance.uncaged)) + 
  geom_jitter(width = 0.01, alpha = 0.8, size = 1) + #,
            #  aes(color = lat_ecotype)) + 
  #geom_vline(xintercept = 0, color = "black", 
  #          alpha = 0.3) +
  #stat_smooth(method = "gam", size = 1, 
  #            aes(color = lat_ecotype)) + 
  theme_pubr(base_size= 14) +
  labs(x= "Absolute Latitude",
       y= "Dominance (raw) in Uncaged") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #legend.position = "", 
    legend.title = element_blank()) 

FigLatRawDom

FigLatRawDom2=
rawdom.df %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = abs(lat), y = dominance.caged)) + 
  geom_jitter(width = 0.01, alpha = 0.8, size = 1,
              aes(color = lat_ecotype)) + 
  stat_smooth(method = "gam", size = 1, 
              aes(color = lat_ecotype)) + 
  theme_pubr(base_size= 14) +
  labs(x= "Absolute Latitude",
       y= "Dominance (raw) in Caged") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #legend.position = "", 
    legend.title = element_blank()) 

FigLatRawDom / FigLatRawDom2

rawdom.df %>% 
  filter(lat_ecotype != "") %>% 
  ggplot(aes(x = lat_ecotype, y = dominance.uncaged)) + 
  geom_jitter(width = 0.01, alpha = 0.8, size = 1) + 
  stat_summary(fun.data = "mean_cl_boot", size = 1,
              aes(color = lat_ecotype)) + 
  theme_pubr(base_size= 18) +
  labs(x= "",
       y= "Dominance Raw in Uncaged") +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #legend.position = "", 
    legend.title = element_blank()) 

