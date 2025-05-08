## --------------------------------------------------------------- ##
# CAGED Model Building
## --------------------------------------------------------------- ##
# Written by: Marc Hensel

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, lme4, performance, lubridate, car, njlyon0/supportR, MuMIn) #, update_all= TRUE)

# Create needed folder(s)
#dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
#BUT might want to put nicks new "pull from google drive" code here
alldata_v1 <- read.csv(file.path("data", "06_caged_with-metadata.csv"))

# ----tidy and wrangle data----
#lets see what we are dealing with
glimpse(alldata_v1)

supportR::num_check(data = alldata_v1, col = "exp.name.spatialextent.category")
sort(unique(alldata_v1$betadisp.design.level))

supportR::count(vec = alldata_v1$consumer.richness) #so many NAs in consumer richness
supportR::count(vec = alldata_v1$cage.treatment_std) #deal with this via a filtering
supportR::count(vec = alldata_v1$betadisp.sample.size) 
supportR::count(vec = alldata_v1$exp.name.spatialextent.category) 
supportR::count(vec = alldata_v1$ecotype1) 

#supportR::count_diff(vec1 = alldata_v1$betadisp.median , 
#                     vec2=alldata_v1$betadisp.comm.dist)

#get years and richness into number form
alldata_v1$year.start.exclosure <- year(as.Date(as.character(alldata_v1$year.start.exclosure), format = "%Y"))
alldata_v1$year.end.exclosure <- year(as.Date(as.character(alldata_v1$year.end.exclosure), format = "%Y"))
#alldata_v1$consumer.richness <- as.numeric(alldata_v1$consumer.richness) #some stupid shit like ">10" in here, so this gives NA
alldata_v1$lat <- as.numeric(alldata_v1$lat)

#first round of slimming down DF. Might be bad practices to do select() this early
modeldata_v1 = alldata_v1 |> 
  select(source, site, project.name, exp.name, #select study ID stuff
         sampling.years, lat:ecotype1, exclusion.duration, natural.vs.artificial.substrate, year.start.exclosure, year.end.exclosure, cage.treatment_std, exp.name.spatialextent.category, #select important study info. prob bad practices to have the lat:ecotype1
         excluded.group, consumer.richness, consumer.richness.category, consumer.native.domestic, #select important stuff about consumers
         
         measured.group, betadisp.design.level:betadisp.comm.dist) |> #select important stuff about community 
  mutate(exp.age = year.end.exclosure - year.start.exclosure) |> #calculate exclusion age
  filter(cage.treatment_std %in% c('caged', 'uncaged')) #dont need the other treatments
  
#data QC to make sure its ready to model
glimpse(modeldata_v1)

#slim down this DF for initial explorations. this DF will DEF be different for real analyses
marc.modeldata_v1 = modeldata_v1 |> 
  select(source, exp.name, lat, climate.zone, aq.or.terr, ecotype1, exp.age, cage.treatment_std, excluded.group, consumer.richness.category, measured.group, betadisp.design.level, betadisp.sample.size, betadisp.comm.dist)

glimpse(marc.modeldata_v1)

supportR::count(vec = marc.modeldata_v1$exp.age) 

# Export locally
write.csv(x = marc.modeldata_v1, row.names = F, na = '',
          file = file.path("data", "marc.modeldata_v1.csv"))

#create the effect size DF 
marc.modeldata_ES = marc.modeldata_v1 |> 
  # Remove missing beta dispersion
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  # Keep only good treatments but shouldnt be any 
  #dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  # Summarize within treatments
  group_by(across(all_of(setdiff(x = names(marc.modeldata_v1), y = c('betadisp.comm.dist'))))) |>
  summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = T)) %>% 
  # Pivot to treatment into wide format
  tidyr::pivot_wider(names_from = cage.treatment_std, values_from = betadisp.mean) %>% 
  # Calculate difference
  dplyr::mutate(diff = uncaged - caged)

dplyr::glimpse(marc.modeldata_ES)

# Export locally
write.csv(x = marc.modeldata_ES, row.names = F, na = '',
          file = file.path("data", "marc.modeldata_ES.csv"))


  

#Blue Skies model structure that will explain everything ----

#I. B community distance across all experiments

# betadisp.comm.dist ~ cage.treatment_std + ecotype1 + latitude + consumer richness + experiment age + gamma diversity + excl size size + successional stage + max consumer size + B sample size + B design level 
#REs: (1| source/expname)

hist(marc.modeldata_v1$betadisp.comm.dist)
range(marc.modeldata_v1$betadisp.comm.dist)

#First cut B comm dist----
BaetaDisp.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1 + consumer.richness.category + lat + exp.age + betadisp.sample.size + betadisp.design.level +
                         (1|exp.name), 
                       data = marc.modeldata_v1)


check_model(BaetaDisp.lmer)
check_collinearity(BaetaDisp.lmer)
summary(BaetaDisp.lmer)
car::Anova(BaetaDisp.lmer, test.statistic = "F")
performance::r2(BaetaDisp.lmer)

#dev.off()
#dev.new(width = 11, height = 6)
#area_ts %>% 
#  distinct(studyid, duration) %>% 
#  ggplot(data = ., aes(x = duration)) + 
#  geom_histogram(fill = "grey40") + 
#  mytheme() + 
#  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
#  labs(x = "Duration (years)", y = "Number of studies")
#ggsave(here::here('figures/SOM/duration_study_count_histogram.png'))

BaetaDispEcoInt.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1 + 
                         betadisp.sample.size + betadisp.design.level + lat + exp.age +
                         (1|source), 
                       data = marc.modeldata_v1)

car::Anova(BaetaDispEcoInt.lmer, test.statistic = "F")
check_model(BaetaDispEcoInt.lmer)
summary(BaetaDispEcoInt.lmer)

BaetaDispEco3Int.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1*consumer.richness + 
                               betadisp.sample.size + betadisp.design.level + lat + exp.age +
                               (1|source), 
                             data = marc.modeldata_v1)

car::Anova(BaetaDispEco3Int.lmer, test.statistic = "F")
check_model(BaetaDispEco3Int.lmer)
summary(BaetaDispEco3Int.lmer)

##post sumamry explore
#Leave best fitting/favorite model up here:
BaeDispAT.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*aq.or.terr + 
                       consumer.richness.category + 
                       gamma.richness + 
                       abs(lat) + 
                       #exp.age + 
                       betadisp.sample.size + 
                       (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp.lmer, panel = F) %>% plot()
check_collinearity(BaeDisp.lmer)
summary(BaeDisp.lmer)
car::Anova(BaeDispAT.lmer, test.statistic = "F")
performance::r2(BaeDispAT.lmer)

emmip(BaeDisp.lmer, ~ cage.treatment_std |ecotype1)
emmip(BaeDisp.lmer, ~ consumer.richness.category)

plot_model(BaeDisp.lmer)

# Next steps: dredge() AIC selection
# Maybe also random effects AIC selection? on full model

#Effect Size model----
glimpse(avg.cage_v1)
avg.cage_v1 <- caged_v1 %>% 
  dplyr::select(source:exp.name, lat:long, ecotype1, consumer.richness.category, betadisp.sample.size, 
                cage.treatment_std, 
                within.cage.treat_betadisp.mean,
                within.cage.treat_betadisp.mean.diff) %>% 
  dplyr::filter(!is.na(within.cage.treat_betadisp.mean.diff)) %>% 
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = within.cage.treat_betadisp.mean)

BaeES.lmer <- lmer(within.cage.treat_betadisp.mean.diff ~ ecotype1 + consumer.richness.category + abs(lat) + gamma.richness + betadisp.sample.size + (1|source), data = avg.cage_v1)

check_model(BaeES.lmer, panel = F) %>% plot()
check_collinearity(BaeES.lmer)
summary(BaeES.lmer)
car::Anova(BaeES.lmer, test.statistic = "F")
performance::r2(BaeES.lmer)
