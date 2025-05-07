## --------------------------------------------------------------- ##
# CAGED Model Building
## --------------------------------------------------------------- ##
# Written by: Marc Hensel

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, lme4, performance, lubridate, car, njlyon0/supportR) #, update_all= TRUE)

# Create needed folder(s)
#dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
#BUT might want to put nicks new "pull from google drive" code here
alldata_v1 <- read.csv(file.path("data", "06_caged_with-metadata.csv"))

# ----tidy and wrangle data----
glimpse(alldata_v1)

supportR::num_check(data = alldata_v1, col = "exp.name.spatialextent.category")
sort(unique(alldata_v1$betadisp.design.level))


#get years and richness into number form
alldata_v1$year.start.exclosure <- year(as.Date(as.character(alldata_v1$year.start.exclosure), format = "%Y"))
alldata_v1$year.end.exclosure <- year(as.Date(as.character(alldata_v1$year.end.exclosure), format = "%Y"))
#alldata_v1$consumer.richness <- as.numeric(alldata_v1$consumer.richness) #some stupid shit like ">10" in here, so this gives NA
alldata_v1$lat <- as.numeric(alldata_v1$lat)

#first round of slimming down DF. Might be bad practices to do select() this early
modeldata_v1 = alldata_v1 |> 
  select(source, site, sampling.years, excluded.group, measured.group, lat:ecotype1, consumer.richness, natural.vs.artificial.substrate, exclusion.duration, year.start.exclosure, year.end.exclosure, cage.treatment_std, betadisp.design.level:betadisp.comm.dist) |> 
  mutate(exp.age = year.end.exclosure - year.start.exclosure) |> #calculate exclusion age
  filter(cage.treatment_std %in% c('caged', 'uncaged')) #dont need the other treatments
  
#data QC to make sure its ready to model
glimpse(modeldata_v1)

#so many NAs in consumer richness
supportR::count(vec = modeldata_v1$consumer.richness)
supportR::count(vec = modeldata_v1$cage.treatment_std)

#slim down this DF for initial explorations. this DF will DEF be different for real analyses
marc.modeldata_v1 = modeldata_v1 |> 
  select(source, excluded.group, measured.group, lat, ecotype1, consumer.richness, cage.treatment_std, betadisp.design.level, betadisp.sample.size, betadisp.median, betadisp.comm.dist, exp.age)

glimpse(marc.modeldata_v1)



#Blue Skies model structure that will explain everything 
# betadisp.comm.dist ~ cage.treatment_std + ecotype1 + latitude + consumer richness + experiment age + gamma diversity + excl size size + successional stage + max consumer size + B sample size + B design level 
#REs: (1| source/expname)

hist(marc.modeldata_v1$betadisp.comm.dist)
range(marc.modeldata_v1$betadisp.comm.dist)

BaetaDisp.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std + ecotype1 + #consumer.richness + consumer.richness + gamma diversity + excl size size + 
                         betadisp.sample.size + betadisp.design.level + lat + exp.age +
                         (1|source), 
                   data = marc.modeldata_v1)


check_model(BaetaDisp.lmer)
summary(BaetaDisp.lmer)
car::Anova(BaetaDisp.lmer, test.statistic = "F")
performance::r2(BaetaDisp.lmer)

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

# Next steps: dredge() AIC selection
# Maybe also random effects AIC selection? on full model
