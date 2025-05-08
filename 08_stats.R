## --------------------------------------------------------------- ##
# CAGED Stats and Analyses 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, performance, easystats, lubridate, car, njlyon0/supportR, MuMIn, visreg, emmeans, tidymodels) #, update_all= TRUE) 

# Create needed folder(s)
#dir.create(path = file.path("data"), showWarnings = F)
#dir.create(path = file.path("data", "raw"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Data ---- 
## ------------------------------------------- ##

# NOTE if we should be downlaoding this from the drive, might need to do this below.
#https://drive.google.com/file/d/1nld9xYSSJVxyA-KlOBXqOvDN4BBrCkhB/view?usp=drive_link 

#I was instructed to work off of this for now. this will change soon! 
caged_v1 <- read.csv(file.path("data", "06_caged_with-metadata_finest-scales.csv"))

#Some code here to create a df that doesnt have the DIFF measurements doubled----


# ----explore, tidy and wrangle data----
#lets see what we are dealing with
glimpse(caged_v1)

#check to make sure that we have numbers where we are supposed to have numbers. num_check doesnt count NAs tho if hey are there?
supportR::num_check(data = caged_v1, col = "exp.name.spatialextent.category")

supportR::num_check(data = caged_v1, col = "year") #UGH why no real years 
supportR::count(vec = caged_v1$year) 
supportR::count(vec = caged_v1$sampling.year) 

supportR::num_check(data = caged_v1, col = "year.start.exclosure") 

supportR::num_check(data = caged_v1, col = "betadisp.comm.dist")
supportR::count(vec = caged_v1$betadisp.comm.dist) 

sort(unique(alldata_v1$betadisp.design.level))

supportR::count(vec = caged_v1$cage.treatment_std) 
supportR::count(vec = caged_v1$consumer.richness) #so many NAs in consumer richness, this is why we will use the categorical for now but this needs to be fixed
supportR::count(vec = caged_v1$cage.treatment_std) #deal with this via a filtering
supportR::count(vec = caged_v1$betadisp.sample.size) 
supportR::count(vec = caged_v1$exp.name.spatialextent.category) 
supportR::count(vec = caged_v1$ecotype1) 
supportR::count(vec = caged_v1$lat) #303 unentered
supportR::count(vec = caged_v1$excluded.group)


#supportR::count_diff(vec1 = alldata_v1$betadisp.median , 
#                     vec2=alldata_v1$betadisp.comm.dist)

#Data Notes that will affect things downstream----
#I (marc) am going to ignore the variables that are missing shit for now. Some prepared code is below to wrangle/reformat if and when we get there. I'll note if I force some of these mis-entered or incomplete data into NAs. E.G., I REALLY want exp age but there are 2K "year" or "2017-2019"

#get years into number form.
#caged_v1$year.start.exclosure <- year(as.Date(as.character(caged_v1$year.start.exclosure), format = "%Y"))
#caged_v1$year.end.exclosure <- year(as.Date(as.character(caged_v1$year.end.exclosure), format = "%Y"))

#richness too
#caged_v1$consumer.richness <- as.numeric(caged_v1$consumer.richness) #some stupid shit like ">10" in here, so this gives NA

#latitude into numbers. 303 blanks here
caged_v1$lat <- as.numeric(caged_v1$lat)

#Build modeling ready DF----
glimpse(caged_v1)

cagedmodel.df = caged_v1 |> 
  #First, select the columns we think we need:
  select(
    #select study ID vars
    source, site, exp.name, 
    #select important experimental info (PLOT SIZE, EXP AREA GO HERE)
    cage.treatment_std, exclusion.duration, year.start.exclosure, year.end.exclosure, exp.name.spatialextent.category, natural.vs.artificial.substrate, betadisp.design.level, 
    #select important habitat info
    lat, climate.zone, aq.or.terr, ecotype1,
    #select consumer info
    excluded.group, consumer.richness, consumer.richness.category, consumer.native.domestic, consumer.trophic.level,
    #select response info (GAMMA GOES HERE)
    #measured.group, resource.type, 
    #select beta RV and beta info
    betadisp.sample.size, betadisp.comm.dist) |> 
  #Grab the treatments
  filter(cage.treatment_std %in% c('caged', 'uncaged')) #%>% 
  #Do some calculations (SKIPPING BC OF MISSING METADATA)
  #mutate(exp.age = year.end.exclosure - year.start.exclosure) 

#data QC to make sure its ready to model
glimpse(cagedmodel.df)

#supportR::num_check(data = cagedmodel.df, col = "consumer.trophic.level") 
supportR::count(vec = cagedmodel.df$consumer.trophic.level) 

#ok lets fuckin do this ----
BaeDisp.df = cagedmodel.df %>% 
  #First, select the columns we think we need:
  select(
    source, exp.name, cage.treatment_std, exp.name.spatialextent.category, betadisp.design.level, 
    lat, climate.zone, aq.or.terr, ecotype1, excluded.group, consumer.richness.category, consumer.native.domestic, consumer.trophic.level,
    betadisp.sample.size, betadisp.comm.dist)

#supportR::count(vec = marc.modeldata_v1$exp.age) 

# Export locally
write.csv(x = BaeDisp.df, row.names = F, na = '',
          file = file.path("data", "BaeDisp.df.csv"))


#Blue Skies model structure----
#I. B community distance across all experiments, ideal model here. One day we will have this

# betadisp.comm.dist ~ cage.treatment_std + ecotype1 + latitude + consumer richness + experiment age + gamma diversity + excl size size + successional stage + max consumer size + B sample size + B design level 
#REs: (1|expname) or (1| source/expname)

BaeDisp.df <- read.csv(file.path("data", "BaeDisp.df.csv"))

glimpse(BaeDisp.df) #10,881 rows
hist(BaeDisp.df$betadisp.comm.dist)
range(BaeDisp.df$betadisp.comm.dist)

#B comm dist ME model----
#Leave best fitting/favorite model up here:
BaeDisp.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1 + 
                       consumer.richness.category + #lat + exp.age + 
                       betadisp.sample.size + 
                         (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp.lmer)
check_collinearity(BaeDisp.lmer)
summary(BaeDisp.lmer)
car::Anova(BaeDisp.lmer, test.statistic = "F")
performance::r2(BaeDisp.lmer)

#simple, no interactions
BaeDisp.lmer_simp <- lmer(betadisp.comm.dist ~ cage.treatment_std + ecotype1 + consumer.richness.category + #lat + exp.age + 
                       betadisp.sample.size + 
                       (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp.lmer_simp) #why tf this not working?
check_model(BaeDisp.lmer_simp) |> plot() #plot them all 
check_collinearity(BaeDisp.lmer_simp)
summary(BaeDisp.lmer_simp)
car::Anova(BaeDisp.lmer_simp, test.statistic = "F")
performance::r2(BaeDisp.lmer_simp)

BaetaDispEco3Int.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1*consumer.richness + 
                                betadisp.sample.size + betadisp.design.level + lat + exp.age +
                                (1|source), 
                              data = marc.modeldata_v1)

car::Anova(BaetaDispEco3Int.lmer, test.statistic = "F")
check_model(BaetaDispEco3Int.lmer)
summary(BaetaDispEco3Int.lmer)

# Next steps: dredge() AIC selection
# Maybe also random effects AIC selection? on full model

#Effect Size model----

BaetaES.lmer <- lmer(diff ~ ecotype1 + consumer.richness.category + lat + exp.age + betadisp.sample.size + betadisp.design.level +
                       (1|exp.name), 
                     data = marc.modeldata_ES)


check_model(BaetaES.lmer)
check_collinearity(BaetaES.lmer)
summary(BaetaES.lmer)
car::Anova(BaetaES.lmer, test.statistic = "F")
performance::r2(BaetaES.lmer)
