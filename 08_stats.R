## --------------------------------------------------------------- ##
# CAGED Stats and Analyses 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, 
                 performance, easystats, lubridate, car, 
                 njlyon0/supportR, MuMIn, visreg, 
                 emmeans, tidymodels, qqplotr, sjPlot) #, update_all= TRUE) 

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("results"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Load data
caged_v1 <- read.csv(file.path("data", "07_caged_w.meta_finest-scales.csv"))

# Check structure
dplyr::glimpse(caged_v1)

# Make a version where the unit of replication is averages within treatment
avg.cage_v1 <- caged_v1 %>% 
  dplyr::select(source:exp.name, lat:long, 
                cage.treatment_std, 
                within.cage.treat_betadisp.mean,
                within.cage.treat_betadisp.mean.diff) %>% 
  dplyr::filter(!is.na(within.cage.treat_betadisp.mean.diff)) %>% 
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = within.cage.treat_betadisp.mean)

# Check structure of that
dplyr::glimpse(avg.cage_v1)

## ------------------------------------------- ##
# Download Data ---- 
## ------------------------------------------- ##


#Dreate the Diff df of 234 observations----
#SHIT. nick told me what to do before they left but now i forgot and am panicking bc i need to have models ready for my friends!! NICK please create this 234 obs dataframe for us <3 
cagedDiff_v1 = caged_v1 %>% 
  select(within.cage.treat_betadisp.mean.diff) %>% 
  distinct()

cagedDiff.df = caged_v1 %>% 
  group_by(exp.name) %>% 
  #caged_v1
  
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

sort(unique(caged_v1$betadisp.design.level))

supportR::count(vec = caged_v1$cage.treatment_std) 
supportR::count(vec = caged_v1$consumer.richness) #so many NAs in consumer richness, this is why we will use the categorical for now but this needs to be fixed
supportR::count(vec = caged_v1$cage.treatment_std) #deal with this via a filtering
supportR::count(vec = caged_v1$betadisp.sample.size) 
supportR::count(vec = caged_v1$exp.name.spatialextent.category) 
supportR::count(vec = caged_v1$ecotype1) 
supportR::count(vec = caged_v1$lat) #303 unentered
supportR::count(vec = caged_v1$excluded.group)

supportR::num_check(data = caged_v1, col = "within.cage.treat_betadisp.mean.diff")

#supportR::count_diff(vec1 = alldata_v1$betadisp.median , 
#                     vec2=alldata_v1$betadisp.comm.dist)

#Data Notes that will affect things downstream----
#I (marc) am going to ignore the variables that are missing shit for now. I'll note if I force some of these mis-entered or incomplete data into NAs. E.G., I REALLY want exp age but there are 2K "year" or "2017-2019"

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
    measured.group, resource.type, gamma.richness, 
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

#DF for modeling ----
BaeDisp.df = cagedmodel.df %>% 
  #only columns we need and have
  select(
    source, exp.name, cage.treatment_std, exp.name.spatialextent.category, betadisp.design.level, 
    lat, climate.zone, aq.or.terr, ecotype1, excluded.group, consumer.richness.category, consumer.native.domestic, consumer.trophic.level, gamma.richness,
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

#B comm dist, LM----
#gotta start most simple! 
BaeDisp.lm <- lm(betadisp.comm.dist ~ cage.treatment_std*ecotype1 + 
                   consumer.richness.category + gamma.richness + abs(lat) + #exp.age + 
                   betadisp.sample.size , data = BaeDisp.df)

check_model(BaeDisp.lm, panel = F) %>% plot()
summary(BaeDisp.lm)
car::Anova(BaeDisp.lm)
performance::r2(BaeDisp.lm)

baecont = emmeans(BaeDisp.lm, specs = ~ cage.treatment_std*ecotype1)
baecont$contrasts

#B comm dist ME model----
#Leave best fitting/favorite model up here:
BaeDisp.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1 + 
                       consumer.richness.category + 
                       gamma.richness + 
                       abs(lat) + 
                       #exp.age + 
                       betadisp.sample.size + 
                       (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp.lmer, panel = F) %>% plot()
check_collinearity(BaeDisp.lmer)
summary(BaeDisp.lmer)
car::Anova(BaeDisp.lmer, test.statistic = "F")
performance::r2(BaeDisp.lmer)

emmip(BaeDisp.lmer, ~ cage.treatment_std |ecotype1)
emmip(BaeDisp.lmer, ~ consumer.richness.category)

plot_model(BaeDisp.lmer)


#simple, no interactions
BaeDispsimp.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std + ecotype1 + consumer.richness.category + gamma.richness + #lat + exp.age + 
                           betadisp.sample.size + 
                           (1|exp.name), data = BaeDisp.df)

check_model(BaeDispsimp.lmer) #why tf this not working?
check_model(BaeDispsimp.lmer, panel = F) |> plot() #plot them all 
summary(BaeDispsimp.lmer)
car::Anova(BaeDispsimp.lmer, test.statistic = "F")
performance::r2(BaeDispsimp.lmer)

#3way cage eco richness interaction
#"rank deficient" but bolker says that is ok. 
BaeDisp3way.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1*consumer.richness.category + gamma.richness + #lat + exp.age + 
                           betadisp.sample.size + 
                           (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp3way.lmer, panel = F) |> plot() #resid normality is wack
summary(BaeDisp3way.lmer)
car::Anova(BaeDisp3way.lmer, test.statistic = "F")
performance::r2(BaeDisp3way.lmer)

emmip(BaeDisp3way.lmer, ~ cage.treatment_std | consumer.richness.category)

#AIC on the 1|exp.name mods
#i dont know how to do dredge
#REexp.dd = dredge(BaeDisp3way.lmer, beta = "sd")
#plot(dd, labAsExpr = TRUE)

AICc(BaeDisp.lmer, BaeDispsimp.lmer, BaeDisp3way.lmer)

#BDisp Nested ME----
#simple, no interactions
BaeDispsimp_nest.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std + ecotype1 + 
                                consumer.richness.category + gamma.richness + #lat + exp.age + 
                                betadisp.sample.size + 
                                (1|source/exp.name), data = BaeDisp.df)

check_model(BaeDispsimp_nest.lmer) #why tf this not working?
check_model(BaeDispsimp_nest.lmer, panel = F) |> plot() #plot them all 
summary(BaeDispsimp_nest.lmer)
car::Anova(BaeDispsimp_nest.lmer, test.statistic = "F")
performance::r2(BaeDispsimp_nest.lmer)

#2 way int
BaeDisp_nest.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1 + 
                            consumer.richness.category + gamma.richness + #lat + exp.age + 
                            betadisp.sample.size + 
                            (1|source/exp.name), data = BaeDisp.df)

check_model(BaeDisp_nest.lmer, panel = F) %>% plot()
summary(BaeDisp_nest.lmer)
car::Anova(BaeDisp_nest.lmer, test.statistic = "F")
performance::r2(BaeDisp_nest.lmer) #conditional .420, marg = .169

#3way cage eco richness interaction
#"rank deficient" 
BaeDisp3way_nest.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1*consumer.richness.category + gamma.richness + #lat + exp.age + 
                                betadisp.sample.size + 
                                (1|source/exp.name), data = BaeDisp.df)

check_model(BaeDisp3way_nest.lmer, panel = F) |> plot() #plot them all 
summary(BaeDisp3way_nest.lmer)
car::Anova(BaeDisp3way_nest.lmer, test.statistic = "F")
performance::r2(BaeDisp3way_nest.lmer)


#Bdisp ME random slope----
#3way cage eco richness interaction
#Singular
#BaeDisp3way_slop.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1*consumer.richness.category + gamma.richness + #lat + exp.age + 
#                                betadisp.sample.size + 
#                                (cage.treatment_std|source/exp.name), data = BaeDisp.df)

#check_model(BaeDisp3way_slop.lmer, panel = F) |> plot() #plot them all 
#summary(BaeDisp3way_slop.lmer)
#car::Anova(BaeDisp3way_slop.lmer, test.statistic = "F") #takes 1 million minutes to run
#performance::r2(BaeDisp3way_slop.lmer)


BaeDisp3way_2ME.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*ecotype1*consumer.richness.category + gamma.richness + #lat + exp.age + 
                               betadisp.sample.size + 
                               (1|source) + (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp3way_2ME.lmer, panel = F) |> plot() #plot them all 
summary(BaeDisp3way_2ME.lmer)
car::Anova(BaeDisp3way_2ME.lmer, test.statistic = "F")
performance::r2(BaeDisp3way_2ME.lmer)


#Effect Size model----

BaetaES.lmer <- lmer(diff ~ ecotype1 + consumer.richness.category + lat + exp.age + betadisp.sample.size + betadisp.design.level +
                       (1|exp.name), 
                     data = marc.modeldata_ES)


check_model(BaetaES.lmer)
check_collinearity(BaetaES.lmer)
summary(BaetaES.lmer)
car::Anova(BaetaES.lmer, test.statistic = "F")
performance::r2(BaetaES.lmer)
