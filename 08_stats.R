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
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Data Download and QC ---- 
## ------------------------------------------- ##

caged_v1 <- read.csv(file.path("data", "07_caged_w.meta_finest-scales.csv"))  %>% 
  #rename these columns that got var. not var_
  rename(var_taxonomic.level = var.taxonomic.level, 
         var_exclosure.area.m2 = var.exclosure.area.m2)

# Check structure
dplyr::glimpse(caged_v1)

# Create B Diff Dataset ----
  #DF where the unit of replication is averages within treatment. Variable is already created, so just need to select and filter
avg.caged_v1 <- caged_v1 %>% 
  dplyr::select(source:exp.name, starts_with("var"), 
                lat:long, cage.treatment_std, 
                within.cage.treat_betadisp.mean, betadisp.sample.size,
                excluded.group, consumer.trophic.level, gamma.richness,
                within.cage.treat_betadisp.mean.diff) %>% 
 # rename_all(~stringr::str_replace(.,"^var_","")) %>% #this line is the result of a fight between Marc and Jamie. 
  dplyr::filter(!is.na(within.cage.treat_betadisp.mean.diff)) %>% 
  dplyr::distinct(exp.name, .keep_all = T) %>% #ALERT!!! This fixed a duplication error within exp.name. If this gets fixed upstream, can delete this line
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = within.cage.treat_betadisp.mean) 
  # Export locally if you want
#write.csv(x = avg.caged_v1, row.names = F, na = '', file = file.path("data","avg.caged_v1.csv"))

# Check structure of that
dplyr::glimpse(avg.cage_v1)


  # ----explore and QC data----
#NOTE: A ton of this can be dumped into antoher file once we have a complete dataframe we like. 
#Skip to line 100 "Tidy and Wrangle the DF" if you dont want to look at the structure

#Data QC Comments and Notes.. update with issues you notice
#5/8: I (marc) am going to ignore the variables that are missing shit for now. I'll note if I force some of these mis-entered or incomplete data into NAs. E.G., I REALLY want exp age but there are 2K "year" or "2017-2019"

#lets see what we are dealing with
glimpse(caged_v1)

#check to make sure that we have numbers where we are supposed to have numbers, NAs, etc:

supportR::count(vec = caged_v1$exp.name.spatialextent.category) #6665 blanks
#Years
supportR::num_check(data = caged_v1, col = "year") #1139="2017-2019", 1191="year"
#View(caged_v1 %>% filter(year == "year") %>% select(exp.name, var_ecotype1) %>% unique() %>% as_tibble()) #these are the problem ones
supportR::count(vec = caged_v1$year) 
supportR::count(vec = caged_v1$sampling.year) #ranges in here too
supportR::count(vec = caged_v1$year.start.exclosure) #340 blank, a couple ranges
supportR::count(vec = caged_v1$year.end.exclosure)
supportR::num_check(data = caged_v1, col = "year.start.exclosure") 
supportR::num_check(data = caged_v1, col = "year.end.exclosure") 

#Response Variables
supportR::num_check(data = caged_v1, col = "betadisp.comm.dist")
supportR::num_check(data = caged_v1, col = "within.cage.treat_betadisp.mean.diff")
supportR::count(vec = avg.caged_v1$betadisp.comm.dist) 

#Independent Variables
sort(unique(caged_v1$betadisp.design.level))
supportR::count(vec = caged_v1$cage.treatment_std) #deal with this via a filtering
supportR::count(vec = caged_v1$betadisp.sample.size) 
supportR::count(vec = caged_v1$exp.name.spatialextent.category) 
supportR::count(vec = caged_v1$var_ecotype1) 
supportR::count(vec = caged_v1$var_resource.type.category) #this column dont exist yet
supportR::count(vec = caged_v1$lat) #398 NAs
#explore consumers
supportR::count(vec = caged_v1$var_consumer.richness.number) #not entered. use var_consumer.richness.category instead
supportR::count(vec = caged_v1$excluded.group)



#Tidy and Wrangle a modeling ready DF----

  #latitude into numbers
caged_v1$lat <- as.numeric(caged_v1$lat)

  #create a big but not huge df for modeling
cagedmodel.df = caged_v1 |> 
  #First, select the columns we think we need:
  select(
    #select study ID vars
    source, site, exp.name, 
    #below is another line of code that is the result of Marc losing a fight with Jamie
    starts_with("var"), 
    #select important experimental info (PLOT SIZE, EXP AREA GO HERE)
    cage.treatment_std, year.start.exclosure, year.end.exclosure, exp.name.spatialextent.category, natural.vs.artificial.substrate, betadisp.design.level, # exclusion.duration, 
    #select important habitat info that doesnt have "var_" in front
    lat, 
    #select consumer info
   # excluded.group, consumer.trophic.level, consumer.richness.category, # consumer.native.domestic, 
    #select response info (GAMMA GOES HERE)
    measured.group, gamma.richness, #resource.type, 
    #select beta RV and beta info
    betadisp.sample.size, betadisp.comm.dist) |> 
  #Grab the treatments
  filter(cage.treatment_std %in% c('caged', 'uncaged')) #%>% 
#Do some calculations (SKIPPING BC OF MISSING METADATA)
  #mutate(exp.age = year.end.exclosure - year.start.exclosure) 

#data QC to make sure its ready to model
glimpse(cagedmodel.df)


#DF for modeling, B disp ----
BaeDisp.df = cagedmodel.df %>% 
  #only columns we need and have
  select(
    source, exp.name, cage.treatment_std, exp.name.spatialextent.category, betadisp.design.level, lat, starts_with("var"), gamma.richness, betadisp.sample.size, betadisp.comm.dist)

#DF for modeling, B diff ES ----
BaeDiff.df = avg.caged_v1 %>% 
  #only columns we need and have
  select(source,exp.name, lat, starts_with("var"), betadisp.sample.size, gamma.richness, within.cage.treat_betadisp.mean.diff)


#supportR::count(vec = marc.modeldata_v1$exp.age) 

# Export locally
write.csv(x = BaeDisp.df, row.names = F, na = '',
          file = file.path("data", "BaeDisp.df.csv"))

write.csv(x = BaeDiff.df, row.names = F, na = '',
          file = file.path("data", "BaeDiff.df.csv"))


## ------------------------------------------- ##
# Modeling ----
## ------------------------------------------- ##

#RVs
  #1. Beta dispersion, per plot, for each experiment (right?? or per-site?) 
    #betadisp.comm.dist = from BaeDisp.df and caged_v1, average community distance 
  #2. Beta dispersion Effect Size (Uncaged - Caged)
   #within.cage.treat_betadisp.mean.diff = from BaeDisp.df and caged_v1, mean beta difference bt uncaged and caged. negative = consumers decrease B dispersion, positive = consumers increase B dispersion
  #3. Beta dispersion ES absolute value

#IVs
  #cage.treatment_std + ecotype1 + latitude + consumer richness + experiment age + gamma diversity + excl size size + successional stage + max consumer size + B sample size + B design level 
  #2way Int IVs: cage.treatment_std*var_ecotype1 OR cage.treatment_std*lat
  #3way Int IVs: cage.treatment_std*var_ecotype1*lat
# betadisp.comm.dist ~ cage.treatment_std + ecotype1 + latitude + consumer richness + experiment age + gamma diversity + excl size size + successional stage + max consumer size + B sample size + B design level 

#REs
  #(1|expname) or (1| source/expname)

#1. Beta dispersion (raw) models ----
  #Leave name of best fitting/most ecological sense model up here for reference (will delete when final models selected)
  #BaeDisp.lmer

#BaeDisp.df <- read.csv(file.path("data", "BaeDisp.df.csv"))

glimpse(BaeDisp.df) #10,881 rows
hist(BaeDisp.df$betadisp.comm.dist)
range(BaeDisp.df$betadisp.comm.dist)

#start with simple LM
BaeDisp.lm <- lm(betadisp.comm.dist ~ cage.treatment_std*var_ecotype1 + 
                   var_consumer.richness.category + gamma.richness + abs(lat) + #exp.age + 
                   betadisp.sample.size , data = BaeDisp.df)

check_model(BaeDisp.lm, panel = F) %>% plot()
summary(BaeDisp.lm)
car::Anova(BaeDisp.lm)
performance::r2(BaeDisp.lm)

baecont = emmeans(BaeDisp.lm, specs = ~ cage.treatment_std*var_ecotype1)
#baecont$contrasts

#B comm dist ME model----
#Currently favorite model, interaction bt cage treat and ecosystem
BaeDisp.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_aq.or.terr  + 
                       var_consumer.richness.category + 
                       gamma.richness + abs(lat) + 
                       #exp.age + 
                       betadisp.sample.size + 
                       (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp.lmer)
summary(BaeDisp.lmer)
car::Anova(BaeDisp.lmer, test.statistic = "F") 
performance::r2(BaeDisp.lmer)

emmip(BaeDisp.lmer, ~ cage.treatment_std |var_ecotype1)
emmip(BaeDisp.lmer, ~ var_consumer.richness.category)

plot_model(BaeDisp.lmer)

#Different iterations of the above planned model: 
#look at the sig interaction w ecotype (in contrast to no interaction with aq or terr)
BaeDisp_ecotype.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_ecotype1  + 
                       var_consumer.richness.category + 
                       gamma.richness + abs(lat) + 
                       #exp.age + 
                       betadisp.sample.size + 
                       (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp_ecotype.lmer)
summary(BaeDisp_ecotype.lmer)
car::Anova(BaeDisp_ecotype.lmer, test.statistic = "F") 
performance::r2(BaeDisp_ecotype.lmer)

emmip(BaeDisp_ecotype.lmer, ~ cage.treatment_std |var_ecotype1)

plot_model(BaeDisp.lmer)

#feedback from the crew
BaeDisp2Way.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_ecotype1 + 
                       cage.treatment_std:var_consumer.richness.category + 
                       gamma.richness + 
                       abs(lat) + 
                       #exp.age + 
                       betadisp.sample.size + 
                       (1|exp.name), data = BaeDisp.df)

check_model(BaeDisp2Way.lmer, panel = F) %>% plot()
check_collinearity(BaeDisp2Way.lmer)
summary(BaeDisp2Way.lmer)
car::Anova(BaeDisp2Way.lmer, test.statistic = "F")
performance::r2(BaeDisp2Way.lmer)

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
BaeDisp3way.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_ecotype1*var_consumer.richness.category + gamma.richness + #lat + exp.age + 
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

AIC(BaeDisp.lmer, BaeDispsimp.lmer, BaeDisp3way.lmer)

#BDisp Nested ME----
#simple, no interactions
BaeDispsimp_nest.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std + var_ecotype1 + 
                                var_consumer.richness.category + gamma.richness + #lat + exp.age + 
                                betadisp.sample.size + 
                                (1|source/exp.name), data = BaeDisp.df)

check_model(BaeDispsimp_nest.lmer) #why tf this not working?
check_model(BaeDispsimp_nest.lmer, panel = F) |> plot() #plot them all 
summary(BaeDispsimp_nest.lmer)
car::Anova(BaeDispsimp_nest.lmer, test.statistic = "F")
performance::r2(BaeDispsimp_nest.lmer)

#2 way int
BaeDisp_nest.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_ecotype1 + 
                            var_consumer.richness.category + gamma.richness + #lat + exp.age + 
                            betadisp.sample.size + 
                            (1|source/exp.name), data = BaeDisp.df)

check_model(BaeDisp_nest.lmer, panel = F) %>% plot()
summary(BaeDisp_nest.lmer)
car::Anova(BaeDisp_nest.lmer, test.statistic = "F")
performance::r2(BaeDisp_nest.lmer) #conditional .420, marg = .169

#3way cage eco richness interaction
#"rank deficient" 
BaeDisp3way_nest.lmer <- lmer(betadisp.comm.dist ~ cage.treatment_std*var_ecotype1*var_consumer.richness.category + gamma.richness + #lat + exp.age + 
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
glimpse(BaeDiff.df)

BaeES.lmer <- lmer(within.cage.treat_betadisp.mean.diff ~ 
                     var_ecotype1 + var_consumer.richness.category + 
                     abs(lat) + gamma.richness + 
                     betadisp.sample.size + (1|source), 
                   data = BaeDiff.df %>% filter(var_consumer.richness.category %in% c("mono", "low", "high")) )

check_model(BaeES.lmer, panel = F) %>% plot()
check_collinearity(BaeES.lmer)
summary(BaeES.lmer)
car::Anova(BaeES.lmer, test.statistic = "F")
performance::r2(BaeES.lmer)

#ES: Lat and Habitat Type Model----
#New Analysis of interest based on final day discussion
BaeDiff.df_lat = BaeDiff.df %>% 
  mutate(ablat = abs(lat), abdiff = abs(within.cage.treat_betadisp.mean.diff) )

#simple model to assess consumer diversity shit

LatHabIntES.lmer <- lmer(abdiff ~ 
                     ablat*var_aq.or.terr + 
                       (1|source), 
                   data = BaeDiff.df_lat)

LatHabES.lmer <- lmer(abdiff ~ 
                        ablat+var_aq.or.terr + 
                        (1|source), 
                      data = BaeDiff.df_lat)
AIC(LatHabIntES.lmer, LatHabES.lmer)

check_model(LatHabIntES.lmer, panel = F) %>% plot()
summary(LatHabIntES.lmer)
car::Anova(LatHabIntES.lmer, test.statistic = "F")
performance::r2(LatHabES.lmer)

emmip(LatHabIntES.lmer, var_aq.or.terr ~ ablat, cov.reduce = range)
emtrends(LatHabIntES.lmer, "var_aq.or.terr", var = "ablat")

# betadisp.comm.dist ~ cage.treatment_std + ecotype1 + latitude + consumer richness + experiment age + gamma diversity + excl size size + successional stage + max consumer size + B sample size + B design level 
#REs: (1|expname) or (1| source/expname)
