## --------------------------------------------------------------- ##
# CAGED Stats and Analyses 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, performance, lubridate, car, njlyon0/supportR, MuMIn, visreg, emmeans) #, update_all= TRUE)

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

#I was instructed to work off of this for today. this will change soon! 
caged_v1 <- read.csv(file.path("data", "06_caged_with-metadata_finest-scales.csv"))

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

#ok lets fuckin do this 
BaeDisp.df = cagedmodel.df %>% 
  #First, select the columns we think we need:
  select(
    #select study ID vars
    source, exp.name, cage.treatment_std, exp.name.spatialextent.category, betadisp.design.level, 
    lat, climate.zone, aq.or.terr, ecotype1,
    excluded.group, consumer.richness, consumer.richness.category, consumer.native.domestic, consumer.trophic.level,
    #select response info (GAMMA GOES HERE)
    #measured.group, resource.type, 
    #select beta RV and beta info
    betadisp.sample.size, betadisp.comm.dist) |> 
  #Grab the treatments
  filter(cage.treatment_std %in% c('caged', 'uncaged')) #%>% 
#Do some calculations (SKIPPING BC OF MISSING METADATA)
#mutate(exp.age = year.end.exclosure - year.start.exclosure) 
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