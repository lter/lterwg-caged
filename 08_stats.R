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
caged.for.analyses <- read.csv(file.path("data", "06_caged_with-metadata_finest-scales.csv"))

# ----tidy and wrangle data----
#lets see what we are dealing with
glimpse(caged.for.analyses)

#check to make sure that we have numbers where we are supposed to have numbers. num_check doesnt count NAs tho if hey are there?
supportR::num_check(data = caged.for.analyses, col = "exp.name.spatialextent.category")
supportR::num_check(data = caged.for.analyses, col = "year") #UGH why no real years 

supportR::num_check(data = caged.for.analyses, col = "betadisp.comm.dist")
supportR::count(vec = caged.for.analyses$betadisp.comm.dist) 

sort(unique(alldata_v1$betadisp.design.level))

supportR::count(vec = caged.for.analyses$cage.treatment_std) 

supportR::count(vec = caged.for.analyses$consumer.richness) #so many NAs in consumer richness, this is why we will use the categorical
supportR::count(vec = caged.for.analyses$cage.treatment_std) #deal with this via a filtering
supportR::count(vec = caged.for.analyses$betadisp.sample.size) 
supportR::count(vec = caged.for.analyses$exp.name.spatialextent.category) 
supportR::count(vec = caged.for.analyses$ecotype1) 

#supportR::count_diff(vec1 = alldata_v1$betadisp.median , 
#                     vec2=alldata_v1$betadisp.comm.dist)

#get years and richness into number form
alldata_v1$year.start.exclosure <- year(as.Date(as.character(alldata_v1$year.start.exclosure), format = "%Y"))
alldata_v1$year.end.exclosure <- year(as.Date(as.character(alldata_v1$year.end.exclosure), format = "%Y"))
#alldata_v1$consumer.richness <- as.numeric(alldata_v1$consumer.richness) #some stupid shit like ">10" in here, so this gives NA
alldata_v1$lat <- as.numeric(alldata_v1$lat)

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