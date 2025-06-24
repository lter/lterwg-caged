## --------------------------------------------------------------- ##
# CAGED Data Wrangling for Stats 
# this makes the dfs for models and raw data figures
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, Jamie McDevitt-Irwin

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

caged_v1 <- read.csv(file.path("data", 
                               "07_caged_w.meta_finest-scales.csv"))  %>% 
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
dplyr::glimpse(avg.caged_v1)


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
avg.caged_v1$lat <- as.numeric(avg.caged_v1$lat)

#create a big but not huge df for modeling
cagedmodel.df <- caged_v1 |> 
  #First, select the columns we think we need:
  select(
    #select study ID vars
    source, site, exp.name, 
    #below is another line of code that is the result of Marc losing a fight with Jamie
    starts_with("var"), 
    #select important experimental info (PLOT SIZE, EXP AREA GO HERE)
    cage.treatment_std, year.start.exclosure, 
    year.end.exclosure, exp.name.spatialextent.category, 
    natural.vs.artificial.substrate, 
    betadisp.design.level, # exclusion.duration, 
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
BaeDisp.df <- cagedmodel.df %>% 
  #only columns we need and have
  select(
    source, exp.name, cage.treatment_std, exp.name.spatialextent.category, betadisp.design.level, lat, starts_with("var"), gamma.richness, betadisp.sample.size, betadisp.comm.dist)

#DF for modeling, B diff ES ----
BaeDiff.df <- avg.caged_v1 %>% 
  #only columns we need and have
  select(source,exp.name, lat, starts_with("var"), betadisp.sample.size, gamma.richness, within.cage.treat_betadisp.mean.diff)


#supportR::count(vec = marc.modeldata_v1$exp.age) 

# Export locally
#WARNING: We may need to change this exporting plan? I think this threw Max off and gave him some errors 
write.csv(x = BaeDisp.df, row.names = F, na = '',
          file = file.path("data", "BaeDisp.df.csv"))

write.csv(x = BaeDiff.df, row.names = F, na = '',
          file = file.path("data", "BaeDiff.df.csv"))

