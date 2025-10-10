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

# Check number of sources
unique(caged_v1$source) # 110
unique(caged_v1$exp.name) # 297

## ------------------------------------------- ##
# Create Beta Dispersion and Difference Dataset ---- 
## ------------------------------------------- ##
# Create a Clean Effect Size Dataframe
# DF where the unit of replication is averages within treatment. 
# Variable is already created in script 06, so just need to select and filter
avg.caged_v1 <- caged_v1 %>% 
  # select the variables we want
  dplyr::select(source:exp.name, starts_with("var"), 
                lat:long, cage.treatment_std, 
                # this is the average beta dispersion for each caging treatment in an exp.name
                within.cage.treat_betadisp.mean,
                betadisp.sample.size,
                excluded.group, consumer.trophic.level, gamma.richness,
                # this is the average uncaged - average caged for each caging treatment in exp.name
                within.cage.treat_betadisp.mean.diff) %>% 
  # this drops any files that dont have a mean difference calculated 
  # this could be from uncertain - uncaged, etc. (see github issue 28)
  dplyr::filter(!is.na(within.cage.treat_betadisp.mean.diff)) %>% 
  # this is necessary because there are duplicates of the mean as its replicated for each sample
  dplyr::distinct(exp.name, .keep_all = T) %>% #ALERT!!! This fixed a duplication error within exp.name. If this gets fixed upstream, can delete this line
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = within.cage.treat_betadisp.mean) %>%
  # get rid of the caged/uncaged beta disp avg values since we are interested in mean diff only
  select(-caged, -uncaged)
# Export locally if you want
#write.csv(x = avg.caged_v1, row.names = F, na = '', file = file.path("data","avg.caged_v1.csv"))

# Check structure of that
dplyr::glimpse(avg.caged_v1)


# Check number of sources
unique(avg.caged_v1$source) # 108
unique(avg.caged_v1$exp.name) # 290

# Look at the NA mean diff data
test <- caged_v1 %>%
  filter(is.na(within.cage.treat_betadisp.mean.diff)) %>%
  select(source, exp.name, cage.treatment_std, cage.treatment_orig,
         betadisp.comm.dist:within.cage.treat_betadisp.mean.diff)

unique(test$cage.treatment_std) # all four types are there
#View(test)



#convert latitude into numbers
caged_v1$lat <- as.numeric(caged_v1$lat)
avg.caged_v1$lat <- as.numeric(avg.caged_v1$lat)



# Crete a Clean Beta Dispersion Dataframe
#trim down some columns to create a nicer DF for modeling
cagedmodel.df <- caged_v1 |> 
  #First, select the columns we think we need:
  select(
    #select study ID vars
    source, site, exp.name, starts_with("var"), 
    #select important experimental info (PLOT SIZE, EXP AREA GO HERE)
    cage.treatment_std, year.start.exclosure, 
    year.end.exclosure, exp.name.spatialextent.category, 
    natural.vs.artificial.substrate, 
    betadisp.design.level, # exclusion.duration, 
    #select important habitat info that doesnt have "var_" in front
    lat, 
    #select consumer info
    #select response info (GAMMA GOES HERE)
    measured.group, gamma.richness, #resource.type, 
    #select beta RV and beta info
    betadisp.sample.size, betadisp.comm.dist) |> 
  #Grab the treatments
  filter(cage.treatment_std %in% c('caged', 'uncaged')) 


# Check number of sources
unique(cagedmodel.df$source) # 110
unique(cagedmodel.df$exp.name) # 297

#data QC to make sure its ready to model
glimpse(cagedmodel.df)

# DF for modeling Beta dispersion ----
betadispersion_df <- cagedmodel.df %>% 
  #only columns we need and have
  select(source, exp.name, cage.treatment_std, 
         exp.name.spatialextent.category, betadisp.design.level, 
         lat, starts_with("var"), gamma.richness,
         betadisp.sample.size, betadisp.comm.dist) %>%
  # filter out sources that dont have both caged and uncaged
  dplyr::group_by(source, exp.name) %>%
  dplyr::mutate(has_both = all(c("caged", "uncaged") %in% cage.treatment_std)) %>%
                  dplyr::ungroup() %>%
                  dplyr::filter(has_both == TRUE) 
# this ended up keeping all of them? 

caged_df <- dplyr::filter(cagedmodel.df, cage.treatment_std == "caged")
uncaged_df <- dplyr::filter(cagedmodel.df, cage.treatment_std == "uncaged")

both_df <- dplyr::filter(cagedmodel.df, exp.name %in% unique(caged_df$exp.name) & exp.name %in% unique(uncage_df$exp.name)) 

dropped_df <- dplyr::filter(cagedmodel.df, exp.name %in% unique(both_df$exp.name) != TRUE)
# still says zero rows


# Check number of sources
unique(betadispersion_df$source) # 110
unique(betadispersion_df$exp.name) # 297


#DF for modeling, Beta Difference Effect Size ----
effectsize_df <- avg.caged_v1 %>% 
  #only columns we need and have
  select(source,exp.name, lat, starts_with("var"), 
         betadisp.sample.size, gamma.richness,
         within.cage.treat_betadisp.mean.diff)

# Check number of sources
unique(effectsize_df$source) # 108
unique(effectsize_df$exp.name) # 290


#supportR::count(vec = marc.modeldata_v1$exp.age) 

# Export locally
write.csv(x = betadispersion_df, row.names = F, na = '',
          file = file.path("data", "betadispersion_df.csv"))

write.csv(x = effectsize_df, row.names = F, na = '',
          file = file.path("data", "effectsize_df.csv"))

