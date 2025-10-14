## --------------------------------------------------------------- ##
# CAGED Data Wrangling for Stats 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, Jamie McDevitt-Irwin

# Purpose:
## Create a clean effect size dataframe
## Where the unit of replication is averages within treatment
## Upstream scripts already create and QA/QC the needed columns,
### so this script mostly just needs to select and filter those

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, ltertools, lme4, lmerTest, 
                 performance, easystats, lubridate, car, 
                 njlyon0/supportR, MuMIn, visreg, 
                 emmeans, tidymodels, qqplotr, sjPlot
                 )
                 # , update_all = TRUE)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ---- 
## ------------------------------------------- ##

# Read in relevant data file
caged_v1 <- read.csv(file.path("data", "07_caged_w.meta_finest-scales.csv"))

# Check structure
dplyr::glimpse(caged_v1)

# Check number of sources
unique(caged_v1$source) # 110
unique(caged_v1$exp.name) # 297

## ------------------------------------------- ##
# Create Beta Dispersion & Difference Data ---- 
## ------------------------------------------- ##

# Do needed wrangling
caged_mean <- caged_v1 %>% 
  # Select the variables we want
  dplyr::select(source:exp.name, starts_with("var"), 
                lat:long, cage.treatment_std, 
                # This (v) is the average beta dispersion for each caging treatment
                within.cage.treat_betadisp.mean, betadisp.sample.size,
                excluded.group, consumer.trophic.level, gamma.richness,
                # This is the uncaged minus caged betadisp (both averaged within exp.name)
                within.cage.treat_betadisp.mean.diff) %>% 
  # Drop columns where there beta dispersion differences weren't calculated
  dplyr::filter(!is.na(within.cage.treat_betadisp.mean.diff)) %>% 
  # Drop any duplicate rows
  dplyr::distinct() %>% 
  # Pivot treatments to wide (makes needed duplicates of other columns)
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = within.cage.treat_betadisp.mean) %>%
  # Remove the caging treatment-specific columns (we only care about the _difference_)
  dplyr::select(-caged, -uncaged)

# What columns are lost/gained?
supportR::diff_check(old = names(caged_v1), new = names(caged_mean))

# Check structure
dplyr::glimpse(caged_mean)

# Check replicates
unique(caged_mean$source) # 108
unique(caged_mean$exp.name) # 290

# Which are lost (if any)
supportR::diff_check(old = unique(caged_v1$source), new = unique(caged_mean$source))
supportR::diff_check(old = unique(caged_v1$exp.name), new = unique(caged_mean$exp.name))

# Export locally (if you want)
# write.csv(x = caged_mean, row.names = F, na = '',
#           file = file.path("data","08a_caged_mean-difference-slim-data.csv"))

# Check structure of that
dplyr::glimpse(caged_mean)

# Check which data are missing mean differences
na.diff <- caged_v1 %>% 
  dplyr::filter(is.na(within.cage.treat_betadisp.mean.diff)) %>%
  dplyr::select(source, exp.name, cage.treatment_std, cage.treatment_orig,
                betadisp.comm.dist:within.cage.treat_betadisp.mean.diff) %>% 
  dplyr::distinct()

# What's in that?
unique(na.diff$source)
unique(na.diff$cage.treatment_std) # all four types are there

# Glimpse it
dplyr::glimpse(na.diff)
# View(na.diff)

## ------------------------------------------- ##
# Effect Size Prep ----
## ------------------------------------------- ##

# Data for modeling beta dispersion effect size
caged_effectsize <- caged_mean %>% 
  # Only columns we need and have
  dplyr::select(source, exp.name, lat, starts_with("var"), 
                betadisp.sample.size, gamma.richness,
                within.cage.treat_betadisp.mean.diff)

# Check number of sources
unique(caged_effectsize$source) # 108
unique(caged_effectsize$exp.name) # 290

# Which are lost (if any)
supportR::diff_check(old = unique(caged_v1$source), new = unique(caged_effectsize$source))
supportR::diff_check(old = unique(caged_v1$exp.name), new = unique(caged_effectsize$exp.name))

# Check structure
dplyr::glimpse(caged_effectsize)

## ------------------------------------------- ##
# Streamline Data for Statistics ----
## ------------------------------------------- ##

# Trim down some columns to create a nicer DF for modeling
caged_model <- caged_v1 %>% 
  # Select columns we (think that we'll) need
  dplyr::select(
    ## Experiment identity info
    source, site, exp.name, 
    ## Critical metadata (see relevant GoogleSheet)
    starts_with("var"), cage.treatment_std, year.start.exclosure, 
    year.end.exclosure, exp.name.spatialextent.category, 
    natural.vs.artificial.substrate, lat, 
    ## Information about beta dispersion calculation
    betadisp.design.level,
    ## Response information
    measured.group, gamma.richness, betadisp.sample.size, betadisp.comm.dist
    ) %>% 
  # Keep only the two treatments that we are interested in
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) 

# Check structure
dplyr::glimpse(caged_model)

# Check replicates
unique(caged_model$source) # 110
unique(caged_model$exp.name) # 297

# Which are lost (if any)
supportR::diff_check(old = unique(caged_v1$source), new = unique(caged_model$source))
supportR::diff_check(old = unique(caged_v1$exp.name), new = unique(caged_model$exp.name))

## ------------------------------------------- ##
# Beta Dispersion Modeling ----
## ------------------------------------------- ##

# Data for modeling beta dispersion
caged_beta <- caged_model %>% 
  # Drop unwanted columns
  dplyr::select(-measured.group, -natural.vs.artificial.substrate,
                -site, -year.end.exclosure, -year.start.exclosure)

# Check difference in columns
supportR::diff_check(old = names(caged_model), new = names(caged_beta))

# Check number of sources
unique(caged_beta$source) # 110
unique(caged_beta$exp.name) # 297

# Check structure
dplyr::glimpse(caged_beta)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Export some of these products locally
## Prepped for beta dispersion modeling
write.csv(x = caged_beta, row.names = F, na = '',
          file = file.path("data", "08a_caged_beta-dispersion-prep.csv"))

## Prepped for effect size modeling
write.csv(x = caged_effectsize, row.names = F, na = '',
          file = file.path("data", "08a_caged_effect-size-prep.csv"))

# End ----

