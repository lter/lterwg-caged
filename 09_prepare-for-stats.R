## --------------------------------------------------------------- ##
# CAGED Data Wrangling for Stats 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, Jamie McDevitt-Irwin

# Purpose:
## Create clean dataframes for beta dispersion and effect size (average - average)

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, njlyon0/supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ---- 
## ------------------------------------------- ##

# Read in relevant data file
betadisp_v1 <- read.csv(file.path("data", "08_caged_w.meta-beta-disp_finest-scales.csv"))
effectsizes_v1 <- read.csv(file.path("data", "08_caged_experiment-level-everything_fine-scales.csv"))


# Check structure
dplyr::glimpse(caged_v1)

# Check number of sources
unique(effectsizes_v1$source) # 117
unique(effectsizes_v1$exp.name) # 347

unique(betadisp_v1$source) # 117
unique(betadisp_v1$exp.name) # 347

# Check number of rows and dataframe
dim(effectsizes_v1) # 1286
dim(effectsizes_v1) # 1286 
# so the effect size needs to be set to only unique 


effectsizes_v2 <- effectsizes_v1 %>%
  select(source, exp.name, contains("lrr")) %>%
distinct()

dim(effectsizes_v2) # 347



## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##


## Effect size df 
write.csv(x = effectsizes_v2, row.names = F, na = '',
          file = file.path("data", "08_caged_prepped-effect-size.csv"))

# End ----









