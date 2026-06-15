## --------------------------------------------------------------- ##
# CAGED Data Wrangling for Stats 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, Jamie McDevitt-Irwin

# Purpose:
## Create clean dataframes for beta dispersion and effect size (average - average)

# NOTE: this script seems unnecessary now

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
betadisp_v1 <- read.csv(file.path("data", "08-A_caged_w.meta-beta-disp_fine-scales.csv"))
effectsizes_v1 <- read.csv(file.path("data", "08-B_caged_expname-effect-size_fine-scales.csv"))

# Check structure
dplyr::glimpse(betadisp_v1)
dplyr::glimpse(effectsizes_v1)


# Check number of sources
unique(effectsizes_v1$source) # 117
unique(effectsizes_v1$exp.name) # 347


unique(betadisp_v1$source) # 117
unique(betadisp_v1$exp.name) # 347
dim(betadisp_v1) # 13964    44


# Check number of rows and dataframe
dim(effectsizes_v1) # 620 52
# now there are 620 rows because each intermediate level also has an effect size calculated 



dim(betadisp_v1) # 13964    42
dim(betadisp_v1) # 13964    42

colnames(effectsizes_v1)


## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##


## Effect size df 
write.csv(x = effectsizes_v2, row.names = F, na = '',
          file = file.path("data", "08_caged_prepped-effect-size.csv"))

# End ----









