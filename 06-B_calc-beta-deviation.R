## --------------------------------------------------------------- ##
# CAGED Beta Deviation Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate "beta deviation" (beta dispersion adjusted for sample size / species pool values)

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ----
## ------------------------------------------- ##

# Load beta dispersion data
dsp_v01 <- read.csv(file = file.path("data", "05-A_caged_beta-disp_all-scales.csv"))

# Check structure
dplyr::glimpse(dsp_v01)

# Also load gamma richness
gam_v01 <- read.csv(file = file.path("data", "05-B_caged_gamma-rich.csv"))

# Check structure
dplyr::glimpse(gam_v01)

# Also load pre-beta dispersion (for abundance values)
abn_v01 <- read.csv(file = file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(abn_v01)


# End ----
