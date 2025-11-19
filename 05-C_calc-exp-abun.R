## --------------------------------------------------------------- ##
# CAGED Experiment-Level Abundance Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate abundance across taxa at all experimental design levels
## Possibly useful for later steps so seems like it'd be useful to handle in a standalone script

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
abun_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(abun_v1)

# End ----
