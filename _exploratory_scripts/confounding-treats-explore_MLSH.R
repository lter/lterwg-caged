## --------------------------------------------------------------- ##
# CAGED Removal of Confounding Treatments
## --------------------------------------------------------------- ##
# Author(s): Nick J Lyon,

# Purpose
## Demo removal of confounding variables

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Clear environment
rm(list = ls()); gc()

# Read in data
## Need to download it manually if you don't have it
caged_v1 <- read.csv(file = file.path("data", "02_caged_tidied.csv"))

# Check structure
dplyr::glimpse(caged_v1)

## ------------------------------------------- ##
# Filtering Treatments ----
## ------------------------------------------- ##

# Check extant treatments
treat_check <- caged_v1 %>% 
  dplyr::select(dplyr::starts_with("treat.")) %>% 
  dplyr::distinct()
  
# Check structure
treat_check
## view(treat_check)

# Do desired filtering
caged_v2 <- caged_v1

# Re-check existing treatments
treat_check2 <- caged_v2 %>% 
  dplyr::select(dplyr::starts_with("treat.")) %>% 
  dplyr::distinct(); treat_check2
## view(treat_check)






# End ----
