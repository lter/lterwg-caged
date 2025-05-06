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
librarian::shelf(tidyverse, supportR, update_all= TRUE)

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
  dplyr::select(dplyr::starts_with("treat."),source) %>% 
  dplyr::distinct()
  
# Check structure
treat_check
## view(treat_check)

# Do desired filtering
caged_v2 <- caged_v1 %>%
  # filter out interacting/confounding treatments we are not interested in
  # not interested in filtering out artificial
  #filter(!treat.artificial) %>%
  # not interested in filtering out exposure
  #filter(!treat.exposure) %>%
  filter(!treat.insecticide %in% c("Sprayed")) %>%
  filter(!treat.nitrogen.addition %in% c(50,16)) %>%
  # not interested in filtering out canopy
  #filter(!treat.canopy %in%) %>%
  # not interested in filtering out disturbance
  #filter(!treat.disturbance ) %>%
  # not interested in filtering out distance 
  #filter(!treat.distance) %>%
  # not interested in filtering out gap
 # filter(!treat.gap) %>%
  filter(!treat.nutrients %in% c(1,"Nutrient Pollution", "enriched",
                                 "NP", "N", "P",
                                 2,3,4,5,6,7,8,9))
  # not interested in filtering out treat.fire
  #filter(!treat.fire) %>%
  
  
  
  
  
  

# Re-check existing treatments
treat_check2 <- caged_v2 %>% 
  dplyr::select(dplyr::starts_with("treat.")) %>% 
  dplyr::distinct(); treat_check2
## view(treat_check2)






# End ----
