## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Dominance Calculation
## --------------------------------------------------------------- ##
# Purpose
## Calculate mean differences and log response ratios (LRR) for dominance

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Load the relevant output
dom.diff_v01 <- read.csv(file.path("data", "05-D_caged_dominance_all-scales.csv"))

# Check structure
dplyr::glimpse(dom.diff_v01)

## ------------------------------------------- ##
# Streamline / Prepare Data ----
## ------------------------------------------- ##

# Filter to only desired treatments and get data in right shape
dom.diff_v02 <- dom.diff_v01 %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  dplyr::select(-dplyr::ends_with(".abundance")) %>% 
  dplyr::mutate(cage.treatment_std = paste0("dominance_", cage.treatment_std)) %>% 
  tidyr::pivot_wider(names_from = cage.treatment_std,
    values_from = dominance)

# Check structure
dplyr::glimpse(dom.diff_v02)

## ------------------------------------------- ##
# Calculate Difference & LRR ----
## ------------------------------------------- ##

# Calculate difference and LRR
dom.diff_v03 <- dom.diff_v02 %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("caged"),
    .fns = ~ ifelse(is.na(.), yes = 0, no = .))) %>% 
  dplyr::mutate(caged = dominance_caged + 0.005,
    uncaged = dominance_uncaged + 0.005) %>% 
  dplyr::mutate(within.cage.treat_dominance.diff = uncaged - caged,
    within.cage.treat_dominance.lrr = log2(uncaged / caged)) %>% 
  dplyr::select(-caged, -uncaged)

# Check structure
dplyr::glimpse(dom.diff_v03)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
dom.diff_v99 <- dom.diff_v03

# Count number of sources/experiments at end
unique(dom.diff_v99$source) # 121
unique(dom.diff_v99$exp.name) # 367

# Identify tidy file name / path
dom.diff_name <- "06-D_caged_dominance_all-scales.csv"
dom.diff_path <- file.path("data", dom.diff_name)

# Export locally
write.csv(x = dom.diff_v99, row.names = F, na = '', file = dom.diff_path)

# End ----
