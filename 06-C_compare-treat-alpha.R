## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Alpha Diversity Calculation
## --------------------------------------------------------------- ##
# Purpose
## Calculate mean differences and log response ratios (LRR) for alpha diversity

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
alp.diff_v01 <- read.csv(file.path("data", "05-C_caged_alpha-div_all-scales.csv"))

# Check structure
dplyr::glimpse(alp.diff_v01)

## ------------------------------------------- ##
# Streamline / Prepare Data ----
## ------------------------------------------- ##

# Filter to only desired treatments and get data in right shape
alp.diff_v02 <- alp.diff_v01 %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  dplyr::mutate(cage.treatment_std = paste0("alpha.diversity_", cage.treatment_std)) %>% 
  tidyr::pivot_wider(names_from = cage.treatment_std,
    values_from = alpha.diversity_richness)

# Check structure
dplyr::glimpse(alp.diff_v02)

## ------------------------------------------- ##
# Calculate Difference & LRR ----
## ------------------------------------------- ##

# Calculate difference and LRR
alp.diff_v03 <- alp.diff_v02 %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("caged"),
    .fns = ~ ifelse(is.na(.), yes = 0, no = .))) %>% 
  # need to change this to the minimum average value of mean alpha diversity (replace 0.005)
  dplyr::mutate(caged = alpha.diversity_caged + 0.005,
    uncaged = alpha.diversity_uncaged + 0.005) %>% 
  dplyr::mutate(within.cage.treat_alpha.diff = uncaged - caged,
    within.cage.treat_alpha.lrr = log2(uncaged / caged)) %>% 
  dplyr::select(-caged, -uncaged)

# Check structure
dplyr::glimpse(alp.diff_v03)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
alp.diff_v99 <- alp.diff_v03

# Count number of sources/experiments at end
unique(alp.diff_v99$source) # 121
unique(alp.diff_v99$exp.name) # 367

# Identify tidy file name / path
alp.diff_name <- "06-C_caged_alpha-div-diff_all-scales.csv"
alp.diff_path <- file.path("data", alp.diff_name)

# Export locally
write.csv(x = alp.diff_v99, row.names = F, na = '', file = alp.diff_path)

# End ----
