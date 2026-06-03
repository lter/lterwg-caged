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
# Filter to Experiment Name Level ----
## ------------------------------------------- ##

# Filter to only experiment name-level, desired treatments, and average across replicates
alp.diff_v02 <- alp.diff_v01 %>% 
  dplyr::select(-dplyr::starts_with("exp.design.")) %>% 
  # dplyr::filter(alpha.diversity_design.level == "exp.name") %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  dplyr::mutate(cage.treatment_std = paste0("alpha.diversity_", cage.treatment_std))

# Check structure
dplyr::glimpse(alp.diff_v02)

## ------------------------------------------- ##
# Streamline Data ----
## ------------------------------------------- ##

# Drop columns that are completely empty (i.e., "exp.design.#" columns)
alp.diff_v03 <- alp.diff_v02 %>% 
  dplyr::select(-dplyr::where(fn = ~ all(nchar(.) == 0 | is.na(.)))) %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(
    setdiff(x = names(.), y = c("alpha.diversity_richness"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = mean(alpha.diversity_richness, na.rm = TRUE),
    .groups = "drop")

# Check structure
dplyr::glimpse(alp.diff_v03)

## ------------------------------------------- ##
# Reshape & Calculate LRR ----
## ------------------------------------------- ##

# Reshape the data and calculate log response ratio
alp.diff_v04 <- alp.diff_v03 %>% 
  tidyr::pivot_wider(names_from = cage.treatment_std,
    values_from = alpha.diversity_richness) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("caged"),
    .fns = ~ ifelse(is.na(.), yes = 0, no = .))) %>% 
  dplyr::mutate(caged = alpha.diversity_caged + 1,
    uncaged = alpha.diversity_uncaged + 1) %>% 
  dplyr::mutate(alpha.diversity_cage.treat.diff = uncaged - caged,
    alpha.diversity_cage.treat.lrr = log2(uncaged / caged)) %>% 
  dplyr::select(-caged, -uncaged)

# Check structure
dplyr::glimpse(alp.diff_v04)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
alp.diff_v99 <- alp.diff_v04

# Count number of sources/experiments at end
unique(alp.diff_v99$source) # 121
unique(alp.diff_v99$exp.name) # 367

# Identify tidy file name / path
alp.diff_name <- "06-C_caged_alpha-div-diff_allscales.csv"
alp.diff_path <- file.path("data", alp.diff_name)

# Export locally
write.csv(x = alp.diff_v99, row.names = F, na = '', file = alp.diff_path)

# Make an 'experiment name' only
alp.diff_exp <- dplyr::filter(alp.diff_v99, alpha.diversity_design.level == "exp.name")

# Check structure
dplyr::glimpse(alp.diff_exp)

# Export
write.csv(x = alp.diff_v99, row.names = F, na = '', 
  file = file.path("data", "06-C_caged_alpha-div-diff_expname.csv"))

# End ----
