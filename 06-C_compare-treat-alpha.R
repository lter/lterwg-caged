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

# Filter to only desired treatments and average across replicates
alp.diff_v02 <- alp.diff_v01 %>% 
  dplyr::filter(alpha.diversity_design.level == "exp.name") %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("exp.design.1", "alpha.diversity_richness"))))) %>% 
  dplyr::summarize(alpha.div = mean(alpha.diversity_richness, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::mutate(cage.treatment_std = paste0("alpha.diversity_", cage.treatment_std))

# Check structure
dplyr::glimpse(alp.diff_v02)

## ------------------------------------------- ##
# Summarize to Experiment Name ----
## ------------------------------------------- ##

# Average (in order) across design replicates up to experiment name
alp.diff_v03 <- alp.diff_v02 %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("exp.design.2", "alpha.div"))))) %>% 
  dplyr::summarize(alpha.div2 = mean(alpha.div, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("exp.design.3", "alpha.div2"))))) %>% 
  dplyr::summarize(alpha.div3 = mean(alpha.div2, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("exp.design.4", "alpha.div3"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = mean(alpha.div3, na.rm = TRUE),
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
unique(alp.diff_v99$source) # 120
unique(alp.diff_v99$exp.name) # 363

# Identify tidy file name / path
alp.diff_name <- "06-C_caged_alpha-div-diff_all-scales.csv"
alp.diff_path <- file.path("data", alp.diff_name)

# Export locally
write.csv(x = alp.diff_v99, row.names = F, na = '', file = alp.diff_path)

# End ----
