## --------------------------------------------------------------- ##
# CAGED Gamma Richness Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate gamma richness for each dataset

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, magrittr, vegan, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
gamma_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(gamma_v1)

## ------------------------------------------- ##
# Calculate Gamma Richness (per Exp/Treatment) ----
## ------------------------------------------- ##

# Do needed wrangling
gamma_trt <- gamma_v1 %>% 
  # Drop zero abundance taxa
  dplyr::filter(abundance > 0 & !is.na(abundance)) %>% 
  # Drop unwanted columns
  dplyr::select(-dplyr::starts_with("exp.design."), -cage.treatment_orig, -abundance) %>% 
    # Group by only desired columns & count number of unique taxa
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("year", "taxa", "abundance"))) 
    )) %>% 
  dplyr::summarize(gamma.richness = length(unique(taxa)),
    .groups = "drop")

# Do we have the expected number of values?
nrow(gamma_trt) == length(unique(paste(gamma_trt$source, gamma_trt$exp.name, gamma_trt$cage.treatment_std)))

# Check structure
dplyr::glimpse(gamma_trt)

## ------------------------------------------- ##
# Calculate Gamma Richness (Per Experiment) ----
## ------------------------------------------- ##

# Now calculate gamma richness across treatments within experiments
gamma_exp <- gamma_v1 %>% 
  dplyr::select(-dplyr::starts_with(c("exp.design.", "cage.treatment")), -abundance) %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("year", "taxa", "abundance"))) 
    )) %>% 
  dplyr::summarize(gamma.richness_exp.name = length(unique(taxa)),
    .groups = "drop")

# Do we have the expected number of values?
nrow(gamma_exp) == length(unique(paste(gamma_exp$source, gamma_exp$exp.name)))

# Check structure
dplyr::glimpse(gamma_exp)

## ------------------------------------------- ##
# Prep for Integration ----
## ------------------------------------------- ##

# Prep the 'by treatment' one for integration with the 'by experiment' one
gamma_trt_v2 <- gamma_trt %>% 
  dplyr::mutate(cage.treatment_std = paste0("gamma.richness_", cage.treatment_std)) %>% 
  tidyr::pivot_wider(names_from = cage.treatment_std, values_from = gamma.richness)

# Check structure
dplyr::glimpse(gamma_trt_v2)

## ------------------------------------------- ##
# Integrate Outputs ----
## ------------------------------------------- ##

# Join the data together
gamma_v2 <- gamma_exp %>% 
  dplyr::left_join(x = ., y = gamma_trt_v2,
    by = dplyr::join_by(source, organization, site, project.name, 
      sampling.years, excluded.group, measured.group, exp.name))

# Check structure
dplyr::glimpse(gamma_v2)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
gamma_v99 <- gamma_v2

# Identify tidy file name / path
gamma_name <- "05-B_caged_gamma-rich.csv"
gamma_path <- file.path("data", gamma_name)

# Export locally
write.csv(x = gamma_v99, row.names = F, na = '', file = gamma_path)

# End ----
