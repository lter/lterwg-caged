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

# Average across caged and uncaged 

# Identify the grouping columns 
diff_groupcols <- c("source", "organization", "site", 
                    "excluded.group", "measured.group", 
                    "exp.name", "alpha.diversity_design.level", 
                    "alpha.diversity_caged", "alpha.diversity_uncaged")

# Do some needed preparatory calculatation
alp.diff_v03 <- alp.diff_v02 %>% 
  # Summarize within treatments/etc.
  dplyr::group_by(dplyr::across(
    dplyr::all_of(c(diff_groupcols)))) %>%  
  dplyr::summarize(alpha.diversity_caged.mean = mean(alpha.diversity_caged, na.rm = TRUE),
                   alpha.diversity_uncaged.mean = mean(alpha.diversity_uncaged, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::select(-alpha.diversity_caged, -alpha.diversity_uncaged)

dim(alp.diff_v03)
  

# minimum value of alpha diversity
range(alp.diff_v03$alpha.diversity_uncaged.mean) # has NA's because some scales you can't calculate? 

alp.diff_v03 %>% 
  summarise(min_val = min(alpha.diversity_uncaged.mean, na.rm = TRUE)) # 1

alp.diff_v03 %>% 
  summarise(min_val = min(alpha.diversity_caged.mean, na.rm = TRUE)) # 1

## ------------------------------------------- ##
# Calculate LRR ----
## ------------------------------------------- ##

# Calculate LRR
alp.diff_v04 <- alp.diff_v03 %>% 
  # dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("caged"),
  #   .fns = ~ ifelse(is.na(.), yes = 0, no = .))) %>% 
  # add minimum value of alpha diversity - 1
  dplyr::mutate(caged = alpha.diversity_caged.mean + 1,
    uncaged = alpha.diversity_uncaged.mean + 1) %>% 
  # create log response ratio
  dplyr::mutate(within.cage.treat_alpha.mean.lrr = log2(uncaged / caged)) %>% 
  dplyr::select(-caged, -uncaged) %>%
  # remove NA's
  drop_na(within.cage.treat_alpha.mean.lrr)

dim(alp.diff_v04)

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
