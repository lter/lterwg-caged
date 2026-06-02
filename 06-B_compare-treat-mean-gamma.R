## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Gamma Diversity Calculation
## --------------------------------------------------------------- ##
# Purpose
## Calculate mean differences and log response ratios (LRR) for gamma richness

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
gam.diff_v01 <- read.csv(file.path("data", "05-B_caged_gamma-rich.csv"))

# Check structure
dplyr::glimpse(gam.diff_v01)

## ------------------------------------------- ##
# Streamline Data ----
## ------------------------------------------- ##

# Ditch any unwanted rows/columns
gam.diff_v02 <- gam.diff_v01 %>% 
  dplyr::select(-gamma.richness_uncertain, -gamma.richness_partial)

# Check structure
dplyr::glimpse(gam.diff_v02)

## ------------------------------------------- ##
# Calculate Difference & LRR ----
## ------------------------------------------- ##

# Calculate difference and LRR
gam.diff_v03 <- gam.diff_v02 %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("caged"),
    .fns = ~ ifelse(is.na(.), yes = 0, no = .))) %>% 
  dplyr::mutate(caged = gamma.richness_caged + 0.005,
    uncaged = gamma.richness_uncaged + 0.005) %>% 
  dplyr::mutate(within.cage.treat_gamma.diff = uncaged - caged,
    within.cage.treat_gamma.lrr = log2(uncaged / caged)) %>% 
  dplyr::select(-caged, -uncaged)

# Check structure
dplyr::glimpse(gam.diff_v03)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
gam.diff_v99 <- gam.diff_v03

# Count number of sources/experiments at end
unique(gam.diff_v99$source) # 121
unique(gam.diff_v99$exp.name) # 367

# Identify tidy file name / path
gam.diff_name <- "06-B_caged_gamma-diff.csv"
gam.diff_path <- file.path("data", gam.diff_name)

# Export locally
write.csv(x = gam.diff_v99, row.names = F, na = '', file = gam.diff_path)

# End ----
