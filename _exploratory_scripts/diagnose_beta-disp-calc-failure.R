## --------------------------------------------------------------- ##
# Diagnosis Script - Failure to Calculate Beta Dispersion
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Create needed folder(s)
dir.create(path = file.path("data", "diagnostic"), showWarnings = F, recursive = T)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Data Prep ----
## ------------------------------------------- ##

# Read in the beta dispersion (all scales) dataset
diag_v1 <- read.csv(file = file.path("data", "05-A_caged_beta-disp_all-scales.csv"))

# Check structure
dplyr::glimpse(diag_v1)

# Pare down to bare minimum content
diag_v2 <- diag_v1 %>% 
  dplyr::select(source, exp.name:cage.treatment_std, 
                betadisp.design.level, betadisp.sample.size, betadisp.comm.dist) %>% 
  dplyr::distinct()

# Re-check structure
dplyr::glimpse(diag_v2)

# Subset to only data files for which beta dispersion was incalculable at any design level
diag_v3 <- diag_v2 %>% 
  dplyr::group_by(source) %>% 
  dplyr::filter(all(is.na(betadisp.comm.dist))) %>% 
  dplyr::ungroup()

# Re-re-check structure
dplyr::glimpse(diag_v3)

## ------------------------------------------- ##
# Create Diagnostic Files ----
## ------------------------------------------- ##

# Iterate across these
for(focal_file in diag_v3$source){
  
  # Progress message
  message("Making diagnostic output for: ", focal_file)
  
  # Subset data
  diag_df <- dplyr::filter(diag_v3, source == focal_file)
  
  # Export
  write.csv(x = diag_df, row.names = F, na = '',
            file = file.path("data", "diagnostic", paste0("DIAGNOSE_", focal_file)))
  
}

# End ----
