## --------------------------------------------------------------- ##
# Diagnosis Script - Exp.Names Dropped
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Manually identify files of interest
want_exp <- c("Camano_protected-Warm", "lake creek", "MNP_30", 
              "parker_wetlands_carpgrass_2005_crayfish_plants.csv",
              "steckley", "west fork", "wildcat")

## ------------------------------------------- ##
# Load All Tidy Data ----
## ------------------------------------------- ##

# Identify local files (excluding some not useful files)
(local_files <- setdiff(x = dir(path = file.path("data"), pattern = "csv"),
                        y = c("caged_data-key.csv", "sitelevel-metadata.csv",
                              ## Lack standard treatment info
                              "01_caged_harmonized.csv", "05-B_caged_gamma-rich.csv", 
                              ## Lack any treatment info (is implied by 'difference' column)
                              "08_caged_prepped-beta-dispersion.csv", 
                              "08_caged_prepped-effect-size.csv")))


# Make an empty list
diag_v0 <- list()

# Read in each tidy data object
for(file in local_files){
  diag_v0[[file]] <- read.csv(file = file.path("data", file)) %>%
    dplyr::mutate(tidy_source = file, .before = source)
}

# Tidy up that list slightly
diag_v1 <- diag_v0 %>%
  # Pare down columns
  purrr::map(.x = ., .f = ~ dplyr::select(
    .data = .x, dplyr::contains("source"), exp.name, 
                dplyr::starts_with(c("exp.design.", "cage.treatment_std"))
  )) %>% 
  # Filter to only the data that we're interested in
  purrr::map(.x = ., .f = ~ dplyr::filter(
    .data = .x, exp.name %in% want_exp)) %>%
  # Drop some columns specifically
  purrr::map(.x = ., .f = ~ dplyr::select(
    .data = .x, -dplyr::contains("spatialextent.category"))) %>% 
  # Drop non-unique rows
  purrr::map(.x = ., .f = dplyr::distinct) %>% 
  # Unlist to dataframe
  purrr::list_rbind(x = .)

# Check structure
dplyr::glimpse(diag_v1)

## ------------------------------------------- ##
# Diagnose Data ----
## ------------------------------------------- ##

# Loop across provided source files
for(focal_exp in sort(unique(want_exp))){
  
  # Progress message
  message("Diagnosing dataset: '", focal_exp, "'")
  
  # Subset the 'all tidy data' to just this dataset
  diag_v2 <- diag_v1 %>% 
    dplyr::filter(exp.name == focal_exp)
  
  # Now actually generate the diagnostic output
  diag_v3 <- diag_v2 %>% 
    # Pare down columns/rows (again)
    dplyr::select(tidy_source, source, exp.name, cage.treatment_std) %>% 
    dplyr::distinct() %>% 
    # Wrangle the tidy source column slightly
    dplyr::mutate(tidy_source = gsub(pattern = "-", replacement = ".", x = tidy_source)) %>% 
    # Make a placeholder column (we'll need it in a sec)
    dplyr::mutate(presence = "present") %>% 
    # Identify whether the dataset is in each of the remaining tidy sources
    tidyr::pivot_wider(names_from = tidy_source, values_from = presence, values_fill = "MISSING")
  
  # Generate a nice-ish file name for this diagnostic output
  diag_name <- paste0("workflow-drop-diagnostic_", focal_exp, ".csv")
   
  # Export this locally
  write.csv(x = diag_v3, row.names = F, na = '',
            file = file.path("data", "diagnostic", diag_name))
  
} # Close loop

# End ----
