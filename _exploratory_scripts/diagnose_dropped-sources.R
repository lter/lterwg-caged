## --------------------------------------------------------------- ##
# Diagnosis Script - Sources Dropped
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools)

# Create needed folder(s)
dir.create(path = file.path("data", "diagnostic"), showWarnings = F, recursive = T)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Manually identify files of interest
want_src <- c("parker_wetlands_carpgrass_2005_crayfish_plants.csv")

## ------------------------------------------- ##
# Load All Tidy Data ----
## ------------------------------------------- ##

# Get a list of data objects
diag_v1 <- ltertools::read(raw_folder = file.path("data"), data_format = "csv") %>% 
  # Pare down columns
  purrr::map(.x = ., .f = ~ dplyr::select(
    .data = .x, source, exp.name, dplyr::starts_with(c("exp.design.", "cage.treatment_std"))
  )) %>% 
  # Drop some columns specifically
  purrr::map(.x = ., .f = ~ dplyr::select(
    .data = .x, -dplyr::contains("spatialextent.category"))) %>% 
  # Drop non-unique rows
  purrr::map(.x = ., .f = dplyr::distinct) %>% 
  # Get 'tidy source' as a column in its own right
  purrr::imap(.x = ., 
              .f = ~ dplyr::mutate(.data = .x, tidy_source = .y, 
                                   .before = source)) %>% 
  # Unlist to dataframe
  purrr::list_rbind(x = .) %>% 
  # Ditch the very first harmonized data and 
  ## (01 is not super useful as a diagnostic because it hasn't had any QC)
  ## (gamma richness is not useful because it doesn't have treatment info)
  dplyr::filter(!tidy_source %in% c("01_caged_harmonized.csv", "05-B_caged_gamma-rich.csv"))

# Check structure
dplyr::glimpse(diag_v1)

## ------------------------------------------- ##
# Diagnose Data ----
## ------------------------------------------- ##

# Loop across provided source files
for(focal_src in sort(unique(want_src))){
  
  # Progress message
  message("Diagnosing dataset: '", focal_src, "'")
  
  # Subset the 'all tidy data' to just this dataset
  diag_v2 <- diag_v1 %>% 
    dplyr::filter(source == focal_src)
  
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
  diag_name <- paste0("workflow-drop-diagnostic_", focal_src)
  
  # Export this locally
  write.csv(x = diag_v3, row.names = F, na = '',
            file = file.path("data", "diagnostic", diag_name))
  
} # Close loop

# End ----
