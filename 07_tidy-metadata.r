## --------------------------------------------------------------- ##
# CAGED Attach Metadata
## --------------------------------------------------------------- ##
# Purpose:
## Group members collectively filled out a metadata GoogleSheet manually
## We want that attached to the data for use in visualization / analysis
## This script accomplishes QCs the data in preparation for later joining

# Note: "source" and "exp.name" columns in metadata are created by "_boatyard_scripts/expand_metadata.R"
# Note: Make sure to redownload site level metadata (with script 000) if it has changed 

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify data files we want to add stuff to
(w.meta_outs <- dir(path = file.path("data"), pattern = "05-A_caged_beta-disp_"))
w.meta_in_list <- purrr::map(.x = w.meta_outs,
                             .f = ~ read.csv(file = file.path("data", .x)))
names(w.meta_in_list) <- w.meta_outs

# Check structure of one
dplyr::glimpse(w.meta_in_list[[1]])

## ------------------------------------------- ##
# Load Metadata ----
## ------------------------------------------- ##

# Read in the metadata
meta_v1 <- read.csv(file = file.path("data", "sitelevel-metadata.csv"))

# Check structure
dplyr::glimpse(meta_v1)

## ------------------------------------------- ##
# Meta - Standardize Lat/Long ----
## ------------------------------------------- ##

# Look for non-numbers in lat/long columns
supportR::num_check(data = meta_v1, col = c("var_lat", "var_long"))

# Do needed repairs
meta_v2 <- meta_v1 %>% 
  # Rename relevant lat/long columns
  dplyr::rename(lat = var_lat, long = var_long) %>%
  # Replace M-dashes with hyphens
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "−", replacement = "-", x = .)))

# Re-check for non-numbers
supportR::num_check(data = meta_v2, col = c("lat", "long"))

# Check structure more generally
dplyr::glimpse(meta_v2)

## ------------------------------------------- ##
# Meta - Standardize Free Text ----
## ------------------------------------------- ##

# This is harder to do extensively but some coarse stuff makes sense
meta_v3 <- meta_v2 %>% 
  # Make some columns lowercase
  dplyr::mutate(dplyr::across(.cols = c(ecotype2, target.consumer, nontarget.consumer,
                                        resource.type.notes, consumer.trophic.level),
                              .fns = ~ tolower(x = .)))

# Re-check structure
dplyr::glimpse(meta_v3)

## ------------------------------------------- ##
# Meta - Remove Unwanted Info ----
## ------------------------------------------- ##

# Needed processing
meta_v4 <- meta_v3 %>% 
  # Make all empty cells true NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(nchar(.) == 0,
                                              yes = NA, no = .))) %>% 
  # Drop rows without either a source name or an experiment names
  dplyr::filter(!is.na(source) | !is.na(exp.name)) %>% 
  # Remove unwanted columns
  dplyr::select(source, exp.name, lat, long, dplyr::starts_with("var")) %>% 
  dplyr::select(-dplyr::contains("note")) %>% 
  # Make lat/long data numeric
  dplyr::mutate(lat = as.numeric(lat),
    long = as.numeric(long)) %>% 
  # Drop any columns that are entirely empty
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.))))

# Check that only drops desired columns
supportR::diff_check(old = names(meta_v3), new = names(meta_v4))

# Check structure
dplyr::glimpse(meta_v4)

## ------------------------------------------- ##
# Meta - Check for Duplicate Rows ----
## ------------------------------------------- ##

# The join approach _requires_ that every source-exp.name combination have _ONLY one row!_
# Count rows per source/exp.name
meta_v4 %>% 
  dplyr::group_by(source, exp.name) %>% 
  dplyr::summarize(ct = dplyr::n(),
    .groups = "drop") %>% 
  dplyr::filter(ct > 1)

# If this returns anything other than the following:
### A tibble: 0 × 3
### ℹ 3 variables: source <chr>, exp.name <chr>, ct <int>

# THEN REVISIT THE METADATA
## And delete/synonymize one of the two rows

## ------------------------------------------- ##
# Meta - Check Join Keys for Mismatches ----
## ------------------------------------------- ##

# Check for mismatches in which datasets are in the data but not metadata (or vice versa)
supportR::diff_check(old = unique(c(w.meta_in_list[[1]]$source,
                                    w.meta_in_list[[2]]$source)), 
                     new = unique(meta_v3$source))

# old = data, new = metadata
# there will be some that are not in the metadata if we decided to exclude them 
## If any are in data but not *metadata*:
### Run "_boatyard_scripts/expand_metadata.R" and follow instructions at end of script

## If any are in metadata but not *data*:
### For some reason no beta dispersion was calculated for any spatial level
### (likely lack of "exp.design" columns in original dataset)
#### Check data key to confirm
### (or potentially removed due to confounding treatments)

# Remove any files not found in the data from the metadata
meta_v5 <- dplyr::filter(.data = meta_v4, source %in% unique(c(w.meta_in_list[[1]]$source,
                                                               w.meta_in_list[[2]]$source)))

# Now check for mismatches in "exp.name" column
## This is why this metadata is "site level"
supportR::diff_check(old = unique(c(w.meta_in_list[[1]]$exp.name,
                                    w.meta_in_list[[2]]$exp.name)),
                     new = unique(meta_v5$exp.name))
# old = data, new = metadata
## If any are in data but not *metadata*:
### The metadata had this info entered incorrectly
### Open the GoogleSheet and edit the "exp.name" column as needed
### Once done, start running this script again from the top to re-download the fixed version

## If any are in metadata but not *data*:
### Again, for some reason, no beta dispersion was calculated
### Check original data and beta dispersion calculation script to debug
### _OR_ could be caused by new "exp.name" in data and an outdated entry in the metaadata

# Remove any experiment names not found in data
meta_v6 <- dplyr::filter(.data = meta_v5, exp.name %in% unique(c(w.meta_in_list[[1]]$exp.name,
                                                                 w.meta_in_list[[2]]$exp.name)))

# Re-check that there are no mismatches
supportR::diff_check(old = unique(c(w.meta_in_list[[1]]$source,
                                    w.meta_in_list[[2]]$source)),
                     new = unique(meta_v6$source))
# old = data, new = metadata

supportR::diff_check(old = unique(c(w.meta_in_list[[1]]$exp.name,
                                    w.meta_in_list[[2]]$exp.name)),
                                  new = unique(meta_v6$exp.name))
# old = data, new = metadata

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Make a final metadata object
meta_v99 <- meta_v6

# Check structure
dplyr::glimpse(meta_v99)

# Export locally
write.csv(x = meta_v99, na = '', row.names = FALSE,
  file = file.path("data", "07_tidy-sitelevel-metadata.csv"))

# End ----
