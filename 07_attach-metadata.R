## --------------------------------------------------------------- ##
# CAGED Attach Metadata
## --------------------------------------------------------------- ##
# Purpose:
## Group members collectively filled out a metadata GoogleSheet manually
## We want that attached to the data for use in visualization / analysis
## This script accomplishes both that joining operation and some minor QC

# Note: "source" and "exp.name" columns in metadata are created by "_boatyard_scripts/expand_metadata.R"

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
# Standardize Lat/Long Format ----
## ------------------------------------------- ##

# Look for non-numbers in the current lat/long columns
supportR::num_check(data = meta_v1, col = "var_lat")
supportR::num_check(data = meta_v1, col = "var_long")

# Do needed repairs
meta_v2 <- meta_v1 %>% 
  # Rename relevant lat/long columns
  dplyr::rename(lat = var_lat, long = var_long) %>%
  # Replace M-dashes with hyphens
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "−", replacement = "-", x = .)))

# Re-check for non-numbers
supportR::num_check(data = meta_v2, col = "lat")
supportR::num_check(data = meta_v2, col = "long")

# Check structure more generally
dplyr::glimpse(meta_v2)

## ------------------------------------------- ##
# Standardize Free Text Columns ----
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
# Remove Unwanted Metadata Info ----
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
  dplyr::select(-dplyr::contains("notes"), -assigned.to, -dropped.reason,
                -dplyr::starts_with("second.round.")) %>% 
  # Drop any columns that are entirely empty
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.))))

# Check that only drops desired columns
supportR::diff_check(old = names(meta_v3), new = names(meta_v4))

# Check structure
dplyr::glimpse(meta_v4)

## ------------------------------------------- ##
# Check Join Keys for Mismatches ----
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
# "duran-2016-peerj-coral-reef-algae_succession-primary-substrate"

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
# Load Gamma Richness Data ----
## ------------------------------------------- ##

# Read in gamma richness
gamma_v1 <- read.csv(file = file.path("data", "05-B_caged_gamma-rich.csv"))

# Check structure
dplyr::glimpse(gamma_v1)

## ------------------------------------------- ##
# Load Mean Difference Data ----
## ------------------------------------------- ##

# Read in the mean difference files too
diff_v1 <- read.csv(file = file.path("data", "06_caged_mean-beta-diff_all-scales.csv"))

# Check structure of one
dplyr::glimpse(diff_v1)

## ------------------------------------------- ##
# Attach *EVERYTHING* to Data ----
## ------------------------------------------- ##
## https://tenor.com/view/everyone-the-professional-shout-gif-12696023

# Make a list for storing outputs
w.meta_out_list <- list()

# Loop across files for which we want 'metadata' attached
for(focal_w.meta in w.meta_outs){
  
  # Processing message
  message("Attaching ancillary data to ", focal_w.meta)
  
  # Grab just that file out of the list of inputs
  w.meta_v1 <- w.meta_in_list[[focal_w.meta]]
  
  # Now attach true metadata GoogleSheet & reorder columns
  w.meta_v2 <- w.meta_v1 %>% 
    dplyr::left_join(y = meta_v6, by = c("source", "exp.name")) %>% 
    dplyr::relocate(exp.design.4:betadisp.comm.dist, 
                    .after = dplyr::everything())
    
  # Now attach gamma richness & reorder columns
  w.meta_v3 <- w.meta_v2 %>% 
    dplyr::left_join(y = gamma_v1, by = c("source", "organization", "site", 
                                          "project.name", "sampling.years",
                                          "excluded.group", "measured.group", 
                                          "exp.name")) %>% 
    dplyr::relocate(gamma.richness, .before = exp.name)
  
  # Now attach summarized beta disp and mean difference
  w.meta_v4 <- w.meta_v3 %>% 
    ## No column re-ordering needed (want these at end)
    dplyr::left_join(y = diff_v1, by = c("source", "organization", "site", 
                                         "excluded.group", "measured.group",
                                         "exp.name", "cage.treatment_std",
                                         "year", "betadisp.design.level"))

  # Add this to the output list
  w.meta_out_list[[focal_w.meta]] <- w.meta_v4
  
} # Close loop
# getting an error here now? generated by villar being year = year 

# Check the structure at various points
## Starting (no metadata added)
dplyr::glimpse(w.meta_v1)
## After adding metadata GoogleSheet
dplyr::glimpse(w.meta_v2)
## After adding gamma richness
dplyr::glimpse(w.meta_v3)
## After adding summarized beta disp + mean diff
dplyr::glimpse(w.meta_v4)

# How many sources and exp.name got through the pipeline?
unique(w.meta_v4$source) # 118
unique(w.meta_v4$exp.name) # 327

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Loop across the list elements to export
for(w.meta_outs in unique(names(w.meta_out_list))){
  # w.meta_outs <- "05-A_caged_beta-disp_all-scales.csv"
  
  # Create a final object
  w.meta_v99 <- w.meta_out_list[[w.meta_outs]]
  
  # Generate tidy name / path
  w.meta_name <- gsub(pattern = "05-A_caged_beta-disp", 
                    replacement = "07_caged_w.meta", x = w.meta_outs)
  w.meta_path <- file.path("data", w.meta_name)
  
  # Export locally
  write.csv(x = w.meta_v99, row.names = F, na = '', file = w.meta_path)
}

# End ----
