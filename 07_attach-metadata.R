## --------------------------------------------------------------- ##
# CAGED Attach Metadata
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

# Purpose:
## Group members collectively filled out a metadata GoogleSheet manually
## We want that attached to the data for use in visualization / analysis
## This script accomplishes both that joining operation and some minor QC

# Note: "source" and "exp.name" columns in metadata are created by "_boatyard_scripts/expand_metadata.R"

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
w.meta_v1 <- read.csv(file.path("data", "05_caged_beta-disp.csv"))

# Check structure
dplyr::glimpse(w.meta_v1)

## ------------------------------------------- ##
# Download Metadata ----
## ------------------------------------------- ##

# Identify the relevant GoogleSheet
meta_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AFR2XIdw_sKbUk9PVA")) %>% 
  dplyr::filter(name == "sitelevel-metadata")

# Check that worked
meta_drive

# Download it
googledrive::drive_download(file = meta_drive$id, type = "csv", overwrite = T,
                            path = file.path("data", meta_drive$name))

# Read it in
meta_v1 <- read.csv(file = file.path("data", "sitelevel-metadata.csv"))

# Check structure
dplyr::glimpse(meta_v1)

## ------------------------------------------- ##
# Check Join Keys for Mismatches ----
## ------------------------------------------- ##

# Check for mismatches in which datasets are in the data but not metadata (or vice versa)
supportR::diff_check(old = unique(w.meta_v1$source), new = unique(meta_v1$source))
## If any are in data but not *metadata*:
### Run "_boatyard_scripts/expand_metadata.R" and follow instructions at end of script

## If any are in metadata but not *data*:
### For some reason no beta dispersion was calculated for any spatial level
### (likely lack of "exp.design" columns in original dataset)
### Check data key to confirm

# Remove any files not found in the data from the metadata
meta_v2 <- dplyr::filter(.data = meta_v1, source %in% w.meta_v1$source)

# Now check for mismatches in "exp.name" column
## This is why this metadata is "site level"
supportR::diff_check(old = unique(w.meta_v1$exp.name), new = unique(meta_v2$exp.name))
## If any are in data but not *metadata*:
### The metadata had this info entered incorrectly
### Open the GoogleSheet and edit the "exp.name" column as needed
### Once done, start running this script again from the top to re-download the fixed version

## If any are in metadata but not *data*:
### Again, for some reason, no beta dispersion was calculated
### Check original data and beta dispersion calculation script to debug

# Remove any experiment names not found in data
meta_v3 <- dplyr::filter(.data = meta_v2, exp.name %in% w.meta_v1$exp.name)

# Re-check that there are no mismatches
supportR::diff_check(old = unique(w.meta_v1$source), new = unique(meta_v3$source))
supportR::diff_check(old = unique(w.meta_v1$exp.name), new = unique(meta_v3$exp.name))

## ------------------------------------------- ##
# Join Metadata ----
## ------------------------------------------- ##

# Actually join the metadata with the 'actual' data
w.meta_v2 <- w.meta_v1 %>% 
  dplyr::left_join(y = meta_v3, by = c("source", "exp.name"))



## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
w.meta_v99 <- w.meta_v2

# Identify tidy file name / path
zerow.meta_name <- "07_caged_with-metadata.csv"
zerow.meta_path <- file.path("data", zerow.meta_name)

# Export locally
write.csv(x = w.meta_v99, row.names = F, na = '', file = zerow.meta_path)

# Upload to Drive
googledrive::drive_upload(media = zerow.meta_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
