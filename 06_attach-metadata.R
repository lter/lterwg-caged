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
# Standardize Lat/Long Format ----
## ------------------------------------------- ##

# Check current lat/long formats
sort(unique(meta_v1$lat))

# Do needed repairs
meta_v2 <- meta_v1 %>% 
  # Rename & duplicate original lat/long cols
  dplyr::mutate(lat.orig = lat,
                long.orig = long) %>% 
  # Replace degree symbol with period
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "°|º", replacement = ".", x = .))) %>% 
  # Remove unwanted characters
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "’|'|′|\\\"", replacement = "", x = .))) %>% 
  # Replace N/S and E/W with negative symbols as needed
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ ifelse(stringr::str_detect(string = ., pattern = "S"),
                                              yes = paste0("-", .), no = .))) %>% 
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ ifelse(stringr::str_detect(string = ., pattern = "W"),
                                              yes = paste0("-", .), no = .))) %>% 
  # Then remove superseded cardinal direction letters
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "N|S|E|W", replacement = "", x = .))) %>% 
  # Remove spaces after periods
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "\\. ", replacement = "\\.", x = .))) %>% 
  # Split based on periods
  tidyr::separate_wider_delim(cols = lat, delim = ".", names = c("tmp__lat", "tmp__lat2"),
                              too_many = "merge", too_few = "align_start") %>% 
  tidyr::separate_wider_delim(cols = long, delim = ".", names = c("tmp__long", "tmp__long2"),
                              too_many = "merge", too_few = "align_start") %>% 
  # Remove periods from all four temp columns
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("tmp__"),
                              .fns = ~ gsub(pattern = "\\.", replacement = "", x = .))) %>% 
  # Recombine temp columns with period between first and second
  dplyr::mutate(lat = ifelse(!is.na(tmp__lat) & !is.na(tmp__lat2),
                             yes = paste0(tmp__lat, ".", tmp__lat2),
                             no = "")) %>% 
  dplyr::mutate(long = ifelse(!is.na(tmp__long) & !is.na(tmp__long2),
                              yes = paste0(tmp__long, ".", tmp__long2),
                              no = "")) %>% 
  # Remove temp columns
  dplyr::select(-dplyr::starts_with("tmp__")) %>% 
  # Reorder some other columns
  dplyr::relocate(lat.orig:long, .after = exp.name)

# Re-check formats
sort(unique(meta_v2$lat))

# Check structure more generally
dplyr::glimpse(meta_v2)

## ------------------------------------------- ##
# Check Join Keys for Mismatches ----
## ------------------------------------------- ##

# Check for mismatches in which datasets are in the data but not metadata (or vice versa)
supportR::diff_check(old = unique(w.meta_v1$source), new = unique(meta_v2$source))
## If any are in data but not *metadata*:
### Run "_boatyard_scripts/expand_metadata.R" and follow instructions at end of script

## If any are in metadata but not *data*:
### For some reason no beta dispersion was calculated for any spatial level
### (likely lack of "exp.design" columns in original dataset)
### Check data key to confirm

# Remove any files not found in the data from the metadata
meta_v3 <- dplyr::filter(.data = meta_v2, source %in% w.meta_v1$source)

# Now check for mismatches in "exp.name" column
## This is why this metadata is "site level"
supportR::diff_check(old = unique(w.meta_v1$exp.name), new = unique(meta_v3$exp.name))
## If any are in data but not *metadata*:
### The metadata had this info entered incorrectly
### Open the GoogleSheet and edit the "exp.name" column as needed
### Once done, start running this script again from the top to re-download the fixed version

## If any are in metadata but not *data*:
### Again, for some reason, no beta dispersion was calculated
### Check original data and beta dispersion calculation script to debug

# Remove any experiment names not found in data
meta_v4 <- dplyr::filter(.data = meta_v3, exp.name %in% w.meta_v1$exp.name)

# Re-check that there are no mismatches
supportR::diff_check(old = unique(w.meta_v1$source), new = unique(meta_v4$source))
supportR::diff_check(old = unique(w.meta_v1$exp.name), new = unique(meta_v4$exp.name))

## ------------------------------------------- ##
# Join Metadata ----
## ------------------------------------------- ##

# Actually join the metadata with the 'actual' data
w.meta_v2 <- w.meta_v1 %>% 
  dplyr::left_join(y = meta_v4, by = c("source", "exp.name")) %>% 
  # Relocate all of these columns more intuitively
  dplyr::relocate(assigned.to:notes, .after = exp.name) %>% 
  # Drop likely unwanted columns
  dplyr::select(-assigned.to, -notes)

# Check structure
dplyr::glimpse(w.meta_v2)

## ------------------------------------------- ##
# Standardize Free Text Columns ----
## ------------------------------------------- ##

# This is harder to do extensively but some coarse stuff makes sense
w.meta_v3 <- w.meta_v2 %>% 
  # Make some columns lowercase
  dplyr::mutate(dplyr::across(.cols = c(ecotype2, target.consumer, nontarget.consumer,
                                        resource.type, consumer.trophic.level),
                              .fns = ~ tolower(x = .)))

# Re-check structure
dplyr::glimpse(w.meta_v3)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
w.meta_v99 <- w.meta_v3

# Identify tidy file name / path
w.meta_name <- "06_caged_with-metadata.csv"
w.meta_path <- file.path("data", w.meta_name)

# Export locally
write.csv(x = w.meta_v99, row.names = F, na = '', file = w.meta_path)

# # Upload to Drive
# googledrive::drive_upload(media = w.meta_path, overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
