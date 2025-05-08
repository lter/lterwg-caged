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

# Identify data files we want to add stuff to
(w.meta_outs <- dir(path = file.path("data"), pattern = "05-A_caged_beta-disp_"))
w.meta_list <- purrr::map(.x = w.meta_outs,
                          .f = ~ read.csv(file = file.path("data", .x)))
names(w.meta_list) <- w.meta_outs

# Check structure of one
dplyr::glimpse(w.meta_list[[1]])

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
# Standardize Free Text Columns ----
## ------------------------------------------- ##

# This is harder to do extensively but some coarse stuff makes sense
meta_v3 <- meta_v2 %>% 
  # Make some columns lowercase
  dplyr::mutate(dplyr::across(.cols = c(ecotype2, target.consumer, nontarget.consumer,
                                        resource.type, consumer.trophic.level),
                              .fns = ~ tolower(x = .)))

# Re-check structure
dplyr::glimpse(meta_v3)

## ------------------------------------------- ##
# Remove Unwanted Metadata Columns ----
## ------------------------------------------- ##

# Remove unwanted columns
meta_v4 <- meta_v3 %>% 
  dplyr::select(-dplyr::contains("notes"), -assigned.to)

# Check that only drops desired columns
supportR::diff_check(old = names(meta_v3), new = names(meta_v4))

# Check structure
dplyr::glimpse(meta_v4)

## ------------------------------------------- ##
# Check Join Keys for Mismatches ----
## ------------------------------------------- ##

# Check for mismatches in which datasets are in the data but not metadata (or vice versa)
supportR::diff_check(old = unique(c(w.meta_list[[1]]$source,
                                    w.meta_list[[2]]$source)), 
                     new = unique(meta_v3$source))
## If any are in data but not *metadata*:
### Run "_boatyard_scripts/expand_metadata.R" and follow instructions at end of script

## If any are in metadata but not *data*:
### For some reason no beta dispersion was calculated for any spatial level
### (likely lack of "exp.design" columns in original dataset)
#### Check data key to confirm
### (or potentially removed due to confounding treatments)

# Remove any files not found in the data from the metadata
meta_v5 <- dplyr::filter(.data = meta_v4, source %in% unique(c(w.meta_list[[1]]$source,
                                                               w.meta_list[[2]]$source)))

# Now check for mismatches in "exp.name" column
## This is why this metadata is "site level"
supportR::diff_check(old = unique(c(w.meta_list[[1]]$exp.name,
                                    w.meta_list[[2]]$exp.name)),
                     new = unique(meta_v5$exp.name))
## If any are in data but not *metadata*:
### The metadata had this info entered incorrectly
### Open the GoogleSheet and edit the "exp.name" column as needed
### Once done, start running this script again from the top to re-download the fixed version

## If any are in metadata but not *data*:
### Again, for some reason, no beta dispersion was calculated
### Check original data and beta dispersion calculation script to debug

# Remove any experiment names not found in data
meta_v6 <- dplyr::filter(.data = meta_v5, exp.name %in% unique(c(w.meta_list[[1]]$exp.name,
                                                                 w.meta_list[[2]]$exp.name)))

# Re-check that there are no mismatches
supportR::diff_check(old = unique(c(w.meta_list[[1]]$source,
                                    w.meta_list[[2]]$source)),
                     new = unique(meta_v6$source))
supportR::diff_check(old = unique(c(w.meta_list[[1]]$exp.name,
                                    w.meta_list[[2]]$exp.name)),
                                  new = unique(meta_v6$exp.name))

## ------------------------------------------- ##
# Download Other Relevant 'Metadata' Info ----
## ------------------------------------------- ##

# We want everything after beta dispersion calculation
## **As of 5/8/2025**, outputs of that script are (05-A_)
other_meta <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")) %>% 
  dplyr::filter(stringr::str_detect(string = name, pattern = "05-B_|06_"))

# Look like the right files?
other_meta

# Download 'em
purrr::walk2(.x = other_meta$id, .y = other_meta$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", .y)))

## ------------------------------------------- ##
# Check 'Other Metadata' Files ----
## ------------------------------------------- ##

# Read in gamma richness
gamma_v1 <- read.csv(file = file.path("data", "05-B_caged_gamma-rich.csv"))

# Check structure
dplyr::glimpse(gamma_v1)

# Read in the mean difference files too
diff_v1 <- read.csv(file = file.path("data", "06_caged_mean-beta-diff_all-scales.csv"))

# Check structure of one
dplyr::glimpse(diff_v1)

## ------------------------------------------- ##
# Attach *EVERYTHING* to Data ----
## ------------------------------------------- ##
## https://tenor.com/view/everyone-the-professional-shout-gif-12696023

# Loop across files for which we want 'metadata' attached
for(focal_w.meta in w.meta_outs){
  
  # Processing message
  message("Attaching ancillary data to ", focal_w.meta)
  
  # Grab just that file out of the list of inputs
  w.meta_v1 <- w.meta_list[[focal_w.meta]]
  
  # Now attach true metadata GoogleSheet & reorder columns
  w.meta_v2 <- w.meta_v1 %>% 
    dplyr::left_join(y = meta_v5, by = c("source", "exp.name")) %>% 
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
  
  
  
}

# Check the structure at various points
## Starting (no metadata added)
dplyr::glimpse(w.meta_v1)
## After adding metadata GoogleSheet
dplyr::glimpse(w.meta_v2)
## After adding gamma richness
dplyr::glimpse(w.meta_v3)
## After adding summarized beta disp + mean diff
dplyr::glimpse(w.meta_v4)


# BASEMENT----



# Identify any beta dispersion outputs
(beta_outs <- dir(path = file.path("data"), pattern = "05-A_caged_beta-disp"))



# Read in data
w.meta_v1 <- read.csv(file.path("data", "05_caged_beta-disp_finest-scales.csv"))

# Check structure
dplyr::glimpse(w.meta_v1)




## ------------------------------------------- ##
# Join Metadata ----
## ------------------------------------------- ##

# Actually join the metadata with the 'actual' data
w.meta_v2 <- w.meta_v1 %>% 
  dplyr::left_join(y = meta_v5, by = c("source", "exp.name")) %>% 
  # Relocate all of these columns more intuitively
  dplyr::relocate(assigned.to:notes, .after = exp.name) %>% 
  # Drop likely unwanted columns
  dplyr::select(-assigned.to, -contains("notes"))

# What is lost?
supportR::diff_check(old = unique(w.meta_v1$source), new = unique(w.meta_v2$source))

# Check structure
dplyr::glimpse(w.meta_v2)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
w.meta_v99 <- w.meta_v2

# Identify tidy file name / path
w.meta_name <- "06_caged_with-metadata_finest-scales.csv"
w.meta_path <- file.path("data", w.meta_name)

# Export locally
write.csv(x = w.meta_v99, row.names = F, na = '', file = w.meta_path)

# # Upload to Drive
# googledrive::drive_upload(media = w.meta_path, overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

## ------------------------------------------- ##
# Attach Metadata to Design-Specific Beta Disp ----
## ------------------------------------------- ##

# List exp.design/.name-specific files
(beta_files <- dir(path = file.path("data"), pattern = "05_caged_beta-disp_exp-"))

# Iterate across them
for(focal_file in beta_files){
  
  # Progress message
  message("Attaching metadata to ", focal_file)
  
  # Read in data
  focal_df <- read.csv(file = file.path("data", focal_file))
  
  # Check for join key mismatches
  supportR::diff_check(old = unique(focal_df$source), new = unique(meta_v5$source))
  supportR::diff_check(old = unique(focal_df$exp.name), new = unique(meta_v5$exp.name))
  
  # Attach metadata & do minor wrangling
  focal_join <- focal_df %>% 
    dplyr::left_join(y = meta_v5, by = c("source", "exp.name")) %>% 
    dplyr::relocate(assigned.to:notes, .after = exp.name) %>% 
    dplyr::select(-assigned.to, -notes)
  
  # Create a new file name
  focal_out1 <- gsub(pattern = "beta-disp", replacement = "with-metadata", x = focal_file)
  focal_out <- gsub(pattern = "05", replacement = "06", x = focal_out1)
  
  # Export locally
  write.csv(x = focal_join, na = '', row.names = F,
            file = file.path("data", focal_out))
  
}

# Check the structure of one of those ouputs
dplyr::glimpse(focal_join)

# # Upload all of these to the Drive
# purrr::walk(.x = dir(path = file.path("data"), pattern = "06_caged_with-metadata_exp"),
#             .f = ~ googledrive::drive_upload(media = file.path("data", .x), overwrite = T,
#                                              path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")))

## ------------------------------------------- ##
# Attach Metadata to 'All Scales' Beta Disp ----
## ------------------------------------------- ##

# Read in data
w.meta_allscales_v1 <- read.csv(file = file.path("data", "05_caged_beta-disp_all-scales.csv"))

# Check for join key mismatches
supportR::diff_check(old = unique(w.meta_allscales_v1$source), new = unique(meta_v5$source))
supportR::diff_check(old = unique(w.meta_allscales_v1$exp.name), new = unique(meta_v5$exp.name))

# Attach metadata & do minor wrangling
w.meta_allscales_v2 <- w.meta_allscales_v1 %>% 
  dplyr::left_join(y = meta_v5, by = c("source", "exp.name")) %>% 
  dplyr::relocate(assigned.to:notes, .after = exp.name) %>% 
  dplyr::select(-assigned.to, -notes)

# Identify tidy file name / path
w.meta_allscales_name <- "06_caged_with-metadata_all-scales.csv"
w.meta_allscales_path <- file.path("data", w.meta_allscales_name)

# Export locally
write.csv(x = w.meta_allscales_v2, row.names = F, na = '', file = w.meta_allscales_path)

# # Upload to Drive
# googledrive::drive_upload(media = w.meta_allscales_path, overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))



# Create a new file name
w.meta_allscales_name <- 

focal_out1 <- gsub(pattern = "beta-disp", replacement = "with-metadata", x = focal_file)
focal_out <- gsub(pattern = "05", replacement = "06", x = focal_out1)

# Export locally
write.csv(x = focal_join, na = '', row.names = F,
          file = file.path("data", focal_out))
# Check structure
dplyr::glimpse(beta_allscales)

# Export locally
write.csv(x = beta_allscales, na = '', row.names = F,
          file = file.path("data", paste0(beta_name, "_all-scales.csv")))



# End ----
