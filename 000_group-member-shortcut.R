## --------------------------------------------------------------- ##
# CAGED Shortcut
## --------------------------------------------------------------- ##

# Purpose:
## Group members may want a "shortcut" so they don't need to run the full workflow
## This script downloads the outputs of core scripts from Drive
## Allowing a user to skip numbered workflow scripts before the one that they are interested in

# Note:
## This script assumes that you (the person running the script):
### 1. Have access to the group's Shared Drive
### 2. Have adopted GitHub (so that you have the right working directory)

# For more information on authentication, see the following tutorial:
## https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls()); gc()

## ------------------------------------------- ##
# Create Local Folders ----
## ------------------------------------------- ##

# Create needed folders
source(file = file.path("00_setup.R"))

## ------------------------------------------- ##
# Download Core Workflow Tidy Outputs ----
## ------------------------------------------- ##

# Identify outputs
core_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# Looks okay?
core_drive

# Download 'em all (overwriting your local versions)
purrr::walk2(.x = core_outs$id, .y = core_outs$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", .y)))

# Clear environment
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Data Key ----
## ------------------------------------------- ##
                               
# Identify key
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M")) %>% 
  dplyr::filter(name == "caged_data-key")

# Did that work?
key_drive

# Download the data key
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                          path = file.path("data", key_drive$name))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Metadata GoogleSheet ----
## ------------------------------------------- ##

# Identify the relevant GoogleSheet
meta_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AFR2XIdw_sKbUk9PVA")) %>% 
  dplyr::filter(name == "sitelevel-metadata")

# Check that worked
meta_drive

# Download it
googledrive::drive_download(file = meta_drive$id, type = "csv", overwrite = T,
                            path = file.path("data", meta_drive$name))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Raw Data ----
## ------------------------------------------- ##

# Identify wanted files
raw_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
raw_drive

# Identify local files
raw_local <- dir(path = file.path("data", "raw"))
raw_local

# Overwrite local data files?
raw_update <- FALSE

# Identify desired files
if(raw_update == T) {
  raw_wanted <- raw_drive 
} else {
  raw_wanted <- raw_drive %>%
    dplyr::filter(!name %in% raw_local)
}

# What does that leave you with?
raw_wanted

# Download them!
purrr::walk2(.x = raw_wanted$id, .y = raw_wanted$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw", .y)))

# Clear environment + collect garbage
rm(list = ls()); gc()
                                                 
# End ----
