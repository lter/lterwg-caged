## --------------------------------------------------------------- ##
                  # Boatyard - Expand Column Key
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, googledrive)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "raw"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Identify Current Raw Data ----
## ------------------------------------------- ##

# Identify raw data files
drive_raw <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
drive_raw

# Download those files
purrr::walk2(.x = drive_raw$id, .y = drive_raw$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw", .y)))

## ------------------------------------------- ##
# Acquire Data Key ----
## ------------------------------------------- ##

# Grab the data key
drive_key <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M")) %>% 
  dplyr::filter(name == "caged_data-key")

# Did that work?
drive_key

# Download the data key
googledrive::drive_download(file = drive_key$id, overwrite = T, type = "csv",
                            path = file.path("data", drive_key$name))

# Read it in
key_df <- read.csv(file = file.path("data", "caged_data-key.csv"))

# Check structure
dplyr::glimpse(key_df)

## ------------------------------------------- ##
# Acquire Harmonized Data ----
## ------------------------------------------- ##

# Identify harmonized data
drive_harmony <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")) %>% 
  dplyr::filter(name == "01_caged_harmonized.csv")

# Check that worked
drive_harmony

# Download the harmonized data
googledrive::drive_download(file = drive_harmony$id, overwrite = T,
                            path = file.path("data", drive_harmony$name))

# Read in harmonized data
harmony_df <- read.csv(file = file.path("data", "01_caged_harmonized.csv"))

# Check structure
dplyr::glimpse(harmony_df)

## ------------------------------------------- ##
# Create Partial Data Key for New Files ----
## ------------------------------------------- ##

# Create data key for all raw data
key_full <- ltertools::begin_key(raw_folder = file.path("data", "raw"),
                                 data_format = "csv", guess_tidy = F)


# Check structure
dplyr::glimpse(key_full)

# Pare down to just data that are not (yet) harmonized
key_expansion <- key_full %>% 
  dplyr::filter(!source %in% harmony_df$source) %>% 
  dplyr::filter(!source %in% key_df$source) %>% 
  dplyr::select(-tidy_name)

# Re-check structure
dplyr::glimpse(key_expansion)

# Export locally
write.csv(x = key_expansion, na = '', row.names = F,
          file = file.path("data", paste(Sys.Date(), "new-rows-for-key_DELETE-AFTER-USE.csv")))

# NOTE TO PERSON RUNNING CODE
## Here's what you should do next:
## 1. Open the data key GoogleSheet file
## 2. Scroll to bottom (i.e., end of currently filled-out section)
## 3. Open the CSV you just exported above
## 4. Copy/paste all of the two columns in that into the end of the data key
## 5. Delete the CSV once you've copy/pasted the content into the GoogleSheet

# End ----
