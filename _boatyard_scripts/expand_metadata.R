## --------------------------------------------------------------- ##
# Boatyard - Expand Metadata
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
# Identify Current Metadata ----
## ------------------------------------------- ##

# Grab the metadata
drive_meta <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AFR2XIdw_sKbUk9PVA")) %>% 
  dplyr::filter(name == "sitelevel-metadata")

# Did that work?
drive_meta

# Download the data meta
googledrive::drive_download(file = drive_meta$id, overwrite = T, type = "csv",
                            path = file.path("data", drive_meta$name))

# Read it in
meta_df <- read.csv(file = file.path("data", "sitelevel-metadata.csv"))

# Check structure
dplyr::glimpse(meta_df)

## ------------------------------------------- ##
# Acquire QC'd Data ----
## ------------------------------------------- ##

# Identify quality controlled data
drive_qc <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")) %>% 
  dplyr::filter(name == "02_caged_tidied.csv")

# Check that worked
drive_qc

# Download the data
googledrive::drive_download(file = drive_qc$id, overwrite = T,
                            path = file.path("data", drive_qc$name))

# Read in data
qc_df <- read.csv(file = file.path("data", "02_caged_tidied.csv"))

# Check structure
dplyr::glimpse(qc_df)

## ------------------------------------------- ##
# Create Partial Metadata for New Files ----
## ------------------------------------------- ##

# Remove data that are in metadata already from QC'd data
meta_expansion <- qc_df %>% 
  dplyr::filter(!source %in% meta_df$source) %>% 
  # Drop all but needed columns
  dplyr::select(source, exp.name) %>% 
  # Drop non-unique rows
  dplyr::distinct() %>% 
  # Add needed column(s)
  dplyr::mutate(assigned.to = NA, .after = source)

# Re-check structure
dplyr::glimpse(meta_expansion)

# Export locally
write.csv(x = meta_expansion, na = '', row.names = F,
          file = file.path("data", paste0(Sys.Date(), "_new-rows-for-metadata_DELETE-AFTER-USE.csv")))

# NOTE TO PERSON RUNNING CODE
## Here's what you should do next:
## 1. Open the metadata GoogleSheet file
## 2. Scroll to bottom (i.e., end of currently filled-out section)
## 3. Open the CSV you just exported above
## 4. Copy/paste all of the two columns in that into the end of the data key
## 5. Delete the CSV once you've copy/pasted the content into the GoogleSheet

# End ----
