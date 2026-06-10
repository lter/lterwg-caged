## --------------------------------------------------------------- ##
# Boatyard - Expand Metadata
## --------------------------------------------------------------- ##
# Purpose
## Create new rows for copy/pasting into the 'sitelevel-metadata' GoogleSheet

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, googledrive)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Identify Current Metadata ----
## ------------------------------------------- ##

# Read in metadata (re-download with script `000` if needed)
meta_df <- read.csv(file = file.path("data", "sitelevel-metadata.csv"))

# Check structure
dplyr::glimpse(meta_df)

## ------------------------------------------- ##
# Acquire QC'd Data ----
## ------------------------------------------- ##

# Read in QC'd data (re-run script `02` or re-download with script `000` if needed)
qc_df <- read.csv(file = file.path("data", "02_caged_tidied.csv")) %>% 
  # Keep only needed columns
  dplyr::select(source, exp.name) %>% 
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(qc_df)

## ------------------------------------------- ##
# Create Partial Metadata for New Files ----
## ------------------------------------------- ##

# Remove data that are in metadata already from QC'd data
meta_expansion <- qc_df %>% 
  dplyr::filter(!source %in% meta_df$source)

# Re-check structure
dplyr::glimpse(meta_expansion)

# Export locally (if any rows need to be added)
if(nrow(meta_expansion) != 0){
  write.csv(x = meta_expansion, na = '', row.names = F,
            file = file.path("data", paste0(Sys.Date(), "_new-rows-for-metadata_DELETE-AFTER-USE.csv")))
}

# NOTE TO PERSON RUNNING CODE:
## Here's what you should do next:
## 1. Open the metadata GoogleSheet file
## 2. Scroll to bottom (i.e., end of currently filled-out section)
## 3. Open the CSV you just exported above
## 4. Copy/paste all of the two columns in that into the end of the data key
## 5. Delete the CSV once you've copy/pasted the content into the GoogleSheet

## ------------------------------------------- ##
# Identify "exp.name" Mismatches ----
## ------------------------------------------- ##

# Identify files that are in both the data and metadata BUT have diff "exp.name" values
meta_check <- qc_df %>% 
  dplyr::filter(source %in% meta_df$source &
                  !exp.name %in% meta_df$exp.name)

# Re-check structure
dplyr::glimpse(meta_check)

# Export locally (if any rows need to be added)
if(nrow(meta_check) != 0){
  write.csv(x = meta_check, na = '', row.names = F,
            file = file.path("data", paste0(Sys.Date(), "_exp.name-values-to-check-in-metadata_DELETE-AFTER-USE.csv")))
}

# NOTE TO PERSON RUNNING CODE:
## Here's what you should do next:
## 1. Open the metadata GoogleSheet file
## 2. Open the CSV(s) you just exported above
## 3. Check the "exp.name" values in the Sheet that are in the CSV
### Where possible, update the **GoogleSheet** to match the CSV!
## 4. Delete the CSV once you've copy/pasted the content into the GoogleSheet

# End ----
