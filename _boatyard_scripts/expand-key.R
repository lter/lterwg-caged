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
# Download Harmonized Data ----
## ------------------------------------------- ##

# Identify harmonized data
drive_harmony <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")) %>% 
  dplyr::filter(name == "caged_harmonized.csv")

# Check that worked
drive_harmony

# Download the harmonized data
googledrive::drive_download(file = drive_harmony$id, overwrite = T, type = "csv",
                            path = file.path("data", drive_harmony$name))




# End ----
