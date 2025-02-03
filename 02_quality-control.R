## --------------------------------------------------------------- ##
                # CAGED Wrangling & Quality Control
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
tidy_v1 <- read.csv(file.path("data", "01_caged_harmonized.csv"))

# Check structure
dplyr::glimpse(tidy_v1)

## ------------------------------------------- ##
# Standardize Treatments ----
## ------------------------------------------- ##

# Check current treatments
tidy_v1 %>% 
  dplyr::select(source, organization, original.treatment) %>% 
  dplyr::distinct()

# Perform needed standardization
tidy_v2 <- tidy_v1
## NOTE
### LEAVING ALONE (FOR NOW)
### Need to discuss with group

# Check standardized treatments
tidy_v2 %>% 
  dplyr::select(source, organization, original.treatment) %>% 
  dplyr::distinct()

## ------------------------------------------- ##
# Standardize Experimental Design Facets ----
## ------------------------------------------- ##

# Check experimental design columns
tidy_v2 %>% 
  dplyr::select(organization, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct()

# Do needed standardization
tidy_v3 <- tidy_v2
## NOTE
### LEAVING ALONE (FOR NOW)
### Need to discuss with group

# Re-check
tidy_v3 %>% 
  dplyr::select(organization, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct()

## ------------------------------------------- ##
# Standardize Misc. Other Variables ----
## ------------------------------------------- ##

# Re-check structure
dplyr::glimpse(tidy_v3)

# Do desired standardization
tidy_v4 <- tidy_v3 %>% 
  # Standardize casing for distance from surface
  dplyr::mutate(distance.from.surface = tolower(distance.from.surface))

# Re-check structure
dplyr::glimpse(tidy_v4)

## ------------------------------------------- ##
# Download Group-Defined Metadata ----
## ------------------------------------------- ##

# Find metadata
meta_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AFR2XIdw_sKbUk9PVA")) %>% 
  dplyr::filter(name == "Data Sources- detailed")

# Check it
meta_drive

# Download it locally
googledrive::drive_download(file = meta_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", "caged_metadata"))

## ------------------------------------------- ##
# Wrangle Metadata ----
## ------------------------------------------- ##

# Read in metadata
meta_v1 <- read.csv(file = file.path("data", "caged_metadata.csv"))

# Do needed wrangling
meta_v2 <- meta_v1 %>% 
  # Rename file name column
  dplyr::rename(source = File.name) %>% 
  # Standardize entries of desired column(s) slightly
  dplyr::mutate(region = tolower(Region),
                lter.site = tolower(LTER),
                ecosystem = tolower(Ecosystem),
                consumer.taxa = tolower(Consumer.Taxa),
                resource.taxa = tolower(Resource.Taxa)) %>% 
  # Pare down to desired column(s)
  dplyr::select(source, region, lter.site, ecosystem, consumer.taxa, resource.taxa) %>% 
  # Remove rows without a file name
  dplyr::filter(is.na(source) != T & nchar(source) != 0) %>% 
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(meta_v2)

# Make sure there's only one row per dataset
meta_v2 %>% 
  dplyr::group_by(source) %>% 
  dplyr::mutate(row.ct = dplyr::n()) %>% 
  dplyr::filter(row.ct != 1)

## ------------------------------------------- ##
# Attach Metadata ----
## ------------------------------------------- ##

# Attach metadata to QC'd data
tidy_v5 <- tidy_v4 %>% 
  dplyr::left_join(y = meta_v2, by = c("source")) %>% 
  # Re-arrange slightly
  dplyr::relocate(region:resource.taxa, 
                  .after = measured.group)

# Check structure
dplyr::glimpse(tidy_v5)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v5

# Check structure
dplyr::glimpse(tidy_v99)

# Identify tidy file name / path
tidy_name <- "02_caged_tidied.csv"
tidy_path <- file.path("data", tidy_name)

# Export locally
write.csv(x = tidy_v99, row.names = F, na = '', file = tidy_path)

# Upload to Drive
googledrive::drive_upload(media = tidy_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
