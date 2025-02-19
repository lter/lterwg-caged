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
tidy_v3 <- tidy_v2 %>% 
  # Conditionally change any that need changing
  dplyr::mutate(exp.design.1 = dplyr::case_when(
    
    T ~ exp.design.1)) %>% 
  # Fill any missing values with experiment name
  ## (All have 'exp.design.1' but not necessarily all have higher levels)
  dplyr::mutate(
    exp.design.2 = ifelse(nchar(exp.design.2) == 0 | is.na(exp.design.2),
                          yes = experiment.name, no = exp.design.2),
    exp.design.3 = ifelse(nchar(exp.design.3) == 0 | is.na(exp.design.3),
                          yes = experiment.name, no = exp.design.3)
    )

# Re-check
tidy_v3 %>% 
  dplyr::select(organization, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct()

## ------------------------------------------- ##
# Standardize Taxon Names ----
## ------------------------------------------- ##

# Check current taxa names
sort(unique(tidy_v3$original.taxa))

# Do desired wrangling
tidy_v4 <- tidy_v3 %>% 
  dplyr::mutate(original.taxa = dplyr::case_when(
    
    T ~ original.taxa))

# Re-check taxa names
sort(unique(tidy_v4$original.taxa))

## ------------------------------------------- ##
# Standardize Study Years ----
## ------------------------------------------- ##

# Check current years
tidy_v4 %>% 
  dplyr::group_by(source, sampling.years) %>% 
  dplyr::summarize(years = paste(unique(year), collapse = ", "))

# Fill in missing years as appropriate
tidy_v5 <- tidy_v4 %>% 
  dplyr::mutate(year = dplyr::case_when(
    !is.na(year) ~ as.character(year),
    source %in% c("hensel_georgia_brackishhogs_2013-2015_hogs_plants.csv") ~ paste0(sampling.point, "_"),
    T ~ sampling.years)) %>% 
  # Do any needed post-processing
  dplyr::mutate(year = dplyr::case_when(
    stringr::str_detect(string = year, pattern = "\\/") ~ paste0("20", gsub(pattern = "\\/|_", replacement = "", x = stringr::str_extract(string = year, pattern = "\\/[:digit:]{2}_"))),
    T ~ year))

# Re-check years
tidy_v5 %>% 
  dplyr::group_by(source, sampling.years) %>% 
  dplyr::summarize(years = paste(unique(year), collapse = ", "))


## ------------------------------------------- ##
# Standardize Misc. Other Variables ----
## ------------------------------------------- ##

# Re-check structure
dplyr::glimpse(tidy_v5)

# Do desired standardization
tidy_v6 <- tidy_v5 %>% 
  # Standardize casing for distance from surface
  dplyr::mutate(distance.from.surface = tolower(distance.from.surface))

# Re-check structure
dplyr::glimpse(tidy_v6)

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
tidy_v7 <- tidy_v6 %>% 
  dplyr::left_join(y = meta_v2, by = c("source")) %>% 
  # Re-arrange slightly
  dplyr::relocate(region:resource.taxa, 
                  .after = measured.group)

# Check structure
dplyr::glimpse(tidy_v7)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v7

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
