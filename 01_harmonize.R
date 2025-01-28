## --------------------------------------------------------------- ##
                  # CAGED Harmonization Workflow
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
# Download Data ----
## ------------------------------------------- ##

# NOTE
## This script assumes (1) access to the "LTER-WG_CAGED" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
files_drive

# Download them!
purrr::walk2(.x = files_drive$id, .y = files_drive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw", .y)))

# Grab the data key
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M")) %>% 
  dplyr::filter(name == "caged_data-key")

# Did that work?
key_drive

# Download the data key
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", key_drive$name))

## ------------------------------------------- ##
# Harmonize! ----
## ------------------------------------------- ##

# Read in data key
key <- read.csv(file = file.path("data", "caged_data-key.csv"))

# Check that looks roughly right
dplyr::glimpse(key)

# Perform harmonization
combo_v1 <- ltertools::harmonize(key = key, 
                                 raw_folder = file.path("data", "raw"),
                                 data_format = "csv", quiet = F)

# Check that structure out
dplyr::glimpse(combo_v1)

## ------------------------------------------- ##
# Wrangle Long vs. Wide Communities ----
## ------------------------------------------- ##

# Need to separate long/wide data to handle 0s/missing data
combo_v2 <- combo_v1 %>% 
  # Generate 'flag' for long versus wide data
  dplyr::group_by(source) %>% 
  dplyr::mutate(data_are_long = any(all(!is.na(orig.species)),
                                    all(!is.na(orig.function)) ) )

# Separate long and wide data
long_split <- combo_v2 %>% 
  dplyr::filter(data_are_long == TRUE) %>% 
  dplyr::select(-data_are_long)

wide_split <- combo_v2 %>% 
  dplyr::filter(data_are_long == FALSE) %>% 
  dplyr::select(-data_are_long)

# Check that's the right number of rows
nrow(combo_v2) == nrow(long_split) + nrow(wide_split)

# Process long data
for(focal_source in unique(long_split$source)){
  
  # Print progress message
  message("Zero filling dataset: '", focal_source, "'")
  
  # Subset to focal dataset
  focal_sub <- long_split %>% 
    dplyr::filter(source == focal_source) %>% 
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.))))
  
  # Pivot to wide format (filling with zeros on the way)
  focal_flip <- focal_sub
  
}






# Check structure
dplyr::glimpse(combo_v2)


long_split <- combo_v1 %>% 
  group_by(source) %>% 
  dplyr::mutate(long_flag = )

# Check structure
dplyr::glimpse(combo_v2)

filter(combo_v2, long_flag == T) %>% view()

## ------------------------------------------- ##
# Wrangle Wide Community Data ----
## ------------------------------------------- ##

names(combo_v1)

# Pivot wide format data to long format
combo_v2 <- combo_v1 %>%
  




## ------------------------------------------- ##
            # Wrangle - "Wide" Data ----
## ------------------------------------------- ##

# Need to handle data that were previously in wide format
combo_v2 <- combo_v1 %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("orig.taxa_"),
                      names_to = "original.taxon",
                      values_to = "abundance_wide") %>% 
  ## Remove placeholder column prefix
  dplyr::mutate(original.taxon = gsub(pattern = "orig.taxa_",
                                      replacement = "",
                                      x = original.taxon)) %>% 
  ## Combine 'abundance' columns
  dplyr::rename(abundance_long = abundance) %>% 
  ## Standardize missing values
  dplyr::mutate(abundance_long = ifelse(nchar(abundance_long) == 0,
                                        yes = NA, no = abundance_long),
                abundance_wide = ifelse(nchar(abundance_wide) == 0,
                                        yes = NA, no = abundance_wide)) %>% 
  ## Combine abundance columns
  dplyr::mutate(abundance = ifelse(is.na(abundance_long),
                                   yes = abundance_wide,
                                   no = abundance_long)) %>% 
  ## Drop superseded columns
  dplyr::select(-dplyr::starts_with("abundance_"))

# Check only desired columns are lost
setdiff(x = names(combo_v1), y = names(combo_v2))

# Re-check structure
dplyr::glimpse(combo_v2)

## ------------------------------------------- ##
# Wrangle - Column Re-Ordering ----
## ------------------------------------------- ##

# Reorder columns more logically
combo_v3 <- combo_v2 %>% 
  # Treatment information first
  dplyr::relocate(dplyr::contains("orig.treat"),
                  .after = source) %>% 
  # Spatial scale (lower numbers are more granular)
  dplyr::relocate(spatial.scale.4, spatial.scale.3,
                  spatial.scale.2, spatial.scale.1,
                  depth, .after = year) %>% 
  # Taxon information after spatial information
  dplyr::relocate(original.taxon, orig.function, orig.species,
                  .after = depth)

# Check structure
dplyr::glimpse(combo_v3)

# Check that no columns are lost
setdiff(x = names(combo_v2), y = names(combo_v3))

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
combo_v4 <- combo_v3 %>% 
  # Drop duplicate rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(combo_v4)
  
# Export locally
write.csv(x = combo_v4, row.names = F, na = '',
          file = file.path("data", "caged_harmonized.csv"))

# End ----
