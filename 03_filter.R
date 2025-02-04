## --------------------------------------------------------------- ##
                        # CAGED Filtering
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
sub_v1 <- read.csv(file.path("data", "02_caged_tidied.csv"))

# Check structure
dplyr::glimpse(sub_v1)

## ------------------------------------------- ##
# Drop Zero-Abundance Samples ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v1)

# Remove 'exp.design.1' levels without any abundance
sub_v2 <- sub_v1 %>% 
  # Average abundance withing experimental design level 1
  dplyr::group_by(
    dplyr::across(dplyr::all_of(setdiff(x = names(.),
                                        y = c("original.taxa", "abundance"))))
  ) %>% 
  dplyr::mutate(avg.abun = mean(abundance, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Drop any rows where the average is 0 (i.e., no observations of any taxon)
  dplyr::filter(avg.abun > 0) %>% 
  # Ditch column used to do this subsetting
  dplyr::select(-avg.abun)

# Check number of lost rows
message(nrow(sub_v1) - nrow(sub_v2), " rows lost")

# Re-check structure
dplyr::glimpse(sub_v2)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
sub_v99 <- sub_v2

# Identify tidy file name / path
filter_name <- "03_caged_filtered.csv"
filter_path <- file.path("data", filter_name)

# Export locally
write.csv(x = sub_v99, row.names = F, na = '', file = filter_path)

# Upload to Drive
googledrive::drive_upload(media = filter_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
