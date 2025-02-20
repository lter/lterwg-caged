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
# Drop Unwanted Columns ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v1)

# Drop any columns we know we don't want at the outset
sub_v2 <- sub_v1 %>% 
  # Supersesed "original" columns (standardized in QC script)
  dplyr::select(-original.treatment, -original.taxa)

# Re-check structure
dplyr::glimpse(sub_v2)

## ------------------------------------------- ##
# Drop Zero-Abundance Samples ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v2)

# Remove 'exp.design.1' levels without any abundance
sub_v3 <- sub_v2 %>% 
  # Average abundance withing experimental design level 1
  dplyr::group_by(
    dplyr::across(dplyr::all_of(setdiff(x = names(.),
                                        y = c("taxa", "abundance"))))
  ) %>% 
  dplyr::mutate(avg.abun = mean(abundance, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Drop any rows where the average is 0 (i.e., no observations of any taxon)
  dplyr::filter(avg.abun > 0) %>% 
  # Ditch column used to do this subsetting
  dplyr::select(-avg.abun)

# Check number of lost rows
message(nrow(sub_v2) - nrow(sub_v3), " rows lost")

# Identify any datasets dropped entirely (shouldn't be any)
setdiff(x = unique(sub_v2$source), y = unique(sub_v3$source))

# Re-check structure
dplyr::glimpse(sub_v3)

## ------------------------------------------- ##
# Handle Sub-Annual Sampling ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v3)

# Do needed processing
sub_v4 <- sub_v3 %>% 
  # Identify cases with more than one sampling point within dataset/year
  dplyr::group_by(source, year) %>% 
  dplyr::mutate(time.ct = length(unique(sampling.point))) %>% 
  dplyr::ungroup() %>% 
  # Filter to only either the _last_ sampling point or any dataset without sub-annual sampling
  dplyr::filter(
    time.ct == 1 |
      (source == "gilson_southafrica_intertidalexclusion_2021_grazers_algae.csv" & 
         sampling.point == "11") | 
      (source == "gilson_southafrica_intertidalexclusion_2021_grazers_inverts.csv" & 
         sampling.point == "12") | 
      (source == "hensel_georgia_brackishhogs_2013-2015_hogs_plants.csv" & 
         sampling.point == "7/5/13") | 
      (source == "pelinson_brazil_predatorisolationcomm_2017_tilapia_insects.csv" & 
         sampling.point == "3")
  ) %>% 
  # Drop "sampling.point" column plus any temporary columns
  dplyr::select(-sampling.point, -time.ct)

# Check number of lost rows
message(nrow(sub_v3) - nrow(sub_v4), " rows lost")

# Identify any datasets dropped entirely (shouldn't be any)
setdiff(x = unique(sub_v3$source), y = unique(sub_v4$source))

# Re-check structure
dplyr::glimpse(sub_v4)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
sub_v99 <- sub_v4

# Identify tidy file name / path
filter_name <- "03_caged_filtered.csv"
filter_path <- file.path("data", filter_name)

# Export locally
write.csv(x = sub_v99, row.names = F, na = '', file = filter_path)

# Upload to Drive
googledrive::drive_upload(media = filter_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
