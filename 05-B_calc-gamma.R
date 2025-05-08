## --------------------------------------------------------------- ##
# CAGED Gamma Richness Calculation
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, magrittr, vegan, supportR)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
gamma_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(gamma_v1)

## ------------------------------------------- ##
# Calculate Gamma Richness ----
## ------------------------------------------- ##

# Do needed wrangling
gamma_v2 <- gamma_v1 %>% 
  # Drop unwanted columns
  dplyr::select(-dplyr::starts_with(c("exp.design.", "cage.treatment")), -abundance) %>% 
  # Group by only desired columns & count number of unique taxa
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("year", "taxa", "abundance"))) 
    )) %>% 
  dplyr::summarize(gamma.richness = length(unique(taxa)),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Do we have the expected number of values?
nrow(gamma_v2) == length(unique(paste(gamma_v2$source, gamma_v2$exp.name)))

# Check structure
dplyr::glimpse(gamma_v2)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
gamma_v99 <- gamma_v2

# Identify tidy file name / path
gamma_name <- "05-B_caged_gamma-rich.csv"
gamma_path <- file.path("data", gamma_name)

# Export locally
write.csv(x = gamma_v99, row.names = F, na = '', file = gamma_path)

# # Upload to Drive
# googledrive::drive_upload(media = gamma_path, overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
