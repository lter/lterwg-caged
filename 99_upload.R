## --------------------------------------------------------------- ##
# CAGED Wrangling & Quality Control
## --------------------------------------------------------------- ##

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive) 

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Upload Data to Drive ----
## ------------------------------------------- ##

# Identify tidy data Drive folder link
drive_tidy <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")

# Upload harmonized data
## As of 9/18/25 this file is >500 MB so better to upload manually
# googledrive::drive_upload(media = file.path("data", "01_caged_harmonized.csv"),
#                           overwrite = T, path = drive_tidy)

# Upload QC'd data
## As of 9/18/25 this file is nearly 1 GB so better to upload manually
# googledrive::drive_upload(media = file.path("data", "02_caged_tidied.csv"),
#                           overwrite = T, path = drive_tidy)

# Upload filtered data
## As of 9/18/25 this file is ~300 MB so better to upload manually
# googledrive::drive_upload(media = file.path("data", "03_caged_filtered.csv"),
#                           overwrite = T, path = drive_tidy)

# Upload zero-filled data
## As of 9/18/25 this file is ~300 MB so better to upload manually
# googledrive::drive_upload(media = file.path("data", "04_caged_zero-filled.csv"),
#                           overwrite = T, path = drive_tidy)

# Upload 'all scales' and 'finest scales' beta dispersion
purrr::walk(.x = dir(path = file.path("data"), pattern = "05-A_caged_beta-disp"),
            .f = ~ googledrive::drive_upload(media = file.path("data", .x), 
                                             overwrite = T, path = drive_tidy))

# Upload gamma richness data
googledrive::drive_upload(media = file.path("data", "05-B_caged_gamma-rich.csv"),
                          overwrite = T, path = drive_tidy)

# Upload 'all scales' and 'finest scales' beta dispersion mean differences
purrr::walk(.x = dir(path = file.path("data"), pattern = "06_caged_mean-beta-diff"),
            .f = ~ googledrive::drive_upload(media = file.path("data", .x), 
                                             overwrite = T, path = drive_tidy))

# Upload 'all scales' and 'finest scales' full data with metadata
purrr::walk(.x = dir(path = file.path("data"), pattern = "07_caged_w.meta"),
            .f = ~ googledrive::drive_upload(media = file.path("data", .x), 
                                             overwrite = T, path = drive_tidy))

# End ----
