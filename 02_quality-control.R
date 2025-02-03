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
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v1

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
