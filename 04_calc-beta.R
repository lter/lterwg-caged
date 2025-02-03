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
beta_v1 <- read.csv(file.path("data", "03_caged_filtered.csv"))

# Check structure
dplyr::glimpse(beta_v1)





## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
beta_v99 <- beta_v1

# Identify tidy file name / path
beta_name <- "04_caged_beta-disp.csv"
beta_path <- file.path("data", beta_name)

# Export locally
write.csv(x = beta_v99, row.names = F, na = '', file = beta_path)

# Upload to Drive
googledrive::drive_upload(media = beta_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
