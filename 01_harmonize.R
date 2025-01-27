## --------------------------------------------------------------- ##
                          # CAGED Harmonization
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, googledrive)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "raw"), showWarnings = F)

## ------------------------------------------- ##
# Download Data ----
## ------------------------------------------- ##

# NOTE
## This script assumes (1) access to the "LTER-WG_CAGED" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv|\\.txt"))

# Did that work?
files_drive

# Download them!
purrr::walk2(.x = files_drive$id, .y = files_drive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw", .y)))

## ------------------------------------------- ##
# Harmonize! ----
## ------------------------------------------- ##

# Will use the follwing function
?ltertools::harmonize








# End ----
