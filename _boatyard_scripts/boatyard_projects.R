
library(tidyverse)

## this script downloads data from purgatory folder in google drive for all data files that require rangling
## then does necessary wrangling to get the data in the needed format
## then uploads back to google drive

# Create needed folder(s)
dir.create(path = file.path("data", "purgatory"), showWarnings = F)
dir.create(path = file.path("data", "drydock"), showWarnings = F)

## ------------------------------------------- ##
# Download Data ----
## ------------------------------------------- ##

# NOTE
## This script assumes (1) access to the "LTER-WG_CAGED" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iH9CHW7xS0ZWk7LdB2Glb0LrfJF8dUOL")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
files_drive

# Download them!
purrr::walk2(.x = files_drive$id, .y = files_drive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))


## ------------------------------------------- ##
# Project 1 ----
# https://github.com/lter/lterwg-caged/issues/5#issuecomment-2619777550
## ------------------------------------------- ##

proj1<-read.csv("data/purgatory/1999gsexclosbm.csv", stringsAsFactors = TRUE)

# data includes above and below ground biomass. we only care about above ground biomass
# subset to exclude below ground biomass

proj1<-subset(proj1, Biomass.type!="below")

proj1 <- proj1 %>% pivot_longer(names_to = "Block.quad", values_to = "Dry.weight")






