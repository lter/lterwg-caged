## --------------------------------------------------------------- ##
# CAGED Harmonization Workflow
## --------------------------------------------------------------- ##
# Written by: Kelly Speare, Nick J Lyon, ...

# Purpose
## this script downloads data from purgatory folder in google drive for all data files that require rangling
## then does necessary wrangling to get the data in the needed format
## then uploads back to google drive

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "purgatory"), showWarnings = F)
dir.create(path = file.path("data", "drydock"), showWarnings = F)

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
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iH9CHW7xS0ZWk7LdB2Glb0LrfJF8dUOL")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv|\\.txt"))

# Did that work?
files_drive

# Download them!
purrr::walk2(.x = files_drive$id, .y = files_drive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

## ------------------------------------------- ##
# Project 1 ----
## ------------------------------------------- ##

# Reason for purgatory status:
## Data are counts of tree seedlings (all baby trees) 
## They differentiated germinants (the newest baby trees from that year) from all seedlings. 
## We just want the data on all seedlings because germinants are a subset of seedlings 
## also want to drop the "ht" (height) data

# Read in data
proj1_raw <- read.csv(file = file.path("data", "purgatory", "allegheny_regen_data.csv"))

# Check structure
dplyr::glimpse(proj1_raw)

# Remove unwanted columns
proj1 <- proj1_raw %>%
  dplyr::select(-dplyr::ends_with(c("_germ", "_ht")))

# Re-check structure
dplyr::glimpse(proj1)

# Gather the better file name
proj1_name <- file.path("data", "drydock", "royo_pennsylvania_allegheny_2000-2010_ungulate_forest.csv")

# Export renamed CSV to data/drydock 
write.csv(x = proj1, na = "", row.names = F, file = proj1_name) 

# Upload to Drive
googledrive::drive_upload(media = proj1_name, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))

## ------------------------------------------- ##
# Project 2 ----
## ------------------------------------------- ##

## CONFUSED: need to contact these authors

# Reason for purgatory status:
## data includes several biomass types, including above and below ground biomass. 
## we only care about above ground biomass
## subset to exclude  "below", "litter", "vole" and "wood"
## also excluding vole and wood because they are within the "litter"
## decided to sum the above ground biomass types "new above" and "old above"

# Read in data
proj2_raw <- read.csv(file = file.path("data", "purgatory", "2006lgdhbmcn.csv"))

# Check structure
dplyr::glimpse(proj2_raw)

# DEPRECATED: 
# proj2<-subset(proj2, Biomass.type!="below")
# proj2 <- proj2 %>% pivot_longer(names_to = "Block.quad", values_to = "Dry.weight")

# UNDER CONSTRUCTION

## ------------------------------------------- ##
# Project 3 ----
## ------------------------------------------- ##

# Output list
proj3_list <- list()

# Loop across relevant files
for(proj_file in dir(path = file.path("data", "purgatory"), pattern = "_Ranktime_")){
  
  # Processing message
  message("Grabbing file '", proj_file, "'")
  
  # Read in data and pivot longer
  proj_df <- read.delim(file=file.path("data", "purgatory", proj_file)) %>% 
    tidyr::pivot_longer(cols = -Species,
                        names_to="Timepoint",
                        values_to="Abundance") %>%
    mutate(input_file=proj_file, .before=everything())
  
  # Read in data and assign to list
  proj3_list[[proj_file]] <- proj_df
  
}

# Unlist output & do needed wrangling
proj3 <- proj3_list %>% 
  purrr::list_rbind(x = .) %>% 
  dplyr::mutate(Timepoint = gsub(pattern = "_closed", replacement = "_closed_",
                                 x = Timepoint),
                Timepoint = gsub(pattern = "_open", replacement = "_open_",
                                 x = Timepoint)) %>% 
  tidyr::separate_wider_delim(cols = input_file, delim="_",cols_remove=F, 
                              names=c("site", "junk2", "junk3")) %>% 
  tidyr::separate_wider_delim(cols = Timepoint, delim="_", 
                              names=c("junk", "Treatment", "Time")) %>% 
  dplyr::select(-contains("junk"))

# Check structure
dplyr::glimpse(proj3)

# for loop to save each site as a separate file
for(focalsite in unique(proj3$site)){
  
  #subset data to just this file (subset data to each of these sites)
  proj3_sub <- dplyr::filter(proj3, site==focalsite)
  
  #assemble better filename
  proj3_subname <- paste0("villar_brazil_",  tolower(unique(proj3_sub$site)), 
                          "_2009-2016_tapirs_forest.csv")
  
  proj3_subname_name<-file.path("data", "drydock", proj3_subname)

  
  #export renamed csv to data/drydock 
  write.csv(proj1, na="", row.names=F, file=proj3_subname_name) 
  
  # Upload to Drive
  googledrive::drive_upload(media = proj3_subname_name, overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))
  
}



## ------------------------------------------- ##
# Project 4 ----
## ------------------------------------------- ##
