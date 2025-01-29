
library(tidyverse)

## this script downloads data from purgatory folder in google drive for all data files that require rangling
## then does necessary wrangling to get the data in the needed format
## then uploads back to google drive

# Create needed folder(s)
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

# data are counts of tree seedlings (all baby trees) 
# they differentiated germinants (the newest baby trees from that year) from all seedlings. 
# We just want the data on all seedlings because germinants are a subset of seedlings 
# also want to drop the "ht" (height) data

proj1<-read.csv(file = file.path("data","purgatory","allegheny_regen_data.csv"))

proj1 <- proj1 %>% select(-dplyr::ends_with(c("_germ", "_ht")))

#export renamed csv to data/drydock 
write.csv(proj1, file=file.path("data", "drydock", "royo_pennsylvania_allegheny_2000-2010_ungulate_forest.csv")) 


## ------------------------------------------- ##
# Project 2 ----
## ------------------------------------------- ##

# CONFUSED: need to contact these authors

# proj2<-read.csv("data/purgatory/2006lgdhbmcn.csv", stringsAsFactors = TRUE)
# 
# levels(proj2$Biomass.Type)
# # data includes several biomass types, including above and below ground biomass. 
# #we only care about above ground biomass
# # subset to exclude  "below", "litter", "vole" and "wood"
# # also excluding vole and wood because they are within the "litter"
# # decided to sum the above ground biomass types "new above" and "old above"
# 
# 
# proj2<-subset(proj2, Biomass.type!="below")
# 
# proj2 <- proj1 %>% pivot_longer(names_to = "Block.quad", values_to = "Dry.weight")


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
                        values_to="Abundance")
  
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
  tidyr::separate_wider_delim(cols = Timepoint, delim="_", 
                              names=c("junk", "Treatment", "Time")) %>% 
  dplyr::select(-junk)

# Check structure
dplyr::glimpse(proj3)


#export renamed csv to data/drydock 
write.csv(proj3, file=file.path("data", "drydock", "villar_brazil_vallar_2009-2016_tapirs_forest.csv")) 


## ------------------------------------------- ##
# Project 4 ----
## ------------------------------------------- ##
