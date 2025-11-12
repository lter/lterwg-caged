## --------------------------------------------------------------- ##
# CAGED Harmonization Workflow
## --------------------------------------------------------------- ##
# Written by: Kelly Speare, Nick J Lyon, ...

# Purpose
## this script downloads data from purgatory folder in google drive for all data files that require rangling
## then does necessary wrangling to get the data in the needed format
## then uploads back to google drive

# NOTE
## This script assumes (1) access to the "LTER-WG_CAGED" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR, readxl)

# Create needed folders
source(file = file.path("00_setup.R"))
dir.create(path = file.path("data", "purgatory"), showWarnings = F)
dir.create(path = file.path("data", "drydock"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 1 (Royo PA) ----
## ------------------------------------------- ##

# Reason for purgatory status:
## Data are counts of tree seedlings (all baby trees) 
## They differentiated germinants (the newest baby trees from that year) from all seedlings. 
## We just want the data on all seedlings because germinants are a subset of seedlings 
## also want to drop the "ht" (height) data

# Identify file(s) name(s)
proj1_raw_name <- "allegheny_regen_data.csv"

# Identify file(s) in Drive
proj1_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1YBZyynyjvQ8dliloVB1i9iH-IqKXBsvZ")) %>% 
  dplyr::filter(name %in% c(proj1_raw_name))

# Download file(s)
purrr::walk2(.x = proj1_gdrive$id, .y = proj1_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj1_raw <- read.csv(file = file.path("data", "purgatory", proj1_raw_name))

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
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 2 (Lamb Galapagos) ----
## ------------------------------------------- ##
# Reason for purgatory status
## we need Locality and Season as one column so it can be experiment name

# Identify file(s) name(s)
proj2_raw_name <- "biomass.csv"

# Identify file(s) in Drive
proj2_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1fgcDOopAeYNWYdUWSFcdMuP1IW1kYeCg")) %>% 
  dplyr::filter(name %in% c(proj2_raw_name))

# Download file(s)
purrr::walk2(.x = proj2_gdrive$id, .y = proj2_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj2_raw <- read.csv(file.path("data", "purgatory", proj2_raw_name))

# Check structure
dplyr::glimpse(proj2_raw)

# Do needed repairs
proj2 <- proj2_raw %>% 
  # Concatenate experiment context columns
  dplyr::mutate(Experiment = paste0(Locality, "-", Season),
                .before = dplyr::everything()) %>% 
  # Drop unwanted column(s)
  dplyr::select(-X)

# Re-check structure
dplyr::glimpse(proj2)

# Check gained/lost columns
supportR::diff_check(old = names(proj2_raw), new = names(proj2))

# Create good/new file name
proj2_name <- "lamb_galapagos_consumermobility_2017_fish-urchins_algae.csv"
proj2_path <- file.path("data", "drydock", proj2_name)

# Export locally
write.csv(x = proj2, file = proj2_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj2_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 3 (Villar Brazil) ----
## ------------------------------------------- ##

# Reason for purgatory status:
## Treatments split into separate data files that need to be combined

# Identify file(s) in Drive
proj3_gdrive <- dplyr::bind_rows(
  ## Looks different because pulling from several different Drive folders
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FAOlV1D79jrVbqrYRuRuwjvObGWrW2wI")),
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1hnYvUf3R97MT6qYSR7c4YP6h0hbB6YD2")),
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1IJNyY6BtaiEokxVgWASIGIOY2k_GiYxb"))) %>% 
  dplyr::filter(stringr::str_detect(string = name, pattern = "Ranktime"))

# Download file(s)
purrr::walk2(.x = proj3_gdrive$id, .y = proj3_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Output list
proj3_list <- list()

# Loop across relevant files
for(proj3_file in dir(path = file.path("data", "purgatory"), pattern = "_Ranktime_")){
  
  # Processing message
  message("Grabbing file '", proj3_file, "'")
  
  # Read in data and pivot longer
  proj3_df <- read.delim(file = file.path("data", "purgatory", proj3_file)) %>% 
    tidyr::pivot_longer(cols = -Species,
                        names_to = "Timepoint",
                        values_to = "Abundance") %>%
    dplyr::mutate(input_file = proj3_file, .before = dplyr::everything())
  
  # Read in data and assign to list
  proj3_list[[proj3_file]] <- proj3_df
  
}

# Unlist output & do needed wrangling
proj3 <- proj3_list %>% 
  purrr::list_rbind(x = .) %>% 
  dplyr::mutate(Timepoint = gsub(pattern = "_closed", replacement = "_closed_",
                                 x = Timepoint),
                Timepoint = gsub(pattern = "_open", replacement = "_open_",
                                 x = Timepoint)) %>% 
  tidyr::separate_wider_delim(cols = Timepoint, delim = "_", 
                              names = c("junk", "Treatment", "Time")) %>% 
  tidyr::separate_wider_delim(cols = input_file, delim = "_",cols_remove = F, 
                              names = c("site", "junk2", "junk3")) %>% 
  dplyr::select(-contains("junk")) %>% 
  
# Check structure
dplyr::glimpse(proj3)

# Create good/new file name
proj3_name <- "villar_brazil_car-cbo-ita_2009-2016_tapirs_forest.csv"
proj3_path <- file.path("data", "drydock", proj3_name)

# Export locally
write.csv(x = proj3, file = proj3_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj3_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 4 (LTER GCE) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Triple header rows

# Identify file(s) name(s)
proj4_raw_name <- "MSH-GCED-2308_Experiment_1_0.CSV"

# Identify file(s) in Drive
proj4_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Q5a3ZYxFbaNAOp_GI55bdmcyY1boErYE")) %>% 
  dplyr::filter(name %in% c(proj4_raw_name))

# Download file(s)
purrr::walk2(.x = proj4_gdrive$id, .y = proj4_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj4_raw <- read.csv(file = file.path("data", "purgatory", proj4_raw_name),
                      row.names = NULL)

# Check structure
dplyr::glimpse(proj4_raw)

# Do needed repair
proj4_prep <- proj4_raw %>% 
  # Drop bad header rows
  dplyr::filter(!row.names %in% c("YYYY", "datetime") & nchar(row.names) != 0)

# Rename columns as a human eye knows they should be called
proj4 <- proj4_prep %>%
  ## (i.e., first row of 'real' data)
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = as.character(proj4_prep[1, ])) %>% 
  # Drop now-superseded first row of data 
  dplyr::filter(Year != "Year") %>% 
  # Drop bad column
  dplyr::select(-`NA`)

# Re-check structure
dplyr::glimpse(proj4)

# Create good/new file name
proj4_name <- "lter-gce_georgia_pred-ex_2016-2019_predators_invertebrates.csv"
proj4_path <- file.path("data", "drydock", proj4_name)

# Export locally
write.csv(x = proj4, na = "", row.names = F, file = proj4_path)

# Export to Drive
googledrive::drive_upload(media = proj4_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 5 (LTER ARC Tussock) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Table is mix of wide and long format.
## Need to parse out columns with Block and Quad info in the column name
## Species column is a mix of species and Other functional groups
## Biomass type column: drop "below" and "old"
## Other tissue types must be summed across
## Can drop columns AE through AX, these are all converted measurements to gram per meter squared
## Count column is just the number of quadrats for all blocks
## Average column is the average of all species and tissue type for all quadrats and across all blocks. So our abundance measurement should come from B#Q# columns???

# Identify file(s) name(s)
proj5_raw_name <- "1999gsexclosbm.csv"

# Identify file(s) in Drive
proj5_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1562H3RGfLnVaP-gnEwVf0aJcj1YxZfEe")) %>% 
  dplyr::filter(name %in% c(proj5_raw_name))

# Download file(s)
purrr::walk2(.x = proj5_gdrive$id, .y = proj5_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj5_raw <- read.csv(file = file.path("data", "purgatory", proj5_raw_name))

# Check structure
dplyr::glimpse(proj5_raw)

# Do needed repair
proj5 <- proj5_raw %>% 
  # Drop unwanted columns
  dplyr::select(-Average, -Std..Err., -Count, 
                -dplyr::ends_with(".g.m.2"),
                -dplyr::contains("Comments"),
                -dplyr::ends_with("gm2")) %>% 
  # Remove unwanted biomass type(s)
  dplyr::filter(!Biomass.type %in% c("below", "old above")) %>% 
  # Pivot spatial information longer
  tidyr::pivot_longer(cols = dplyr::contains(paste0("Q", 1:5)),
                      names_to = "block.quad",
                      values_to = "abundance") %>% 
  # Wrangle the spatial information into separate columns
  dplyr::mutate(block.quad = gsub(pattern = "Q", replacement = "_Q", x = block.quad)) %>% 
  tidyr::separate_wider_delim(cols = block.quad, delim = "_",
                              names = c("Block", "Quadrat")) %>% 
  # Drop NA abundance values (seems like they were unsampled from structure of original data)
  dplyr::filter(abundance != "na") %>% 
  # Make abundance is a number
  dplyr::mutate(abundance = as.numeric(abundance)) %>% 
  # Summarize abundance within remaining grouping vars
  dplyr::group_by(Date, Site, Block, Quadrat, Treatment, Growth.Form, Species) %>% 
  dplyr::summarize(abundance = sum(abundance, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Re-check structure
dplyr::glimpse(proj5)

# Create good/new file name
proj5_name <- "lter-arc_alaska_acidictussock_1996-1999_vertebrates_plants.csv"
proj5_path <- file.path("data", "drydock", proj5_name)

# Export locally
write.csv(x = proj5, na = '', row.names = F, file = proj5_path)

# Export to Drive
googledrive::drive_upload(media = proj5_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 6 (Freestone) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Extra headers **that contain necessary metadata**

# Identify the input files
proj6_rawfiles <- c("Freestone_et_al_2019_data_newjersey.csv",
                    "Freestone_et_al_2019_data_panama.csv")

# Identify file(s) in Drive
proj6_gdrive <- dplyr::bind_rows(
  ## Looks different because pulling from several different Drive folders
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1msPdHowORXZo3crd2BY_TMV0pRsrgMIH")),
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1OJD2XUx-u5OCFFLECzB8nAy8RgJpc9El"))) %>% 
  dplyr::filter(name %in% proj6_rawfiles)

# Download file(s)
purrr::walk2(.x = proj6_gdrive$id, .y = proj6_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Identify the name for each tidied file
proj6_tidyfiles <- c("freestone_newjersey_freestone-new-jersey_2019_predators_seagrass.csv",
                     "freestone_panama_freestone-panama_2019_predators_seagrass.csv")

# Loop across the two files (because they share structure/problems)
for(k in seq_along(proj6_rawfiles)){
  
  # Identify raw filename of specific file
  proj6_subname <- proj6_rawfiles[k]
  
  # Processing message
  message("Processing 'project 6' purgatory file: '", proj6_subname, "'")
  
  # Read in data
  proj6_raw <- read.csv(file = file.path("data", "purgatory", proj6_subname))
  
  # Separate metadata from header
  proj6_meta <- data.frame("Site" = names(proj6_raw),
                           "Treatment" = as.character(proj6_raw[1, ]),
                           "Seagrass" = as.character(proj6_raw[2, ]),
                           "Code" = as.character(proj6_raw[3, ])) %>% 
    dplyr::filter(Code != "Code")
  
  # Do needed repair on 'actual' data
  proj6 <- proj6_raw %>% 
    # Drop weird/empty columns
    dplyr::select(-dplyr::starts_with("X")) %>% 
    # Remove metadata headers
    dplyr::filter(!Site %in% c("Treatment", "Seagrass type", "Code")) %>% 
    # Rename faux 'site' column
    dplyr::rename(Species = Site) %>% 
    # Reshape spatial information into long format
    tidyr::pivot_longer(cols = -Species,
                        names_to = "Site",
                        values_to = "abundance") %>% 
    # Re-attach metadata extracted above
    dplyr::left_join(y = proj6_meta, by = c("Site")) %>% 
    # Relocate columns slightly
    dplyr::relocate(Species, abundance, .after = dplyr::everything())
  
  # Identify file name in correct format
  proj6_name <- proj6_tidyfiles[k]
  proj6_path <- file.path("data", "drydock", proj6_name)
  
  # Export locally
  write.csv(x = proj6, na = '', row.names = F, file = proj6_path)
  
  # Upload to Drive
  googledrive::drive_upload(media = proj6_path, overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))
    
}

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 7 (Ashton Predators) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Abundance is implied by number of rows with particular genera so: 
## needs to be calculated by number of rows per combination of grouping variables

# Identify file(s) name(s)
proj7_raw_name <- "PointCounts_Week12.csv"

# Identify file(s) in Drive
proj7_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1M5xhOalBqsrUlVjHv4lz3tuLvW0T4X3t")) %>% 
  dplyr::filter(name %in% c(proj7_raw_name))

# Download file(s)
purrr::walk2(.x = proj7_gdrive$id, .y = proj7_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj7_raw <- read.csv(file = file.path("data", "purgatory", proj7_raw_name))

# Check structure
dplyr::glimpse(proj7_raw)

# Do needed repairs
proj7 <- proj7_raw %>% 
  # Fill missing 'genus' info
  dplyr::mutate(Original.Genus = ifelse(is.na(Original.Genus) |
                                          nchar(Original.Genus) == 0,
                                        yes = Plate.Cover.taxa,
                                        no = Original.Genus)) %>% 
  # Get abundance from point intercept identifications
  dplyr::group_by(Site, Plate, Block, Treat, Treatment, Original.Genus) %>% 
  dplyr::summarize(abundance = dplyr::n(),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Re-check structure
dplyr::glimpse(proj7)

# Create good/new file name
proj7_name <- "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv"
proj7_path <- file.path("data", "drydock", proj7_name)

# Export locally
write.csv(x = proj7, na = '', row.names = F, file = proj7_path)

# Export to Drive
googledrive::drive_upload(media = proj7_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 8 (LTER Harvard) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Data collected per tree / seedling
## Need to summarize within spatial groups & species to get tree counts

# Identify file(s) name(s)
proj8_raw_name <- "hf174-06-tree-seedlings-2010.csv"

# Identify file(s) in Drive
proj8_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/16nI87V_gJ1pqUMD27kcNDV1BdkzJQPau")) %>% 
  dplyr::filter(name %in% c(proj8_raw_name))

# Download file(s)
purrr::walk2(.x = proj8_gdrive$id, .y = proj8_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj8_raw <- read.csv(file.path("data", "purgatory", proj8_raw_name))

# Check structure
dplyr::glimpse(proj8_raw)

# Do needed repairs
proj8 <- proj8_raw %>% 
  dplyr::group_by(site, treatment, species) %>% 
  dplyr::summarize(abundance = dplyr::n(),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Re-check structure
dplyr::glimpse(proj8)

# Create good/new file name
proj8_name <- "lter-harvard_newengland_plantcover_2008_2019_moose_treeseedling.csv"
proj8_path <- file.path("data", "drydock", proj8_name)

# Export locally
write.csv(x = proj8, file = proj8_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj8_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 9 (Burkepile FL) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Bad header in first two rows & inclusion of standard error columns
## Deleted bad header manually and re-uploaded to purgatory as a CSV
## Removal of standard error columns accomplished below

# Identify file(s) name(s)
proj9_raw_name <- "41467_2016_BFncomms11833_MOESM1571_ESM.csv"

# Identify file(s) in Drive
proj9_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1u_qzWf-W5V0Cq0IbBB6rfgndIdUssFBL")) %>% 
  dplyr::filter(name %in% c(proj9_raw_name))

# Download file(s)
purrr::walk2(.x = proj9_gdrive$id, .y = proj9_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj9_raw <- read.csv(file.path("data", "purgatory", proj9_raw_name))

# Check structure
dplyr::glimpse(proj9_raw)

# Do needed repairs
proj9 <- proj9_raw %>% 
  dplyr::select(-dplyr::ends_with(".Std.Err")) %>% 
  tidyr::pivot_longer(cols = dplyr::ends_with(".Mean"),
                      names_to = "Species",
                      values_to = "Abundance") %>% 
  dplyr::mutate(Species = gsub(pattern = "\\.Mean|\\.\\.Mean",
                               replacement = "", x = Species))

# Re-check structure
dplyr::glimpse(proj9)

# Create good/new file name
proj9_name <- "burkepile_florida_herbvr_2009-2012_fish_benthic.csv"
proj9_path <- file.path("data", "drydock", proj9_name)

# Export locally
write.csv(x = proj9, file = proj9_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj9_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 10 (GEX Boer) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Unequal numbers of plots within treatments
## Need to **randomly** subset within grouping variables to make reps equal

# Identify file(s) name(s)
proj10_raw_name <- "boer-ca-n4.csv"

# Identify file(s) in Drive
proj10_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1BKLOFzyBtPTbLRzwL5gr-6FYvo5lEJDl")) %>% 
  dplyr::filter(name %in% c(proj10_raw_name))

# Download file(s)
purrr::walk2(.x = proj10_gdrive$id, .y = proj10_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj10_raw <- read.csv(file.path("data", "purgatory", proj10_raw_name))

# Check structure
dplyr::glimpse(proj10_raw)

# Do some generally-useful repairs
proj10_prep <- proj10_raw %>% 
  ## Drop unwanted column(s)
  dplyr::select(-X) %>% 
  ## Summarize within provided variables (several instances of multiple observations of same species within same plot)
  dplyr::group_by(reserve_site, reserve, site, treatment, plot, taxa) %>% 
  dplyr::summarize(cover = mean(cover, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() 

# Check structure
dplyr::glimpse(proj10_prep)

# Make a list for storing outputs
proj10_list <- list()

# Set seed for reproducibility of randomness
set.seed(seed = 53)

# Loop across "reserve_site" values
for(focal_site in unique(proj10_prep$reserve_site)){
  
  # Processing message
  message("Working on site '", focal_site, "'")
  
  # Subset to this site
  proj10_sub <- proj10_prep %>% 
    dplyr::filter(reserve_site == focal_site)
  
  # Split to grazed and ungrazed
  proj10_gz <- dplyr::filter(proj10_sub, treatment == "GRAZED")
  proj10_ug <- dplyr::filter(proj10_sub, treatment == "UNGRAZED")
  
  # Identify number of plots in each
  proj10_gz_plotct <- length(unique(proj10_gz$plot))
  proj10_ug_plotct <- length(unique(proj10_ug$plot))
  
  # Now handle the three possibilities:
  ## If they are equal, simply recombine and move on
  if(proj10_gz_plotct == proj10_ug_plotct){
    
    proj10_sub_fix <- proj10_sub
    
    ## If more grazed,
  } else if(proj10_gz_plotct > proj10_ug_plotct){
    
    # Randomly identify subset of plot IDs
    proj10_wantplots <- sample(x = unique(proj10_gz$plot), size = proj10_ug_plotct)
    
    # Identify unwanted plots
    proj10_unwantplots <- setdiff(x = unique(proj10_gz$plot), y = proj10_wantplots)
    
    # Process the grazed data (ungrazed is fine if there are fewer)
    proj10_sub_gz <- proj10_gz %>% 
      ## Subset to only these plots
      dplyr::filter(plot %in% proj10_wantplots)
    
    # Combine with good treatment subsetted (never messed with it)
    proj10_sub_fix <- dplyr::bind_rows(proj10_ug, proj10_sub_gz) %>% 
      ## Document dropped plot(s) as 'notes' (even if ultimately ignored, good to know)
      dplyr::mutate(notes = paste0("Following GRAZED plot(s) randomly removed: ",
                                   paste(proj10_unwantplots, collapse = ", ")))
    
    ## If more ungrazed, do the same set of operations
  } else {
    
    # Randomly identify subset of plot IDs
    proj10_wantplots <- sample(x = unique(proj10_ug$plot), size = proj10_gz_plotct)
    
    # Identify unwanted plots
    proj10_unwantplots <- setdiff(x = unique(proj10_ug$plot), y = proj10_wantplots)
    
    # Process the data
    proj10_sub_ug <- proj10_ug %>% 
      dplyr::filter(plot %in% proj10_wantplots)
    
    # Combine with good treatment subsetted (never messed with it)
    proj10_sub_fix <- dplyr::bind_rows(proj10_sub_ug, proj10_gz) %>% 
      dplyr::mutate(notes = paste0("Following UNGRAZED plot(s) randomly removed: ",
                                   paste(proj10_unwantplots, collapse = ", ")))
    
  }
  
  # Regardless of how it was handled, add fixed site-specific data to list
  proj10_list[[focal_site]] <- proj10_sub_fix
  
  # Clear environment of intermediary objects to reduce error chances
  ## Suppressing warnings because each run of the loop may have either "proj10_sub_gz" OR "proj10_sub_ug" (or neither)
  suppressWarnings(rm(list = c("proj10_sub", "proj10_gz", "proj10_ug", "proj10_sub_fix",
                               "proj10_sub_gz", "proj10_sub_ug",
                               "proj10_gz_plotct", "proj10_ug_plotct",
                               "proj10_wantplots", "proj10_unwantplots")))
  
}

# Unlist and do any other needed repairs (if any)
proj10 <- proj10_list %>% 
  purrr::list_rbind(x = .)

# Re-check structure
dplyr::glimpse(proj10)

# Re-count plots to make sure that worked
proj10 %>% 
  dplyr::group_by(reserve_site, treatment) %>% 
  dplyr::summarize(plot_ct = length(unique(plot)),
                   .groups = "keep") %>% 
  tidyr::pivot_wider(names_from = treatment, values_from = plot_ct)

# Create good/new file name
proj10_name <- "gex_boer-ca-n4_grazing_year_grazers_plants.csv"
proj10_path <- file.path("data", "drydock", proj10_name)

# Export locally
write.csv(x = proj10, file = proj10_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj10_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 11 (Clausing NZ Intertidal) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Need to attach metadata from a separate file

# Identify raw data and metadata file names
proj11_raw_name <- "Clausing_algal_count_data.csv"
proj11_meta_name <- "Clausing_metadata.csv"

# Download the relevant metadata file too
proj11_gdrive <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1X9vCLm1GRE2-HWhzg8KdUEue62wskRxJ")) %>% 
  dplyr::filter(name %in% c(proj11_raw_name, proj11_meta_name))

# Download file(s)
purrr::walk2(.x = proj11_gdrive$id, .y = proj11_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in metadata
proj11_meta_raw <- read.csv(file.path("data", "purgatory", proj11_meta_name))

# Check structure
dplyr::glimpse(proj11_meta_raw)

# Do any needed metadata repair
proj11_meta <- proj11_meta_raw

# Re-check structure
dplyr::glimpse(proj11_meta)

# Read in data
proj11_raw <- read.csv(file.path("data", "purgatory", proj11_raw_name))

# Check structure
dplyr::glimpse(proj11_raw)

# Do needed repairs
proj11 <- proj11_raw %>% 
  # Break "ID" into relevant columns
  tidyr::separate_wider_delim(cols = ID, delim = "_", names = c("month", "plot")) %>% 
  dplyr::mutate(plot = as.numeric(plot),
                month = as.numeric(month)) %>% 
  # Join on metadata info
  dplyr::left_join(y = proj11_meta, by = "plot") %>% 
  # Relocate somewhat
  dplyr::relocate(dplyr::all_of(names(proj11_meta)), .before = dplyr::everything()) %>% 
  # Clarify treatment columns
  dplyr::mutate(herbivore_treatment = ifelse(H == 0, yes = "removal", no = "ambient"),
                nutrient_treatment = ifelse(N == 0, yes = "ambient", no = "enriched"),
                .after = H) %>% 
  # Clarify month column too
  ## Derived directly from the published paper
  dplyr::mutate(date = as.Date("2010-03-01") + months(month),
                year = stringr::str_sub(string = date, start = 1, end = 4),
                .after = nutrient_treatment) %>% 
  # Drop superseded columns
  dplyr::select(-N, -H, -month)

# Re-check structure
dplyr::glimpse(proj11)

# Create good/new file name
proj11_name <- "clausing_newzealand_intertidalexclosure_2010-2012_grazers_algae.csv"
proj11_path <- file.path("data", "drydock", proj11_name)

# Export locally
write.csv(x = proj11, file = proj11_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj11_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 12 (GEX Bakker) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Too many plots in one treatment versus the other two

# Identify file(s) name(s)
proj12_raw_name <- "Bakker_ShortGrassSteppe.csv"

# Identify file(s) in Drive
proj12_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1rWZf3Jl-h1cMK2FZoe96oC2BwJb0sbQ5")) %>% 
  dplyr::filter(name %in% c(proj12_raw_name))

# Download file(s)
purrr::walk2(.x = proj12_gdrive$id, .y = proj12_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj12_raw <- read.csv(file.path("data", "purgatory", proj12_raw_name)) %>% 
  ## Fix obvious issue with column names
  dplyr::rename(SITE = SITE..,
                Plot = Plot..)

# Check structure
dplyr::glimpse(proj12_raw)

# Count number of plots / treatment
proj12_raw %>% 
  dplyr::group_by(YEAR, SITE, Grazing) %>% 
  dplyr::summarize(plot_ct = length(unique(Plot)),
                   .groups = "keep") %>% 
  tidyr::pivot_wider(names_from = Grazing, values_from = plot_ct)

# Separate the problem treatment from the others
proj12_bad <- dplyr::filter(proj12_raw, Grazing == "UU")
proj12_good <- dplyr::filter(proj12_raw, Grazing != "UU")

# Check that lost nothing
nrow(proj12_raw) == nrow(proj12_bad) + nrow(proj12_good)

# Identify a smaller number of plots in the treatment with too many
proj12_wantplots <- sample(x = unique(proj12_bad$Plot),
                           ## Hard-coding correct number of plots
                           size = 35)

# Identify unwanted plots
proj12_unwantplots <- setdiff(x = unique(proj12_bad$Plot), y = proj12_wantplots)

# Do needed repairs
proj12_fix <- proj12_bad %>% 
  ## Keep only desired plots
  dplyr::filter(Plot %in% proj12_wantplots) %>% 
  ## Add note about dropped plot IDs
  dplyr::mutate(notes = paste0("Following UU plot(s) randomly removed: ",
                               paste(proj12_unwantplots, collapse = ", ")))

# Recombine with rows that were good from outset
proj12 <- dplyr::bind_rows(proj12_good, proj12_fix)

# Re-check structure
dplyr::glimpse(proj12)

# Re-count number of plots / treatment
proj12 %>% 
  dplyr::group_by(YEAR, SITE, Grazing) %>% 
  dplyr::summarize(plot_ct = length(unique(Plot)),
                   .groups = "keep") %>% 
  tidyr::pivot_wider(names_from = Grazing, values_from = plot_ct)

# Create good/new file name
proj12_name <- "gex_bakker-sgs_bakker-sgs_2001_cattle&lagomorphs_plants.csv"
proj12_path <- file.path("data", "drydock", proj12_name)

# Export locally
write.csv(x = proj12, file = proj12_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj12_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 13 (Sonnier FL) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Need to attach metadata from a separate file

# Identify relevant raw data and metadata file names
proj13_raw_name <- "species_incidence.csv"
proj13_meta_name <- "wetland_id_treatments.csv"

# Download the relevant metadata file too
proj13_gdrive <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1uZvL1NI5AkIxMVkB_CSvFbj9tCa2zB_r")) %>% 
  dplyr::filter(name %in% c(proj13_raw_name, proj13_meta_name))

# Download file(s)
purrr::walk2(.x = proj13_gdrive$id, .y = proj13_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in metadata
proj13_meta_raw <- read.csv(file.path("data", "purgatory", proj13_meta_name))

# Check structure
dplyr::glimpse(proj13_meta_raw)

# Do any needed metadata repair
proj13_meta <- proj13_meta_raw %>% 
  # Clarify 'pasture type'
  dplyr::mutate(pasture_type = dplyr::case_when(
    pasture_type == "IMP" ~ "improved",
    pasture_type == "SNP" ~ "semi-native"))

# Re-check structure
dplyr::glimpse(proj13_meta)

# Read in data
proj13_raw <- read.csv(file.path("data", "purgatory", proj13_raw_name))

# Check structure
dplyr::glimpse(proj13_raw)

# Do needed repairs
proj13 <- proj13_raw %>% 
  # Attach metadata
  dplyr::left_join(y = proj13_meta, by = "wetland_ID") %>% 
  # Reorder columns
  dplyr::relocate(pasture_type:burn_type, .after = wetland_ID)

# Re-check structure
dplyr::glimpse(proj13)

# Create good/new file name
proj13_name <- "sonnier_florida_wetlandexclosure_2006-2020_cattle_plants.csv"
proj13_path <- file.path("data", "drydock", proj13_name)

# Export locally
write.csv(x = proj13, file = proj13_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj13_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 14 (Gex Kenya Pringle) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Plots are only uniquely identified within the context of treatments
## Need to add treatment info to plot info in a new column between plot / block

# Identify file(s) name(s)
proj14_raw_name <- "RAWish_gex_pringle-kenya_klee_2008-2013_grazers_plants.csv"

# Identify file(s) in Drive
proj14_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/10AMs27vn07TQKCI8lg1UVfhtrpLvlt7E")) %>% 
  dplyr::filter(name %in% c(proj14_raw_name))

# Download file(s)
purrr::walk2(.x = proj14_gdrive$id, .y = proj14_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj14_raw <- read.csv(file.path("data", "purgatory", proj14_raw_name))

# Check structure
dplyr::glimpse(proj14_raw)

# Do needed repairs
proj14 <- proj14_raw %>% 
  # Make a new block + treatment column
  dplyr::mutate(subblock = paste0(block, "-", trt),
                .before = plot) %>% 
  # Make site names lowercase to account for casing difference
  dplyr::mutate(site = tolower(site))

# How many plot reps within 'subblock'?
proj14 %>% 
  dplyr::group_by(block, subblock) %>%
  dplyr::summarize(plots = paste(unique(plot), collapse = "; "),
                   plot_ct = length(unique(plot)))

# Re-check structure
dplyr::glimpse(proj14)

# Create good/new file name
proj14_name <- "gex_pringle-kenya_klee_2008-2013_grazers_plants.csv"
proj14_path <- file.path("data", "drydock", proj14_name)

# Export locally
write.csv(x = proj14, file = proj14_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj14_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 15 (Sevilleta Mammals) ----
## ------------------------------------------- ##

# Reason for purgatory status
## Need to sum presences of taxa across "start" points

# Identify file(s) name(s)
proj15_raw_name <- "sev095_smeslineint_01122009_0.csv"

# Identify file(s) in Drive
proj15_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1yGVkzEub7JDMEL2tF8LAqLouN7s0lPO2")) %>% 
  dplyr::filter(name %in% c(proj15_raw_name))

# Download file(s)
purrr::walk2(.x = proj15_gdrive$id, .y = proj15_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj15_raw <- read.csv(file.path("data", "purgatory", proj15_raw_name))

# Check structure
dplyr::glimpse(proj15_raw)

# Do needed repairs
proj15_tmp <- proj15_raw %>% 
  # Generate year column
  dplyr::mutate(year = stringr::str_sub(string = date, 
                                        start = nchar(date) - 3,
                                        end = nchar(date)),
                .before = date) %>% 
  # Drop 'comments' column
  dplyr::select(-comments, -intercept)

# Do needed repairs
proj15 <- proj15_tmp %>% 
  # Count rows within all columns except 'start'
  dplyr::group_by(
    dplyr::across(
      dplyr::all_of(setdiff(x = names(proj15_tmp), y = "start")))) %>% 
  dplyr::summarize(abun = dplyr::n(),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Re-check structure
dplyr::glimpse(proj15)

# Create good/new file name
proj15_name <- "lter-sevilleta_newmexico_sev-project_1995-2005_smallmammals_vegetation.csv"
proj15_path <- file.path("data", "drydock", proj15_name)

# Export locally
write.csv(x = proj15, file = proj15_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj15_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 16 (Galetti Cardoso) ----
## ------------------------------------------- ##
# Reason for purgatory status
## add column with site name (these count a separate experiment - Cardoso). For future notes - treatment: Control = uncaged, Defaunated = caged
## Also make "T#" month designations into real months/years columns

# Identify file(s) name(s)
proj16_raw_name <- "Cardoso_T0_T156.xlsx"

# Identify file(s) in Drive
proj16_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/13WJ3iko1YeTuSS3kvEe50e81edpDgSft")) %>% 
  dplyr::filter(name %in% c(proj16_raw_name))

# Download file(s)
purrr::walk2(.x = proj16_gdrive$id, .y = proj16_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Identify all sheets
proj16_sheets <- readxl::excel_sheets(path = file.path("data", "purgatory", proj16_raw_name))

# Read in data as a list
proj16_raw <- purrr::map(.x = setdiff(x = proj16_sheets, y = "Metadata"),
                         .f = ~ readxl::read_xlsx(
                           path = file.path("data", "purgatory", proj16_raw_name),
                           sheet = .x))

# Check raw structure of one sheet/list element
dplyr::glimpse(proj16_raw[[1]])

# Do needed repairs
proj16 <- proj16_raw %>% 
  # Get sheet name into dataset
  purrr::map(.f = ~ dplyr::mutate(.data = .x, 
                                  month_qual = names(.x)[1], 
                                  .before = dplyr::everything())) %>% 
  # Drop bad/superseded first column
  purrr::map(.f = ~ dplyr::select(.data = .x, 
                                  -dplyr::starts_with(paste0("T", 0:400)))) %>% 
  # Combine into a flat dataframe
  purrr::list_rbind(x = .) %>% 
  # Tidy up the qualitative month
  dplyr::mutate(month_qual = as.numeric(gsub("T", "", x = month_qual))) %>% 
  # Make it "real" & extract year
  dplyr::mutate(month = as.Date("07/01/2009", format = "%m/%d/%Y") + months(month_qual),
                .before = month_qual) %>% 
  dplyr::mutate(year = year(month), .before = month) %>% 
  # Remove qualitative month now that we have 'real' time
  dplyr::select(-month_qual) %>% 
  # Pivot community data to long format
  tidyr::pivot_longer(cols = -year:-treatment,
                      names_to = "species", values_to = "abun") %>% 
  # Drop NAs/0s
  dplyr::filter(!is.na(abun), nchar(abun) != 0, abun > 0) %>% 
  # Make cage/non-cage more explicit
  dplyr::mutate(treatment = dplyr::case_when(
    treatment == "Control" ~ "no cage",
    treatment == "Defaunation" ~ "cage",
    T ~ treatment))

# Re-check structure
dplyr::glimpse(proj16)

# Create good/new file name
proj16_name <- "galetti_brazil-atlanticforest_cardoso_2009-2023_herbivores_trees.csv"
proj16_path <- file.path("data", "drydock", proj16_name)

# Export locally
write.csv(x = proj16, file = proj16_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj16_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 17 (Galetti Carlos Botelho) ----
## ------------------------------------------- ##
# Reason for purgatory status
## add column with site name (these count a separate experiment - Cardoso). For future notes - treatment: Control = uncaged, Defaunated = caged
## Also make "T#" month designations into real months/years columns

# Identify file(s) name(s)
proj17_raw_name <- "Carlos_Botelho_T0_T108.xlsx"

# Identify file(s) in Drive
proj17_gdrive <- googledrive::drive_ls(googledrive::as_id("http://drive.google.com/drive/folders/1_EQlwA8hl36wD9lMKBV9ebk0bjooZGBK")) %>% 
  dplyr::filter(name %in% c(proj17_raw_name))

# Download file(s)
purrr::walk2(.x = proj17_gdrive$id, .y = proj17_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Identify all sheets
proj17_sheets <- readxl::excel_sheets(path = file.path("data", "purgatory", proj17_raw_name))

# Read in data as a list
proj17_raw <- purrr::map(.x = setdiff(x = proj17_sheets, y = "metadata"),
                         .f = ~ readxl::read_xlsx(
                           path = file.path("data", "purgatory", proj17_raw_name),
                           sheet = .x))

# Check raw structure of one sheet/list element
dplyr::glimpse(proj17_raw[[1]])

# Do needed repairs
proj17 <- proj17_raw %>% 
  # Get sheet name into dataset
  purrr::map(.f = ~ dplyr::mutate(.data = .x, 
                                  month_qual = names(.x)[1], 
                                  .before = dplyr::everything())) %>% 
  # Drop bad/superseded first column
  purrr::map(.f = ~ dplyr::select(.data = .x, 
                                  -dplyr::starts_with(paste0("T", 0:400)))) %>% 
  # Combine into a flat dataframe
  purrr::list_rbind(x = .) %>% 
  # Tidy up the qualitative month
  dplyr::mutate(month_qual = as.numeric(gsub("T", "", x = month_qual))) %>% 
  # Make it "real" & extract year
  dplyr::mutate(month = as.Date("07/01/2009", format = "%m/%d/%Y") + months(month_qual),
                .before = month_qual) %>% 
  dplyr::mutate(year = year(month), .before = month) %>% 
  # Remove qualitative month now that we have 'real' time
  dplyr::select(-month_qual) %>% 
  # Pivot community data to long format
  tidyr::pivot_longer(cols = -year:-treatment,
                      names_to = "species", values_to = "abun") %>% 
  # Drop NAs/0s
  dplyr::filter(!is.na(abun), nchar(abun) != 0, abun > 0) %>% 
  # Make cage/non-cage more explicit
  dplyr::mutate(treatment = dplyr::case_when(
    treatment == "Control" ~ "no cage",
    treatment == "Defaunation" ~ "cage",
    T ~ treatment))

# Re-check structure
dplyr::glimpse(proj17)

# Create good/new file name
proj17_name <- "galetti_brazil-atlanticforest_carlosbotelho_2009-2018_herbivores_trees.csv"
proj17_path <- file.path("data", "drydock", proj17_name)

# Export locally
write.csv(x = proj17, file = proj17_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj17_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 18 (Galetti Itamambuca) ----
## ------------------------------------------- ##
# Reason for purgatory status
## add column with site name (these count a separate experiment - Cardoso). For future notes - treatment: Control = uncaged, Defaunated = caged
## Also make "T#" month designations into real months/years columns

# Identify file(s) name(s)
proj18_raw_name <- "Itamambuca_T0_T156.xlsx"

# Identify file(s) in Drive
proj18_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1TPju8hGs_T6Kf7_VES5KyUPjpP-9wbqV")) %>% 
  dplyr::filter(name %in% c(proj18_raw_name))

# Download file(s)
purrr::walk2(.x = proj18_gdrive$id, .y = proj18_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Identify all sheets
proj18_sheets <- readxl::excel_sheets(path = file.path("data", "purgatory", proj18_raw_name))

# Read in data as a list
proj18_raw <- purrr::map(.x = setdiff(x = proj18_sheets, y = "metadata"),
                         .f = ~ readxl::read_xlsx(
                           path = file.path("data", "purgatory", proj18_raw_name),
                           sheet = .x))

# Check raw structure of one sheet/list element
dplyr::glimpse(proj18_raw[[1]])

# Do needed repairs
proj18 <- proj18_raw %>% 
  # Get sheet name into dataset
  purrr::map(.f = ~ dplyr::mutate(.data = .x, 
                                  month_qual = names(.x)[1], 
                                  .before = dplyr::everything())) %>% 
  # Drop bad/superseded first column
  purrr::map(.f = ~ dplyr::select(.data = .x, 
                                  -dplyr::starts_with(paste0("T", 0:400)))) %>% 
  # Combine into a flat dataframe
  purrr::list_rbind(x = .) %>% 
  # Tidy up the qualitative month
  dplyr::mutate(month_qual = as.numeric(gsub("T", "", x = month_qual))) %>% 
  # Make it "real" & extract year
  dplyr::mutate(month = as.Date("07/01/2009", format = "%m/%d/%Y") + months(month_qual),
                .before = month_qual) %>% 
  dplyr::mutate(year = year(month), .before = month) %>% 
  # Remove qualitative month now that we have 'real' time
  dplyr::select(-month_qual) %>% 
  # Pivot community data to long format
  tidyr::pivot_longer(cols = -year:-treatment,
                      names_to = "species", values_to = "abun") %>% 
  # Drop NAs/0s
  dplyr::filter(!is.na(abun), nchar(abun) != 0, abun > 0) %>% 
  # Make cage/non-cage more explicit
  dplyr::mutate(treatment = dplyr::case_when(
    treatment == "Control" ~ "no cage",
    treatment == "Defaunation" ~ "cage",
    T ~ treatment))

# Re-check structure
dplyr::glimpse(proj18)

# Create good/new file name
proj18_name <- "galetti_brazil-atlanticforest_itamambuca_2009-2023_herbivores_trees.csv"
proj18_path <- file.path("data", "drydock", proj18_name)

# Export locally
write.csv(x = proj18, file = proj18_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj18_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 19 (Galetti Vargem) ----
## ------------------------------------------- ##
# Reason for purgatory status
## add column with site name (these count a separate experiment - Cardoso). For future notes - treatment: Control = uncaged, Defaunated = caged
## Also make "T#" month designations into real months/years columns

# Identify file(s) name(s)
proj19_raw_name <- "Vargem_Grande_T0_T156.xlsx"

# Identify file(s) in Drive
proj19_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1CXfgW-kbw20mS2dlgOgv4tVn7arqbyAk")) %>% 
  dplyr::filter(name %in% c(proj19_raw_name))

# Download file(s)
purrr::walk2(.x = proj19_gdrive$id, .y = proj19_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Identify all sheets
proj19_sheets <- readxl::excel_sheets(path = file.path("data", "purgatory", proj19_raw_name))

# Read in data as a list
proj19_raw <- purrr::map(.x = setdiff(x = proj19_sheets, y = "metadata"),
                         .f = ~ readxl::read_xlsx(
                           path = file.path("data", "purgatory", proj19_raw_name),
                           sheet = .x))

# Check raw structure of one sheet/list element
dplyr::glimpse(proj19_raw[[1]])

# Do needed repairs
proj19 <- proj19_raw %>% 
  # Get sheet name into dataset
  purrr::map(.f = ~ dplyr::mutate(.data = .x, 
                                  month_qual = names(.x)[1], 
                                  .before = dplyr::everything())) %>% 
  # Drop bad/superseded first column
  purrr::map(.f = ~ dplyr::select(.data = .x, 
                                  -dplyr::starts_with(paste0("T", 0:400)))) %>% 
  # Combine into a flat dataframe
  purrr::list_rbind(x = .) %>% 
  # Tidy up the qualitative month
  dplyr::mutate(month_qual = as.numeric(gsub("T", "", x = month_qual))) %>% 
  # Make it "real" & extract year
  dplyr::mutate(month = as.Date("07/01/2009", format = "%m/%d/%Y") + months(month_qual),
                .before = month_qual) %>% 
  dplyr::mutate(year = year(month), .before = month) %>% 
  # Remove qualitative month now that we have 'real' time
  dplyr::select(-month_qual) %>% 
  # Pivot community data to long format
  tidyr::pivot_longer(cols = -year:-treatment,
                      names_to = "species", values_to = "abun") %>% 
  # Drop NAs/0s
  dplyr::filter(!is.na(abun), nchar(abun) != 0, abun > 0) %>% 
  # Make cage/non-cage more explicit
  dplyr::mutate(treatment = dplyr::case_when(
    treatment == "Control" ~ "no cage",
    treatment == "Defaunation" ~ "cage",
    T ~ treatment))

# Re-check structure
dplyr::glimpse(proj19)

# Create good/new file name
proj19_name <- "galetti_brazil-atlanticforest_vargemgrande_2009-2023_herbivores_trees.csv"
proj19_path <- file.path("data", "drydock", proj19_name)

# Export locally
write.csv(x = proj19, file = proj19_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj19_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 20 (Silock Queensland) ----
## ------------------------------------------- ##
# Reason for purgatory status:
## Someone dragged down the "Block" column too far and now there are duplicates (e.g., row 38 should be a 2 in block not 1)

# Identify file(s) name(s)
proj20_raw_name <- "Silcock_QueenslandAUS.xlsx"

# Identify file(s) in Drive
proj20_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/14Sr7iBgnWU2LZIRMgo53kTloUX7JM8R4")) %>% 
  dplyr::filter(name %in% c(proj20_raw_name))

# Download file(s)
purrr::walk2(.x = proj20_gdrive$id, .y = proj20_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj20_raw <- readxl::read_excel(path = file.path("data", "purgatory", proj20_raw_name),
                                 sheet = "SpeciesData")

# Check structure
dplyr::glimpse(proj20_raw)

# Check number of plots per block per site
proj20_raw %>% 
  dplyr::group_by(`Site Name`, Block) %>% 
  dplyr::summarize(row_ct = dplyr::n(), .groups = "keep")

# Do needed repairs
proj20_tmp <- proj20_raw %>% 
  # Rename some columns better
  dplyr::rename(Site = `Site Name`,
                block_orig = Block,
                Year = `Calendar Year`,
                year_since_excl_start = `Year since Excl. been up`,
                anpp = `Biomass/ANPP`,
                light_availability = `Light availability`) %>% 
  # Trial the repaired block numbering
  dplyr::group_by(Site) %>% 
  dplyr::mutate(Block = c(rep(x = 1, times = 36),
                          rep(x = 2, times = 36),
                          rep(x = 3, times = 36)),
                .after = block_orig) %>% 
  dplyr::ungroup()

# Does that seem reasonable/correct?
proj20_tmp %>% 
  filter(Block != block_orig) %>% 
  dplyr::select(Site:Treatment) %>% 
  as.data.frame()

# Re-check to make sure plot numbers are repaired
proj20_tmp %>% 
  dplyr::group_by(Site, Block) %>% 
  dplyr::summarize(row_ct = dplyr::n(), .groups = "keep")

# Do some other nice stuff while we are here
proj20 <- proj20_tmp %>% 
  # Drop bad original block info
  dplyr::select(-block_orig) %>% 
  # Pivot taxon information long
  tidyr::pivot_longer(cols = -Site:-light_availability,
                      names_to = "species",
                      values_to = "abundance") %>% 
  # Remove missing abundance
  dplyr::filter(abundance > 0)

# Re-check structure
dplyr::glimpse(proj20)

# Create good/new file name
proj20_name <- "gex_queenslandaus1-5_Silcock_2009_grazers_plants.csv"
proj20_path <- file.path("data", "drydock", proj20_name)

# Export locally
write.csv(x = proj20, file = proj20_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj20_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 21 (Jennie's DH Tundra) ----
## ------------------------------------------- ##
# Reason for purgatory status
## Treatment column includes nutrient and caging treatments
## Need to parse out treatment column for nutrients and cage treatment
### First 2 letters are fencing (LF - excludes caribou only; SF - excludes caribou and small mammals; NF - no fence)
### Second 2 letters are fertilizing (CT - control; NP - fertilized))

# Identify file(s) name(s)
proj21_raw_name <- "JennieNew_lter-arc_DHTundra_nutrientsandexclosures_2005-2013-2017_vertebrates_vegetation.csv"

# Identify file(s) in Drive
proj21_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/16GEmeNy9qDvkH3E3-GuyVlus2Y9KZhna")) %>% 
  dplyr::filter(name %in% c(proj21_raw_name))

# Download file(s)
purrr::walk2(.x = proj21_gdrive$id, .y = proj21_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj21_raw <- read.csv(file.path("data", "purgatory", proj21_raw_name))

# Check structure
dplyr::glimpse(proj21_raw)

# Do needed repairs
proj21 <- proj21_raw %>% 
  # Split treatments
  dplyr::mutate(Fence = stringr::str_sub(string = Treatment, start = 1, end = 2),
                Fertilizer = stringr::str_sub(string = Treatment, start = 3, end = 4),
                .after = Treatment) %>% 
  # Expand those
  dplyr::mutate(Fence = dplyr::case_when(
    Fence == "LF" ~ "caribou fence", # "large fence"
    Fence == "SF" ~ "caribou and small mammal fence", # "small fence"
    Fence == "NF" ~ "no fence")) %>% 
  dplyr::mutate(Fertilizer = dplyr::case_when(
    Fertilizer == "CT" ~ "fertilizer control",
    Fertilizer == "NP" ~ "fertilized")) %>% 
  # Drop original composite treatment
  dplyr::select(-Treatment)

# Re-check structure
dplyr::glimpse(proj21)

# Create good/new file name
proj21_name <- "lter-arc_DHTundra_nutrientsandexclosures_2005-2013-2017_vertebrates_vegetation.csv"
proj21_path <- file.path("data", "drydock", proj21_name)

# Export locally
write.csv(x = proj21, file = proj21_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj21_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 22 (Jennie's MA Tundra) ----
## ------------------------------------------- ##
# Reason for purgatory status
## Treatment column includes nutrient and caging treatments
## Need to parse out treatment column for nutrients and cage treatment
### First 2 letters are fencing (LF - excludes caribou only; SF - excludes caribou and small mammals; NF - no fence)
### Second 2 letters are fertilizing (CT - control; NP - fertilized))

# Identify file(s) name(s)
proj22_raw_name <- "JennieNew_lter-arc_MATundra_nutrientsandexclosures_2005-2015-2017_vertebrates_vegetation.csv"

# Identify file(s) in Drive
proj22_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1xx7NXRD-oMeQqsb6dk6vv_pnw6oOI5lh")) %>% 
  dplyr::filter(name %in% c(proj22_raw_name))

# Download file(s)
purrr::walk2(.x = proj22_gdrive$id, .y = proj22_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj22_raw <- read.csv(file.path("data", "purgatory", proj22_raw_name))

# Check structure
dplyr::glimpse(proj22_raw)

# Do needed repairs
proj22 <- proj22_raw %>% 
  # Split treatments
  dplyr::mutate(Fence = stringr::str_sub(string = Treatment, start = 1, end = 2),
                Fertilizer = stringr::str_sub(string = Treatment, start = 3, end = 4),
                .after = Treatment) %>% 
  # Expand those
  dplyr::mutate(Fence = dplyr::case_when(
    Fence == "LF" ~ "caribou fence", # "large fence"
    Fence == "SF" ~ "caribou and small mammal fence", # "small fence"
    Fence == "NF" ~ "no fence")) %>% 
  dplyr::mutate(Fertilizer = dplyr::case_when(
    Fertilizer == "CT" ~ "fertilizer control",
    Fertilizer == "NP" ~ "fertilized")) %>% 
  # Drop original composite treatment
  dplyr::select(-Treatment)

# Re-check structure
dplyr::glimpse(proj22)

# Create good/new file name
proj22_name <- "lter-arc_MATundra_nutrientsandexclosures_2005-2015-2017_vertebrates_vegetation.csv"
proj22_path <- file.path("data", "drydock", proj22_name)

# Export locally
write.csv(x = proj22, file = proj22_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj22_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 23 (Duran et al. Coral) ----
## ------------------------------------------- ##
# Reason for purgatory status
## Data are malformed (bizarre headers, empty columns, merged cells)
## Also, data are found in several separate sheets

# Identify file(s) name(s)
proj23_raw_name <- "Raw-Data_2016_PeerJ_paper_For_Kelly.xlsx"

# Identify file(s) in Drive
proj23_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/17K1NWtD5mVVY1BNXFeFiXvz6ssuWfyr7")) %>% 
  dplyr::filter(name %in% c(proj23_raw_name))

# Download file(s)
purrr::walk2(.x = proj23_gdrive$id, .y = proj23_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Identify sheets in the data
(proj23_sheets <- readxl::excel_sheets(path = file.path("data", "purgatory", proj23_raw_name)))

# Read in each sheet
## First sheet ("A")
proj23_a_raw <- readxl::read_excel(file.path("data", "purgatory", proj23_raw_name),
                                   sheet = proj23_sheets[1])
## Second sheet ("B")
proj23_b_raw <- readxl::read_excel(file.path("data", "purgatory", proj23_raw_name),
                                   sheet = proj23_sheets[2])
## Third sheet ("C")
proj23_c_raw <- readxl::read_excel(file.path("data", "purgatory", proj23_raw_name),
                                   sheet = proj23_sheets[3])

# Check structure of first sheet
dplyr::glimpse(proj23_a_raw)

# Do needed repairs
proj23_a <- proj23_a_raw %>% 
  # Manually rename columns correctly
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = c("season", "algal_groups",
                          paste0(
                            sort(rep(x = c("site1", "site2", "site3", "site4"), 
                                     times = 8)), "___",
                            rep(c(rep("NE", 2), rep("CE", 2), 
                                  rep("CO", 2), rep("NO", 2)),
                                times = 4), "___",
                            rep(c("a", "b"), times = 16))) ) %>% 
  # Filter bad heders rows
  dplyr::filter(season %in% c("January", "June")) %>% 
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("site"),
                      values_to = "percent_cover") %>% 
  # Slip site information into useable components
  tidyr::separate_wider_delim(cols = name, delim = "___",
                              names = c("site", "treatment", "tile")) %>% 
  # Clarify 'treatment' abbreviations
  dplyr::mutate(
    treatment_cage = ifelse(stringr::str_detect(string = treatment,
                                                pattern = "E"),
                            yes = "exclosure", no = "uncaged"),
    treatment_nutrient = ifelse(stringr::str_detect(string = treatment,
                                                pattern = "N"),
                            yes = "enriched", no = "ambient")) %>% 
  # Reorder columns (implicitly dropping ones we don't want)
  dplyr::select(site, dplyr::starts_with("treatment_"), season, tile,
                algal_groups, percent_cover) %>% 
  # Ditch empty rows
  dplyr::mutate(percent_cover = as.numeric(percent_cover)) %>% 
  dplyr::filter(!is.na(percent_cover)) %>% 
  # Add a column for experiment and for year
  dplyr::mutate(experiment = "succession", .before = dplyr::everything()) %>% 
  dplyr::mutate(year = 2012, .before = season)

# Check structure
dplyr::glimpse(proj23_a)

# Check structure of second sheet
dplyr::glimpse(proj23_b_raw)

# Do needed repairs (essentially same structure/problems as "A")
proj23_b <- proj23_b_raw %>% 
  # Manually rename columns correctly
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = c("season", "algal_groups",
                                       paste0(
                                         sort(rep(x = c("site1", "site2", "site3", "site4"), 
                                                  times = 8)), "___",
                                         rep(c(rep("NE", 2), rep("CE", 2), 
                                               rep("CO", 2), rep("NO", 2)),
                                             times = 4), "___",
                                         rep(c("a", "b"), times = 16))) ) %>% 
  # Filter bad heders rows
  dplyr::filter(season %in% c("January", "June")) %>% 
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("site"),
                      values_to = "percent_cover") %>% 
  # Slip site information into useable components
  tidyr::separate_wider_delim(cols = name, delim = "___",
                              names = c("site", "treatment", "tile")) %>% 
  # Clarify 'treatment' abbreviations
  dplyr::mutate(
    treatment_cage = ifelse(stringr::str_detect(string = treatment,
                                                pattern = "E"),
                            yes = "exclosure", no = "uncaged"),
    treatment_nutrient = ifelse(stringr::str_detect(string = treatment,
                                                    pattern = "N"),
                                yes = "enriched", no = "ambient")) %>% 
  # Reorder columns (implicitly dropping ones we don't want)
  dplyr::select(site, dplyr::starts_with("treatment_"), season, tile,
                algal_groups, percent_cover) %>% 
  # Ditch empty rows
  dplyr::mutate(percent_cover = as.numeric(percent_cover)) %>% 
  dplyr::filter(!is.na(percent_cover)) %>% 
  # Add a column for experiment
  dplyr::mutate(experiment = "established communities", .before = dplyr::everything()) %>% 
  dplyr::mutate(year = 2012, .before = season)

# Check structure
dplyr::glimpse(proj23_b)

# Check structure of third sheet
dplyr::glimpse(proj23_c_raw)

# Do needed repairs (same rough fixes but different specifics because structure is diff)
proj23_c <- proj23_c_raw %>% 
  # Manually rename columns correctly
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = c("species",
                                       paste0(
                                         sort(rep(c("set1", "set2", "set3"), times = 15)), "___",
                                         rep(c(paste0("site1", "___", c("NE", "CE", "CO", "NO")),
                                               paste0("site2", "___", c("NE", "CE", "CO", "NO")),
                                               # note not all treatments are included for this site (vvv)!
                                               paste0("site3", "___", c("NE", "CO", "NO")), 
                                               paste0("site4", "___", c("NE", "CE", "CO", "NO"))
                                         ), times = 3))) ) %>% 
  # Filter bad heders rows
  dplyr::filter(species != "Species" & !is.na(species)) %>% 
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("set"),
                      values_to = "percent_cover") %>% 
  # Slip site information into useable components
  tidyr::separate_wider_delim(cols = name, delim = "___",
                              names = c("set", "site", "treatment")) %>% 
  # Clarify 'treatment' abbreviations
  dplyr::mutate(
    treatment_cage = ifelse(stringr::str_detect(string = treatment,
                                                pattern = "E"),
                            yes = "exclosure", no = "uncaged"),
    treatment_nutrient = ifelse(stringr::str_detect(string = treatment,
                                                    pattern = "N"),
                                yes = "enriched", no = "ambient")) %>% 
  # Reorder columns (implicitly dropping ones we don't want)
  dplyr::select(site, dplyr::starts_with("treatment_"), set, 
                species, percent_cover) %>% 
  # Ditch empty rows
  dplyr::mutate(percent_cover = as.numeric(percent_cover)) %>% 
  dplyr::filter(!is.na(percent_cover)) %>% 
  # Identify year information from ambiguous "sets"
  dplyr::mutate(year = dplyr::case_when(
   set == "set1" ~ 2011, 
   set == "set2" ~ 2012, # technically includes December 2011 but that feels pretty close to 2012
   set == "set3" ~ 2012),
   .before = species) %>% 
  # Rename 'set' information too
  dplyr::rename(tile_deployment = set) %>% 
  # Add a column for experiment
  dplyr::mutate(experiment = "recruitment", .before = dplyr::everything())

# Check structure
dplyr::glimpse(proj23_c)

# Create good/new file name for each sheet
proj23_name_a <- "duran_florida-keys_succession_2012_fishes_algae.csv"
proj23_name_b <- "duran_florida-keys_established_2012_fishes_algae.csv"
proj23_name_c <- "duran_florida-keys_recruitment_2011-2012_fishes_algae.csv"

# Create file paths using these
proj23_path_a <- file.path("data", "drydock", proj23_name_a)
proj23_path_b <- file.path("data", "drydock", proj23_name_b)
proj23_path_c <- file.path("data", "drydock", proj23_name_c)

# Export locally
write.csv(x = proj23_a, file = proj23_path_a, na = '', row.names = F)
write.csv(x = proj23_b, file = proj23_path_b, na = '', row.names = F)
write.csv(x = proj23_c, file = proj23_path_c, na = '', row.names = F)

# Export to Drive
purrr::walk(.x = dir(path = file.path("data", "drydock"), pattern = "duran_florida-keys_"),
            .f = ~ googledrive::drive_upload(media = file.path("data", "drydock", .x),
                                      overwrite = T,
                                      path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX")))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 24 (Doherty & Sale Coral) ----
## ------------------------------------------- ##
# Reason for purgatory status
## Separate sampling time points occupy different sheets
## Data are also in spatial wide format and we want that in long

# Identify file(s) name(s)
proj24_raw_name <- "Cage experiment data from first season.xlsx"

# Identify file(s) in Drive
proj24_gdrive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1TnYtqecznI9SdbHGbI_EqgJO6izvciYZ")) %>% 
  dplyr::filter(name %in% c(proj24_raw_name))

# Download file(s)
purrr::walk2(.x = proj24_gdrive$id, .y = proj24_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Identify sheets in the Excel file
(proj24_sheets <- readxl::excel_sheets(path = file.path("data", "purgatory", proj24_raw_name)))

# Make an empty list to store the raw data
proj24_raw <- list()

# Loop across the sheets
for(proj24_tab in proj24_sheets){
  
  # Read in that sheet
  proj24_raw[[proj24_tab]] <- readxl::read_excel(path = file.path("data", "purgatory",
                                                                  proj24_raw_name),
                                                 sheet = proj24_tab) %>% 
    # Add a column for the sheet name
    dplyr::mutate(tab_name = proj24_tab)
}

# Check structure
dplyr::glimpse(proj24_raw[[1]])

# Do needed repairs
proj24 <- proj24_raw %>% 
  # Collapse to dataframe
  purrr::list_rbind(x = .) %>% 
  # Ditch empty columns
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>% 
  # Ditch bad header rows
  dplyr::filter(!is.na(Sites) & Sites != "Species") %>% 
  # Rename species column correctly
  dplyr::rename(species = Sites) %>% 
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::contains("..."),
                      names_to = "treat_rep", values_to = "fish_count") %>% 
  # Separate treatment from replicate number
  tidyr::separate_wider_delim(cols = treat_rep, delim = "...",
                              names = c("cage_treatment", "replicate")) %>% 
  # Fix replicate numbering
  dplyr::mutate(replicate = as.numeric(replicate)) %>% 
  dplyr::mutate(replicate = dplyr::case_when(
    cage_treatment == "Cage" ~ (replicate - 1),
    cage_treatment == "Partial" ~ (replicate - 10),
    cage_treatment == "Open" ~ (replicate - 19))) %>% 
  dplyr::mutate(replicate = paste(cage_treatment, replicate)) %>% 
  # Extract date from tab names
  dplyr::mutate(tab_name = gsub(pattern = "7Feb", replacement = "07Feb",
                                x = tab_name)) %>% 
  dplyr::mutate(date = as.Date(
    paste(stringr::str_sub(string = tab_name, start = 1, end = 2),
          ifelse(stringr::str_detect(string = tab_name, pattern = "Jan"),
                 yes = "01", no = "02"),
          "1981", sep = "-"), format = "%d-%m-%Y"), 
    .after = replicate) %>% 
  # Reorder columns more intuitively
  dplyr::select(date, cage_treatment, replicate, species, fish_count) %>% 
  # Add a 'year' column
  dplyr::mutate(year = 1981, .before = date)

# Re-check structure
dplyr::glimpse(proj24)

# Create good/new file name
proj24_name <- "doherty-sale_great-barrier-reef_juvenile-fish-predation_1981_fish_fish.csv"
proj24_path <- file.path("data", "drydock", proj24_name)

# Export locally
write.csv(x = proj24, file = proj24_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj24_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Purgatory TEMPLATE ----
## ------------------------------------------- ##
## Duplicate and flesh out one copy!

# Reason for purgatory status
## 

# Identify file(s) name(s)
proj0_raw_name <- "BAD_FILE.csv"

# Identify file(s) in Drive
proj0_gdrive <- googledrive::drive_ls(googledrive::as_id("raw file GDrive link (in subfolder of 'metadata' folder)")) %>% 
  dplyr::filter(name %in% c(proj0_raw_name))

# Download file(s)
purrr::walk2(.x = proj0_gdrive$id, .y = proj0_gdrive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Read in data
proj0_raw <- read.csv(file.path("data", "purgatory", proj0_raw_name))

# Check structure
dplyr::glimpse(proj0_raw)

# Do needed repairs
proj0 <- proj0_raw

# Re-check structure
dplyr::glimpse(proj0)

# Create good/new file name
proj0_name <- "organization_region_experiment-name_study-years_excluded-group_measured-group.csv"
proj0_path <- file.path("data", "drydock", proj0_name)

# Export locally
write.csv(x = proj0, file = proj0_path, na = '', row.names = F)

# Export to Drive
googledrive::drive_upload(media = proj0_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1E11bCAJQ8UzV80s1tf4KC4kiTa5fRwCX"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# End ----
