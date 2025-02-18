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
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv|\\.txt|\\.xlsx|\\.xls"))

# Did that work?
files_drive

# Download them!
purrr::walk2(.x = files_drive$id, .y = files_drive$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "purgatory", .y)))

# Clear environment + collect garbage
rm(list = ls()); gc()

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

# Clear environment + collect garbage
rm(list = ls()); gc()

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

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 3 ----
## ------------------------------------------- ##

# Reason for purgatory status:
## Treatments split into separate data files that need to be combined

# Output list
proj3_list <- list()

# Loop across relevant files
for(proj3_file in dir(path = file.path("data", "purgatory"), pattern = "_Ranktime_")){
  
  # Processing message
  message("Grabbing file '", proj3_file, "'")
  
  # Read in data and pivot longer
  proj3_df <- read.delim(file=file.path("data", "purgatory", proj3_file)) %>% 
    tidyr::pivot_longer(cols = -Species,
                        names_to="Timepoint",
                        values_to="Abundance") %>%
    dplyr::mutate(input_file = proj3_file, .before=everything())
  
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
  dplyr::select(-contains("junk"))

# Check structure
dplyr::glimpse(proj3)

# for loop to save each site as a separate file
for(focalsite3 in unique(proj3$site)){
  
  # subset data to just this file (subset data to each of these sites)
  proj3_sub <- dplyr::filter(proj3, site == focalsite3)
  
  # assemble better filename
  proj3_subname <- paste0("villar_brazil_",  tolower(unique(proj3_sub$site)), 
                          "_2009-2016_tapirs_forest.csv")
  
  proj3_subpath <- file.path("data", "drydock", proj3_subname)

  
  #export renamed csv to data/drydock 
  write.csv(proj3_sub, na = "", row.names = F, file = proj3_subpath) 
  
  # Upload to Drive
  googledrive::drive_upload(media = proj3_subpath, overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))
  
}

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 4 ----
## ------------------------------------------- ##

# Reason for purgatory status
## Triple header rows

# Read in data
proj4_raw <- read.csv(file = file.path("data", "purgatory", "MSH-GCED-2308_Experiment_1_0.CSV"))

# Check structure
dplyr::glimpse(proj4_raw)

# Do needed repair
proj4 <- proj4_raw %>% 
  dplyr::filter(!Year %in% c("YYYY", "datetime"))

# Re-check structure
dplyr::glimpse(proj4)

# Create good/new file name
proj4_name <- "lter-gce_georgia_pred-ex_2016-2019_predators_invertebrates.csv"
proj4_path <- file.path("data", "drydock", proj4_name)

# Export locally
write.csv(x = proj4, na = "", row.names = F, file = proj4_path)

# Export to Drive
googledrive::drive_upload(media = proj4_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 5 ----
## ------------------------------------------- ##

# Reason for purgatory status
## Table is mix of wide and long format.
## Need to parse out columns with Block and Quad info in the column name
## Species column is a mix of species and Other functional groups
## Biomass type column: drop "below"
## Can drop columns AE through AX, these are all converted measurements to gram per meter squared
## Count column is just the number of quadrats for all blocks
## Average column is the average of all species and tissue type for all quadrats and across all blocks. So our abundance measurement should come from B#Q# columns???

# Read in data
proj5_raw <- read.csv(file = file.path("data", "purgatory", "1999gsexclosbm.csv"))

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
  dplyr::filter(Biomass.type != "below") %>% 
  # Pivot spatial information longer
  tidyr::pivot_longer(cols = dplyr::contains(paste0("Q", 1:5)),
                      names_to = "block.quad",
                      values_to = "abundance") %>% 
  # Wrangle the spatial information into separate columns
  dplyr::mutate(block.quad = gsub(pattern = "Q", replacement = "_Q", x = block.quad)) %>% 
  tidyr::separate_wider_delim(cols = block.quad, delim = "_",
                              names = c("Block", "Quadrat")) %>% 
  # Drop NA abundance values (seems like they were unsampled from structure of original data)
  dplyr::filter(abundance != "na")

# Re-check structure
dplyr::glimpse(proj5)

# Create good/new file name
proj5_name <- "lter-arc_alaska_acidictussock_1996-1999_vertebrates_plants.csv"
proj5_path <- file.path("data", "drydock", proj5_name)

# Export locally
write.csv(x = proj5, na = '', row.names = F, file = proj5_path)

# Export to Drive
googledrive::drive_upload(media = proj5_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 6 ----
## ------------------------------------------- ##

# Reason for purgatory status
## Extra headers **that contain necessary metadata**

# Identify the input files + what they should be called when they are output
proj6_rawfiles <- c("Freestone_et_al_2019_data_newjersey.csv",
                    "Freestone_et_al_2019_data_panama.csv")
proj6_tidyfiles <- c("freestone_newjersey_year_predators_seagrass.csv",
                     "freestone_panama_year_predators_seagrass.csv")

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
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))
    
}

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 7 ----
## ------------------------------------------- ##

# Reason for purgatory status
## Abundance is implied by number of rows with particular genera so: 
## needs to be calculated by number of rows per combination of grouping variables

# Read in data
proj7_raw <- read.csv(file = file.path("data", "purgatory", "PointCounts_Week12.csv"))

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
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 8 ----
## ------------------------------------------- ##

# Reason for purgatory status
## Data collected per tree / seedling
## Need to summarize within spatial groups & species to get tree counts

# Read in data
proj8_raw <- read.csv(file.path("data", "purgatory", "hf174-06-tree-seedlings-2010.csv"))

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
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 9 ----
## ------------------------------------------- ##

# Reason for purgatory status
## Bad header in first two rows & inclusion of standard error columns
## Deleted bad header manually and re-uploaded to purgatory as a CSV
## Removal of standard error columns accomplished below

# Read in data
proj9_raw <- read.csv(file.path("data", "purgatory", "41467_2016_BFncomms11833_MOESM1571_ESM.csv"))

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
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Project 10 ----
## ------------------------------------------- ##

# Reason for purgatory status
## Unequal numbers of plots within treatments
## Need to **randomly** subset within grouping variables to make reps equal

# Read in data
proj10_raw <- read.csv(file.path("data", "purgatory", "boer-ca-n4.csv"))

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
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Purgatory TEMPLATE ----
## ------------------------------------------- ##
## Duplicate and flesh out one copy!

# Reason for purgatory status
## 

# Read in data
proj0_raw <- read.csv(file.path("data", "purgatory", "BAD_FILE.csv"))

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
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# End ----
