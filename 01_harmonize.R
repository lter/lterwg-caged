## --------------------------------------------------------------- ##
                  # CAGED Harmonization Workflow
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, googledrive, supportR)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "raw"), showWarnings = F)

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
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
files_drive

# Identify local files
files_local <- dir(path = file.path("data", "raw"))
files_local

# Overwrite local data files?
update <- TRUE

# Identify desired files
if(update == T) {
  files_wanted <- files_drive 
} else {
  files_wanted <- files_drive %>%
    dplyr::filter(!name %in% files_local)
}

# Download them!
purrr::walk2(.x = files_wanted$id, .y = files_wanted$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw", .y)))

# Grab the data key
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M")) %>% 
  dplyr::filter(name == "caged_data-key")

# Did that work?
key_drive

# Download the data key
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", key_drive$name))

## ------------------------------------------- ##
# Harmonize! ----
## ------------------------------------------- ##

# Read in data key
key <- read.csv(file = file.path("data", "caged_data-key.csv"))

# Check that looks roughly right
dplyr::glimpse(key)

# Perform harmonization
combo_v1 <- ltertools::harmonize(key = key, 
                                 raw_folder = file.path("data", "raw"),
                                 data_format = "csv", quiet = F)

# Check that structure out
dplyr::glimpse(combo_v1)

## ------------------------------------------- ##
# Treatments ----
## ------------------------------------------- ##

# Combine/streamline treatment information
combo_v2 <- combo_v1 %>% 
  # Combine into a single treatment column
  dplyr::mutate(
    original.treatment = dplyr::case_when(
      ## Use central treatment (if exists)
      nchar(orig.treat) != 0 ~ orig.treat,
      ## Combine fire/fence/gap for relevant study
      source == "royo_westvirginia_fernow_2000-2013_deer_plants.csv" ~ paste(orig.treat_fire, orig.treat_fence, orig.treat_gap, sep = "_"),
      ## Combine cage/disturbance/nutrients for relevant study
      source == "lter-mcr_moorea_recharge_2018-2022_fish_benthic.csv" ~ paste(orig.treat_cage, orig.treat_disturbance, orig.treat_nutrients, sep = "_"),
      ## Combine cage/insecticide
      source == "lter-bonanzacreek_alaska_bnz-lter_2012-2015_vertebrate_plants.csv" ~ paste(orig.treat_cage, orig.treat_insecticide, sep = "_"),
      ## Combine prairie dog & cattle cages
      source == "porensky_wyoming_nex_2015-2024_prairiedogs_vegetation.csv" ~ paste(orig.treat_cage.prairie.dog, orig.treat_cage.cattle, sep = "_"),
      ## Otherwise, put in warning text
      T ~ "NO TREATMENT IDENTIFIED"),
    .before = orig.treat) %>% 
  # Drop now superseded precursor columns
  dplyr::select(-dplyr::contains("orig.treat"))

# Check resulting treatment / source combos
combo_v2 %>% 
  dplyr::select(source, original.treatment) %>% 
  dplyr::distinct() %>% 
  as.data.frame()

# Check for any 'bad' treatments
combo_v2 %>% 
  dplyr::select(source, original.treatment) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(original.treatment == "NO TREATMENT IDENTIFIED")

# Check for lost columns
setdiff(x = names(combo_v1), y = names(combo_v2))

# Check structure
dplyr::glimpse(combo_v2)

## ------------------------------------------- ##
# Zero Fill Long Communities ----
## ------------------------------------------- ##

# Need to separate long/wide data to handle 0s/missing data
combo_v3 <- combo_v2 %>% 
  # Generate 'flag' for long versus wide data
  dplyr::group_by(source) %>% 
  dplyr::mutate(data_are_long = any( all(!is.na(orig.species)) ) ) %>% 
  dplyr::ungroup()

# Also check for non-numbers in the 'abundance' column for long data
supportR::num_check(data = combo_v3, col = "abundance")

# Separate long from wide data
long_split <- combo_v3 %>% 
  dplyr::filter(data_are_long == TRUE) %>% 
  dplyr::select(-data_are_long) %>% 
  # Make abundance numeric in case we need to summarize across duplicates
  dplyr::mutate(abundance = as.numeric(abundance))

# Separate wide from long data
wide_split <- combo_v3 %>% 
  dplyr::filter(data_are_long == FALSE) %>% 
  dplyr::select(-data_are_long)

# Check that's the right number of rows
nrow(combo_v3) == nrow(long_split) + nrow(wide_split)

# Make a list to store outputs
zerofill_list <- list()

# Process long data
for(focal_source in unique(long_split$source)){
  
  # Print progress message
  message("Zero filling dataset: '", focal_source, "'")
  
  # Subset to focal dataset
  focal_sub <- long_split %>% 
    dplyr::filter(source == focal_source) %>% 
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>% 
    dplyr::filter(nchar(orig.species) != 0 & !is.na(orig.species)) %>% 
    dplyr::distinct()
  
  # Identify all columns other than the 'abundance' column
  focal_names <- setdiff(x = names(focal_sub), y = c("orig.species", "abundance"))
  
  # Average across any duplicates (should be no duplicates)
  focal_smy <- focal_sub %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(x = c(focal_names, "orig.species")))) %>% 
    dplyr::summarize(abundance = mean(abundance, na.rm = T),
                     .groups = "keep") %>% 
    dplyr::ungroup()
  
  # Pivot to wide format (filling with zeros on the way)
  focal_flip <- focal_smy %>% 
    tidyr::pivot_wider(names_from = orig.species,
                       values_from = abundance,
                       values_fill = 0)
  
  # Pivot back to long format
  focal_zerofill <- focal_flip %>% 
    tidyr::pivot_longer(cols = -dplyr::all_of(focal_names),
                        names_to = "original.species",
                        values_to = "abundance")
  
  # Add to output list
  zerofill_list[[focal_source]] <- focal_zerofill
  
}

# Unlist the list
long_v2 <- zerofill_list %>% 
  purrr::list_rbind(x = .) %>% 
  # And make abundance back into a character vector
  dplyr::mutate(abundance = as.character(abundance))

# Check structure
dplyr::glimpse(long_v2)

## ------------------------------------------- ##
# Reshape Wide Communities to Long ----
## ------------------------------------------- ##

# Re-check structure of wide 'split' of data
dplyr::glimpse(wide_split)

# Make an output list
pivot_list <- list()

# Process wide data
for(focal_source in unique(wide_split$source)){
  
  # Print progress message
  message("Reshaping dataset: '", focal_source, "'")
  
  # Subset data to relevant source
  focal_sub <- wide_split %>% 
    dplyr::filter(source == focal_source) %>% 
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>% 
    dplyr::distinct()
  
  # Identify taxonomic granularity of this dataset
  tax_gran <- sort(unique(stringr::str_extract(string = names(focal_sub), 
                                   pattern = "\\.[:alpha:]{1,9}_")))
  
  # Pivot longer for this taxonomic level
  focal_pivot <- focal_sub %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with(paste0("orig", tax_gran)),
                        names_to = paste0("orig", tax_gran),
                        values_to = paste0("abun", tax_gran))
  
  # Add output to list
  pivot_list[[focal_source]] <- focal_pivot
  
}

# Unlist the output
wide_v2 <- pivot_list %>% 
  purrr::list_rbind(x = .) %>% 
  # Coalesce abundance information
  dplyr::mutate(abundance = dplyr::case_when(
    is.na(abun.species_) == F ~ abun.species_,
    is.na(abun.taxa_) == F ~ abun.taxa_,
    is.na(abun.function_) == F ~ abun.function_,
    T ~ NA)) %>% 
  # Drop superseded abundance columns
  dplyr::select(-dplyr::starts_with("abun.")) %>% 
  # Tidy up taxon column names
  dplyr::rename(original.species = orig.species_,
                original.taxon = orig.taxa_,
                original.function = orig.function_)

# Check structure
dplyr::glimpse(wide_v2)

# Check for lost/gained columns from pre-loop data
supportR::diff_check(old = names(wide_split), new = names(wide_v2))

## ------------------------------------------- ##
# Recombine Wide / Long Data ----
## ------------------------------------------- ##

# Re-check structure to remind self
dplyr::glimpse(wide_v2)
dplyr::glimpse(long_v2)

# Combine the data and do needed wrangling
combo_v4 <- dplyr::bind_rows(wide_v2, long_v2)

# Recheck structure
dplyr::glimpse(combo_v4)

## ------------------------------------------- ##
# Column Re-Ordering ----
## ------------------------------------------- ##

# Reorder columns more logically
combo_v5 <- combo_v4 %>% 
  # Treatment / unique ID information first
  dplyr::relocate(original.treatment, .after = source) %>% 
  dplyr::relocate(unique.id, .after = source) %>% 
  # Spatial scale (lower numbers are more granular)
  dplyr::relocate(spatial.scale.4, spatial.scale.3, spatial.scale.2, spatial.scale.1,
                  depth, .after = year) %>% 
  # Taxon information after spatial information
  dplyr::relocate(original.taxon, original.function, original.species,
                  .after = depth)

# Check structure
dplyr::glimpse(combo_v5)

# Check that no columns are lost
setdiff(x = names(combo_v4), y = names(combo_v5))

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
combo_v6 <- combo_v5 %>% 
  # Remove any rows where no taxon information is included
  dplyr::filter(all(is.na(original.taxon),
                     is.na(original.function),
                     is.na(original.species)) != T) %>% 
  # Remove any rows where no metric of abundance is included
  dplyr::filter(nchar(abundance) != 0 & is.na(abundance) != T) %>% 
  # Drop duplicate rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(combo_v6)
  
# Export locally
write.csv(x = combo_v6, row.names = F, na = '',
          file = file.path("data", "caged_harmonized.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "caged_harmonized.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
