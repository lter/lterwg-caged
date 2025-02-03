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
update <- FALSE

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
# Pivot Wide Spatial Data ----
## ------------------------------------------- ##

# Identify all data files in the key that were in wide spatial format
spat_wide_files <- key %>% 
  dplyr::filter(stringr::str_detect(string = tidy_name, pattern = "exp.design.1_")) %>% 
  dplyr::select(source) %>% 
  dplyr::distinct() %>% 
  dplyr::pull()

# Separate data
spat_wides <- combo_v2 %>% 
  dplyr::filter(source %in% spat_wide_files) %>% 
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.))))
spat_longs <- combo_v2 %>% 
  dplyr::filter(source %in% spat_wide_files == F) %>% 
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.))))

# Check none are lost
nrow(combo_v2) == nrow(spat_wides) + nrow(spat_longs)

# Rotate wide spatial into long
spat_wides_pivot <- spat_wides %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("exp.design.1_"),
                      names_to = "exp.design.1_pivot",
                      values_to = "abundance") %>% 
  dplyr::filter(!is.na(exp.design.1_pivot) & !is.na(abundance)) %>% 
  dplyr::mutate(exp.design.1 = gsub(pattern = "exp.design.1_", replacement = "",
                                    x = exp.design.1_pivot)) %>% 
  dplyr::select(-exp.design.1_pivot)
 
# Check structure
dplyr::glimpse(spat_wides_pivot)

# Recombine data
combo_v3 <- dplyr::bind_rows(spat_longs, spat_wides_pivot)

# Identify columns that are dropped
supportR::diff_check(old = names(combo_v2), new = names(combo_v3))

# Check structure
dplyr::glimpse(combo_v3)

## ------------------------------------------- ##
# Zero Fill Long Communities ----
## ------------------------------------------- ##

# Need to separate long/wide data to handle 0s/missing data
combo_v4 <- combo_v3 %>% 
  # Generate 'flag' for long versus wide data
  dplyr::group_by(source) %>% 
  dplyr::mutate(data_are_long = !is.na(orig.taxa) ) %>% 
  dplyr::ungroup()

# Also check for non-numbers in the 'abundance' column for long data
supportR::num_check(data = combo_v4, col = "abundance")

# Separate long from wide data
tax_longs <- combo_v4 %>% 
  dplyr::filter(data_are_long == TRUE) %>% 
  dplyr::select(-data_are_long) %>% 
  # Make abundance numeric in case we need to summarize across duplicates
  dplyr::mutate(abundance = as.numeric(abundance))

# Separate wide from long data
tax_wides <- combo_v4 %>% 
  dplyr::filter(data_are_long == FALSE) %>% 
  dplyr::select(-data_are_long)

# Check that's the right number of rows
nrow(combo_v4) == nrow(tax_longs) + nrow(tax_wides)

# Make a list to store outputs
zerofill_list <- list()

# Process long data
for(focal_source in unique(tax_longs$source)){
  
  # Print progress message
  message("Zero filling dataset: '", focal_source, "'")
  
  # Subset to focal dataset
  focal_sub <- tax_longs %>% 
    dplyr::filter(source == focal_source) %>% 
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>% 
    dplyr::filter(nchar(orig.taxa) != 0 & !is.na(orig.taxa)) %>% 
    dplyr::distinct()
  
  # Identify all columns other than the 'taxonomy' & 'abundance' columns
  focal_names <- setdiff(x = names(focal_sub), y = c("orig.taxa", "abundance"))
  
  # Average across any duplicates (should be no duplicates)
  focal_smy <- focal_sub %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(x = c(focal_names, "orig.taxa")))) %>% 
    dplyr::summarize(abundance = mean(abundance, na.rm = T),
                     .groups = "keep") %>% 
    dplyr::ungroup()
  
  # Pivot to wide format (filling with zeros on the way)
  focal_flip <- focal_smy %>% 
    tidyr::pivot_wider(names_from = orig.taxa,
                       values_from = abundance,
                       values_fill = 0)
  
  # Pivot back to long format
  focal_zerofill <- focal_flip %>% 
    tidyr::pivot_longer(cols = -dplyr::all_of(focal_names),
                        names_to = "original.taxa",
                        values_to = "abundance")
  
  # Add to output list
  zerofill_list[[focal_source]] <- focal_zerofill
  
}

# Unlist the list
tax_longs_v2 <- zerofill_list %>% 
  purrr::list_rbind(x = .) %>% 
  # And make abundance back into a character vector
  dplyr::mutate(abundance = as.character(abundance))

# Check structure
dplyr::glimpse(tax_longs_v2)

## ------------------------------------------- ##
# Reshape Wide Communities to Long ----
## ------------------------------------------- ##

# Re-check structure of wide 'split' of data
dplyr::glimpse(tax_wides)

# Make an output list
pivot_list <- list()

# Process wide data
for(focal_source in unique(tax_wides$source)){
  
  # Print progress message
  message("Reshaping dataset: '", focal_source, "'")
  
  # Subset data to relevant source
  focal_sub <- tax_wides %>% 
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
tax_wides_v2 <- pivot_list %>% 
  purrr::list_rbind(x = .) %>% 
  # Coalesce abundance information
  dplyr::mutate(abundance = ifelse(is.na(abun.taxa_),
                                   yes = NA, no = abun.taxa_)) %>% 
  # Drop superseded abundance columns
  dplyr::select(-dplyr::starts_with("abun.")) %>% 
  # Tidy up taxon column names
  dplyr::rename(original.taxa = orig.taxa_)

# Check structure
dplyr::glimpse(tax_wides_v2)

# Check for lost/gained columns from pre-loop data
supportR::diff_check(old = names(tax_wides), new = names(tax_wides_v2))

## ------------------------------------------- ##
# Recombine Wide / Long Data ----
## ------------------------------------------- ##

# Re-check structure to remind self
dplyr::glimpse(tax_wides_v2)
dplyr::glimpse(tax_longs_v2)

# Combine the data and do needed wrangling
combo_v5 <- dplyr::bind_rows(tax_wides_v2, tax_longs_v2) %>% 
  # Clean up taxa names
  dplyr::mutate(original.taxa = gsub(pattern = "orig\\.taxa_", replacement = "",
                                     x = original.taxa))

# Recheck structure
dplyr::glimpse(combo_v5)

## ------------------------------------------- ##
# Column Re-Ordering ----
## ------------------------------------------- ##

# Reorder columns more logically
combo_v6 <- combo_v5 %>% 
  # Treatment information first
  dplyr::relocate(original.treatment, exclosure.age, .after = source) %>% 
  # Experimental design nestedness (lower numbers are more granular)
  dplyr::relocate(exp.name, .after = year) %>% 
  dplyr::relocate(exp.design.3, exp.design.2, exp.design.1, .after = exp.name) %>% 
  dplyr::relocate(dplyr::starts_with("distance.from."), .after = exp.design.1) %>% 
  # Order temporal information
  dplyr::relocate(sampling.point, .after = year) %>% 
  # Taxon information after spatial information
  dplyr::relocate(original.taxa, .after = dplyr::starts_with("distance.from."))

# Check structure
dplyr::glimpse(combo_v6)

# Check that no columns are lost / gained
supportR::diff_check(old = names(combo_v5), new = names(combo_v6))

## ------------------------------------------- ##
# File Name Information ----
## ------------------------------------------- ##

# Break useful information out of file name ("source" column)
combo_v7 <- combo_v6 %>% 
  # Separate by underscore
  tidyr::separate_wider_delim(cols = source, delim = "_",
                              names = c("organization", "site", 
                                        "experiment.name", "sampling.years", 
                                        "excluded.group", "measured.group"),
                              cols_remove = F) %>%
  # Remove file name from final bit of file
  dplyr::mutate(measured.group = gsub(pattern = "\\.csv", replacement = "",
                                      x = measured.group)) %>% 
  # Relocate source back to first position
  dplyr::relocate(source, .before = dplyr::everything())

# Check structure
dplyr::glimpse(combo_v7)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
combo_v99 <- combo_v7 %>% 
  # Remove any rows where no taxon information is included
  dplyr::filter(is.na(original.taxa) != T) %>% 
  # Replace "NA" with zero where appropriate
  dplyr::mutate(abundance = dplyr::case_when(
    source == "cain_australia_herbexclusion_2021_macropod_plants.csv" &
      abundance == "n/a" ~ "0",
    T ~ abundance)) %>% 
  # Remove any rows where no metric of abundance is included
  dplyr::filter(is.na(abundance) != T &
                  nchar(abundance) != 0 &
                  abundance != "NaN") %>% 
  # Drop duplicate rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(combo_v99)
  
# Export locally
write.csv(x = combo_v99, row.names = F, na = '',
          file = file.path("data", "01_caged_harmonized.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "01_caged_harmonized.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
