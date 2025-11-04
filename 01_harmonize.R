## --------------------------------------------------------------- ##
# CAGED Harmonization Workflow
## --------------------------------------------------------------- ##

# Purpose:
## Use a data key to standardize the column names of all input files
## And combine all standardized tables into one large table
## You need to run 00 and 000 before you can run this script

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Harmonize! ----
## ------------------------------------------- ##

# Read in & check data key
key <- read.csv(file = file.path("data", "caged_data-key.csv")) %>% 
  ltertools::check_key(key = .)

# Check that looks roughly right
dplyr::glimpse(key)

# Read in all data (as a list)
list_raw <- ltertools::read(raw_folder = file.path("data", "raw"), 
                            data_format = "csv")

# Check structure of one element
dplyr::glimpse(list_raw[10])

# Make a list to store standardized outputs
list_std <- list()

# Now, let's loop across datasets in the key
for(focal_src in sort(intersect(x = key$source, y = names(list_raw)))){
  # focal_src <- "beguin_quebec_largeherbivores_1995-2011_whitetaileddeer_understoryplants.csv" # composite
  # focal_src <- "clausing_newzealand_intertidalexclosure_2010-2012_grazers_algae.csv" # wide tax
  # focal_src <- "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" # wide spatial
  # focal_src <- "villar_brazil_cbo_2009-2016_tapirs_forest.csv" # long tax
  
  # Progress message
  message("Standarizing file: '", focal_src, "'")
  
  # Standardize this file
  focal_v1 <- ltertools::standardize(focal_file = focal_src, 
                                     key = key, 
                                     df_list = list_raw)
  
  # Handle 'composite' columns if any are present
  if(any(stringr::str_detect(string = names(focal_v1), pattern = "composite_"))){
    
    # Identify composite column name
    comp_col <- names(focal_v1)[stringr::str_detect(string = names(focal_v1), pattern = "composite_")]
    
    # Identify what columns it should be split into (removing "composite" flag)
    split_cols <- stringr::str_split_1(string = gsub("composite_", "", comp_col), pattern = "_")
    
    # Actually split composite column
    focal_v2 <- focal_v1 %>% 
      tidyr::separate_wider_delim(cols = dplyr::contains(comp_col),
                                  names = split_cols, delim = " ")
    
    # If no such columns found, just increment the data object version number
  } else { focal_v2 <- focal_v1 }
  
  # Do some bonus processing if taxa are in wide format
  if(any(stringr::str_detect(string = names(focal_v2), pattern = "orig.taxa_"))){
    
    # Flip it to long format & tidy up taxa names
    focal_v3 <- focal_v2 %>% 
      tidyr::pivot_longer(cols = dplyr::starts_with("orig.taxa_"),
                          names_to = "orig.taxa",
                          values_to = "abundance") %>% 
      dplyr::mutate(orig.taxa = gsub(pattern = "orig.taxa_", replacement = "", x = orig.taxa))
    
  } else { focal_v3 <- focal_v2 }
  
  # Also process wide-format spatial information (if any is found)
  if(any(stringr::str_detect(string = names(focal_v3), pattern = "exp.design.1_"))){
    
    # Flip to long format and tidy up resulting long-form design column
    focal_v4 <- focal_v3 %>% 
      tidyr::pivot_longer(cols = dplyr::starts_with("exp.design.1_"),
                          names_to = "exp.design.1",
                          values_to = "abundance") %>% 
      dplyr::mutate(exp.design.1 = gsub(pattern = "exp.design.1_", replacement = "",
                                        x = exp.design.1))
    
  } else { focal_v4 <- focal_v3 }
  
  # Make a final object
  focal_std <- focal_v4
  
  # Add to output list
  list_std[[focal_src]] <- focal_std
  
} # Close loop

# Unlist outputs 
combo_v1 <- purrr::list_rbind(x = list_std)

# Check structure
dplyr::glimpse(combo_v1)

## ------------------------------------------- ##
# Streamline Treatments ----
## ------------------------------------------- ##

# What kinds of treatments are used (in at least one dataset)?
combo_v1 %>% 
  dplyr::select(dplyr::contains("orig.treat")) %>% 
  dplyr::glimpse()

# Combine/streamline treatment information
combo_v2 <- combo_v1 %>% 
  # Rename treatments more clearly
  dplyr::rename(
    treat.artificial = orig.treat_artificial,
    # treat.exposure = orig.treat_exposure,
    treat.insecticide = orig.treat_insecticide,
    # treat.canopy = orig.treat_canopy,
    treat.distance = orig.treat_dist,
    treat.disturbance = orig.treat_disturbance, #IDK what happened to this but it disappeared during the great May 6th power outage
    treat.gap = orig.treat_gap,
    treat.nitrogen.addition = orig.treat_nitrogen.addition,
    treat.fire = orig.treat_burn
  ) %>% 
  # Combine synonymous-sounding treatments
  dplyr::mutate(
    treat.nutrients = dplyr::coalesce(orig.treat_nut.trt, orig.treat_nutrients, 
                                      orig.treat_nitrogen.treatment),
  ) %>% 
  # Coalesce cage/cage-related treatments separately
  dplyr::mutate(treat.cage = dplyr::case_when(
    !is.na(orig.treat_cage) ~ orig.treat_cage,
    !is.na(orig.treat_fence) ~ orig.treat_fence,
    #  !is.na(orig.treat_cage.prairie.dog) & !is.na(orig.treat_cage.cattle) ~ 
    #   paste0(orig.treat_cage.prairie.dog, "__", orig.treat_cage.cattle),
    #  !is.na(orig.treat_cage.prairie.dog) ~ orig.treat_cage.prairie.dog,
    !is.na(orig.treat_cage.cattle) ~ orig.treat_cage.cattle,
    ## If all else fails, just use whatever the singualr original treatment column is
    !is.na(orig.treat) ~ orig.treat,
    T ~ "NO CAGE TREATMENT IDENTIFIED")) %>% 
  # Move treatment columns to the left
  dplyr::relocate(dplyr::starts_with("treat."), .after = source) %>% 
  # Drop now superseded precursor columns
  dplyr::select(-dplyr::contains("orig.treat"))

# Check for any 'bad' treatments
combo_v2 %>% 
  dplyr::select(source, treat.cage) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(treat.cage == "NO CAGE TREATMENT IDENTIFIED")

# Check for lost columns
supportR::diff_check(old = names(combo_v1), new = names(combo_v2))

# Check structure
dplyr::glimpse(combo_v2)

## ------------------------------------------- ##
# Column Re-Ordering ----
## ------------------------------------------- ##

# Reorder columns more logically
combo_v3 <- combo_v2 %>% 
  # Treatment information first
  dplyr::relocate(treat.cage, dplyr::starts_with("treat."), .after = source) %>% 
  # Experimental design nestedness (lower numbers are more granular)
  dplyr::relocate(exp.name, exclosure.age, .after = source) %>% 
  dplyr::relocate(exp.design.4, exp.design.3, 
                  exp.design.2, exp.design.1, .after = exclosure.age) %>% 
  dplyr::relocate(dplyr::starts_with("distance.from."), .after = exp.design.1) %>% 
  # Order temporal information
  dplyr::relocate(sampling.point, .after = year) %>% 
  # Taxon information after spatial information
  dplyr::relocate(orig.taxa, .after = dplyr::everything()) %>% 
  # Rename taxon information to avoid abbreviation
  dplyr::rename(original.taxa = orig.taxa) %>% 
  # Put abundance last
  dplyr::relocate(abundance, .after = dplyr::everything())

# Check structure
dplyr::glimpse(combo_v3)

# Check that **no columns are lost / gained**
supportR::diff_check(old = names(combo_v3), new = names(combo_v2))

## ------------------------------------------- ##
# File Name Information ----
## ------------------------------------------- ##

# Make sure all file names have correctly-formatted filenames
combo_v3 %>% 
  dplyr::select(source) %>% dplyr::distinct() %>% 
  dplyr::filter(stringr::str_count(string = source, pattern = "_") != 5)

# Break useful information out of file name ("source" column)
combo_v4 <- combo_v3 %>% 
  # Separate by underscore
  tidyr::separate_wider_delim(cols = source, delim = "_",
                              names = c("organization", "site", 
                                        "project.name", "sampling.years", 
                                        "excluded.group", "measured.group"),
                              cols_remove = F) %>%
  # Remove file name from final bit of file
  dplyr::mutate(measured.group = gsub(pattern = "\\.csv", replacement = "",
                                      x = measured.group)) %>% 
  # Relocate source back to first position
  dplyr::relocate(source, .before = dplyr::everything())

# Check structure
dplyr::glimpse(combo_v4)

## ------------------------------------------- ##
# Check for Non-Numeric Abundance ----
## ------------------------------------------- ##

# Identify any instances of non-numeric abundance
supportR::num_check(data = combo_v4, col = "abundance")

# Do needed processing
combo_v5 <- combo_v4 %>% 
  # Remove any rows where no taxon information is included
  dplyr::filter(is.na(original.taxa) != T) %>% 
  # Remove non-numbers
  dplyr::mutate(abundance = gsub(pattern = "^\\.$", replacement = "", x = abundance)) %>% 
  dplyr::mutate(abundance = ifelse(test = abundance %in% c("n/a", "—", "na",
                                                           "#VALUE!"),
                                   yes = "", no = abundance)) %>% 
  # Remove any rows where no metric of abundance is included
  dplyr::filter(is.na(abundance) != T &
                  nchar(abundance) != 0 &
                  abundance != "NaN") %>% 
  # Make abundance truly a number
  dplyr::mutate(abundance = as.numeric(abundance))

# Check structure
dplyr::glimpse(combo_v5)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
combo_v99 <- combo_v5

# Check structure
dplyr::glimpse(combo_v99)

# Identify tidy file name / path
combo_name <- "01_caged_harmonized.csv"
combo_path <- file.path("data", combo_name)

# Export locally
write.csv(x = combo_v99, row.names = F, na = '', file = combo_path)

# End ----
