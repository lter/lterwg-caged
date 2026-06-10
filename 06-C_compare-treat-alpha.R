## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Alpha Diversity Calculation
## --------------------------------------------------------------- ##
# Purpose
## Calculate mean differences and log response ratios (LRR) for alpha diversity

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Load the relevant output
alp.diff_v01 <- read.csv(file.path("data", "05-C_caged_alpha-div_all-scales.csv"))

# Check structure
dplyr::glimpse(alp.diff_v01)

## ------------------------------------------- ##
# Prepare the Data ----
## ------------------------------------------- ##

# Do some needed preparatory calculatation
alp.diff_v02 <- alp.diff_v01 %>% 
  # Remove columns that we can get back from 'source'
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group"))) %>% 
  # Remove missing values & bad cage treatments
  dplyr::filter(!is.na(alpha.diversity_richness)) %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  # Summarize within treatments/etc.
  dplyr::group_by(dplyr::across(dplyr::all_of(
    setdiff(x = names(.), y = c("alpha.diversity_richness"))))) %>% 
  dplyr::summarize(alpha.mean = mean(alpha.diversity_richness, na.rm = TRUE),
    .groups = "drop")

# Check structure
dplyr::glimpse(alp.diff_v02)

## ------------------------------------------- ##
# Separate Caged & Uncaged ---
## ------------------------------------------- ##

# Split off uncaged data
uncage_diff <- alp.diff_v02 %>% 
  dplyr::filter(cage.treatment_std == "uncaged") %>% 
  dplyr::select(-dplyr::starts_with("cage.treatment_")) %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("alpha.mean"))))) %>% 
  dplyr::summarize(uncaged.alpha.mean = mean(alpha.mean, na.rm = TRUE),
    .groups = "drop") 

# Check structure
dplyr::glimpse(uncage_diff)

# And ditch uncaged from the other data
cage_diff <- alp.diff_v02 %>% 
  dplyr::filter(cage.treatment_std == "caged") %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("alpha.mean"))))) %>% 
  dplyr::summarize(alpha.mean = mean(alpha.mean, na.rm = TRUE),
    .groups = "drop") 

# Check structure
dplyr::glimpse(cage_diff)

## ------------------------------------------- ##
# Join Caged/Uncaged Data ----
## ------------------------------------------- ##

# Join the two data together
alp.diff_v03 <- cage_diff %>% 
  dplyr::left_join(x = ., y = uncage_diff,
    by = dplyr::join_by(source, exp.name, exp.design.4, exp.design.3, exp.design.2, 
      exp.design.1, year, alpha.design.level)) %>% 
  dplyr::mutate(uncaged.alpha.mean = ifelse(is.na(uncaged.alpha.mean),
    yes = 0, no = uncaged.alpha.mean))

# Check structure
dplyr::glimpse(alp.diff_v03)

## ------------------------------------------- ##
# Calculate Difference & LRR ----
## ------------------------------------------- ##

# Find minimum value greater than 0
(bump <- alp.diff_v02 %>% 
  dplyr::filter(!is.na(alpha.mean) & alpha.mean > 0) %>% 
  dplyr::pull(alpha.mean) %>% 
  min())

# Calculate diff and LRR
alp.diff_v04 <- alp.diff_v03 %>% 
  dplyr::mutate(
    alpha.mean.diff = (uncaged.alpha.mean + bump) - (alpha.mean + bump),
    alpha.mean.lrr = log2((uncaged.alpha.mean + bump) / (alpha.mean + bump)))

# Check structure
dplyr::glimpse(alp.diff_v04)

## ------------------------------------------- ##
# Re-Generate 'Source' Component Columns ----
## ------------------------------------------- ##

# Split 'source' by delimiter
alp.diff_v05 <- alp.diff_v04 %>% 
  tidyr::separate_wider_delim(cols = source, delim = "_",
    names = c("organization", "site", "project.name", 
      "sampling.years", "excluded.group", "measured.group"),
    cols_remove = FALSE) %>% 
  dplyr::relocate(source, .before = dplyr::everything())

# Check structure
dplyr::glimpse(alp.diff_v05)

## ------------------------------------------- ##
# Re-Identify 'Finest Scales' ----
## ------------------------------------------- ##

# Get a 'no NA' version of beta dispersion
alp.diff_fine_v01 <- alp.diff_v05 %>% 
  dplyr::filter(!is.na(alpha.mean) & !is.na(uncaged.alpha.mean))

# Make a list for outputs
alp.diff_fine_list <- list()

# Loop across sources and experiments to re-identify finest scales
for(finest_src in sort(unique(alp.diff_fine_v01$source))){
  # finest_src <- "pascual_argentina_saltmarshexpa_2014_guineapigs_vegetation.csv"
  
  # Subset to that source
  alp.diff_fine_src <- dplyr::filter(alp.diff_fine_v01, source == finest_src)
  
  # Iterate across exp.names
  for(finest_name in sort(unique(alp.diff_fine_src$exp.name))){
    # finest_name <- "pascual_argentina_saltmarshexpa_2014_guineapigs_vegetation.csv"
    
    # Progress message
    message("Identifying finest scale for '", finest_name, "'")
    
    # Subset the data to only this experiment name
    alp.diff_fine_sub <- dplyr::filter(alp.diff_fine_src, exp.name == finest_name)
    
    # Make another subset for each design level
    alp.diff_fine_sub_des1 <- dplyr::filter(alp.diff_fine_sub, alpha.design.level == "exp.design.1")
    alp.diff_fine_sub_des2 <- dplyr::filter(alp.diff_fine_sub, alpha.design.level == "exp.design.2")
    alp.diff_fine_sub_des3 <- dplyr::filter(alp.diff_fine_sub, alpha.design.level == "exp.design.3")
    alp.diff_fine_sub_des4 <- dplyr::filter(alp.diff_fine_sub, alpha.design.level == "exp.design.4")
    alp.diff_fine_sub_name <- dplyr::filter(alp.diff_fine_sub, alpha.design.level == "exp.name")
    
    # Work through the design levels sequentially (lowest to highest)
    if(nrow(alp.diff_fine_sub_des1) >= 1){
      
      # Add to list
      alp.diff_fine_list[[paste0(finest_src, finest_name)]] <- alp.diff_fine_sub_des1
      
      # Print a message too
      message("For '", finest_name, "' exp.design.1 was the finest level with both treatments") }
    
    # Do the same for design 2
    else if(nrow(alp.diff_fine_sub_des2) >= 1){      
      alp.diff_fine_list[[paste0(finest_src, finest_name)]] <- alp.diff_fine_sub_des2
      message("For '", finest_name, "' exp.design.2 was the finest level with both treatments") }
    
    # And design 3
    else if(nrow(alp.diff_fine_sub_des3) >= 1){      
      alp.diff_fine_list[[paste0(finest_src, finest_name)]] <- alp.diff_fine_sub_des3
      message("For '", finest_name, "' exp.design.3 was the finest level with both treatments") }
    
    # And design 4
    else if(nrow(alp.diff_fine_sub_des4) >= 1){      
      alp.diff_fine_list[[paste0(finest_src, finest_name)]] <- alp.diff_fine_sub_des4
      message("For '", finest_name, "' exp.design.4 was the finest level with both treatments") }
    
    # And the experiment name
    else if(nrow(alp.diff_fine_sub_name) >= 1){      
      alp.diff_fine_list[[paste0(finest_src, finest_name)]] <- alp.diff_fine_sub_name
      message("For '", finest_name, "' exp.name was the finest level with both treatments") }
    
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist the list that we just created
alp.diff_fine_v02 <- purrr::list_rbind(alp.diff_fine_list)

# Check structure
dplyr::glimpse(alp.diff_fine_v02)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Make final data objects
alp.diff_all <- alp.diff_v05
alp.diff_fine <- alp.diff_fine_v02

# Check structure
dplyr::glimpse(alp.diff_all)
dplyr::glimpse(alp.diff_fine)

# Define file names
alp.diff_all_filename <- "06-C_caged_alpha-div-diff_all-scales.csv"
alp.diff_fine_filename <- "06-C_caged_alpha-div-diff_fine-scales.csv"

# Export locally
write.csv(x = alp.diff_all, row.names = FALSE, na = '',
  file = file.path("data", alp.diff_all_filename))
write.csv(x = alp.diff_fine, row.names = FALSE, na = '',
  file = file.path("data", alp.diff_fine_filename))

# Check source/experiment counts for both
length(unique(alp.diff_all$source)); length(unique(alp.diff_all$exp.name))
length(unique(alp.diff_fine$source)); length(unique(alp.diff_fine$exp.name))

# Make an 'experiment name' only
alp.diff_exp <- alp.diff_all %>% 
  dplyr::filter(alpha.design.level == "exp.name") %>% 
  dplyr::select(-dplyr::starts_with("exp.design."))

# Check structure
dplyr::glimpse(alp.diff_exp)

# Export
write.csv(x = alp.diff_exp, row.names = F, na = '', 
  file = file.path("data", "06-C_caged_alpha-div-diff_expname.csv"))

# End ----
