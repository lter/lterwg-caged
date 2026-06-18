## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Dominance Calculation
## --------------------------------------------------------------- ##
# Purpose
## Calculate mean differences and log response ratios (LRR) for dominance

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
dom.diff_v01 <- read.csv(file.path("data", "05-D_caged_dominance_all-scales.csv"))

# Check structure
dplyr::glimpse(dom.diff_v01)

## ------------------------------------------- ##
# Prepare the Data ----
## ------------------------------------------- ##

# Do some needed preparatory calculatation
dom.diff_v02 <- dom.diff_v01 %>% 
  # Remove columns that we can get back from 'source'
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group"))) %>% 
  # Remove missing values & bad cage treatments
  dplyr::filter(!is.na(dominance)) %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  # Summarize within treatments/etc.
  dplyr::group_by(dplyr::across(dplyr::all_of(
    setdiff(x = names(.), y = c("dominance", "max.abundance", "total.abundance"))))) %>% 
  dplyr::summarize(dominance.mean = mean(dominance, na.rm = TRUE),
    .groups = "drop")

# Check structure
dplyr::glimpse(dom.diff_v02)

## ------------------------------------------- ##
# Separate Caged & Uncaged ---
## ------------------------------------------- ##

# Split off uncaged data
uncage_diff <- dom.diff_v02 %>% 
  dplyr::filter(cage.treatment_std == "uncaged") %>% 
  dplyr::select(-dplyr::starts_with("cage.treatment_")) %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("dominance.mean"))))) %>% 
  dplyr::summarize(uncaged.dominance.mean = mean(dominance.mean, na.rm = TRUE),
    .groups = "drop") 

# Check structure
dplyr::glimpse(uncage_diff)

# And ditch uncaged from the other data
cage_diff <- dom.diff_v02 %>% 
  dplyr::filter(cage.treatment_std == "caged") %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("dominance.mean"))))) %>% 
  dplyr::summarize(dominance.mean = mean(dominance.mean, na.rm = TRUE),
    .groups = "drop") 

# Check structure
dplyr::glimpse(cage_diff)

## ------------------------------------------- ##
# Join Caged/Uncaged Data ----
## ------------------------------------------- ##

# Join the two data together
dom.diff_v03 <- cage_diff %>% 
  dplyr::full_join(x = ., y = uncage_diff,
    by = dplyr::join_by(source, exp.name, exp.design.4, exp.design.3, exp.design.2, 
      exp.design.1, year, dom.design.level)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("dominance.mean"),
    .fns = ~ ifelse(is.na(.), yes = 0, no = .))) %>% 
  dplyr::filter(!is.na(cage.treatment_std))

# Which sources/experiments lacked data for either caged or uncaged replicates?
supportR::diff_check(old = unique(dom.diff_v02$source), new = unique(dom.diff_v03$source))
supportR::diff_check(old = unique(dom.diff_v02$exp.name), new = unique(dom.diff_v03$exp.name))

# Check structure
dplyr::glimpse(dom.diff_v03)

## ------------------------------------------- ##
# Calculate Difference & LRR ----
## ------------------------------------------- ##

# Find minimum value greater than 0
(bump <- dom.diff_v02 %>% 
  dplyr::filter(!is.na(dominance.mean) & dominance.mean > 0) %>% 
  dplyr::pull(dominance.mean) %>% 
  min())

# Calculate diff and LRR
dom.diff_v04 <- dom.diff_v03 %>% 
  dplyr::mutate(
    dominance.mean.diff = (uncaged.dominance.mean + bump) - (dominance.mean + bump),
    dominance.mean.lrr = log2((uncaged.dominance.mean + bump) / (dominance.mean + bump)))

# Check structure
dplyr::glimpse(dom.diff_v04)

## ------------------------------------------- ##
# Re-Generate 'Source' Component Columns ----
## ------------------------------------------- ##

# Split 'source' by delimiter
dom.diff_v05 <- dom.diff_v04 %>% 
  tidyr::separate_wider_delim(cols = source, delim = "_",
    names = c("organization", "site", "project.name", 
      "sampling.years", "excluded.group", "measured.group"),
    cols_remove = FALSE) %>% 
  dplyr::relocate(source, .before = dplyr::everything())

# Check structure
dplyr::glimpse(dom.diff_v05)

## ------------------------------------------- ##
# Re-Identify 'Finest Scales' ----
## ------------------------------------------- ##

# Get a 'no NA' version of beta dispersion
dom.diff_fine_v01 <- dom.diff_v05 %>% 
  dplyr::filter(!is.na(dominance.mean) & !is.na(uncaged.dominance.mean))

# Make a list for outputs
dom.diff_fine_list <- list()

# Loop across sources and experiments to re-identify finest scales
for(finest_src in sort(unique(dom.diff_fine_v01$source))){
  # finest_src <- "pascual_argentina_saltmarshexpa_2014_guineapigs_vegetation.csv"
  
  # Subset to that source
  dom.diff_fine_src <- dplyr::filter(dom.diff_fine_v01, source == finest_src)
  
  # Iterate across exp.names
  for(finest_name in sort(unique(dom.diff_fine_src$exp.name))){
    # finest_name <- "pascual_argentina_saltmarshexpa_2014_guineapigs_vegetation.csv"
    
    # Progress message
    message("Identifying finest scale for '", finest_name, "'")
    
    # Subset the data to only this experiment name
    dom.diff_fine_sub <- dplyr::filter(dom.diff_fine_src, exp.name == finest_name)
    
    # Make another subset for each design level
    dom.diff_fine_sub_des1 <- dplyr::filter(dom.diff_fine_sub, dom.design.level == "exp.design.1")
    dom.diff_fine_sub_des2 <- dplyr::filter(dom.diff_fine_sub, dom.design.level == "exp.design.2")
    dom.diff_fine_sub_des3 <- dplyr::filter(dom.diff_fine_sub, dom.design.level == "exp.design.3")
    dom.diff_fine_sub_des4 <- dplyr::filter(dom.diff_fine_sub, dom.design.level == "exp.design.4")
    dom.diff_fine_sub_name <- dplyr::filter(dom.diff_fine_sub, dom.design.level == "exp.name")
    
    # Work through the design levels sequentially (lowest to highest)
    if(nrow(dom.diff_fine_sub_des1) >= 1){
      
      # Add to list
      dom.diff_fine_list[[paste0(finest_src, finest_name)]] <- dom.diff_fine_sub_des1
      
      # Print a message too
      message("For '", finest_name, "' exp.design.1 was the finest level with both treatments") }
    
    # Do the same for design 2
    else if(nrow(dom.diff_fine_sub_des2) >= 1){      
      dom.diff_fine_list[[paste0(finest_src, finest_name)]] <- dom.diff_fine_sub_des2
      message("For '", finest_name, "' exp.design.2 was the finest level with both treatments") }
    
    # And design 3
    else if(nrow(dom.diff_fine_sub_des3) >= 1){      
      dom.diff_fine_list[[paste0(finest_src, finest_name)]] <- dom.diff_fine_sub_des3
      message("For '", finest_name, "' exp.design.3 was the finest level with both treatments") }
    
    # And design 4
    else if(nrow(dom.diff_fine_sub_des4) >= 1){      
      dom.diff_fine_list[[paste0(finest_src, finest_name)]] <- dom.diff_fine_sub_des4
      message("For '", finest_name, "' exp.design.4 was the finest level with both treatments") }
    
    # And the experiment name
    else if(nrow(dom.diff_fine_sub_name) >= 1){      
      dom.diff_fine_list[[paste0(finest_src, finest_name)]] <- dom.diff_fine_sub_name
      message("For '", finest_name, "' exp.name was the finest level with both treatments") }
    
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist the list that we just created
dom.diff_fine_v02 <- purrr::list_rbind(dom.diff_fine_list)

# Check structure
dplyr::glimpse(dom.diff_fine_v02)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Make final data objects
dom.diff_all <- dom.diff_v05
dom.diff_fine <- dom.diff_fine_v02

# Check structure
dplyr::glimpse(dom.diff_all)
dplyr::glimpse(dom.diff_fine)

# Define file names
dom.diff_all_filename <- "06-D_caged_dominance-diff_all-scales.csv"
dom.diff_fine_filename <- "06-D_caged_dominance-diff_fine-scales.csv"

# Export locally
write.csv(x = dom.diff_all, row.names = FALSE, na = '',
  file = file.path("data", dom.diff_all_filename))
write.csv(x = dom.diff_fine, row.names = FALSE, na = '',
  file = file.path("data", dom.diff_fine_filename))

# Check source/experiment counts for both
length(unique(dom.diff_all$source)); length(unique(dom.diff_all$exp.name))
length(unique(dom.diff_fine$source)); length(unique(dom.diff_fine$exp.name))

# Make an 'experiment name' only
dom.diff_exp <- dom.diff_all %>% 
  dplyr::filter(dom.design.level == "exp.name") %>% 
  dplyr::select(-dplyr::starts_with("exp.design."))

# Check structure
dplyr::glimpse(dom.diff_exp)

# Export
write.csv(x = dom.diff_exp, row.names = F, na = '', 
  file = file.path("data", "06-D_caged_dominance-diff_expname.csv"))

# End ----
