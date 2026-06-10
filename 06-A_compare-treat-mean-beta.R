## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Beta Dispersion Calculation
## --------------------------------------------------------------- ##

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Load data
diff_v01 <- read.csv(file = file.path("data", "05-A_caged_beta-disp_all-scales.csv"))

# Check structure
dplyr::glimpse(diff_v01)

## ------------------------------------------- ##
# Prepare the Data ----
## ------------------------------------------- ##

# Do some needed preparatory calculatation
diff_v02 <- diff_v01 %>% 
  # Remove columns that we can get back from 'source'
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group"))) %>% 
  # Remove missing beta disp & bad cage treatments
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  # Summarize within treatments/etc.
  dplyr::group_by(dplyr::across(dplyr::all_of(
    setdiff(x = names(.), y = c(paste0("betadisp.", c("sample.size", "median", "comm.dist"))))))) %>% 
  dplyr::summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = TRUE),
    betadisp.centroid.mean = mean(betadisp.median, na.rm = TRUE),
    .groups = "drop")

# Check structure
dplyr::glimpse(diff_v02)

## ------------------------------------------- ##
# Separate Caged & Uncaged ---
## ------------------------------------------- ##

# Split off uncaged data
uncage_diff <- diff_v02 %>% 
  dplyr::filter(cage.treatment_std == "uncaged") %>% 
  dplyr::select(-dplyr::starts_with("cage.treatment_")) %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
    y = c("betadisp.mean", "betadisp.centroid.mean"))))) %>% 
  dplyr::summarize(uncaged.betadisp.mean = mean(betadisp.mean, na.rm = TRUE),
    uncaged.betadisp.centroid.mean = mean(betadisp.centroid.mean, na.rm = TRUE),
    .groups = "drop") 

# Check structure
dplyr::glimpse(uncage_diff)

# And ditch uncaged from the other data
cage_diff <- diff_v02 %>% 
  dplyr::filter(cage.treatment_std == "caged") %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
    y = c("betadisp.mean", "betadisp.centroid.mean"))))) %>% 
  dplyr::summarize(betadisp.mean = mean(betadisp.mean, na.rm = TRUE),
    betadisp.centroid.mean = mean(betadisp.centroid.mean, na.rm = TRUE),
    .groups = "drop") 

# Check structure
dplyr::glimpse(cage_diff)

## ------------------------------------------- ##
# Join Caged/Uncaged Data ----
## ------------------------------------------- ##

# Join the two data together
diff_v03 <- cage_diff %>% 
  dplyr::left_join(x = ., y = uncage_diff,
    by = dplyr::join_by(source, exp.name, exp.design.4, exp.design.3, exp.design.2, 
      exp.design.1, year, betadisp.design.level))

# Check structure
dplyr::glimpse(diff_v03)

## ------------------------------------------- ##
# Calculate Difference & LRR ----
## ------------------------------------------- ##

# Find minimum betadispersion value greater than 0
(beta_bump <- diff_v02 %>% 
  dplyr::filter(!is.na(betadisp.mean) & betadisp.mean > 0) %>% 
  dplyr::pull(betadisp.mean) %>% 
  min())

# Calculate diff and LRR
diff_v04 <- diff_v03 %>% 
  dplyr::mutate(
    betadisp.mean.diff = (uncaged.betadisp.mean + beta_bump) - (betadisp.mean + beta_bump),
    betadisp.mean.lrr = log2((uncaged.betadisp.mean + beta_bump) / (betadisp.mean + beta_bump)),
    betadisp.centroid.diff = (uncaged.betadisp.centroid.mean + beta_bump) - (betadisp.centroid.mean + beta_bump),
    betadisp.centroid.lrr = log2((uncaged.betadisp.centroid.mean + beta_bump) / (betadisp.centroid.mean + beta_bump)))

# Check structure
dplyr::glimpse(diff_v04)

## ------------------------------------------- ##
# Re-Generate 'Source' Component Columns ----
## ------------------------------------------- ##

# Split 'source' by delimiter
diff_v05 <- diff_v04 %>% 
  tidyr::separate_wider_delim(cols = source, delim = "_",
    names = c("organization", "site", "project.name", 
      "sampling.years", "excluded.group", "measured.group"),
    cols_remove = FALSE)

# Check structure
dplyr::glimpse(diff_v05)

## ------------------------------------------- ##
# Re-Identify 'Finest Scales' ----
## ------------------------------------------- ##

# Get a 'no NA' version of beta dispersion
diff_fine_v01 <- diff_v05 %>% 
  dplyr::filter(!is.na(betadisp.mean) & !is.na(uncaged.betadisp.mean))

# Make a list for outputs
diff_fine_list <- list()

# Loop across sources and experiments to re-identify finest scales
for(finest_src in sort(unique(diff_fine_v01$source))){
  # finest_src <- "pascual_argentina_saltmarshexpa_2014_guineapigs_vegetation.csv"
  
  # Subset to that source
  diff_fine_src <- dplyr::filter(diff_fine_v01, source == finest_src)
  
  # Iterate across exp.names
  for(finest_name in sort(unique(diff_fine_src$exp.name))){
    # finest_name <- "pascual_argentina_saltmarshexpa_2014_guineapigs_vegetation.csv"
    
    # Progress message
    message("Identifying finest scale for '", finest_name, "'")
    
    # Subset the data to only this experiment name
    diff_fine_sub <- dplyr::filter(diff_fine_src, exp.name == finest_name)
    
    # Make another subset for each design level
    diff_fine_sub_des1 <- dplyr::filter(diff_fine_sub, betadisp.design.level == "exp.design.1")
    diff_fine_sub_des2 <- dplyr::filter(diff_fine_sub, betadisp.design.level == "exp.design.2")
    diff_fine_sub_des3 <- dplyr::filter(diff_fine_sub, betadisp.design.level == "exp.design.3")
    diff_fine_sub_des4 <- dplyr::filter(diff_fine_sub, betadisp.design.level == "exp.design.4")
    diff_fine_sub_name <- dplyr::filter(diff_fine_sub, betadisp.design.level == "exp.name")
    
    # Work through the design levels sequentially (lowest to highest)
    ## And add the lowest one with beta dispersion for both standardized cage treatments to the output list
    if(nrow(diff_fine_sub_des1) >= 1){
      
      # Add to list
      diff_fine_list[[paste0(finest_src, finest_name)]] <- diff_fine_sub_des1
      
      # Print a message too
      message("For '", finest_name, "' exp.design.1 was the finest level with beta dispersion for both treatments") }
    
    # Do the same for design 2
    else if(nrow(diff_fine_sub_des2) >= 1){      
      diff_fine_list[[paste0(finest_src, finest_name)]] <- diff_fine_sub_des2
      message("For '", finest_name, "' exp.design.2 was the finest level with beta dispersion for both treatments") }
    
    # And design 3
    else if(nrow(diff_fine_sub_des3) >= 1){      
      diff_fine_list[[paste0(finest_src, finest_name)]] <- diff_fine_sub_des3
      message("For '", finest_name, "' exp.design.3 was the finest level with beta dispersion for both treatments") }
    
    # And design 4
    else if(nrow(diff_fine_sub_des4) >= 1){      
      diff_fine_list[[paste0(finest_src, finest_name)]] <- diff_fine_sub_des4
      message("For '", finest_name, "' exp.design.4 was the finest level with beta dispersion for both treatments") }
    
    # And the experiment name
    else if(nrow(diff_fine_sub_name) >= 1){      
      diff_fine_list[[paste0(finest_src, finest_name)]] <- diff_fine_sub_name
      message("For '", finest_name, "' exp.name was the finest level with beta dispersion for both treatments") }
    
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist the list that we just created
diff_fine_v02 <- purrr::list_rbind(diff_fine_list)

# Check structure
dplyr::glimpse(diff_fine_v02)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Make final data objects
diff_all <- diff_v05
diff_fine <- diff_fine_v02

# Check structure
dplyr::glimpse(diff_all)
dplyr::glimpse(diff_fine)

# Define file names
diff_all_filename <- "06-A_caged_mean-beta-diff_all-scales.csv"
diff_fine_filename <- "06-A_caged_mean-beta-diff_fine-scales.csv"

# Export locally
write.csv(x = diff_all, row.names = FALSE, na = '',
  file = file.path("data", diff_all_filename))
write.csv(x = diff_fine, row.names = FALSE, na = '',
  file = file.path("data", diff_fine_filename))

# Check source/experiment counts for both
length(unique(diff_all$source)); length(unique(diff_all$exp.name))
length(unique(diff_fine$source)); length(unique(diff_fine$exp.name))

# End ----
