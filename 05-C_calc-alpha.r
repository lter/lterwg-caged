## --------------------------------------------------------------- ##
# CAGED Alpha Diversity Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate alpha diversity (i.e., richness) within experiment and within caging treatment

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, magrittr, vegan, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
alpha_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check what data made it through 04
unique(alpha_v1$source)
unique(alpha_v1$exp.name)

# Check structure
dplyr::glimpse(alpha_v1)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Design 1) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.1"
alpha_des1 <- alpha_v1 %>% 
  dplyr::filter(abundance > 0) %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("year", "taxa", "abundance"))) 
    )) %>% 
  dplyr::summarize(alpha.diversity_richness = length(unique(taxa)),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.diversity_design.level = "exp.design.1",
    .after = cage.treatment_orig)

# Check structure
dplyr::glimpse(alpha_des1)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Design 2) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.1"
alpha_des2 <- alpha_v1 %>% 
  dplyr::filter(abundance > 0) %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("exp.design.1", "year", "taxa", "abundance"))) 
    )) %>% 
  dplyr::summarize(alpha.diversity_richness = length(unique(taxa)),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.diversity_design.level = "exp.design.2",
    .after = cage.treatment_orig)

# Check structure
dplyr::glimpse(alpha_des2)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Design 3) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.1"
alpha_des3 <- alpha_v1 %>% 
  dplyr::filter(abundance > 0) %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c(paste0("exp.design.", 1:2), 
      "year", "taxa", "abundance"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = length(unique(taxa)),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.diversity_design.level = "exp.design.3",
    .after = cage.treatment_orig)

# Check structure
dplyr::glimpse(alpha_des3)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Design 4) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.1"
alpha_des4 <- alpha_v1 %>% 
  dplyr::filter(abundance > 0) %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c(paste0("exp.design.", 1:3), 
      "year", "taxa", "abundance"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = length(unique(taxa)),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.diversity_design.level = "exp.design.4",
    .after = cage.treatment_orig)

# Check structure
dplyr::glimpse(alpha_des4)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Exp Name) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.1"
alpha_name <- alpha_v1 %>% 
  dplyr::filter(abundance > 0) %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c(paste0("exp.design.", 1:4), 
      "year", "taxa", "abundance"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = length(unique(taxa)),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.diversity_design.level = "exp.name",
    .after = cage.treatment_orig)

# Check structure
dplyr::glimpse(alpha_name)

## ------------------------------------------- ##
# Process Outputs
## ------------------------------------------- ##

# Add these to a list (useful later)
alpha_deslists <- list(alpha_des1, alpha_des2, alpha_des3, alpha_des4, alpha_name)

# Unlist them to create an 'all scales' table
alpha_allscales <- purrr::list_rbind(x = alpha_deslists)

# Check structure
dplyr::glimpse(alpha_allscales)

## ------------------------------------------- ##
# Identify 'Finest Scale' of Alpha Diversity ----
## ------------------------------------------- ##

# Make a list for storing outputs
alpha_finelist <- list()

# Pare down the 'all scales' output slightly (to only instances with beta dispersion
alpha_fine_v1 <- alpha_allscales %>% 
  dplyr::filter(is.na(alpha.diversity_richness) != TRUE)

# To do this, we'll loop across sources and experiments
for(finest_src in sort(unique(alpha_allscales$source))){
  
  # Subset to that source
  alpha_fine_src <- alpha_fine_v1 %>% 
    dplyr::filter(source == finest_src)
  
  for(finest_name in sort(unique(alpha_fine_src$exp.name))){
    # finest_name <- "Palmas_Exposed-Cool"
    
    # Progress message
    message("Identifying finest scale for '", finest_name, "'")
    
    # Subset the beta dispersion table to only this source
    alpha_fine_sub <- alpha_fine_src %>% 
      dplyr::filter(exp.name == finest_name)
    
    # Make another subset for each design level
    alpha_fine_sub_des1 <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.design.1")
    alpha_fine_sub_des2 <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.design.2")
    alpha_fine_sub_des3 <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.design.3")
    alpha_fine_sub_des4 <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.design.4")
    alpha_fine_sub_name <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.name")
    
    # Work through the design levels sequentially (lowest to highest)
    ## And add the lowest one with beta dispersion for both standardized cage treatments to the output list
    if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_des1$cage.treatment_std))){
      
      # Add to list
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_des1
      
      # Print a message too
      message("For '", finest_name, "' exp.design.1 was the finest level with beta dispersion for both treatments") }
    
    # Do the same for design 2
    else if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_des2$cage.treatment_std))){
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_des2
      message("For '", finest_name, "' exp.design.2 was the finest level with beta dispersion for both treatments") }
    
    # And design 3
    else if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_des3$cage.treatment_std))){
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_des3
      message("For '", finest_name, "' exp.design.3 was the finest level with beta dispersion for both treatments") }
    
    # And design 4
    else if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_des4$cage.treatment_std))){
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_des4
      message("For '", finest_name, "' exp.design.4 was the finest level with beta dispersion for both treatments") }
    
    # And the experiment name
    else if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_name$cage.treatment_std))){
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_name
      message("For '", finest_name, "' exp.name was the finest level with beta dispersion for both treatments") }
    
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist the list that we just created
alpha_fine_v2 <- purrr::list_rbind(alpha_finelist)

# Check structure
dplyr::glimpse(alpha_fine_v2)

# Did we lose any sources?
# these are datasets that don't have both caging treatments for at least one design level
supportR::diff_check(old = unique(alpha_allscales$source), new = unique(alpha_fine_v2$source))
#now alderson is getting dropped here because it only has n=3 now that we split it up

# Did we lose any experiments?
## From sources that were not dropped
alpha_test <- dplyr::filter(alpha_allscales, source %in% alpha_fine_v2$source)
supportR::diff_check(old = unique(alpha_test$exp.name), new = unique(alpha_fine_v2$exp.name))

## ------------------------------------------- ##
# Diagnose Lost Sources ----
## ------------------------------------------- ##

# Identify lost sources
dropped_sources <- setdiff(x = unique(alpha_allscales$source), y = unique(alpha_fine_v2$source))

# Create a nice diagnostic output for sources that we do lose in this process
for(lost_src in dropped_sources){
  
  # Subset the 'all scales' output to just this source
  alpha_lost <- dplyr::filter(alpha_allscales, source == lost_src)
  
  # Generate a file name
  lost_file <- paste0("source_betadisp-failure_", lost_src)
  
  # Export locally
  write.csv(x = alpha_lost, na = '', row.names = F,
            file = file.path("data", "diagnostic", lost_file))
  
}

## ------------------------------------------- ##
# Diagnose Lost Experiment Names ----
## ------------------------------------------- ##

# Identify lost experiments
dropped_names <- setdiff(x = unique(alpha_test$exp.name), y = unique(alpha_fine_v2$exp.name))

# Create a nice diagnostic output for sources that we do lose in this process
for(lost_exp in dropped_names){
  
  # Subset the 'all scales' output to just this source
  alpha_lost_tmp <- dplyr::filter(alpha_allscales, !source %in% unique(dropped_sources))
  alpha_lost <- dplyr::filter(alpha_lost_tmp, exp.name == lost_exp)

  # Generate a file name
  lost_file <- paste0("expname_betadisp-failure_", lost_exp, ".csv")
  
  # Export locally
  write.csv(x = alpha_lost, na = '', row.names = F,
            file = file.path("data", "diagnostic", lost_file)) 
}

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
alpha_v99 <- alpha_fine_v2

# How many sources and exp.name got through the pipeline?
unique(alpha_v99$source) # 117
unique(alpha_v99$exp.name) # 346

# Identify tidy file name / path
alpha_name <- "05-C_caged_alpha-div"
alpha_path <- file.path("data", paste0(alpha_name, "_finest-scales.csv"))

# Export locally
write.csv(x = alpha_v99, row.names = F, na = '', file = alpha_path)

# Re-check 'all scales' structure
dplyr::glimpse(alpha_allscales)

# Export locally
write.csv(x = alpha_allscales, na = '', row.names = F,
          file = file.path("data", paste0(alpha_name, "_all-scales.csv")))

# End ----

