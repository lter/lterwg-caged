## --------------------------------------------------------------- ##
# CAGED Dominance Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate dominance at all design levels
## Done _exactly_ the same way as for beta dispersion 
### (i.e., aggregating only when beta dispersion replicates needed to be aggregated)

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, magrittr, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Define the minimum number of replicates for which we want to calculate beta dispersion
## Inclusive of this number (so 5 becomes >= 5)
min_reps <- 4

# Define maximum number of allowed pseudoreps _without_ averaging across them
## Necessary for preserving pseudoreps when there are few and averaging across them when there are many
## Inclusive of this number (so 5 is >= 5)
max_pseudoreps <- 5

# Read in data
dom_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(dom_v1)

# Check what data made it through 04
unique(dom_v1$source) # 121
unique(dom_v1$exp.name) # 367

## ------------------------------------------- ##
# Data Preparation ----
## ------------------------------------------- ##

# Do needed pre-calculation wrangling
dom_v2 <- dom_v1
  # No such wrangling currently needed

# Re-check structure
dplyr::glimpse(dom_v2)

## ------------------------------------------- ##
# Calculate Dominance ----
## ------------------------------------------- ##

# Create a list for storing outputs
dom_des1_list <- list()
dom_des2_list <- list()
dom_des3_list <- list()
dom_des4_list <- list()
dom_name_list <- list()

# Loop across original data source
for(focal_src in setdiff(x = sort(unique(dom_v2$source)),
                         # Manually (temporarily) removing datasets as/if needed
                         y = c(""))){
  # focal_src <- "beguin_quebec_largeherbivores_1995-2011_whitetaileddeer_understoryplants.csv"
  
  # Progress message
  message("Processing source '", focal_src, "'")
  
  # Subset data
  src_sub <- dplyr::filter(dom_v2, source == focal_src)
  
  # Loop across treatments
  for(focal_trt in unique(src_sub$cage.treatment_orig)){
    # focal_trt <- "OUI"
    
    # Subset again
    trt_sub <- dplyr::filter(src_sub, cage.treatment_orig == focal_trt)
    
    # Loop across study years
    for(focal_yr in unique(trt_sub$year)){
      # focal_yr <- "2011"
      
      # Subset again
      yr_sub <- dplyr::filter(trt_sub, year == focal_yr)
      
      ## ------------------------ ##
      # Dom for Design 1 ----
      ## ------------------------ ##
      
      # Loop across most granular level of experimental design
      for(focal_des1 in unique(yr_sub$exp.design.1)){
        # focal_des1 <- "H__6__9"
        
        # Subset yet again
        des1_sub <-  dplyr::filter(yr_sub, exp.design.1 == focal_des1)
        
        # Calculate
        des1_dom <- des1_sub %>% 
          dplyr::group_by(dplyr::across(
            dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
          dplyr::summarize(max.abundance = max(abundance, na.rm = TRUE),
            total.abundance = sum(abundance, na.rm = TRUE),
            .groups = "drop") %>% 
          dplyr::mutate(dom.design.level = "exp.design.1",
            .after = year)
        
        # Add to list
        dom_des1_list[[paste0(focal_src, focal_trt, focal_des1)]] <- des1_dom
        
      } # Close "exp.design.1" loop
      
      ## ------------------------ ##
      # Dom for Design 2 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 2
      for(focal_des2 in unique(yr_sub$exp.design.2)){
        # focal_des2 <- "H__6"
        
        # Subset yet again
        des2_sub <- dplyr::filter(yr_sub, exp.design.2 == focal_des2)
        
        # Calculate
        des2_dom <- des2_sub %>% 
          dplyr::select(-dplyr::contains(paste0("exp.design.", 1))) %>% 
          dplyr::group_by(dplyr::across(
            dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
          dplyr::summarize(max.abundance = max(abundance, na.rm = TRUE),
            total.abundance = sum(abundance, na.rm = TRUE),
            .groups = "drop") %>% 
          dplyr::mutate(dom.design.level = "exp.design.2",
            .after = year)
        
        # Add to list
        dom_des2_list[[paste0(focal_src, focal_trt, focal_des2)]] <- des2_dom
        
      } # Close "exp.design.2" loop
      
      ## ------------------------ ##
      # Dom for Design 3 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 3
      for(focal_des3 in unique(yr_sub$exp.design.3)){
        # focal_des3 <- "H"
        
        # Subset yet again
        des3_sub <- dplyr::filter(yr_sub, exp.design.3 == focal_des3)
        
        # Identify the number of psuedoreplicates at lower design levels
        des3_pseudorep_ct <- des3_sub %>% 
          dplyr::group_by(exp.design.2) %>% 
          dplyr::summarize(des1.ct = length(unique(exp.design.1)),
                           .groups = "drop")
        
        # If there are X pseudoreplicates...
        if(any(des3_pseudorep_ct$des1.ct >= max_pseudoreps) & 
           length(unique(des3_sub$exp.design.2)) > 1){
          
          # Average across experimental design 1 (within exp. design 2 levels)
          des3_sub %<>% 
            dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "drop")
          
        } # Close conditional aggregation
        
        # Calculate
        des3_dom <- des3_sub %>%
          dplyr::select(-dplyr::contains(paste0("exp.design.", 1:2))) %>% 
          dplyr::group_by(dplyr::across(
            dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
          dplyr::summarize(max.abundance = max(abundance, na.rm = TRUE),
            total.abundance = sum(abundance, na.rm = TRUE),
            .groups = "drop") %>% 
          dplyr::mutate(dom.design.level = "exp.design.3",
            .after = year)
        
        # Add to list
        dom_des3_list[[paste0(focal_src, focal_trt, focal_des3)]] <- des3_dom
        
      } # Close "exp.design.3" loop
      
      ## ------------------------ ##
      # Dom for Design 4 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 4
      for(focal_des4 in unique(yr_sub$exp.design.4)){
        # focal_des4 <- "H"
        
        # Subset yet again
        des4_sub <- dplyr::filter(yr_sub, exp.design.4 == focal_des4)  
        
        # Identify the number of psuedoreplicates at lower design levels
        des4_pseudorep_ct <- des4_sub %>% 
          dplyr::group_by(exp.design.3, exp.design.2) %>% 
          dplyr::summarize(des1.ct = length(unique(exp.design.1)),
                          .groups = "drop")
        
        # If there are X pseudoreplicates...
        if(any(des4_pseudorep_ct$des1.ct >= max_pseudoreps) & 
           length(unique(des4_sub$exp.design.2)) > 1){
          
          # Average across experimental design 1 (within exp. design 2 levels)
          des4_sub %<>% 
            dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                            .groups = "drop")
          
        } # Close conditional aggregation
        
        # Re-identify the number of psuedoreplicates at design level 2
        des4_pseudorep_ct <- des4_sub %>% 
          dplyr::group_by(exp.design.3) %>% 
          dplyr::summarize(des2.ct = length(unique(exp.design.2)),
                          .groups = "drop")
        
        # If there are X pseudoreplicates...
        if(any(des4_pseudorep_ct$des2.ct >= max_pseudoreps) & 
           length(unique(des4_sub$exp.design.3)) > 1){
          
          # Average across experimental design 2 (within exp. design 3 levels)
          des4_sub %<>% 
            dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.2", "exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                            .groups = "drop")
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        des4_dom <- des4_sub %>% 
          dplyr::select(-dplyr::contains(paste0("exp.design.", 1:3))) %>% 
          dplyr::group_by(dplyr::across(
            dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
          dplyr::summarize(max.abundance = max(abundance, na.rm = TRUE),
            total.abundance = sum(abundance, na.rm = TRUE),
            .groups = "drop") %>% 
          dplyr::mutate(dom.design.level = "exp.design.4",
            .after = year)
        
        # Add to list
        dom_des4_list[[paste0(focal_src, focal_trt, focal_des4)]] <- des4_dom
        
      } # Close "exp.design.4" loop
      
      ## ------------------------ ##
      # Dom for Exp.Name ----
      ## ------------------------ ##
      
      # Loop across experimental design level 4
      for(focal_name in unique(yr_sub$exp.name)){
        # focal_name <- "H"
        
        # Subset yet again
        name_sub <- dplyr::filter(yr_sub, exp.name == focal_name)
        
        # Identify the number of psuedoreplicates at lower design levels
        name_pseudorep_ct <- name_sub %>% 
          dplyr::group_by(exp.design.4, exp.design.3, exp.design.2) %>% 
          dplyr::summarize(des1.ct = length(unique(exp.design.1)),
                          .groups = "drop")
        
        # If there are X pseudoreplicates...
        if(any(name_pseudorep_ct$des1.ct >= max_pseudoreps) & 
           length(unique(name_sub$exp.design.2)) > 1){
          # Average across experimental design 1 (within exp. design 2 levels)
          name_sub %<>% 
            dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                            .groups = "drop")
          
        } # Close conditional aggregation
        
        # Re-identify the number of psuedoreplicates at design level 2
        name_pseudorep_ct <- name_sub %>% 
          dplyr::group_by(exp.design.4, exp.design.3) %>% 
          dplyr::summarize(des2.ct = length(unique(exp.design.2)),
                          .groups = "drop")
        
        # If there are X pseudoreplicates...
        if(any(name_pseudorep_ct$des2.ct >= max_pseudoreps) & 
           length(unique(name_sub$exp.design.3)) > 1){
          # Average across experimental design 2 (within exp. design 3 levels)
          name_sub %<>% 
            dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.2", "exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                            .groups = "drop")
          
        } # Close conditional aggregation
        
        # Re-re-identify the number of psuedoreplicates at design level 3
        name_pseudorep_ct <- name_sub %>% 
          dplyr::group_by(exp.design.4) %>% 
          dplyr::summarize(des3.ct = length(unique(exp.design.3)),
                          .groups = "drop")
        
        # If there are X pseudoreplicates...
        if(any(name_pseudorep_ct$des3.ct >= max_pseudoreps) & 
           length(unique(name_sub$exp.design.4)) > 1){
          
          # Average across experimental design 3 (within exp. design 4 levels)
          name_sub %<>% 
            dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.3", "exp.design.2", 
                                            "exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                              .groups = "drop")
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        name_dom <- name_sub %>% 
          dplyr::select(-dplyr::contains(paste0("exp.design.", 1:4))) %>% 
          dplyr::group_by(dplyr::across(
            dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
          dplyr::summarize(max.abundance = max(abundance, na.rm = TRUE),
            total.abundance = sum(abundance, na.rm = TRUE),
            .groups = "drop") %>% 
          dplyr::mutate(dom.design.level = "exp.name",
            .after = year)
        
        # Add to list
        dom_name_list[[paste0(focal_src, focal_trt, focal_name)]] <- name_dom
        
      } # Close "exp.name" loop
    } # Close year loop
  } # Close treatment loop
} # Close source loop

## ------------------------------------------- ##
# Process Calculation Output Lists ----
## ------------------------------------------- ##

# Do some minor processing on the output lists and unlist them
dom_des1 <- purrr::list_rbind(x = dom_des1_list)
dom_des2 <- purrr::list_rbind(x = dom_des2_list)
dom_des3 <- purrr::list_rbind(x = dom_des3_list)
dom_des4 <- purrr::list_rbind(x = dom_des4_list)
dom_expname <- purrr::list_rbind(x = dom_name_list)

# Check the structure of one
dplyr::glimpse(dom_des1)

# Add these to a list (useful later)
dom_deslists <- list(dom_des1, dom_des2, dom_des3, dom_des4, dom_expname)

# Unlist them to create an 'all scales' table
dom_allscales <- purrr::list_rbind(x = dom_deslists) %>% 
  # Then actually calculate dominance
  dplyr::mutate(dominance = ifelse(total.abundance > 0,
    yes = max.abundance / total.abundance, no = 0))

# Check structure
dplyr::glimpse(dom_allscales)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Identify tidy file name / path
dom_filename <- "05-D_caged_dominance_all-scales.csv"

# How many make it through (should be all but there's subsetting happening)
sort(unique(dom_allscales$source)) #121
sort(unique(dom_allscales$exp.name)) #367

# Re-check 'all scales' structure
dplyr::glimpse(dom_allscales)

# Export locally
write.csv(x = dom_allscales, na = '', row.names = FALSE,
  file = file.path("data", dom_filename))

# End ----

