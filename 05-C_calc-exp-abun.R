## --------------------------------------------------------------- ##
# CAGED Experiment-Level Abundance Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate abundance across taxa at all experimental design levels
## Possibly useful for later steps so seems like it'd be useful to handle in a standalone script

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
abun_v01 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(abun_v01)

## ------------------------------------------- ##
# Calculate Abundance
## ------------------------------------------- ##

# Create lists for storing outputs
abun_des1_list <- list()
abun_des2_list <- list()
abun_des3_list <- list()
abun_des4_list <- list()
abun_name_list <- list()

# Loop across sources
for(focal_src in setdiff(x = sort(unique(abun_v01$source)),
                         # Manually (temporarily) removing datasets as/if needed
                         y = c(""))){
  # focal_src <- "gilson_southafrica_intertidalexclusion_2021_grazers_inverts.csv"
  
  # Progress message
  message("Processing source '", focal_src, "'")

  # Subset data to just that source
  src_sub <- dplyr::filter(abun_v01, source == focal_src)

  # Loop across treatments
  for(focal_trt in unique(src_sub$cage.treatment_orig)){
    # focal_trt <- "F"
    
    # Subset again
    trt_sub <- dplyr::filter(src_sub, cage.treatment_orig == focal_trt)
    
    # Loop across study years
    for(focal_yr in sort(unique(trt_sub$year))){
      # focal_yr <- "2021"
      
      # Subset again
      yr_sub <- dplyr::filter(trt_sub, year == focal_yr)

      # Calculate abundance at design level 1
      des1_abun <- yr_sub %>% 
        dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
          y = c("taxa", "abundance"))))) %>% 
        dplyr::summarize(abun = sum(abundance, na.rm = T),
          .groups = "keep") %>% 
        dplyr::ungroup() %>% 
        # Add needed columns
        dplyr::mutate(abundance.design.level = "exp.design.1",
          .before = abun) %>% 
        dplyr::rename(abundance = abun)

      # Add to relevant list
      abun_des1_list[[paste0(focal_src, focal_trt, focal_yr, "design.1")]] <- des1_abun

      # Calculate abundance at next design level 
      des2_abun <- des1_abun %>% 
        dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
          y = c("exp.design.1", "abundance"))))) %>% 
        dplyr::summarize(abun = sum(abundance, na.rm = T),
          .groups = "keep") %>% 
        dplyr::ungroup() %>% 
        # Add needed columns
        dplyr::mutate(abundance.design.level = "exp.design.2",
          .before = abun) %>% 
        dplyr::rename(abundance = abun)

      # Add to relevant list
      abun_des2_list[[paste0(focal_src, focal_trt, focal_yr, "design.2")]] <- des2_abun

      # Calculate abundance at next design level 
      des3_abun <- des2_abun %>% 
        dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
          y = c("exp.design.2", "abundance"))))) %>% 
        dplyr::summarize(abun = sum(abundance, na.rm = T),
          .groups = "keep") %>% 
        dplyr::ungroup() %>% 
        # Add needed columns
        dplyr::mutate(abundance.design.level = "exp.design.3",
          .before = abun) %>% 
        dplyr::rename(abundance = abun)

      # Add to relevant list
      abun_des3_list[[paste0(focal_src, focal_trt, focal_yr, "design.3")]] <- des3_abun

      # Calculate abundance at next design level 
      des4_abun <- des3_abun %>% 
        dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
          y = c("exp.design.3", "abundance"))))) %>% 
        dplyr::summarize(abun = sum(abundance, na.rm = T),
          .groups = "keep") %>% 
        dplyr::ungroup() %>% 
        # Add needed columns
        dplyr::mutate(abundance.design.level = "exp.design.4",
          .before = abun) %>% 
        dplyr::rename(abundance = abun)

      # Add to relevant list
      abun_des4_list[[paste0(focal_src, focal_trt, focal_yr, "design.4")]] <- des4_abun

      # Finally, calculate abundance at experiment name
      name_abun <- des4_abun %>% 
        dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.),
          y = c("exp.design.4", "abundance"))))) %>% 
        dplyr::summarize(abun = sum(abundance, na.rm = T),
          .groups = "keep") %>% 
        dplyr::ungroup() %>% 
        # Add needed columns
        dplyr::mutate(abundance.design.level = "exp.name",
          .before = abun) %>% 
        dplyr::rename(abundance = abun)

      # Add to relevant list
      abun_name_list[[paste0(focal_src, focal_trt, focal_yr, "name")]] <- name_abun

    } # Close year loop
  } # Close treatment loop
} # Close source loop

## ------------------------------------------- ##
# Process Outputs ----
## ------------------------------------------- ##

# Check structure of one such list
dplyr::glimpse(abun_name_list)

# Unlist each of the output lists
abun_des1 <- purrr::list_rbind(x = abun_des1_list)
abun_des2 <- purrr::list_rbind(x = abun_des2_list)
abun_des3 <- purrr::list_rbind(x = abun_des3_list)
abun_des4 <- purrr::list_rbind(x = abun_des4_list)
abun_expname <- purrr::list_rbind(x = abun_name_list)

# Check the structure of one
dplyr::glimpse(abun_des1)

# Stack these up into one, larger table
abun_v02 <- dplyr::bind_rows(abun_des1, abun_des2, abun_des3, abun_des4, abun_expname) %>% 
  # Ditch design level columns (they're implied by the 'abundance.design.level' values)
  dplyr::select(-dplyr::starts_with("exp.design."))

# Check structure of that
dplyr::glimpse(abun_v02)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
abun_v99 <- abun_v02

# One last structure check
dplyr::glimpse(abun_v99)

# Identify tidy file name / path
abun_name <- "05-C_caged_design-level-abundance_all-scales.csv"
abun_path <- file.path("data", abun_name)

# Export locally
write.csv(x = abun_v99, row.names = F, na = '', file = abun_path)

# End ----

