## --------------------------------------------------------------- ##
# CAGED Join Data
## --------------------------------------------------------------- ##
# Purpose:
## A number of scripts' outputs need to all be in the same table for analysis/visualization
## This script does the necessary joining

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Beta Dispersion ----
## ------------------------------------------- ##

# Identify data files we want to add stuff to
(w.meta_outs <- dir(path = file.path("data"), pattern = "05-A_caged_beta-disp_"))
w.meta_in_list <- purrr::map(.x = w.meta_outs,
                             .f = ~ read.csv(file = file.path("data", .x)))
names(w.meta_in_list) <- w.meta_outs

# Check structure of one
dplyr::glimpse(w.meta_in_list[[1]])

## ------------------------------------------- ##
# Load Tidy Metadata ----
## ------------------------------------------- ##

# Grab the tidy metadata
meta_v1 <- read.csv(file.path("data", "07_tidy-sitelevel-metadata.csv"))

# Check structure
dplyr::glimpse(meta_v1)

## ------------------------------------------- ##
# Load Gamma 'Raw' ----
## ------------------------------------------- ##

# Read in the data
gamma_v1 <- read.csv(file.path("data", "05-B_caged_gamma-rich.csv"))

# Check structure
dplyr::glimpse(gamma_v1)

# Do some necessary wrangling
gamma_v2 <- gamma_v1 %>% 
  dplyr::filter(!is.na(gamma.richness))

# Check structure
dplyr::glimpse(gamma_v2)

## ------------------------------------------- ##
# Load Alpha 'Raw' ----
## ------------------------------------------- ##

# Read in the data
alpha_v1 <- read.csv(file.path("data", "05-C_caged_alpha-div_all-scales.csv"))

# Check structure
dplyr::glimpse(alpha_v1)

# Do needed wrangling
alpha_v2 <- alpha_v1 %>% 
  dplyr::relocate(cage.treatment_orig, .after = cage.treatment_std) %>% 
  dplyr::rename(design.level = alpha.diversity_design.level)

# Check structure
dplyr::glimpse(alpha_v2)

## ------------------------------------------- ##
# Load Dominance 'Raw' ----
## ------------------------------------------- ##

# Read in the data
dom_v1 <- read.csv(file.path("data", "05-D_caged_dominance_all-scales.csv"))

# Check structure
dplyr::glimpse(dom_v1)

# Do needed wrangling
dom_v2 <- dom_v1 %>% 
  dplyr::select(-dplyr::ends_with("abundance")) %>% 
  dplyr::relocate(cage.treatment_orig, .after = cage.treatment_std) %>% 
  dplyr::filter(!is.na(dominance)) %>% 
  dplyr::rename(design.level = dominance_design.level)

# Check structure
dplyr::glimpse(dom_v2)

## ------------------------------------------- ##
# Join 'Raw' Beta with Everything Else ----
## ------------------------------------------- ##
# Make a list for storing outputs
w.meta_out_list <- list()

# Loop across files for which we want 'metadata' attached
for(focal_w.meta in w.meta_outs){
  # focal_w.meta <- "05-A_caged_beta-disp_all-scales.csv"
  
  # Processing message
  message("Attaching ancillary data to ", focal_w.meta)
  
  # Grab just that file out of the list of inputs
  w.meta_v1 <- w.meta_in_list[[focal_w.meta]] %>% 
    dplyr::rename(design.level = betadisp.design.level)
  
  # Attach metadata pre-joined object
  w.meta_v2 <- w.meta_v1 %>% 
    dplyr::left_join(x = ., y = meta_v1,
      by = dplyr::join_by(source, exp.name)) %>% 
    dplyr::relocate(lat, long, dplyr::starts_with("var_"), 
      .before = exp.name)
  
  # Attach gamma richness
  w.meta_v3 <- w.meta_v2 %>% 
    dplyr::left_join(x = ., y = gamma_v2,
      by = dplyr::join_by(source, organization, site, project.name, sampling.years, 
        excluded.group, measured.group, exp.name,
        cage.treatment_std, cage.treatment_orig))
  
  # Attach alpha diversity
  w.meta_v4 <- w.meta_v3 %>% 
    dplyr::left_join(x = ., y = alpha_v2,
      by = dplyr::join_by(source, organization, site, project.name, sampling.years, 
        excluded.group, measured.group, exp.name, exp.design.4, exp.design.3, 
        exp.design.2, exp.design.1, cage.treatment_std, cage.treatment_orig))

  # Attach dominance
  w.meta_v5 <- w.meta_v4 %>% 
    dplyr::left_join(x = ., y = dom_v2,
      by = dplyr::join_by(source, organization, site, project.name, sampling.years, 
        excluded.group, measured.group, exp.name, exp.design.4, exp.design.3,
        exp.design.2, exp.design.1, cage.treatment_std, cage.treatment_orig))
  
  # Add this to the output list
  w.meta_out_list[[focal_w.meta]] <- w.meta_v5
  
} # Close loop

# Check the structure of the final output
dplyr::glimpse(w.meta_v5)

# Look at what's gained/lost at various points
## After adding metadata GoogleSheet info
supportR::diff_check(old = names(w.meta_v1), new = names(w.meta_v2))
## After adding gamma richness
supportR::diff_check(old = names(w.meta_v2), new = names(w.meta_v3))
## After adding alpha diversity
supportR::diff_check(old = names(w.meta_v3), new = names(w.meta_v4))
## After adding dominance
supportR::diff_check(old = names(w.meta_v4), new = names(w.meta_v5))

# How many sources and exp.name got through the pipeline?
unique(w.meta_v5$source) # 117
unique(w.meta_v5$exp.name) # 347
dim(w.meta_v5) # 13964    44

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Loop across the list elements to export
for(w.meta_outs in unique(names(w.meta_out_list))){
  # w.meta_outs <- "05-A_caged_beta-disp_all-scales.csv"
  
  # Create a final object
  w.meta_v99 <- w.meta_out_list[[w.meta_outs]]
  
  # Generate tidy name / path
  w.meta_name <- gsub(pattern = "05-A_caged_", 
                    replacement = "08-A_caged_w.meta-", x = w.meta_outs)
  w.meta_path <- file.path("data", w.meta_name)
  
  # Export locally
  write.csv(x = w.meta_v99, row.names = F, na = '', file = w.meta_path)
}

# Structure check of that
dplyr::glimpse(w.meta_v99)

# End ----
