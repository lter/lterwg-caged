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
# Load Tidy Metadata ----
## ------------------------------------------- ##

# Grab the tidy metadata
meta_v1 <- read.csv(file.path("data", "07_tidy-sitelevel-metadata.csv"))

# Check structure
dplyr::glimpse(meta_v1)

## ------------------------------------------- ##
# Load Gamma Diffs & LRRs  ----
## ------------------------------------------- ##
# Read in gamma richness diffs
gam.diff_v1 <- read.csv(file = file.path("data", "06-B_caged_gamma-diff.csv")) %>% 
  # Remove columns that we can get back from 'source'
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group")))

# Check structure
dplyr::glimpse(gam.diff_v1)

## ------------------------------------------- ##
# Load Alpha Diffs & LRRs  ----
## ------------------------------------------- ##

# Load alpha diversity diffs
alp.diff_v1 <- read.csv(file = file.path("data", "06-C_caged_alpha-div-diff_all-scales.csv")) %>% 
  dplyr::rename(design.level = alpha.design.level) %>% 
  # Remove columns that we can get back from 'source'
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group"))) %>% 
  dplyr::select(-cage.treatment_std)

# Check structure
dplyr::glimpse(alp.diff_v1)

## ------------------------------------------- ##
# Load Dominance Diffs & LRRs  ----
## ------------------------------------------- ##

# Load alpha diversity diffs
dom.diff_v1 <- read.csv(file = file.path("data", "06-D_caged_dominance-diff_all-scales.csv")) %>% 
  dplyr::rename(design.level = dom.design.level) %>% 
  # Remove columns that we can get back from 'source'
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group"))) %>% 
  dplyr::select(-cage.treatment_std)

# Check structure
dplyr::glimpse(dom.diff_v1)

## ------------------------------------------- ##
# Load Mean Beta Diffs & LRRs ----
## ------------------------------------------- ##

# Read in the mean difference files too
beta.diff_v1 <- read.csv(file = file.path("data", "06-A_caged_mean-beta-diff_all-scales.csv")) %>% 
  dplyr::rename(design.level = betadisp.design.level) %>% 
  dplyr::rename_with(.cols = dplyr::starts_with("within.cage.treat"),
    .fn = ~ gsub("within.cage.treat_", "", x = .)) %>% 
  # Remove columns that we can get back from 'source'
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group"))) %>% 
  dplyr::select(-cage.treatment_std)

# Check structure of one
dplyr::glimpse(beta.diff_v1)

## ------------------------------------------- ##
# Join Gamma & Metadata ----
## ------------------------------------------- ##

# Join data at 'exp.name' resolution
join_v1 <- meta_v1 %>% 
  dplyr::left_join(x = ., y = gam.diff_v1,
    by = dplyr::join_by(source, exp.name))

# Check structure
dplyr::glimpse(join_v1)

## ------------------------------------------- ##
# Join Alpha & Dominance ----
## ------------------------------------------- ##

# Join 'allscales' alpha & dominance data
join_v2 <- alp.diff_v1 %>% 
  dplyr::left_join(x = ., y = dom.diff_v1,
    by = dplyr::join_by(source, exp.name, exp.design.4, exp.design.3, 
      exp.design.2, exp.design.1, cage.treatment_orig, 
      year, design.level))
  
# Check structure
dplyr::glimpse(join_v2)

## ------------------------------------------- ##
# Join Gamma, Alpha, Meta, and Dominance ----
## ------------------------------------------- ##

# Combine all ancillary data (with 'all scales' on left)
join_v3 <- join_v2 %>% 
  dplyr::left_join(x = ., y = join_v1,
    by = dplyr::join_by(source, exp.name))

# Check structure
dplyr::glimpse(join_v3)

## ------------------------------------------- ##
# Join Beta (Means) with Everything ----
## ------------------------------------------- ##

# Make a list for outputs
join_list <- list()

# Iterate across design levels
for(des_level in unique(beta.diff_v1$design.level)){
  # des_level <- "exp.design.2"

  # Progress message
  message("Joining beta dispersion and everything else for ", des_level)

  # Subset both bits
  beta_sub <- dplyr::filter(beta.diff_v1, design.level == des_level)
  ee_sub <- dplyr::filter(join_v3, design.level == des_level) %>% 
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0)))

  # Join and add to list
  join_list[[des_level]] <- dplyr::left_join(x = beta_sub, y = ee_sub)
}

# Unlist back to dataframe
join_v4 <- purrr::list_rbind(x = join_list)

# Check structure
dplyr::glimpse(join_v4)

## ------------------------------------------- ##
# Generate Finest-Scales Experiment-Level Data ----
## ------------------------------------------- ##

# Make a list for outputs
fine_list <- list()

# The prior object includes all calculable scales, let's make a 'finest scales' variant
for(join_src.name in sort(unique(join_v4$source))){
  # join_src.name <- "lter-mcr_moorea_recharge_2018-2022_fish_benthic.csv"
  
  # Subset to that source
  join_src <- dplyr::filter(join_v4, source == join_src.name)
  
  for(join_exp.name in sort(unique(join_src$exp.name))){
    # join_exp.name <- "Disturbed"
    
    # Progress message
    message("Identifying finest scale for '", join_exp.name, "'")
    
    # Subset to this experiment
    join_exp <- dplyr::filter(join_src, exp.name == join_exp.name)
    
    # Make another subset for each design level
    join_des1 <- dplyr::filter(join_exp, design.level == "exp.design.1")
    join_des2 <- dplyr::filter(join_exp, design.level == "exp.design.2")
    join_des3 <- dplyr::filter(join_exp, design.level == "exp.design.3")
    join_des4 <- dplyr::filter(join_exp, design.level == "exp.design.4")
    join_name <- dplyr::filter(join_exp, design.level == "exp.name")
    
    # Work through the design levels sequentially (lowest to highest) to identify finest
    if(any(!is.na(join_des1$betadisp.mean.diff))){
      fine_list[[paste0(join_src.name, join_exp.name)]] <- join_des1
    } else if(any(!is.na(join_des2$betadisp.mean.diff))){
      fine_list[[paste0(join_src.name, join_exp.name)]] <- join_des2
    } else if(any(!is.na(join_des3$betadisp.mean.diff))){
      fine_list[[paste0(join_src.name, join_exp.name)]] <- join_des3
    } else if(any(!is.na(join_des4$betadisp.mean.diff))){
      fine_list[[paste0(join_src.name, join_exp.name)]] <- join_des4
    } else if(any(!is.na(join_name$betadisp.mean.diff))){
      fine_list[[paste0(join_src.name, join_exp.name)]] <- join_name
    }
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist to a dataframe
join_v5 <- purrr::list_rbind(x = fine_list)

# Did that work?
## The following should return a 0-row tibble (if it worked)
join_v5 %>% 
  dplyr::group_by(source, exp.name) %>% 
  dplyr::summarize(design.ct = length(unique(design.level)),
    designs = paste(design.level, collapse = " & "),
    .groups = "drop") %>% 
  dplyr::filter(design.ct != 1)

# Check structure
dplyr::glimpse(join_v5)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Make final treatment/experiment level joined data (incl, LRRs)
join_all <- join_v4
join_fine <- join_v5

# How many sources/experiments
length(unique(join_all$source)); length(unique(join_all$exp.name))
length(unique(join_fine$source)); length(unique(join_fine$exp.name))

# Check their structure
dplyr::glimpse(join_all)
dplyr::glimpse(join_fine)

# Also export these
write.csv(x = join_all, na = '', row.names = FALSE,
  file = file.path("data", "08-B_caged_expname-effect-size_all-scales.csv"))

write.csv(x = join_fine, na = '', row.names = FALSE,
  file = file.path("data", "08-B_caged_expname-effect-size_fine-scales.csv"))

# End ----
