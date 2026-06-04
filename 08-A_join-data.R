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

# Load just the 'all scales' beta dispersion data
## We'll re-generate the 'fine scales' at the end so we'll skip it here for convenience
beta_v1 <- read.csv(file = file.path("data", "05-A_caged_beta-disp_all-scales.csv"))

# Check structure
dplyr::glimpse(beta_v1)

# Do needed wrangling
beta_v2 <- beta_v1 %>% 
  dplyr::rename(design.level = betadisp.design.level) %>% 
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group")))

# Check structure
dplyr::glimpse(beta_v2)

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
  dplyr::filter(!is.na(gamma.richness)) %>% 
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group")))

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
  dplyr::rename(design.level = alpha.diversity_design.level) %>% 
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group")))

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
  dplyr::rename(design.level = dominance_design.level) %>% 
  dplyr::select(-dplyr::all_of(c("organization", "site", "project.name", 
    "sampling.years", "excluded.group", "measured.group")))

# Check structure
dplyr::glimpse(dom_v2)

## ------------------------------------------- ##
# Join 'Raw' Beta with Everything Else ----
## ------------------------------------------- ##
# Make a list for storing outputs
w.meta_outs <- list()

# Loop across design levels
for(des_lvl in c(paste0("exp.design.", 1:4), "exp.name")){
  # des_lvl <- "exp.name"

  # Progress message
  message("Joining everything at ", des_lvl)

  # Subset beta/alpha/dominance to that level (gamma and meta only exist at 'exp.name' so don't need subsetting)
  ## Beta dispersion
  beta_des <- dplyr::filter(beta_v2, design.level == des_lvl) %>% 
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0)))
  ## Alpha diversity (richness)
  alpha_des <- dplyr::filter(alpha_v2, design.level == des_lvl) %>% 
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0)))
  ## Dominance
  dom_des <- dplyr::filter(dom_v2, design.level == des_lvl) %>% 
    dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0)))
  
  # Join everything (incl. gamma/meta this time)
  join_des <- beta_des %>% 
    dplyr::left_join(x = ., y = alpha_des, 
      by = intersect(x = names(.), y = names(alpha_des))) %>% 
    dplyr::left_join(x = ., y = dom_des,
      by = intersect(x = names(.), y = names(dom_des))) %>% 
    dplyr::left_join(x = ., y = gamma_v2,
      by = intersect(x = names(.), y = names(gamma_v2))) %>% 
    dplyr::left_join(x = ., y = meta_v1,
      by = intersect(x = names(.), y = names(meta_v1)))
    
  # Add to list
  w.meta_outs[[des_lvl]] <- join_des

}

# Unlist to dataframe and zero-fill alpha diversity, gamma richness, and dominance
## Both became NA for 0 abundance groups as an artifact of how they were calculated
w.meta_v1 <- purrr::list_rbind(x = w.meta_outs) %>% 
  dplyr::mutate(dplyr::across(.cols = c(alpha.diversity_richness, gamma.richness, dominance),
    .fns = ~ ifelse(is.na(.), yes = 0, no = .)))

# Check structure
dplyr::glimpse(w.meta_v1)

## ------------------------------------------- ##
# Reorder Columns ----
## ------------------------------------------- ##

# Do some column reordering for clarity and recapture parts of 'source' column
w.meta_v2 <- w.meta_v1 %>% 
  dplyr::relocate(lat, long, dplyr::starts_with("var"), .after = exp.name) %>% 
  dplyr::relocate(dplyr::starts_with(c("betadisp", "alpha", "gamma", "dominance")), 
    .after = dplyr::everything()) %>% 
  tidyr::separate_wider_delim(cols = source, delim = "_",
    names = c("organization", "site", "project.name", "sampling.years", "excluded.group", "measured.group"),
    cols_remove = FALSE)

# Check structure
dplyr::glimpse(w.meta_v2)

## ------------------------------------------- ##
# Re-Identify 'Finest Scales' ----
## ------------------------------------------- ##

# Make a list for outputs
w.meta_fine_list <- list()

# Loop across sources and experiments to re-identify finest scales
for(finest_src in sort(unique(w.meta_v2$source))){
  # finest_src <- "royo_westvirginia_fernow_2000-2013_deer_plants.csv"
  
  # Subset to that source
  w.meta_fine_src <- dplyr::filter(w.meta_v2, source == finest_src)
  
  # Iterate across exp.names
  for(finest_name in sort(unique(w.meta_fine_src$exp.name))){
    # finest_name <- "Fire"
    
    # Progress message
    message("Identifying finest scale for '", finest_name, "'")
    
    # Subset the data to only this experiment name
    w.meta_fine_sub <- dplyr::filter(w.meta_fine_src, exp.name == finest_name) %>% 
      dplyr::filter(!is.na(betadisp.median))
    
    # Make another subset for each design level
    w.meta_fine_sub_des1 <- dplyr::filter(w.meta_fine_sub, design.level == "exp.design.1")
    w.meta_fine_sub_des2 <- dplyr::filter(w.meta_fine_sub, design.level == "exp.design.2")
    w.meta_fine_sub_des3 <- dplyr::filter(w.meta_fine_sub, design.level == "exp.design.3")
    w.meta_fine_sub_des4 <- dplyr::filter(w.meta_fine_sub, design.level == "exp.design.4")
    w.meta_fine_sub_name <- dplyr::filter(w.meta_fine_sub, design.level == "exp.name")
    
    # Work through the design levels sequentially (lowest to highest)
    ## And add the lowest one with beta dispersion for both standardized cage treatments to the output list
    if(all(c("caged", "uncaged") %in% unique(w.meta_fine_sub_des1$cage.treatment_std))){
      
      # Add to list
      w.meta_fine_list[[paste0(finest_src, finest_name)]] <- w.meta_fine_sub_des1
      
      # Print a message too
      message("For '", finest_name, "' exp.design.1 was the finest level with beta dispersion for both treatments") }
    
    # Do the same for design 2
    else if(all(c("caged", "uncaged") %in% unique(w.meta_fine_sub_des2$cage.treatment_std))){
      w.meta_fine_list[[paste0(finest_src, finest_name)]] <- w.meta_fine_sub_des2
      message("For '", finest_name, "' exp.design.2 was the finest level with beta dispersion for both treatments") }
    
    # And design 3
    else if(all(c("caged", "uncaged") %in% unique(w.meta_fine_sub_des3$cage.treatment_std))){
      w.meta_fine_list[[paste0(finest_src, finest_name)]] <- w.meta_fine_sub_des3
      message("For '", finest_name, "' exp.design.3 was the finest level with beta dispersion for both treatments") }
    
    # And design 4
    else if(all(c("caged", "uncaged") %in% unique(w.meta_fine_sub_des4$cage.treatment_std))){
      w.meta_fine_list[[paste0(finest_src, finest_name)]] <- w.meta_fine_sub_des4
      message("For '", finest_name, "' exp.design.4 was the finest level with beta dispersion for both treatments") }
    
    # And the experiment name
    else if(all(c("caged", "uncaged") %in% unique(w.meta_fine_sub_name$cage.treatment_std))){
      w.meta_fine_list[[paste0(finest_src, finest_name)]] <- w.meta_fine_sub_name
      message("For '", finest_name, "' exp.name was the finest level with beta dispersion for both treatments") }
    
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist the list that we just created
w.meta_fine_v1 <- purrr::list_rbind(w.meta_fine_list)

# Check structure
dplyr::glimpse(w.meta_fine_v1)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Make final data objects
w.meta_all <- w.meta_v2
w.meta_fine <- w.meta_fine_v1

# Check structure
dplyr::glimpse(w.meta_all)
dplyr::glimpse(w.meta_fine)

# Define file names
w.meta_all_filename <- "08-A_caged_w.meta-beta-disp_all-scales.csv"
w.meta_fine_filename <- "08-A_caged_w.meta-beta-disp_fine-scales.csv"

# Export locally
write.csv(x = w.meta_all, row.names = FALSE, na = '',
  file = file.path("data", w.meta_all_filename))
write.csv(x = w.meta_fine, row.names = FALSE, na = '',
  file = file.path("data", w.meta_fine_filename))

# Check source/experiment counts for both
length(unique(w.meta_all$source)); length(unique(w.meta_all$exp.name))
length(unique(w.meta_fine$source)); length(unique(w.meta_fine$exp.name))

# End ----
