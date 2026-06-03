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
gam.diff_v1 <- read.csv(file = file.path("data", "06-B_caged_gamma-diff.csv"))

# Check structure
dplyr::glimpse(gam.diff_v1)

## ------------------------------------------- ##
# Load Alpha Diffs & LRRs  ----
## ------------------------------------------- ##

# Load alpha diversity diffs
alp.diff_v1 <- read.csv(file = file.path("data", "06-C_caged_alpha-div-diff_expname.csv")) %>% 
  dplyr::rename(design.level = alpha.diversity_design.level)

# Check structure
dplyr::glimpse(alp.diff_v1)

## ------------------------------------------- ##
# Load Dominance Diffs & LRRs  ----
## ------------------------------------------- ##

# Load alpha diversity diffs
dom.diff_v1 <- read.csv(file = file.path("data", "06-D_caged_dominance-diff_expname.csv")) %>% 
  dplyr::rename(design.level = dominance_design.level)

# Check structure
dplyr::glimpse(dom.diff_v1)

## ------------------------------------------- ##
# Load Mean Beta Diffs & LRRs ----
## ------------------------------------------- ##

# Read in the mean difference files too
beta.diff_v1 <- read.csv(file = file.path("data", "06-A_caged_mean-beta-diff_all-scales.csv")) %>% 
  dplyr::rename(design.level = betadisp.design.level) %>% 
  dplyr::rename_with(.cols = dplyr::starts_with("within.cage.treat"),
    .fn = ~ gsub("within.cage.treat_", "", x = .))

# Check structure of one
dplyr::glimpse(beta.diff_v1) # 2410

# Do some post-processing here to get the format to match alpha/dominance LRR data
beta.diff_v2 <- beta.diff_v1 %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("betadisp")) %>% 
  dplyr::mutate(new.name = paste0(name, "_", cage.treatment_std)) %>% 
  dplyr::select(-name, -cage.treatment_std) %>% 
  tidyr::pivot_wider(names_from = new.name, values_from = value) %>% 
  dplyr::relocate(dplyr::contains("mean_"), dplyr::contains("n_"),
    dplyr::contains("sd_"), dplyr::contains("se_"), 
    dplyr::contains("diff_"), dplyr::contains("lrr_"),
    .after = design.level)

# Check structure of one
dplyr::glimpse(beta.diff_v2)

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
# Join Beta (Means), Alpha & Dominance ----
## ------------------------------------------- ##

glimpse(beta.diff_v2)
glimpse(alp.diff_v1)

# Join data available at all/multiple design levels
join_v2 <- beta.diff_v2 %>% 
  dplyr::left_join(x = ., y = alp.diff_v1,
    by = dplyr::join_by(source, organization, site, excluded.group, measured.group, 
      exp.name, design.level)) %>% 
  dplyr::left_join(x = ., y = dom.diff_v1,
    by = dplyr::join_by(source, organization, site, excluded.group, measured.group, 
      exp.name, design.level, project.name, sampling.years, cage.treatment_orig)) %>%
  dplyr::relocate(project.name, sampling.years, dplyr::starts_with("exp.design."),
    .before = design.level)

# Check structure
dplyr::glimpse(join_v2)

## ------------------------------------------- ##
# Join Alpha, Beta (Means), Gamma, Dominance, & Meta ----
## ------------------------------------------- ##

# Join all preceding data
join_v3 <- join_v1 %>% 
  dplyr::left_join(x = ., y = join_v2,
    by = dplyr::join_by(source, exp.name, organization, site, project.name, 
      sampling.years, excluded.group, measured.group)) %>% 
  dplyr::relocate(year:design.level,
    .after = measured.group)

# Check structure
dplyr::glimpse(join_v3)

## ------------------------------------------- ##
# Reshape to Long Format ----
## ------------------------------------------- ##

# Get a long-format version to join on beta dispersion (non-averaged)
join_v4 <- join_v3 %>% 
  tidyr::pivot_longer(cols = dplyr::ends_with("caged")) %>% 
  tidyr::separate_wider_delim(cols = name, delim = "_",
    names = c("metric", "cage.treatment_std"), cols_remove = TRUE) %>% 
  tidyr::pivot_wider(names_from = metric, values_from = value) %>% 
  dplyr::relocate(cage.treatment_std,
    .after = year)

# Check structure
dplyr::glimpse(join_v4)

## ------------------------------------------- ##
# Generate Finest-Scales Experiment-Level Data ----
## ------------------------------------------- ##

# Make a list for outputs
join_list <- list()

# The prior object includes all calculable scales, let's make a 'finest scales' variant
for(join_src.name in sort(unique(join_v4$source))){
  # join_src.name <- "gex_junnerkoeland_bakkerjunnerkoeland_2001_cattle_plants.csv"
  
  # Subset to that source
  join_src <- dplyr::filter(join_v4, source == join_src.name)
  
  for(join_exp.name in sort(unique(join_src$exp.name))){
    # join_exp.name <- "gex_junnerkoeland_bakkerjunnerkoeland_2001_cattle_plants.csv"
    
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
    if(all(c("caged", "uncaged") %in% unique(join_des1$cage.treatment_std))){
      
      # Add to list
      join_list[[paste0(join_src.name, join_exp.name)]] <- join_des1
    
    # Do the same for design 2
    } else if(all(c("caged", "uncaged") %in% unique(join_des2$cage.treatment_std))){
      join_list[[paste0(join_src.name, join_exp.name)]] <- join_des2 }
    
    # And design 3
    else if(all(c("caged", "uncaged") %in% unique(join_des3$cage.treatment_std))){
      join_list[[paste0(join_src.name, join_exp.name)]] <- join_des3 }
    
    # And design 4
    else if(all(c("caged", "uncaged") %in% unique(join_des4$cage.treatment_std))){
      join_list[[paste0(join_src.name, join_exp.name)]] <- join_des4 }
    
    # And the experiment name
    else if(all(c("caged", "uncaged") %in% unique(join_name$cage.treatment_std))){
      join_list[[paste0(join_src.name, join_exp.name)]] <- join_name }
    
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist to a dataframe
join_v5 <- purrr::list_rbind(x = join_list)

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
