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
# Load Gamma Diffs & LRRs  ----
## ------------------------------------------- ##
# Note we can skip (for now) 05 B-D outputs...
## ...because the 06 variants have all relevant info plus LRRs

# Read in gamma richness diffs
gam.diff_v1 <- read.csv(file = file.path("data", "06-B_caged_gamma-diff.csv")) %>% 
  dplyr::rename(gamma.richness_cage.treat.diff = within.cage.treat_gamma.diff,
    gamma.richness_cage.treat.lrr = within.cage.treat_gamma.lrr)

# Check structure
dplyr::glimpse(gam.diff_v1)

## ------------------------------------------- ##
# Load Alpha Diffs & LRRs  ----
## ------------------------------------------- ##

# Load alpha diversity diffs
alp.diff_v1 <- read.csv(file = file.path("data", "06-C_caged_alpha-div-diff_all-scales.csv")) %>% 
  dplyr::rename(design.level = alpha.diversity_design.level,
    alpha.diversity_cage.treat.diff = within.cage.treat_alpha.diff,
    alpha.diversity_cage.treat.lrr = within.cage.treat_alpha.lrr)

# Check structure
dplyr::glimpse(alp.diff_v1)

## ------------------------------------------- ##
# Load Dominance Diffs & LRRs  ----
## ------------------------------------------- ##

# Load alpha diversity diffs
dom.diff_v1 <- read.csv(file = file.path("data", "06-D_caged_dominance-diff_all-scales.csv")) %>% 
  dplyr::rename(design.level = dominance_design.level,
    dominance_cage.treat.diff = within.cage.treat_dominance.diff,
    dominance_cage.treat.lrr = within.cage.treat_dominance.lrr)

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
dplyr::glimpse(beta.diff_v1)

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
# Join Alpha & Dominance ----
## ------------------------------------------- ##

# Join data available at all/multiple design levels
join_v2 <- meta_v1 %>% 
  dplyr::left_join(x = ., y = gam.diff_v1,
    by = dplyr::join_by(source, exp.name))

# Check structure
dplyr::glimpse(join_v1)

## ------------------------------------------- ##




## ------------------------------------------- ##
# Combine Gamma/Alpha/Dominance LRRs ----
## ------------------------------------------- ##

# Join these three together (to make later joins easier)
diff_v1 <- gam.diff_v1 %>% 
  dplyr::left_join(x = ., y = alp.diff_v1,
    by = c("source", "organization", "site", "project.name", "sampling.years", 
      "excluded.group", "measured.group", "exp.name")) %>% 
  dplyr::left_join(x = ., y = dom.diff_v1,
    by = c("source", "organization", "site", "project.name", "sampling.years", 
    "excluded.group", "measured.group", "exp.name", "exp.design.4",
    "exp.design.3", "exp.design.2", "exp.design.1",
    "alpha.diversity_design.level" = "dominance_design.level")) %>% 
  dplyr::relocate(dplyr::all_of(c(paste0("exp.design.", 1:4))), 
    .after = exp.name) %>% 
  dplyr::rename(betadisp.design.level = alpha.diversity_design.level)

# Check structure
dplyr::glimpse(diff_v1)

## ------------------------------------------- ##
# Beta - Load Mean Differences ----
## ------------------------------------------- ##

# Read in the mean difference files too
beta.diff_v1 <- read.csv(file = file.path("data", "06-A_caged_mean-beta-diff_all-scales.csv"))

# Check structure of one
dplyr::glimpse(beta.diff_v1)

## ------------------------------------------- ##
# Beta - Reformat Data ----
## ------------------------------------------- ##

# Get this data into a comparable format to the gamma/alpha/dominance data
beta.diff_v2 <- beta.diff_v1 %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("within.cage.treat")) %>% 
  dplyr::mutate(name = gsub("within.cage.treat_", "", name)) %>% 
  dplyr::mutate(new.name = paste0(name, "_", cage.treatment_std)) %>% 
  dplyr::select(-name, -cage.treatment_std) %>% 
  tidyr::pivot_wider(names_from = new.name, values_from = value) %>% 
  dplyr::relocate(dplyr::contains(".mean"), dplyr::contains(".n"), 
    dplyr::contains(".sd"), dplyr::contains(".se"), 
    .after = dplyr::everything()) %>% 
  dplyr::relocate(dplyr::contains("mean_"), dplyr::contains("mean.diff"), 
    dplyr::contains("mean.lrr"),
    .after = year) %>% 
  dplyr::relocate(exp.name, betadisp.design.level, .after = year) %>% 
  dplyr::select(-betadisp.mean.diff_uncaged, -betadisp.mean.lrr_uncaged) %>% 
  dplyr::rename(betadisp.mean.treat.diff = betadisp.mean.diff_caged,
    betadisp.mean.lrr = betadisp.mean.lrr_caged)

# Check structure
dplyr::glimpse(beta.diff_v2)

## ------------------------------------------- ##
# Attach *EVERYTHING* to Data ----
## ------------------------------------------- ##
## https://tenor.com/view/everyone-the-professional-shout-gif-12696023

# Make a list for storing outputs
w.meta_out_list <- list()

# Loop across files for which we want 'metadata' attached
for(focal_w.meta in w.meta_outs){
  # focal_w.meta <- "05-A_caged_beta-disp_all-scales.csv"
  
  # Processing message
  message("Attaching ancillary data to ", focal_w.meta)
  
  # Grab just that file out of the list of inputs
  w.meta_v1 <- w.meta_in_list[[focal_w.meta]]
  
  # Now attach true metadata GoogleSheet & reorder columns
  w.meta_v2 <- w.meta_v1 %>% 
    dplyr::left_join(y = meta_v6, by = c("source", "exp.name")) %>% 
    dplyr::relocate(exp.design.4:betadisp.comm.dist, 
                    .after = dplyr::everything())
  
  # Now attach gamma richness & reorder columns
  w.meta_v3 <- w.meta_v2 %>% 
    dplyr::left_join(x = ., y = diff_v1, 
      by = c("source", "organization", "site", "project.name", "sampling.years",
      "excluded.group", "measured.group", "exp.name", "betadisp.design.level",
      "exp.design.4", "exp.design.3", "exp.design.2", "exp.design.1")) 
    dplyr::relocate(gamma.richness, .before = exp.name)
  
  # Now attach summarized beta disp and mean difference
  w.meta_v4 <- w.meta_v3 %>% 
    ## No column re-ordering needed (want these at end)
    dplyr::left_join(y = diff_v1, by = c("source", "organization", "site", 
                                         "excluded.group", "measured.group",
                                         "exp.name", "cage.treatment_std",
                                         "year", "betadisp.design.level")) %>%
  # now attach log response ratio
  dplyr::left_join(lrr_v1 %>%
                     select("within.cage.treat_betadisp.mean.lrr", "source", "organization", "site", 
                            "excluded.group", "measured.group",
                            "exp.name", "cage.treatment_std",
                            "year", "betadisp.design.level"), by= c("source", "organization", "site", 
                                 "excluded.group", "measured.group",
                                 "exp.name", "cage.treatment_std",
                                 "year", "betadisp.design.level"))
  
  # Add this to the output list
  w.meta_out_list[[focal_w.meta]] <- w.meta_v4
  
} # Close loop

# Check the structure at various points
## Starting (no metadata added)
dplyr::glimpse(w.meta_v1)
## After adding metadata GoogleSheet
dplyr::glimpse(w.meta_v2)
## After adding gamma richness
dplyr::glimpse(w.meta_v3)
## After adding summarized beta disp + mean diff
dplyr::glimpse(w.meta_v4)

# How many sources and exp.name got through the pipeline?
unique(w.meta_v4$source) # 117
unique(w.meta_v4$exp.name) # 346

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Loop across the list elements to export
for(w.meta_outs in unique(names(w.meta_out_list))){
  # w.meta_outs <- "05-A_caged_beta-disp_all-scales.csv"
  
  # Create a final object
  w.meta_v99 <- w.meta_out_list[[w.meta_outs]]
  
  # Generate tidy name / path
  w.meta_name <- gsub(pattern = "05-A_caged_beta-disp", 
                    replacement = "07_caged_w.meta", x = w.meta_outs)
  w.meta_path <- file.path("data", w.meta_name)
  
  # Export locally
  write.csv(x = w.meta_v99, row.names = F, na = '', file = w.meta_path)
}

colnames(w.meta_v99)
# End ----
