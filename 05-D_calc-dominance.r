## --------------------------------------------------------------- ##
# CAGED Dominance Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate Berger Parker dominance at all design levels

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
dom_v01 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(dom_v01)

# Sum across years
dom_v02 <- dom_v01 %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("year", "abundance"))))) %>% 
  dplyr::summarize(abundance = sum(abundance, na.rm = TRUE),
    .groups = "drop")

# Check structure
dplyr::glimpse(dom_v02)

## ------------------------------------------- ##
# Calculate Dominance (Design 1) ----
## ------------------------------------------- ##

# Calculate dominance for "exp.design.1"
dom_des1 <- dom_v02 %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
  dplyr::summarize(total.abundance = sum(abundance, na.rm = TRUE),
    max.abundance = max(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_orig) %>% 
  dplyr::mutate(dominance_design.level = "exp.design.1",
    .before = dominance)

# Check structure
dplyr::glimpse(dom_des1)

## ------------------------------------------- ##
# Calculate Dominance (Design 2) ----
## ------------------------------------------- ##

# Calculate dominance for "exp.design.2"
dom_des2 <- dom_v02 %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c(paste0("exp.design.", 1), 
    "taxa", "abundance"))))) %>% 
  dplyr::summarize(total.abundance = sum(abundance, na.rm = TRUE),
    max.abundance = max(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_orig) %>% 
  dplyr::mutate(dominance_design.level = "exp.design.2",
    .before = dominance)

# Check structure
dplyr::glimpse(dom_des2)

## ------------------------------------------- ##
# Calculate Dominance (Design 3) ----
## ------------------------------------------- ##

# Calculate dominance for "exp.design.3"
dom_des3 <- dom_v02 %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c(paste0("exp.design.", 1:2), 
    "taxa", "abundance"))))) %>% 
  dplyr::summarize(total.abundance = sum(abundance, na.rm = TRUE),
    max.abundance = max(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_orig) %>% 
  dplyr::mutate(dominance_design.level = "exp.design.3",
    .before = dominance)

# Check structure
dplyr::glimpse(dom_des3)

## ------------------------------------------- ##
# Calculate Dominance (Design 4) ----
## ------------------------------------------- ##

# Calculate dominance for "exp.design.4"
dom_des4 <- dom_v02 %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c(paste0("exp.design.", 1:3), 
    "taxa", "abundance"))))) %>% 
  dplyr::summarize(total.abundance = sum(abundance, na.rm = TRUE),
    max.abundance = max(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_orig) %>% 
  dplyr::mutate(dominance_design.level = "exp.design.4",
    .before = dominance)

# Check structure
dplyr::glimpse(dom_des4)

## ------------------------------------------- ##
# Calculate Dominance (Experiment Name) ----
## ------------------------------------------- ##

# Calculate dominance for "exp.design.4"
dom_name <- dom_v02 %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c(paste0("exp.design.", 1:4), 
    "taxa", "abundance"))))) %>% 
  dplyr::summarize(total.abundance = sum(abundance, na.rm = TRUE),
    max.abundance = max(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_orig) %>% 
  dplyr::mutate(dominance_design.level = "exp.name",
    .before = dominance)

# Check structure
dplyr::glimpse(dom_name)

## ------------------------------------------- ##
# Process Outputs
## ------------------------------------------- ##

# Combine all of those to create an 'all scales' table
dom_allscales <- dplyr::bind_rows(dom_des1, dom_des2, dom_des3, dom_des4, dom_name)

# Check structure
dplyr::glimpse(dom_allscales)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Identify tidy file name / path
dom_name <- "05-D_caged_dominance"

# Re-check 'all scales' structure
dplyr::glimpse(dom_allscales)

# Export locally
write.csv(x = dom_allscales, na = '', row.names = F,
  file = file.path("data", paste0(dom_name, "_all-scales.csv")))

# End ----
