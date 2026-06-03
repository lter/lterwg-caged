## --------------------------------------------------------------- ##
# CAGED Dominance Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate Berger Parker dominance at all design levels

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
dom_v01 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(dom_v01)

# Sum across years
dom_v02 <- dom_v01 %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("year", "abundance"))))) %>% 
  dplyr::summarize(abundance = mean(abundance, na.rm = TRUE),
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
    .after = cage.treatment_std) %>% 
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
    dplyr::all_of(setdiff(x = names(.), y = c("abundance"))))) %>% 
  dplyr::summarize(tax.abun1 = mean(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.1", "taxa", "tax.abun1"))))) %>% 
  dplyr::summarize(total.abundance = sum(tax.abun1, na.rm = TRUE),
    max.abundance = max(tax.abun1, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_std) %>% 
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
    dplyr::all_of(setdiff(x = names(.), y = c("abundance"))))) %>% 
  dplyr::summarize(tax.abun1 = mean(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.1", "tax.abun1"))))) %>% 
  dplyr::summarize(tax.abun2 = mean(tax.abun1, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.2",  "taxa", "tax.abun2"))))) %>% 
  dplyr::summarize(total.abundance = sum(tax.abun2, na.rm = TRUE),
    max.abundance = max(tax.abun2, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_std) %>% 
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
    dplyr::all_of(setdiff(x = names(.), y = c("abundance"))))) %>% 
  dplyr::summarize(tax.abun1 = mean(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.1", "tax.abun1"))))) %>% 
  dplyr::summarize(tax.abun2 = mean(tax.abun1, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.2", "tax.abun2"))))) %>% 
  dplyr::summarize(tax.abun3 = mean(tax.abun2, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.3", "taxa", "tax.abun3"))))) %>% 
  dplyr::summarize(total.abundance = sum(tax.abun3, na.rm = TRUE),
    max.abundance = max(tax.abun3, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_std) %>% 
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
    dplyr::all_of(setdiff(x = names(.), y = c("abundance"))))) %>% 
  dplyr::summarize(tax.abun1 = mean(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.1", "tax.abun1"))))) %>% 
  dplyr::summarize(tax.abun2 = mean(tax.abun1, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.2", "tax.abun2"))))) %>% 
  dplyr::summarize(tax.abun3 = mean(tax.abun2, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.3", "tax.abun3"))))) %>% 
  dplyr::summarize(tax.abun4 = mean(tax.abun3, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::group_by(dplyr::across(
  dplyr::all_of(setdiff(x = names(.), y = c("exp.design.4", "taxa", "tax.abun4"))))) %>% 
  dplyr::summarize(total.abundance = sum(tax.abun4, na.rm = TRUE),
    max.abundance = max(tax.abun4, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dominance = max.abundance / total.abundance,
    .after = cage.treatment_std) %>% 
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
dom_name <- "05-D_caged_dominance_all-scales.csv"

# How many make it through (should be all but there's subsetting happening)
sort(unique(dom_allscales$source)) #121
sort(unique(dom_allscales$exp.name)) #367

# Re-check 'all scales' structure
dplyr::glimpse(dom_allscales)

# Export locally
write.csv(x = dom_allscales, na = '', row.names = FALSE,
  file = file.path("data", dom_name))

# End ----
