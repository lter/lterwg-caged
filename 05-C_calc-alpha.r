## --------------------------------------------------------------- ##
# CAGED Alpha Diversity Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate alpha diversity (i.e., richness) at all design levels

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
alpha_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv")) %>% 
  dplyr::mutate(abundance = ifelse(abundance > 0, yes = 1, no = 0))

# Check structure
dplyr::glimpse(alpha_v1)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Design 1) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.1"
alpha_des1 <- alpha_v1 %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = sum(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.design.level = "exp.design.1",
    .after = year)

# Check structure
dplyr::glimpse(alpha_des1)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Design 2) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.2"
alpha_des2 <- alpha_v1 %>% 
  dplyr::select(-dplyr::all_of(paste0("exp.design.", 1))) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = sum(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.design.level = "exp.design.2",
    .after = year)

# Check structure
dplyr::glimpse(alpha_des2)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Design 3) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.3"
alpha_des3 <- alpha_v1 %>% 
  dplyr::select(-dplyr::all_of(paste0("exp.design.", 1:2))) %>% 
  dplyr::distinct() %>%   
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = sum(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.design.level = "exp.design.3",
    .after = year)

# Check structure
dplyr::glimpse(alpha_des3)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Design 4) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.design.4"
alpha_des4 <- alpha_v1 %>% 
  dplyr::select(-dplyr::all_of(paste0("exp.design.", 1:3))) %>% 
  dplyr::distinct() %>%   
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = sum(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.design.level = "exp.design.4",
    .after = year)

# Check structure
dplyr::glimpse(alpha_des4)

## ------------------------------------------- ##
# Calculate Alpha Diversity (Exp Name) ----
## ------------------------------------------- ##

# Calculate alpha diversity for "exp.name"
alpha_name <- alpha_v1 %>% 
  dplyr::select(-dplyr::all_of(paste0("exp.design.", 1:4))) %>% 
  dplyr::distinct() %>%   
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("taxa", "abundance"))))) %>% 
  dplyr::summarize(alpha.diversity_richness = sum(abundance, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::mutate(alpha.design.level = "exp.name",
    .after = year)

# Check structure
dplyr::glimpse(alpha_name)

## ------------------------------------------- ##
# Process Outputs
## ------------------------------------------- ##

# Create an 'all scales' table
alpha_allscales <- dplyr::bind_rows(alpha_des1, alpha_des2, alpha_des3, alpha_des4, alpha_name)

# Check structure
dplyr::glimpse(alpha_allscales)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# How many sources and exp.name got through the pipeline?
unique(alpha_allscales$source) # 121
unique(alpha_allscales$exp.name) # 367

# Identify tidy file name / path
alpha_filename <- "05-C_caged_alpha-div_all-scales.csv"

# Re-check 'all scales' structure
dplyr::glimpse(alpha_allscales)

# Export locally
write.csv(x = alpha_allscales, na = '', row.names = F, 
  file = file.path("data", alpha_filename))

# End ----

