## --------------------------------------------------------------- ##
# CAGED Uncage v. Cage Diff. Mega Figure
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

# Purpose:
## 

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Data Preparation (Across Design Levels) ----
## ------------------------------------------- ##

# Read in the data
caged_v1 <- read.csv(file = file.path("data", "05_caged_beta-disp.csv"))

# Check structure
dplyr::glimpse(caged_v1)

# Do needed preparing of data
caged_v2 <- caged_v1 %>% 
  # Remove missing beta dispersion
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  # Keep only good treatments
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  # Summarize
  # Summarize within treatments
  dplyr::group_by(source, betadisp.design.level, cage.treatment_std) %>% 
  dplyr::summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Pivot to treatment into wide format
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = betadisp.mean) %>% 
  # Calculate difference
  dplyr::mutate(diff = uncaged - caged)

# Re-check structure
dplyr::glimpse(caged_v2)

## ------------------------------------------- ##
# Create Graph (Across Design Levels) ----
## ------------------------------------------- ##

# Create desired graph
ggplot(caged_v2, aes(x = diff, y = reorder(source, dplyr::desc(-diff)), 
                     color = betadisp.design.level)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = 3) +
  labs(x = "Uncaged - Caged Beta Dispersion",
       y = "Dataset Source") +
  supportR::theme_lyon() +
  theme(axis.text.y = element_blank())

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Data Preparation (Within Design Levels) ----
## ------------------------------------------- ##

# Identify local data files
beta_files <- dir(path = file.path("data"), pattern = "05_caged_beta-disp_exp-design-")

# Output list
beta_list <- list()

# Loop across needed data files
for(focal_file in sort(unique(beta_files))){
  
  # Progress message
  message("Processing file: ", focal_file)
  
  # Read in the data
  caged_v1 <- read.csv(file = file.path("data", focal_file)) %>% 
    # Remove missing beta dispersion
    dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
    # Keep only good treatments
    dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
    # Remove unwanted columns
    dplyr::select(source, cage.treatment_std, betadisp.design.level, 
                  betadisp.comm.dist) %>% 
    # Summarize within treatments
    dplyr::group_by(source, betadisp.design.level, cage.treatment_std) %>% 
    dplyr::summarize(betadisp = mean(betadisp.comm.dist, na.rm = T),
                     .groups = "keep") %>% 
    dplyr::ungroup() %>% 
    # Pivot to treatment into wide format
    tidyr::pivot_wider(names_from = cage.treatment_std, values_from = betadisp) %>% 
    # Calculate difference
    dplyr::mutate(diff = uncaged - caged)
  
  # Add to list
  beta_list[[focal_file]] <- caged_v1
  
}

# Unlist output
caged_v2 <- purrr::list_rbind(x = beta_list)

# Check structure
dplyr::glimpse(caged_v2)

## ------------------------------------------- ##
# Generate Exploratory Graph (Within Design Levels) ----
## ------------------------------------------- ##

# Start graphing!
ggplot(caged_v2, aes(x = diff, y = reorder(source, dplyr::desc(-diff)), 
                     color = betadisp.design.level)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = 3) +
  labs(x = "Uncaged - Caged Beta Dispersion",
       y = "Dataset Source") +
  supportR::theme_lyon() +
  theme(axis.text.y = element_blank())






# End ----
