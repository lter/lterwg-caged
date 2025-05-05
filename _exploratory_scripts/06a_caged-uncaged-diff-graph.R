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
# Data Preparation ----
## ------------------------------------------- ##

# Identify local data files
beta_files <- dir(path = file.path("data"), pattern = "05_caged_beta-disp_exp-design-")

# Read in one file
caged_v1 <- read.csv(file = file.path("data", beta_files[[1]])) %>% 
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  dplyr::select(source, cage.treatment_std, betadisp.comm.dist) %>% 
  dplyr::group_by(source, cage.treatment_std) %>% 
  dplyr::summarize(betadisp = mean(betadisp.comm.dist, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = cage.treatment_std, values_from = betadisp) %>% 
  dplyr::mutate(diff = uncaged - caged) %>% 
  dplyr::arrange(dplyr::desc(diff))

# Check structure
dplyr::glimpse(caged_v1)

# Make graph
ggplot(caged_v1, aes(x = diff, y = source)) +
  geom_point() +
  supportR::theme_lyon() +
  theme(legend.position = "none",
        axis.text.y = element_blank())






# Read in data
caged_v1 <- purrr::map(.x = beta_files, .f = ~ read.csv(file.path("data", .x))) %>% 
  purrr::map(.x = ., .f = ~ dplyr::filter(.data = .x, !is.na(betadisp.median) & 
                                          !is.na(betadisp.comm.dist))) %>% 
  purrr::map(.x = ., .f = ~ dplyr::filter(.data = .x, cage.treatment_std %in% c("caged", "uncaged"))) %>% 
  purrr::map(.x = ., .f = ~ dplyr::select(.data = .x, source, year, cage.treatment_std,
                                          betadisp.design.level, betadisp.comm.dist)) %>% 
  purrr::map(.x = ., .f = ~ tidyr::pivot_wider(data = .x, names_from = cage.treatment_std,
                                               values_from = betadisp.comm.dist))
  

# Check structure
dplyr::glimpse(caged_v1[[1]])

  
  purrr::list_rbind(x = .)

# Process it further
caged_v2 <- caged_v1 %>% 
  # Filter out missing beta dispersion
  dplyr::filter(!is.na(betadisp.median) & !is.na(betadisp.comm.dist)) %>% 
  # Keep only bare minimum of required columns
  # dplyr::select(source:measured.group, year, cage.treatment_std,
  #               betadisp.design.level, betadisp.comm.dist) %>% 
  # Filter to only caged/uncaged
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged"))

  # Pivot wider
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = betadisp.comm.dist)
  
# Check structure
dplyr::glimpse(caged_v2)

## ------------------------------------------- ##
# Generate Exploratory Graph ----
## ------------------------------------------- ##

# Start graphing!
ggplot(caged_v2, aes(x = ))






# End ----
