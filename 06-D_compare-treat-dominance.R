## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Dominance Calculation
## --------------------------------------------------------------- ##
# Purpose
## Calculate mean differences and log response ratios (LRR) for dominance

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Load the relevant output
dom.diff_v01 <- read.csv(file.path("data", "05-D_caged_dominance_all-scales.csv"))

# Check structure
dplyr::glimpse(dom.diff_v01)

## ------------------------------------------- ##
# Streamline / Prepare Data ----
## ------------------------------------------- ##

# Filter to only desired treatments and get data in right shape
dom.diff_v02 <- dom.diff_v01 %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  dplyr::select(-dplyr::ends_with(".abundance")) %>% 
  dplyr::mutate(cage.treatment_std = paste0("dominance_", cage.treatment_std)) %>% 
  tidyr::pivot_wider(names_from = cage.treatment_std,
    values_from = dominance)

# Check structure
dplyr::glimpse(dom.diff_v02)

# Average across caged and uncaged 

# Identify the grouping columns 
diff_groupcols <- c("source", "organization", "site", 
                    "excluded.group", "measured.group", 
                    "exp.name", "dominance_design.level", 
                    "dominance_caged", "dominance_uncaged")

# Do some needed preparatory calculatation
dom.diff_v03 <- dom.diff_v02 %>% 
  # Summarize within treatments/etc.
  dplyr::group_by(dplyr::across(
    dplyr::all_of(c(diff_groupcols)))) %>%  
  dplyr::summarize(dominance_caged.mean = mean(dominance_caged, na.rm = TRUE),
                   dominance_uncaged.mean = mean(dominance_uncaged, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::select(-dominance_caged, -dominance_uncaged)

dim(dom.diff_v03) # 9221


# minimum value of alpha diversity
range(dom.diff_v03$dominance_uncaged.mean) # has NA's because some scales you can't calculate? 

dom.diff_v03 %>% 
  summarise(min_val = min(dominance_uncaged.mean, na.rm = TRUE)) #  0.00221

dom.diff_v03 %>% 
  summarise(min_val = min(dominance_caged.mean, na.rm = TRUE)) # 0.00182


## ------------------------------------------- ##
# Calculate LRR ----
## ------------------------------------------- ##

# Calculate LRR
dom.diff_v04 <- dom.diff_v03 %>% 
  # dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("caged"),
  #   .fns = ~ ifelse(is.na(.), yes = 0, no = .))) %>% 
  # add minimum value of dominance - 0.00182
  dplyr::mutate(caged = dominance_caged.mean + 0.00182,
                uncaged = dominance_uncaged.mean + 0.00182) %>% 
  # create log response ratio
  dplyr::mutate(within.cage.treat_dom.mean.lrr = log2(uncaged / caged)) %>% 
  dplyr::select(-caged, -uncaged) %>%
  # remove NA's
  drop_na(within.cage.treat_dom.mean.lrr)

# Check structure
dplyr::glimpse(dom.diff_v04) # 6,336

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
dom.diff_v99 <- dom.diff_v04

# Count number of sources/experiments at end
unique(dom.diff_v99$source) # 120
unique(dom.diff_v99$exp.name) # 363

# Identify tidy file name / path
dom.diff_name <- "06-D_caged_dominance-diff_all-scales.csv"
dom.diff_path <- file.path("data", dom.diff_name)

# Export locally
write.csv(x = dom.diff_v99, row.names = F, na = '', file = dom.diff_path)

# End ----
