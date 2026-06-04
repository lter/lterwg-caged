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
# Filter to Experiment Name Level ----
## ------------------------------------------- ##

# Filter to only experiment name-level, desired treatments, and average across replicates
dom.diff_v02 <- dom.diff_v01 %>% 
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  dplyr::mutate(cage.treatment_std = paste0("dominance_", cage.treatment_std))

# Check structure
dplyr::glimpse(dom.diff_v02)

## ------------------------------------------- ##
# Streamline Data ----
## ------------------------------------------- ##
# Identify the grouping columns (we'll use this twice)
diff_groupcols <- c("source", "organization", "site", 
                    "excluded.group", "measured.group", 
                    "exp.name","dominance_design.level")


# Drop columns that are completely empty (i.e., "exp.design.#" columns)
dom.diff_v03 <- dom.diff_v02 %>% 
  dplyr::select(-dplyr::where(fn = ~ all(nchar(.) == 0 | is.na(.)))) %>%
# Summarize within treatments/etc.
dplyr::group_by(dplyr::across(
  dplyr::all_of(c(diff_groupcols, "cage.treatment_std"))
)) %>% 
  dplyr::summarize(dominance = mean(dominance, na.rm = TRUE),
                   .groups = "drop")
  
  
# Check structure
dplyr::glimpse(dom.diff_v03)

## ------------------------------------------- ##
# Reshape & Calculate LRR ----
## ------------------------------------------- ##

# Reshape the data and calculate log response ratio
dom.diff_v04 <- dom.diff_v03 %>% 
  dplyr::select(-dplyr::ends_with("abundance")) %>% 
  tidyr::pivot_wider(names_from = cage.treatment_std,
    values_from = dominance) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("caged"),
    .fns = ~ ifelse(is.na(.), yes = 0, no = .))) %>% 
  # Add minimum value of dominance to avoid 'division by 0' problem
  dplyr::mutate(caged = dominance_caged + min(dom.diff_v03$dominance, na.rm = TRUE),
    uncaged = dominance_uncaged + min(dom.diff_v03$dominance, na.rm = TRUE)) %>% 
  dplyr::mutate(dominance_cage.treat.diff = uncaged - caged,
    dominance_cage.treat.lrr = log2(uncaged / caged)) %>% 
  dplyr::select(-caged, -uncaged)

# Check structure
dplyr::glimpse(dom.diff_v04)

## ------------------------------------------- ##
# Streamline Output ----
## ------------------------------------------- ##

# Pare down this output somewhat
dom.diff_v05 <- dom.diff_v04 %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(
    setdiff(x = names(.), 
      y = c(paste0("dominance_", c("caged", "uncaged", 
        "cage.treat.diff", "cage.treat.lrr")), "cage.treatment_orig"))))) %>% 
  dplyr::summarize(dominance.caged = mean(dominance_caged, na.rm = TRUE),
    dominance.uncaged = mean(dominance_uncaged, na.rm = TRUE),
    dominance.diff = mean(dominance_cage.treat.diff, na.rm = TRUE), 
    dominance.lrr = mean(dominance_cage.treat.lrr, na.rm = TRUE),
    .groups = "drop")

# Check structure
dplyr::glimpse(dom.diff_v05)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
dom.diff_v99 <- dom.diff_v05

# Count number of sources/experiments at end
unique(dom.diff_v99$source) # 121
unique(dom.diff_v99$exp.name) # 367

# Identify tidy file name / path
dom.diff_name <- "06-D_caged_dominance-diff_allscales.csv"
dom.diff_path <- file.path("data", dom.diff_name)

# Export locally
write.csv(x = dom.diff_v99, row.names = F, na = '', file = dom.diff_path)

# Make an 'experiment name' only
dom.diff_exp <- dom.diff_v99 %>% 
  dplyr::filter(dominance_design.level == "exp.name") %>% 
  dplyr::select(-dplyr::starts_with("exp.design."))

# Check structure
dplyr::glimpse(dom.diff_exp)

# Export
write.csv(x = dom.diff_exp, row.names = F, na = '', 
  file = file.path("data", "06-D_caged_dominance-diff_expname.csv"))

# End ----
