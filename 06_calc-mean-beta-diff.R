## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Beta Dispersion Calculation
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

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
# Calculate Mean Difference ----
## ------------------------------------------- ##

# Identify any beta dispersion outputs
(beta_outs <- dir(path = file.path("data"), pattern = "05-A_caged_beta-disp"))

# Make a list for storing outputs
diff_list <- list()

# Loop across these to be more interpretable than purrr-style functional programming
for(focal_beta in beta_outs){
  # focal_beta <- "05-A_caged_beta-disp_all-scales.csv"
  
  # Progress message
  message("Calculating mean difference / summary stats for ", focal_beta)
  
  # Read the file in
  diff_v1 <- read.csv(file = file.path("data", focal_beta))
  
  # Identify the grouping columns (we'll use this twice)
  diff_groupcols <- c("source", "organization", "site", 
                      "excluded.group", "measured.group", 
                      "exp.name", "year", "betadisp.design.level")
  
  # Do some needed preparatory calculatation
  diff_v2 <- diff_v1 %>% 
    # Remove missing beta disp & bad cage treatments
    dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
    dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
    # Summarize within treatments/etc.
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c(diff_groupcols, "cage.treatment_std"))
      )) %>% 
    dplyr::summarize(within.cage.treat_betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
                     within.cage.treat_betadisp.sd = sd(betadisp.comm.dist, na.rm = T),
                     within.cage.treat_betadisp.n = dplyr::n(),
                     within.cage.treat_betadisp.se = within.cage.treat_betadisp.sd / sqrt(within.cage.treat_betadisp.n),
                     .groups = "keep") %>% 
    dplyr::ungroup()
  
  # Caculate difference in means
  diff_v3 <- diff_v2 %>% 
    # Dump unwanted columns
    dplyr::select(-within.cage.treat_betadisp.sd, -within.cage.treat_betadisp.n, -within.cage.treat_betadisp.se) %>% 
    # Pivot wider
    tidyr::pivot_wider(names_from = cage.treatment_std,
                       values_from = within.cage.treat_betadisp.mean) %>% 
    # Calculate difference between uncaged & caged
    dplyr::mutate(within.cage.treat_betadisp.mean.diff = uncaged - caged)
  
  # Tidy up that output slightly
  diff_v4 <- diff_v3 %>% 
    # Drop the cage/uncage columns
    dplyr::select(-dplyr::ends_with("caged")) %>% 
    # Keep only unique rows
    dplyr::distinct()
  
  # Attach that back on the summarized version of the output
  diff_v5 <- diff_v2 %>% 
    # ALWAYS CHECK THE 'Y' OBJECT IS CORRECT IF UPDATING SCRIPT
    dplyr::left_join(y = diff_v4,  by = diff_groupcols)
  
  # Add this to the output list
  diff_list[[focal_beta]] <- diff_v5
  
} # Close loop

# Check the structure at various stages
## Starting version
dplyr::glimpse(diff_v1)
## After summary stat calculation
dplyr::glimpse(diff_v2)
## After diff calculation
dplyr::glimpse(diff_v3)
## Tidy versionof data with diffs calculated
dplyr::glimpse(diff_v4)
## After joining the summarized data with the diffs
dplyr::glimpse(diff_v5)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Loop across the list elements to export
for(diff_outs in unique(names(diff_list))){
  
  # Create a final object
  diff_v99 <- diff_list[[diff_outs]]
  
  # Generate tidy name / path
  diff_name <- gsub(pattern = "05-A_caged_beta-disp", 
                    replacement = "06_caged_mean-beta-diff", x = diff_outs)
  diff_path <- file.path("data", diff_name)
  
  # Export locally
  write.csv(x = diff_v99, row.names = F, na = '', file = diff_path)

}

# # Upload all of these to the Drive
# purrr::walk(.x = dir(path = file.path("data"), pattern = "06_caged_mean-beta-diff"),
#             .f = ~ googledrive::drive_upload(media = file.path("data", .x), overwrite = T,
#                                              path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")))

# End ----
