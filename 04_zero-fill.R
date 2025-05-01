## --------------------------------------------------------------- ##
# CAGED Zero Filling
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

# NOTE
## This is _extremely_ computationally-intensive
## That's why a relatively short operation is housed in its own script
### (to minimize existing memory usage)

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
fill_v1 <- read.csv(file.path("data", "03_caged_filtered.csv"))

# Check structure
dplyr::glimpse(fill_v1)

## ------------------------------------------- ##
# Aggregate Within Groups ----
## ------------------------------------------- ##

# Why is this done here?
## Zero-filling adds a _huge_ number of rows
## If we did this aggregation step after this script does its job,
## we'd just be creating a bunch of useless rows and blowing up the size of file coming out of this script
### (beyond how huge it will be if it works as designed!)

# Summarize to only one replicate within the finest design scale
## Standardization of treatments alone will result in "duplicates" across which we'd want to average
fill_v2 <- fill_v1 %>% 
  dplyr::group_by(
    dplyr::across(
      dplyr::all_of(setdiff(x = names(fill_v1), y = "abundance")))) %>% 
  dplyr::summarize(abundance = mean(abundance, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# How many rows were summarized across?
message(nrow(fill_v1) - nrow(fill_v2), " rows lost by summarizing within 'exp.design.1'")
## May need to double check source of this if this number is non-zero!
## Note though that streamlining treatments will likely make this number non-zero 
### (E.g., "Exclosure" and "Fence" would be different rows but synonymizing them fixes that)

# Identify any datasets dropped entirely (shouldn't be any)
supportR::diff_check(old = unique(fill_v1$source), new = unique(fill_v2$source))

# Re-check structure
dplyr::glimpse(fill_v2)

## ------------------------------------------- ##
# Zero-Fill Community Data ----
## ------------------------------------------- ##

# Make list for outputs
fill_list <- list()

# Loop across datasets
for(focal_src in sort(unique(fill_v2$source))){
  
  # Progress message
  message("Zero-filling file: '", focal_src, "'")
  
  # Subset the data
  fill_sub <- dplyr::filter(.data = fill_v2, source == focal_src)
  
  # Zero fill by flipping to wide format then back to long
  focal_fill <- fill_sub %>% 
    # Make row ID column + make taxa names better for col names
    dplyr::mutate(unique.id = 1:nrow(.),
                  taxa = paste0("temporary_", taxa)) %>% 
    # Pivot wide filling with 0
    tidyr::pivot_wider(names_from = taxa,
                       values_from = abundance,
                       values_fill = 0) %>% 
    # Pivot back into long format
    tidyr::pivot_longer(cols = dplyr::starts_with("temporary_"),
                        names_to = "taxa",
                        values_to = "abundance") %>% 
    # Remove temp label from taxon names
    dplyr::mutate(taxa = gsub("temporary_", replacement = "",
                              x = taxa)) %>% 
    # Drop row ID column
    dplyr::select(-unique.id) %>% 
    # Drop non-unique rows
    dplyr::distinct()
  
  # Add to list output
  fill_list[[focal_src]] <- focal_fill
  
  # Clear environment (as much as possible) and collect garbage
  rm(list = c("fill_sub", "focal_fill")); gc() 
  
} # Close loop

# Collapse list
fill_v3 <- purrr::list_rbind(x = fill_list)

# Out of curiosity, how many rows does that add?
message(nrow(fill_v3) - nrow(fill_v2), " rows gained by zero-filling")

# Check structure
dplyr::glimpse(fill_v3)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
fill_v99 <- fill_v3

# Identify tidy file name / path
zerofill_name <- "04_caged_zero-filled.csv"
zerofill_path <- file.path("data", zerofill_name)

# Export locally
write.csv(x = fill_v99, row.names = F, na = '', file = zerofill_path)

# # Upload to Drive
# googledrive::drive_upload(media = zerofill_path, overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
