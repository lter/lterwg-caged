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
# Zero-Fill Community Data ----
## ------------------------------------------- ##

# Make list for outputs
fill_list <- list()

# Loop across datasets
for(focal_src in sort(unique(fill_v1$source))){
  
  # Progress message
  message("Zero-filling file: '", focal_src, "'")
  
  # Subset the data
  fill_sub <- dplyr::filter(.data = fill_v1, source == focal_src)
  
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
    dplyr::select(-unique.id)
  
  # Add to list output
  fill_list[[focal_src]] <- focal_fill
  
  # Clear environment (as much as possible) and collect garbage
  rm(list = c("fill_sub", "focal_fill")); gc() 
  
} # Close loop

# Collapse list
fill_v2 <- purrr::list_rbind(x = fill_list)

# Out of curiosity, how many rows does that add?
nrow(fill_v2) - nrow(fill_v1)

# Check structure
dplyr::glimpse(fill_v2)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
fill_v99 <- fill_v2

# Identify tidy file name / path
zerofill_name <- "04_caged_zero-filled.csv"
zerofill_path <- file.path("data", zerofill_name)

# Export locally
write.csv(x = fill_v99, row.names = F, na = '', file = zerofill_path)

# # Upload to Drive
# googledrive::drive_upload(media = zerofill_path, overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
