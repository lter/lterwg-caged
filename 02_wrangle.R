## --------------------------------------------------------------- ##
                # CAGED Wrangling & Quality Control
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

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
tidy_v1 <- read.csv(file.path("data", "caged_harmonized.csv"))

# Check structure
dplyr::glimpse(tidy_v1)

## ------------------------------------------- ##
# Treatments ----
## ------------------------------------------- ##

# Combine/streamline treatment information
tidy_v2 <- tidy_v1 %>% 
  # Combine into a single treatment column
  dplyr::mutate(
    original.treatment = dplyr::case_when(
      ## Use central treatment (if exists)
      nchar(orig.treat) != 0 ~ orig.treat,
      ## Combine fire/fence/gap for relevant study
      source == "royo_westvirginia_fernow_2000-2013_deer_plants.csv" ~ paste(orig.treat_fire, orig.treat_fence, orig.treat_gap, sep = "; "),
      ## Otherwise, put in warning text
      T ~ "NO TREATMENT IDENTIFIED"),
    .before = orig.treat) %>% 
  # Drop now superseded precursor columns
  dplyr::select(-dplyr::contains("orig.treat"))
  
# Check structure
dplyr::glimpse(tidy_v2)



## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v3 <- tidy_v2 %>% 
  # Drop duplicate rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(tidy_v3)

# Export locally
write.csv(x = tidy_v3, row.names = F, na = '',
          file = file.path("data", "caged_tidied.csv"))


# End ----
