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
# File Name Information ----
## ------------------------------------------- ##

# Break useful information out of file name ("source" column)
tidy_v2 <- tidy_v1 %>% 
  # Separate by underscore
  tidyr::separate_wider_delim(cols = source, delim = "_",
                              names = c("organization", "site", 
                                        "experiment.name", "sampling.years", 
                                        "excluded.group", "measured.group"),
                              cols_remove = F) %>% 
  # Relocate source back to first position
  dplyr::relocate(source, .before = dplyr::everything())

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
