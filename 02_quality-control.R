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
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v2 <- tidy_v1 %>% 
  # Drop duplicate rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(tidy_v2)

# Export locally
write.csv(x = tidy_v2, row.names = F, na = '',
          file = file.path("data", "caged_tidied.csv"))


# End ----
