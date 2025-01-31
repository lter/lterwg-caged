## --------------------------------------------------------------- ##
# CAGED Filtering
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
beta_v1 <- read.csv(file.path("data", "caged_filtered.csv"))

# Check structure
dplyr::glimpse(beta_v1)





## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
beta_v99 <- beta_v1

# Export
write.csv(x = beta_v99, na = '', row.names = F,
          file = file.path("data", "caged_beta-disp.csv"))

# End ----
