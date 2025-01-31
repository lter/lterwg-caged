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
sub_v1 <- read.csv(file.path("data", "caged_tidied.csv"))

# Check structure
dplyr::glimpse(sub_v1)





## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
sub_v99 <- sub_v1

# Export
write.csv(x = sub_v99, na = '', row.names = F,
          file = file.path("data", "caged_filtered.csv"))

# End ----
