## --------------------------------------------------------------- ##
# CAGED Setup
## --------------------------------------------------------------- ##

# Purpose:
## Creates all folders needed by any subsequent phase of the workflow

## ------------------------------------------- ##
# Create Local Folders ----
## ------------------------------------------- ##

# Create needed folders
## 'data/' folder and sub-folders
dir.create(path = file.path("data", "raw"), showWarnings = F, recursive = T)
dir.create(path = file.path("data", "diagnostic"), showWarnings = F)

## 'graphs/' folder for exploratory graphs
dir.create(path = file.path("graphs"), showWarnings = F)

## 'results/' folder for statistical results
dir.create(path = file.path("results"), showWarnings = F)

# End ----
