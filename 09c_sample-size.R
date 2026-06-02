## --------------------------------------------------------------- ##
# CAGED Raw data figures
## --------------------------------------------------------------- ##
# Written by: 

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, njlyon0/supportR,
                 ggpubr) #, update_all= TRUE) 

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ----
# these dfs were created in script 08 script
## ------------------------------------------- ##
caged_beta <- read.csv(file.path("data", "08_caged_prepped-beta-dispersion.csv"))


# View(caged_beta) 
dim(caged_beta)# 12905  rows

## ------------------------------------------- ##
# Plots ---- 
## ------------------------------------------- ##
colnames(caged_beta)
