## --------------------------------------------------------------- ##
# CAGED Sample size by aquatic vs terrestrial data figures
## --------------------------------------------------------------- ##
# Written by: Nico Matallana, Raine Detmer, Hillary Krumbholz

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
# Check  ---- 
## ------------------------------------------- ##
colnames(caged_beta)

# List the predictors
caged_beta %>% select(starts_with("var")) %>% names()

# [1] "var_upper.source"                       "var_climate.zone"                      
# [3] "var_aq.or.terr"                         "var_ecotype1"                          
# [5] "var_consumer.taxonomy"                  "var_consumer.metabolism"               
# [7] "var_resource.type.category"             "var_consumer.richness.number"          
# [9] "var_consumer.richness.category"         "var_max.consumer.size.category"        
#[11] "var_dominant.consumer.species.category" "var_consumer.native.domestic"          
#[13] "var_succ.vs.late"                       "var_exclusion.duration.continuousyears"
#[15] "var_taxonomic.level"                    "var_exclosure.area.m2"                 
#[17] "var_whereisthecage"

# Check which columns are categorical and numerical 
caged_beta %>% 
  select(starts_with("var")) %>% 
  sapply(class)
