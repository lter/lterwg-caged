## --------------------------------------------------------------- ##
# CAGED Data Wrangling for Stats 
## --------------------------------------------------------------- ##
# Written by: Marc J S Hensel, Nick J Lyon, Jamie McDevitt-Irwin

# Purpose:
## Create clean dataframes for beta dispersion and effect size (average - average)


## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries (performance might be within easystats)
librarian::shelf(tidyverse, njlyon0/supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ---- 
## ------------------------------------------- ##

# Read in relevant data file
betadisp_v1 <- read.csv(file.path("data", "08-A_caged_w.meta-beta-disp_fine-scales.csv"))
effectsizes_v1 <- read.csv(file.path("data", "08-B_caged_expname-effect-size_fine-scales.csv"))


# Check structure
dplyr::glimpse(betadisp_v1)
dplyr::glimpse(effectsizes_v1)


# Check number of sources
unique(effectsizes_v1$source) # 117
unique(effectsizes_v1$exp.name) # 347


unique(betadisp_v1$source) # 117
unique(betadisp_v1$exp.name) # 347
dim(betadisp_v1) # 13964    46


# Check number of rows and dataframe
dim(effectsizes_v1) # 620 53
dim(betadisp_v1) # 13964    46
dim(betadisp_v1) # 13964    46



## ------------------------------------------- ##
# What data made it through the pipeline? ---- 
## ------------------------------------------- ##
# which sources and experiment names make it through the pipeline 
test<- betadisp_v1 %>% 
  select(exp.name, source) %>% 
  distinct()

write.csv(test, "data/final.data.through.pipeline.csv")





## ------------------------------------------- ##
# Data Wrangling ---- 
## ------------------------------------------- ##

effectsizes_v2 <- effectsizes_v1 %>%
  # average by experiment name because any lower levels have a lrr
  group_by(source, var_upper.source, exp.name, var_resource.type.category,
           lat, var_grassy_v_stubtidal,
           cage.treatment_orig) %>%
  summarize(mean.beta.lrr = mean(betadisp.mean.lrr),
            mean.alpha.lrr = mean(alpha.mean.lrr),
            mean.dom.lrr = mean(dominance.mean.lrr),
            mean.cent.lrr = mean(betadisp.centroid.lrr))
# now we will have one value per experiment name 

dim(effectsizes_v1) # 620
dim(effectsizes_v2) # 404
# 404 - not the true number of exp.names because some have different original treatments 
# (e.g., recharge 1X1 and 3X3 have a row here )

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##


## Effect size df 
write.csv(x = effectsizes_v2, row.names = F, na = '',
          file = file.path("data", "08_caged_prepped-effect-size.csv"))

# End ----









