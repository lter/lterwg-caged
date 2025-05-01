## --------------------------------------------------------------- ##
# Boatyard - Dataset Inclusion Checker
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

# Purpose:
## Want to quickly identify whether a dataset makes it through all steps of the workflow?
## Use this script!

# Cautionary note:
## This script assumes you have run _all_ of the core workflow scripts!

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ----
## ------------------------------------------- ##

# Read in each core workflow's output file
caged_combo <- read.csv(file = file.path("data", "01_caged_harmonized.csv"))
caged_tidy <- read.csv(file = file.path("data", "02_caged_tidied.csv"))
caged_sub <- read.csv(file = file.path("data", "03_caged_filtered.csv"))
caged_zero.fill <- read.csv(file = file.path("data", "04_caged_zero-filled.csv"))
caged_beta <- read.csv(file = file.path("data", "05_caged_beta-disp.csv"))
caged_w.meta <- read.csv(file = file.path("data", "06_caged_with-metadata.csv"))

# Check the structure of one of them
dplyr::glimpse(caged_beta)

## ------------------------------------------- ##
# Create Custom Function ----
## ------------------------------------------- ##

# Wrapping this in a function makes it easier to expand
## But because it's still very situational, we're not going to put it in the "tools" folder (yet at least)

drop_checker <- function(check_col = "source", check_val = NULL,
                         obj1 = caged_combo, obj2 = caged_tidy, 
                         obj3 = caged_sub, obj4 = caged_zero.fill, 
                         obj5 = caged_beta, obj6 = caged_w.meta){
  
  # Error checks for data objects
  if(all(c("data.frame" %in% class(obj1), "data.frame" %in% class(obj2),
           "data.frame" %in% class(obj3), "data.frame" %in% class(obj4),
           "data.frame" %in% class(obj5), "data.frame" %in% class(obj6))) != T)
    stop("All data object arguments must be dataframe-like")
  
  # Error checks for 'check_col' argument
  if(is.character(check_col) != T || any(c(check_col %in% names(obj1), 
                                           check_col %in% names(obj2),
                                           check_col %in% names(obj3), 
                                           check_col %in% names(obj4),
                                           check_col %in% names(obj5), 
                                           check_col %in% names(obj6))) != T)
    stop("'check_col' must exactly match a column found in _all_ data objects")
  
  # Error checks for 'check_val' argument
  if(is.null(check_val))
    stop("'check_val' must be specified")
  
  # Grab contents of relevant column from all datasets
  obj1_vals <- unique(obj1[[check_col]])
  obj2_vals <- unique(obj2[[check_col]])
  obj3_vals <- unique(obj3[[check_col]])
  obj4_vals <- unique(obj4[[check_col]])
  obj5_vals <- unique(obj5[[check_col]])
  obj6_vals <- unique(obj6[[check_col]])
  
  # Is the desired entry found in each?
  obj1_p.a <- check_val %in% obj1_vals
  obj2_p.a <- check_val %in% obj2_vals
  obj3_p.a <- check_val %in% obj3_vals
  obj4_p.a <- check_val %in% obj4_vals
  obj5_p.a <- check_val %in% obj5_vals
  obj6_p.a <- check_val %in% obj6_vals
  
  # Print informative messages depending on result
  if(obj1_p.a != T)
    message("Specified 'check_val' not found in first object!")
  if(obj1_p.a == T & obj2_p.a != T)
    message("Specified 'check_val' lost between first and second objects")
  if(obj2_p.a == T & obj3_p.a != T)
    message("Specified 'check_val' lost between second and third objects")
  if(obj3_p.a == T & obj4_p.a != T)
    message("Specified 'check_val' lost between third and fourth objects")
  if(obj4_p.a == T & obj5_p.a != T)
    message("Specified 'check_val' lost between fourth and fifth objects")
  if(obj5_p.a == T & obj6_p.a != T)
    message("Specified 'check_val' lost between fifth and sixth objects")
  
  # And if it's found in all of 'em?
  if(all(obj1_p.a, obj2_p.a, obj3_p.a, obj4_p.a, obj5_p.a, obj6_p.a))
    message("Specified 'check_val' found in all objects!")
  
}

## ------------------------------------------- ##
# Check for Dropped Value ----
## ------------------------------------------- ##

# Invoke our custom function to test for the desired output
drop_checker(check_val = "lter-harvard_simestract_hemlockremoval_2012-2013_ungulates_shrubherb.csv")

# End ----
