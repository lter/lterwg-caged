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
librarian::shelf(tidyverse)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ----
## ------------------------------------------- ##

# Identify desired data files
(core_files <- setdiff(x = dir(path = file.path("data"), pattern = "\\d{2}_"),
                       y = dir(path = file.path("data"), pattern = "_exp-")) )

# Read them all in
## Note that the data files _must_ be in the order in which they are created
## (The function we're about to create/use assumes they are in that order)
core_list <- purrr::map(.x = core_files,
                        .f = ~ read.csv(file = file.path("data", .x)))

# Check structure of one of those
dplyr::glimpse(core_list[[5]])

## ------------------------------------------- ##
# Create Custom Function ----
## ------------------------------------------- ##

# Wrapping this in a function makes it easier to expand
## But because it's still very situational, we're not going to put it in the "tools" folder (yet at least)

drop_checker <- function(check_col = "source", check_val = NULL, df_list = NULL){
  
  # Error checks for data list
  if(is.null(df_list) || any(purrr::map_lgl(.x = df_list, .f = ~ "data.frame" %in% class(.x))) != T)
    stop("'df_list' must be provided as a list of data.frame-like objects")
  
  # Error checks for 'check_col' argument
  if(is.null(check_col) || is.character(check_col) != T || any(purrr::map_lgl(.x = df_list, .f = ~ check_col %in% names(.x))) != T)
    stop("'check_col' must exactly match a column name found in _all_ data objects")
  
  # Error checks for 'check_val' argument
  if(is.null(check_val))
    stop("'check_val' must be specified")
  
  # Grab relevant column from each data object
  val_list <- purrr::map(.x = df_list, .f = ~ unique(.x[[check_col]]))
  
  # Is the desired entry found in each?
  p.a_vect <- purrr::map_lgl(.x = val_list, .f = ~ check_val %in% .x)
  
  # Convert this to a dataframe
  p.a_df <- data.frame("data.obj" = seq_along(p.a_vect),
                       "incl" = p.a_vect)
  
  # Handle the three possibilities:
  ## 1. Never dropped
  if(all(p.a_df$incl) == T){ 
    message("Specified 'check_val' found in all data objects!")
    
    ## 2. Never included
  } else if(all(p.a_df$incl) == F){
    message("Specified 'check_val' not found in any data object")
    
    ## 3. Dropped along the way
  } else {
    
    # Identify _where_ it was dropped
    first_w.o <- p.a_df %>% 
      dplyr::filter(incl != dplyr::lag(incl, n = 1))
    
    # Print an informative message
    message("Specified 'check_val' lost between data objects ", (first_w.o$data.obj - 1), 
            " and ", first_w.o$data.obj)
  }
  
  # Then return the little presence/absence data frame
  return(p.a_df) }

## ------------------------------------------- ##
# Check for Dropped Value(s) ----
## ------------------------------------------- ##

# Identify whether/when a particular dataset was dropped
drop_checker(check_val =  "lter-harvard_simestract_hemlockremoval_2012-2013_ungulates_shrubherb.csv",
            check_col = "source", df_list = core_list)

# End ----
