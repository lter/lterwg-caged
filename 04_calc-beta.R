## --------------------------------------------------------------- ##
# CAGED Beta Dispersion Calculation
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, magrittr, ltertools, vegan, supportR)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Load needed tool(s)
source(file.path("tools", "fxn_calc-betadisp.R"))

# Define the minimum number of replicates for which we want to calculate beta dispersion
## Inclusive of this number (so 5 becomes >= 5)
min_reps <- 4

# Read in data
beta_v1 <- read.csv(file.path("data", "03_caged_filtered.csv"))

# Check structure
dplyr::glimpse(beta_v1)

## ------------------------------------------- ##
# Data Preparation ----
## ------------------------------------------- ##

# Summarize to only one replicate within the finest design scale
## Should already be one rep by now but better to make sure
beta_v2 <- beta_v1 %>% 
  dplyr::group_by(
    dplyr::across(
      dplyr::all_of(setdiff(x = names(beta_v1), y = "abundance")))) %>% 
  dplyr::summarize(abundance = mean(abundance, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# How many rows were summarized across?
message(nrow(beta_v1) - nrow(beta_v2), " rows lost by summarizing within 'exp.design.1'")
## May need to double check source of this if this number is non-zero!
## Note though that streamlining treatments will likely make this number non-zero 
### (E.g., "Exclosure" and "Fence" would be different rows but synonymizing them fixes that)

# Identify any datasets dropped entirely (shouldn't be any)
setdiff(x = unique(beta_v1$source), y = unique(beta_v2$source))

# Re-check structure
dplyr::glimpse(beta_v2)

## ------------------------------------------- ##
# Calculate Beta Dispersion ----
## ------------------------------------------- ##

# Create a list for storing outputs
beta_des1_list <- list()
beta_des2_list <- list()
beta_des3_list <- list()
beta_des4_list <- list()
beta_name_list <- list()

# Loop across original data source
for(focal_src in unique(beta_v2$source)){
  # focal_src <- "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv"
  
  # Progress message
  message("Processing source '", focal_src, "'")
  
  # Subset data
  src_sub <- beta_v2 %>% 
    dplyr::filter(source == focal_src)
  
  # Loop across treatments
  for(focal_trt in unique(src_sub$cage.treatment)){
    # focal_trt <- "caged"
    
    # Subset again
    trt_sub <- src_sub %>% 
      dplyr::filter(cage.treatment == focal_trt)
    
    # Loop across study years
    for(focal_yr in unique(trt_sub$year)){
      # focal_yr <- "2017-2019"
      
      # Subset again
      yr_sub <- trt_sub %>% 
        dplyr::filter(year == focal_yr)
      
      ## ------------------------ ##
      # Beta Disp for Design 1 ----
      ## ------------------------ ##
      
      # Loop across most granular level of experimental design
      for(focal_des1 in unique(yr_sub$exp.design.1)){
        # focal_des1 <- "marinepredexcl__ADC.4__13"
        
        # Subset yet again
        des1_sub <- yr_sub %>% 
          dplyr::filter(exp.design.1 == focal_des1)
        
        # Calculate beta dispersion
        des1_beta <- calc_betadisp(df = des1_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = "bray", result_prefix = "exp.design.1")
        
        # Do needed post-processing
        des1_out <- des1_beta %>% 
          tidyr::pivot_longer(cols = exp.design.1.n,
                              names_to = "betadisp.design.level",
                              values_to = "betadisp.sample.size") %>% 
          dplyr::mutate(betadisp.design.level = gsub("\\.n", "", x = betadisp.design.level)) %>% 
          dplyr::rename(betadisp.median = exp.design.1.betadisp.median,
                        betadisp.comm.dist = exp.design.1.betadisp.site.dist) %>% 
          dplyr::distinct()
        
        # Add to list
        beta_des1_list[[paste0(focal_src, focal_trt, focal_des1)]] <- des1_out
        
      } # Close "exp.design.1" loop

      ## ------------------------ ##
      # Beta Disp for Design 2 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 2
      for(focal_des2 in unique(yr_sub$exp.design.2)){
        # focal_des2 <- "marinepredexcl__ADC.4"
        
        # Subset yet again
        des2_sub <- yr_sub %>% 
          dplyr::filter(exp.design.2 == focal_des2)
        
        # Calculate beta dispersion
        des2_beta <- calc_betadisp(df = des2_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = "bray", result_prefix = "exp.design.2")
        
        # Do needed post-processing
        des2_out <- des2_beta %>% 
          dplyr::select(-exp.design.1) %>% 
          tidyr::pivot_longer(cols = exp.design.2.n,
                              names_to = "betadisp.design.level",
                              values_to = "betadisp.sample.size") %>% 
          dplyr::mutate(betadisp.design.level = gsub("\\.n", "", x = betadisp.design.level)) %>% 
          dplyr::rename(betadisp.median = exp.design.2.betadisp.median,
                        betadisp.comm.dist = exp.design.2.betadisp.site.dist) %>% 
          dplyr::distinct()
        
        # Add to list
        beta_des2_list[[paste0(focal_src, focal_trt, focal_des2)]] <- des2_out
        
      } # Close "exp.design.2" loop
      
      ## ------------------------ ##
      # Beta Disp for Design 3 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 3
      for(focal_des3 in unique(yr_sub$exp.design.3)){
        # focal_des3 <- "marinepredexcl"
        
        # Subset yet again
        des3_sub <- yr_sub %>% 
          dplyr::filter(exp.design.3 == focal_des3)
        
        # If there is more than one experimental design level 2...
        if(length(unique(des3_sub$exp.design.2)) > 1){
          
          # Average across experimental design 1 (within exp. design 2 levels)
          des3_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        des3_beta <- calc_betadisp(df = des3_sub, floor = min_reps,
                                       taxa_col = "taxa", abun_col = "abundance",
                                       dist_method = "bray", result_prefix = "exp.design.3")
        
        # Do needed post-processing
        des3_out <- des3_beta %>% 
          dplyr::select(-dplyr::ends_with(c("exp.design.1", "exp.design.2"))) %>% 
          tidyr::pivot_longer(cols = exp.design.3.n,
                              names_to = "betadisp.design.level",
                              values_to = "betadisp.sample.size") %>% 
          dplyr::mutate(betadisp.design.level = gsub("\\.n", "", x = betadisp.design.level)) %>% 
          dplyr::rename(betadisp.median = exp.design.3.betadisp.median,
                        betadisp.comm.dist = exp.design.3.betadisp.site.dist) %>% 
          dplyr::distinct()
        
        # Add to list
        beta_des3_list[[paste0(focal_src, focal_trt, focal_des3)]] <- des3_out
        
      } # Close "exp.design.3" loop
      
      ## ------------------------ ##
      # Beta Disp for Design 4 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 4
      for(focal_des4 in unique(yr_sub$exp.design.4)){
        # focal_des4 <- "marinepredexcl"
        
        # Subset yet again
        des4_sub <- yr_sub %>% 
          dplyr::filter(exp.design.4 == focal_des4)
        
        # If there is more than one experimental design level 2...
        if(length(unique(des4_sub$exp.design.2)) > 1){
          
          # Average across experimental design 1 (within exp. design 2 levels)
          des4_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(des4_sub), 
                                      y = c("exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # If there is more than one experimental design level 3...
        if(length(unique(des4_sub$exp.design.3)) > 1){
          
          # Average across experimental design 2 (within exp. design 3 levels)
          des4_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(des4_sub), 
                                      y = c("exp.design.2", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        des4_beta <- calc_betadisp(df = des4_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = "bray", result_prefix = "exp.design.4")
        
        # Do needed post-processing
        des4_out <- des4_beta %>% 
          dplyr::select(-dplyr::ends_with(c("exp.design.1", "exp.design.2", 
                                            "exp.design.3"))) %>%
          tidyr::pivot_longer(cols = exp.design.4.n,
                              names_to = "betadisp.design.level",
                              values_to = "betadisp.sample.size") %>% 
          dplyr::mutate(betadisp.design.level = gsub("\\.n", "", x = betadisp.design.level)) %>% 
          dplyr::rename(betadisp.median = exp.design.4.betadisp.median,
                        betadisp.comm.dist = exp.design.4.betadisp.site.dist) %>% 
          dplyr::distinct()
        
        # Add to list
        beta_des4_list[[paste0(focal_src, focal_trt, focal_des4)]] <- des4_out
        
      } # Close "exp.design.4" loop
      
      ## ------------------------ ##
      # Beta Disp for Exp.Name ----
      ## ------------------------ ##
      
      # Loop across experimental design level 4
      for(focal_name in unique(yr_sub$exp.name)){
        # focal_name <- "ADC"
        
        # Subset yet again
        name_sub <- yr_sub %>% 
          dplyr::filter(exp.name == focal_name)
        
        # If there is more than one experimental design level 2...
        if(length(unique(name_sub$exp.design.2)) > 1){
          
          # Average across experimental design 1 (within exp. design 2 levels)
          name_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(name_sub), 
                                      y = c("exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # If there is more than one experimental design level 3...
        if(length(unique(name_sub$exp.design.3)) > 1){
          
          # Average across experimental design 2 (within exp. design 3 levels)
          name_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(name_sub), 
                                      y = c("exp.design.2", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation

        # If there is more than one experimental design level 4...
        if(length(unique(name_sub$exp.design.4)) > 1){
          
          # Average across experimental design 2 (within exp. design 3 levels)
          name_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(name_sub), 
                                      y = c("exp.design.3", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        name_beta <- calc_betadisp(df = name_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = "bray", result_prefix = "exp.name")
        
        # Do needed post-processing
        name_out <- name_beta %>% 
          dplyr::select(-dplyr::ends_with(c("exp.design.1", "exp.design.2", 
                                            "exp.design.3", "exp.design.4"))) %>%
          tidyr::pivot_longer(cols = exp.name.n,
                              names_to = "betadisp.design.level",
                              values_to = "betadisp.sample.size") %>% 
          dplyr::mutate(betadisp.design.level = gsub("name\\.n", "name", 
                                                     x = betadisp.design.level)) %>% 
          dplyr::rename(betadisp.median = exp.name.betadisp.median,
                        betadisp.comm.dist = exp.name.betadisp.site.dist) %>% 
          dplyr::distinct()
        
        # Add to list
        beta_name_list[[paste0(focal_src, focal_trt, focal_name)]] <- name_out
        
      } # Close "exp.name" loop
    } # Close year loop
  } # Close treatment loop
} # Close source loop

## ------------------------------------------- ##
# Process Calculation Output Lists ----
## ------------------------------------------- ##

# Do some minor processing on the output lists and unlist them
## Design 1 Betas
beta_des1 <- beta_des1_list %>% 
  purrr::map(.x = ., .f = ~ dplyr::relocate(.data = .x, betadisp.median, betadisp.comm.dist,
                                    .after = betadisp.sample.size)) %>% 
  purrr::list_rbind(x = .)
## Design 2 Betas
beta_des2 <- beta_des2_list %>% 
  purrr::map(.x = ., .f = ~ dplyr::relocate(.data = .x, betadisp.median, betadisp.comm.dist,
                                            .after = betadisp.sample.size)) %>% 
  purrr::list_rbind(x = .)
## Design 3 Betas
beta_des3 <- beta_des3_list %>% 
  purrr::map(.x = ., .f = ~ dplyr::relocate(.data = .x, betadisp.median, betadisp.comm.dist,
                                            .after = betadisp.sample.size)) %>% 
  purrr::list_rbind(x = .)
## Design 4 Betas
beta_des4 <- beta_des4_list %>% 
  purrr::map(.x = ., .f = ~ dplyr::relocate(.data = .x, betadisp.median, betadisp.comm.dist,
                                            .after = betadisp.sample.size)) %>% 
  purrr::list_rbind(x = .)
## Exp Name Betas
beta_expname <- beta_name_list %>% 
  purrr::map(.x = ., .f = ~ dplyr::relocate(.data = .x, betadisp.median, betadisp.comm.dist,
                                            .after = betadisp.sample.size)) %>% 
  purrr::list_rbind(x = .)

# Check the structure of one
dplyr::glimpse(beta_des1)

# Add these to a list (useful later)
beta_deslists <- list(beta_des1, beta_des2, beta_des3, beta_des4, beta_expname)

## ------------------------------------------- ##
# Coalesce Across Design Levels ----
## ------------------------------------------- ##

# Ultimately we want only the best dispersion available in a given dataset
## Regardless of the "level" for that site

# Start with design level 1 (minus any NA dispersion values)
beta_v3 <- beta_des1 %>% 
  dplyr::filter(!is.na(betadisp.comm.dist))

# Check structure 
dplyr::glimpse(beta_v3)

# Drop NAs from level 2 output and drop data for which a finer level already exists
beta_des2_v2 <- beta_des2 %>% 
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  dplyr::filter(!exp.design.2 %in% beta_v3$exp.design.2 &
                  !source %in% beta_v3$source)

# Add this to the output object
beta_v4 <- dplyr::bind_rows(beta_v3, beta_des2_v2)

# Do the same for the next design level
beta_des3_v2 <- beta_des3 %>% 
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  dplyr::filter(!exp.design.3 %in% beta_v4$exp.design.3 &
                  !source %in% beta_v4$source)

# Add this to the output object
beta_v5 <- dplyr::bind_rows(beta_v4, beta_des3_v2)

# Do the same for the next design level
beta_des4_v2 <- beta_des4 %>% 
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  dplyr::filter(!exp.design.4 %in% beta_v5$exp.design.4 &
                  !source %in% beta_v5$source)

# Add this to the output object
beta_v6 <- dplyr::bind_rows(beta_v5, beta_des4_v2)

# Finally, do the same for exp.name too
beta_name_v2 <- beta_expname %>% 
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  dplyr::filter(!exp.name %in% beta_v6$exp.name &
                  !source %in% beta_v6$source)

# Add *this* to the output object
beta_v7 <- dplyr::bind_rows(beta_v6, beta_name_v2)

# Check structure
dplyr::glimpse(beta_v7)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Check for data sources in starting data but not output
supportR::diff_check(old = unique(beta_v2$source), new = unique(beta_v7$source))

# Create final object name
beta_v99 <- beta_v7

# Identify tidy file name / path
beta_name <- "04_caged_beta-disp.csv"
beta_path <- file.path("data", beta_name)

# Export locally
write.csv(x = beta_v99, row.names = F, na = '', file = beta_path)

# Let's also export the design-specific files (for posterity)
for(item in seq_along(beta_deslists)){
  
  # Processing message
  message("Processing sub-output ", item)
  
  # Assemble file path
  partial_level <- gsub(pattern = "\\.", replacement = "-", 
                        x = unique(beta_deslists[[item]]$betadisp.design.level))
  partial_beta_name <- paste0("04_caged_beta-disp_", partial_level, ".csv")
  partial_beta_path <- file.path("data", partial_beta_name)
  
  # Export locally
  write.csv(x = beta_deslists[[item]], row.names = F, na = '', file = partial_beta_path)
  
} # Close loop

# Upload all of these to the Drive
purrr::walk(.x = dir(path = file.path("data"), pattern = "04_caged_beta-disp"),
            .f = ~ googledrive::drive_upload(media = file.path("data", .x), overwrite = T,
                                             path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")))

# End ----



# BASEMENT -----


# What names are shared?
intersect(names(beta_des1), names(beta_des2))

# Combine them!
beta_v3 <- beta_des1 %>% 
  dplyr::left_join(y = beta_des2,
                   by = c("source", "organization", "site", 
                          "project.name", "sampling.years", "excluded.group", 
                          "measured.group", "cage.treatment", "year", 
                          "exp.name", "exp.design.4", "exp.design.3", "exp.design.2")) %>% 
  dplyr::left_join(y = beta_des3,
                   by = c("source", "organization", "site", 
                          "project.name", "sampling.years", "excluded.group", 
                          "measured.group", "cage.treatment", "year", 
                          "exp.name", "exp.design.4", "exp.design.3")) %>% 
# Drop non-unique rows that may result from this joining process
dplyr::distinct()

# Check structure
dplyr::glimpse(beta_v3)

## ------------------------------------------- ##
# Coalesce Beta Dispersion Values ----
## ------------------------------------------- ##

# Do needed wrangling
beta_v4 <- beta_v3 %>% 
  # Want finest non-NA level of beta dispersion for each dataset
  dplyr::mutate(
    ## Beta dispersion
    betadisp = dplyr::coalesce(exp.design.1.betadisp, exp.design.2.betadisp, exp.design.3.betadisp),
    ## Respective sample size
    betadisp.sample.size = dplyr::case_when(
      exp.design.1.n >= min_reps ~ exp.design.1.n,
      exp.design.2.n >= min_reps ~ exp.design.2.n,
      exp.design.3.n >= min_reps ~ exp.design.3.n),
    ## Original design level corresponding to that beta dispersion value
    betadisp.design.level = dplyr::case_when(
      exp.design.1.n >= min_reps ~ "exp.design.1",
      exp.design.2.n >= min_reps ~ "exp.design.2",
      exp.design.3.n >= min_reps ~ "exp.design.3")
  ) %>% 
  # Drop the now-superseded beta dispersion columns / experimental design level
  dplyr::select(-dplyr::ends_with(c(".n", ".betadisp"))) %>% 
  # Drop non-unique rows
  dplyr::distinct()

# Check for gained/lost columns
supportR::diff_check(old = names(beta_v3), new = names(beta_v4))

# Re-check structure
dplyr::glimpse(beta_v4)

## ------------------------------------------- ##
# Check for Missing Beta Dispersions ----
## ------------------------------------------- ##

# Any datasets without beta dispersion?
beta_v4 %>% 
  dplyr::filter(is.na(betadisp)) %>% 
  dplyr::select(source, exp.design.3, exp.design.2, exp.design.1) %>% 
  dplyr::distinct()

# Remove missing beta dispersion values
beta_v5 <- beta_v4 %>% 
  dplyr::filter(is.na(betadisp) != T)

# Double check that worked (below pipe should return 0 rows)
beta_v5 %>% 
  dplyr::filter(is.na(betadisp)) %>% 
  dplyr::select(source, exp.design.3, exp.design.2, exp.design.1) %>% 
  dplyr::distinct()

# How many rows are lost?
message(nrow(beta_v4) - nrow(beta_v5), " rows lost")

# Any entire datasets lost?
supportR::diff_check(old = unique(beta_v4$source), new = unique(beta_v5$source))

# General structure check
dplyr::glimpse(beta_v5)

## ------------------------------------------- ##
# Consolidate 'Design' Columns ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(beta_v5)

# Want to simplify design columns to only relevant one(s) for each dataset
beta_v6 <- beta_v5 %>% 
  dplyr::mutate(exp.design = dplyr::case_when(
    ## Design level 3 is the finest level available
    betadisp.design.level == "exp.design.3" ~ exp.design.3,
    ## Design level 2 is the finest level available
    betadisp.design.level == "exp.design.2" & 
      exp.design.2 == exp.design.3 ~ exp.design.2,
    betadisp.design.level == "exp.design.2" & 
      exp.design.2 != exp.design.3 ~ paste(exp.design.3, exp.design.2, sep = "___"),
    ## Design level 1 is the finest level available
    betadisp.design.level == "exp.design.1" & 
      exp.design.1 != exp.design.2 & exp.design.2 != exp.design.3 ~ paste(exp.design.3, exp.design.2, exp.design.1, sep = "___"),
    betadisp.design.level == "exp.design.1" & 
      exp.design.1 != exp.design.2 & exp.design.2 == exp.design.3 ~ paste(exp.design.2, exp.design.1, sep = "___"),
    exp.design.1 == exp.design.2 & exp.design.2 != exp.design.3 ~ paste(exp.design.3, exp.design.1, sep = "___"),
    exp.design.1 == exp.design.2 & exp.design.2 == exp.design.3 ~ paste(exp.design.1, sep = "___"),
    T ~ NA), .after = exp.name) %>% 
  # Drop now-superseded experimental design columns
  dplyr::select(-dplyr::starts_with(c("exp.design.1", "exp.design.2", "exp.design.3"))) %>% 
  # Drop non-unique rows
  dplyr::distinct()

# Make sure all datasets have a value in the new "exp.design" column
beta_v6 %>% 
  dplyr::filter(is.na(exp.design) == T) %>% 
  dplyr::select(source, betadisp.design.level) %>% 
  dplyr::distinct()

# Check for gained/lost columns
supportR::diff_check(old = names(beta_v5), new = names(beta_v6))

# Re-check structure
dplyr::glimpse(beta_v6)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
beta_v99 <- beta_v6

# Identify tidy file name / path
beta_name <- "04_caged_beta-disp.csv"
beta_path <- file.path("data", beta_name)

# Export locally
write.csv(x = beta_v99, row.names = F, na = '', file = beta_path)

# Upload to Drive
googledrive::drive_upload(media = beta_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

## ------------------------------------------- ##
# Exploratory Graphs ----
## ------------------------------------------- ##

# Clear environment & collect garbage
rm(list = ls()); gc()

# Read beta dispersion data back in
beta_v99 <- read.csv(file.path("data", "04_caged_beta-disp.csv"))

# Check structure
dplyr::glimpse(beta_v99)

# Do some pre-visualization wrangling
beta_viz <- beta_v99 %>% 
  dplyr::mutate(
    betadisp.n.bin = dplyr::case_when(
      betadisp.sample.size == 1 ~ "N = 1",
      betadisp.sample.size > 1 & betadisp.sample.size <= 5 ~ "N = 2-5",
      betadisp.sample.size > 5 & betadisp.sample.size <= 15 ~ "N = 6-15",
      betadisp.sample.size > 15 & betadisp.sample.size <= 30 ~ "N = 16-30",
      betadisp.sample.size > 30 ~ "N > 30",
      T ~ NA)) %>% 
  dplyr::mutate(betadisp.n.bin = factor(x = betadisp.n.bin, 
                                        levels = c("N = 1", "N = 2-5", 
                                                   "N = 6-15", "N = 16-30", 
                                                   "N > 30")))
# Re-check structure
dplyr::glimpse(beta_viz)

# Exploratory graph
ggplot(beta_viz, aes(x = cage.treatment, y = betadisp)) +
  geom_jitter(aes(fill = cage.treatment), width = 0.15,
              alpha = 0.3, size = 1, pch = 21) +
  facet_wrap(. ~ source) +
  labs(x = "Cage Treatment", y = "Beta Dispersion") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "04_betadisp-violins.png"),
       width = 12, height = 12, units = "in")

# End ----
