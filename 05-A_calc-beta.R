## --------------------------------------------------------------- ##
# CAGED Beta Dispersion Calculation
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, magrittr, vegan, supportR)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Load needed tool(s)
purrr::walk(.x = dir(path = "tools", pattern = "fxn_"),
            .f = ~ source(file.path("tools", .x)) )

# Define the minimum number of replicates for which we want to calculate beta dispersion
## Inclusive of this number (so 5 becomes >= 5)
min_reps <- 4

# Define maximum number of allowed pseudoreps _without_ averaging across them
## Necessary for preserving pseudoreps when there are few and averaging across them when there are many
## Inclusive of this number (so 5 is >= 5)
max_pseudoreps <- 5

# Read in data
beta_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(beta_v1)

## ------------------------------------------- ##
# Data Preparation ----
## ------------------------------------------- ##

# Perform any needed pre-calculation wrangling
beta_v2 <- beta_v1
# NO SUCH WRANGLING REQUIRED (CURRENTLY)

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
  # focal_src <- "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv"
  
  # Progress message
  message("Processing source '", focal_src, "'")
  
  # Subset data
  src_sub <- beta_v2 %>% 
    dplyr::filter(source == focal_src)
  
  # Loop across treatments
  for(focal_trt in unique(src_sub$cage.treatment_orig)){
    # focal_trt <- "Exclusion"
    
    # Subset again
    trt_sub <- src_sub %>% 
      dplyr::filter(cage.treatment_orig == focal_trt)
    
    # Loop across study years
    for(focal_yr in unique(trt_sub$year)){
      # focal_yr <- "2011"
      
      # Subset again
      yr_sub <- trt_sub %>% 
        dplyr::filter(year == focal_yr)
      
      ## ------------------------ ##
      # Beta Disp for Design 1 ----
      ## ------------------------ ##
      
      # Loop across most granular level of experimental design
      for(focal_des1 in unique(yr_sub$exp.design.1)){
        # focal_des1 <- "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv__C__1"
        
        # Subset yet again
        des1_sub <- yr_sub %>% 
          dplyr::filter(exp.design.1 == focal_des1)
        
        # Calculate beta dispersion
        des1_beta <- calc_betadisp(df = des1_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = "bray", result_prefix = "exp.design.1") %>% 
          # Wrangle that output slightly
          tidy_betadisp(beta = ., result_prefix = "exp.design.1")
        
        # Add to list
        beta_des1_list[[paste0(focal_src, focal_trt, focal_des1)]] <- des1_beta
        
      } # Close "exp.design.1" loop
      
      ## ------------------------ ##
      # Beta Disp for Design 2 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 2
      for(focal_des2 in unique(yr_sub$exp.design.2)){
        # focal_des2 <- "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv__C"
        
        # Subset yet again
        des2_sub <- yr_sub %>% 
          dplyr::filter(exp.design.2 == focal_des2)
        
        # Calculate beta dispersion
        des2_beta <- calc_betadisp(df = des2_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = "bray", result_prefix = "exp.design.2") %>% 
          # Wrangle that output slightly
          tidy_betadisp(beta = ., result_prefix = "exp.design.2")
        
        # Add to list
        beta_des2_list[[paste0(focal_src, focal_trt, focal_des2)]] <- des2_beta
        
      } # Close "exp.design.2" loop
      
      ## ------------------------ ##
      # Beta Disp for Design 3 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 3
      for(focal_des3 in unique(yr_sub$exp.design.3)){
        # focal_des3 <- "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv"
        
        # Subset yet again
        des3_sub <- yr_sub %>% 
          dplyr::filter(exp.design.3 == focal_des3)
        
        # Identify the number of psuedoreplicates at lower design levels
        des3_pseudorep_ct <- des3_sub %>% 
          dplyr::group_by(exp.design.2) %>% 
          dplyr::summarize(des1.ct = length(unique(exp.design.1)),
                           .groups = "keep") %>% 
          dplyr::ungroup()
        
        # If there are X pseudoreplicates...
        if(any(des3_pseudorep_ct$des1.ct >= max_pseudoreps) & 
           length(unique(des3_sub$exp.design.2)) > 1){
          
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
                                   dist_method = "bray", result_prefix = "exp.design.3") %>% 
          # Wrangle that output slightly
          tidy_betadisp(beta = ., result_prefix = "exp.design.3")
        
        # Add to list
        beta_des3_list[[paste0(focal_src, focal_trt, focal_des3)]] <- des3_beta
        
      } # Close "exp.design.3" loop
      
      ## ------------------------ ##
      # Beta Disp for Design 4 ----
      ## ------------------------ ##
      
      # Loop across experimental design level 4
      for(focal_des4 in unique(yr_sub$exp.design.4)){
        # focal_des4 <- "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv"
        
        # Subset yet again
        des4_sub <- yr_sub %>% 
          dplyr::filter(exp.design.4 == focal_des4)
        
        # Identify the number of psuedoreplicates at lower design levels
        des4_pseudorep_ct <- des4_sub %>% 
          dplyr::group_by(exp.design.3, exp.design.2) %>% 
          dplyr::summarize(des1.ct = length(unique(exp.design.1)),
                           .groups = "keep")  %>% 
          dplyr::ungroup()
        
        # If there are X pseudoreplicates...
        if(any(des4_pseudorep_ct$des1.ct >= max_pseudoreps) & 
           length(unique(des4_sub$exp.design.2)) > 1){
          
          # Average across experimental design 1 (within exp. design 2 levels)
          des4_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # Re-identify the number of psuedoreplicates at design level 2
        des4_pseudorep_ct <- des4_sub %>% 
          dplyr::group_by(exp.design.3) %>% 
          dplyr::summarize(des2.ct = length(unique(exp.design.2)),
                           .groups = "keep")  %>% 
          dplyr::ungroup()
        
        # If there are X pseudoreplicates...
        if(any(des4_pseudorep_ct$des2.ct >= max_pseudoreps) & 
           length(unique(des4_sub$exp.design.3)) > 1){
          
          # Average across experimental design 2 (within exp. design 3 levels)
          des4_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.2", "exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        des4_beta <- calc_betadisp(df = des4_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = "bray", result_prefix = "exp.design.4") %>% 
          # Wrangle that output slightly
          tidy_betadisp(beta = ., result_prefix = "exp.design.4")
        
        # Add to list
        beta_des4_list[[paste0(focal_src, focal_trt, focal_des4)]] <- des4_beta
        
      } # Close "exp.design.4" loop
      
      ## ------------------------ ##
      # Beta Disp for Exp.Name ----
      ## ------------------------ ##
      
      # Loop across experimental design level 4
      for(focal_name in unique(yr_sub$exp.name)){
        # focal_name <- "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv"
        
        # Subset yet again
        name_sub <- yr_sub %>% 
          dplyr::filter(exp.name == focal_name)
        
        # Identify the number of psuedoreplicates at lower design levels
        name_pseudorep_ct <- name_sub %>% 
          dplyr::group_by(exp.design.4, exp.design.3, exp.design.2) %>% 
          dplyr::summarize(des1.ct = length(unique(exp.design.1)),
                           .groups = "keep")  %>% 
          dplyr::ungroup()
        
        # If there are X pseudoreplicates...
        if(any(name_pseudorep_ct$des1.ct >= max_pseudoreps) & 
           length(unique(name_sub$exp.design.2)) > 1){
          # Average across experimental design 1 (within exp. design 2 levels)
          name_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # Re-identify the number of psuedoreplicates at design level 2
        name_pseudorep_ct <- name_sub %>% 
          dplyr::group_by(exp.design.4, exp.design.3) %>% 
          dplyr::summarize(des2.ct = length(unique(exp.design.2)),
                           .groups = "keep")  %>% 
          dplyr::ungroup()
        
        # If there are X pseudoreplicates...
        if(any(name_pseudorep_ct$des2.ct >= max_pseudoreps) & 
           length(unique(name_sub$exp.design.3)) > 1){
          # Average across experimental design 2 (within exp. design 3 levels)
          name_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.2", "exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # Re-re-identify the number of psuedoreplicates at design level 3
        name_pseudorep_ct <- name_sub %>% 
          dplyr::group_by(exp.design.4) %>% 
          dplyr::summarize(des3.ct = length(unique(exp.design.3)),
                           .groups = "keep")  %>% 
          dplyr::ungroup()
        
        # If there are X pseudoreplicates...
        if(any(name_pseudorep_ct$des3.ct >= max_pseudoreps) & 
           length(unique(name_sub$exp.design.4)) > 1){
          
          # Average across experimental design 3 (within exp. design 4 levels)
          name_sub %<>% 
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(setdiff(x = names(trt_sub), 
                                      y = c("exp.design.3", "exp.design.2", 
                                            "exp.design.1", "abundance"))))) %>% 
            dplyr::summarize(abundance = mean(abundance, na.rm = T),
                             .groups = "keep") %>% 
            dplyr::ungroup()
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        name_beta <- calc_betadisp(df = name_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = "bray", result_prefix = "exp.name") %>% 
          # Wrangle that output slightly
          tidy_betadisp(beta = ., result_prefix = "exp.name")
        
        # Add to list
        beta_name_list[[paste0(focal_src, focal_trt, focal_name)]] <- name_beta
        
      } # Close "exp.name" loop
    } # Close year loop
  } # Close treatment loop
} # Close source loop

## ------------------------------------------- ##
# Process Calculation Output Lists ----
## ------------------------------------------- ##

# Do some minor processing on the output lists and unlist them
## Design 1 Betas
beta_des1 <- purrr::list_rbind(x = beta_des1_list)
## Design 2 Betas
beta_des2 <- purrr::list_rbind(x = beta_des2_list)
## Design 3 Betas
beta_des3 <- purrr::list_rbind(x = beta_des3_list)
## Design 4 Betas
beta_des4 <- purrr::list_rbind(x = beta_des4_list)
## Exp Name Betas
beta_expname <- purrr::list_rbind(x = beta_name_list)

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

# How many sources and exp.name got through the pipeline?
unique(beta_v99$source)
unique(beta_v99$exp.name)

# Identify tidy file name / path
beta_name <- "05-A_caged_beta-disp"
beta_path <- file.path("data", paste0(beta_name, "_finest-scales.csv"))

# Export locally
write.csv(x = beta_v99, row.names = F, na = '', file = beta_path)

# And, generate an 'all scales' output too
beta_allscales <- purrr::list_rbind(x = beta_deslists)

# Check structure
dplyr::glimpse(beta_allscales)

# Export locally
write.csv(x = beta_allscales, na = '', row.names = F,
          file = file.path("data", paste0(beta_name, "_all-scales.csv")))

# # Upload all of these to the Drive
# purrr::walk(.x = dir(path = file.path("data"), pattern = "05-A_caged_beta-disp"),
#             .f = ~ googledrive::drive_upload(media = file.path("data", .x), overwrite = T,
#                                             path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")))

# End ----
