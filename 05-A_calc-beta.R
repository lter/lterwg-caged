## --------------------------------------------------------------- ##
# CAGED Beta Dispersion Calculation
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, magrittr, vegan, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

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

# What distance method do we want to use?
pref_dist_method <- "bray"

# Read in data
beta_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(beta_v1)

## ------------------------------------------- ##
# Data Preparation ----
## ------------------------------------------- ##

# Bray-Curtis doesn't work for 2 or more replicates with a total abundance of zero
## For datasets with that problem then, we need to pick one (at random) and drop the other
zero_abun_v1 <- beta_v1 %>% 
  # Calculate total abundance within design 1
  dplyr::group_by(source, exp.name, exp.design.4, exp.design.3,
                  exp.design.2, exp.design.1, cage.treatment_std) %>% 
  dplyr::summarize(tot_abundance = sum(abundance, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Filter to only rows with a total abundance of zero
  dplyr::filter(tot_abundance == 0) %>% 
  # Identify datasets with more than one of these zero abundance replicates
  dplyr::group_by(source, exp.name, exp.design.4, exp.design.3,
                  exp.design.2, cage.treatment_std) %>% 
  dplyr::mutate(rep_ct = seq_along(along.with = unique(exp.design.1))) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(zero_abun_v1)

# List for storing outputs
zero_list <- list()

# Loop across design level 2s with this problem
for(zero_des2 in sort(unique(zero_abun_v1$exp.design.2))){
  # zero_des2 <- "gex_augustine-kenya-mpala_mpala_1999&2002_africanungulates_plants.csv__bushland2"
  
  # Subset to just the relevant group
  zero_sub <- dplyr::filter(zero_abun_v1, exp.design.2 == zero_des2)
  
  # Loop across treatments too
  for(zero_trt in sort(unique(zero_sub$cage.treatment_std))){
    # zero_trt <- "caged" 
    
    # Subset again
    zero_subtrt <- dplyr::filter(zero_sub, cage.treatment_std == zero_trt)
    
    # If there's only one zero abundance rep, skip it
    if(nrow(zero_subtrt) == 1){
      message("For '", zero_des2, "' only ", max(zero_subtrt$rep_ct), " zero-abundance replicate identified in the '", zero_trt, "' treatment.")
      print("Retaining that single replicate")
      
      # If there's more than one...
    } else {
      
      # Identify a random replicate
      rand_rep <- sample(x = zero_subtrt$rep_ct, size = 1, replace = F)
      
      # Subset to all replicates except that replicate and add to the list
      zero_list[[paste0(zero_des2, "__", zero_trt)]] <- zero_subtrt %>% 
        dplyr::filter(rep_ct != rand_rep)
      
      # Conclusion message
      message("For '", zero_des2, "' ", max(zero_subtrt$rep_ct), " zero-abundance replicates identified in the '", zero_trt, "' treatment. Choosing replicate number: ")
      print(rand_rep)
    } # Close conditional
  } # Close treatment loop
} # Close design 2 loop

# Unlist & wrangle the list that we just created
zero_abun_v2 <- purrr::list_rbind(x = zero_list) %>% 
  dplyr::select(-tot_abundance, -rep_ct)

# Re-check structure
dplyr::glimpse(zero_abun_v2)

# Anti join the unwanted data to the original data to remove the 'extra' zero abundance replicates
beta_v2 <- beta_v1 %>% 
  anti_join(y = zero_abun_v2, by = c("source", "exp.name", "exp.design.4", 
                                     "exp.design.3", "exp.design.2",
                                     "exp.design.1", "cage.treatment_std"))
# Re-check structure
dplyr::glimpse(beta_v2)

# Check how many design 1-by-treatment reps were dropped via this operation
message(length(unique(paste0(beta_v1$exp.design.1, beta_v1$cage.treatment_std))) - 
          length(unique(paste0(beta_v2$exp.design.1, beta_v2$cage.treatment_std))),
        " zero abundance replicates were dropped")

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
for(focal_src in setdiff(x = sort(unique(beta_v2$source)),
                         # Manually (temporarily) removing datasets as/if needed
                         y = c(""))){
  # focal_src <- "gilson_southafrica_intertidalexclusion_2021_grazers_inverts.csv"
  
  # Progress message
  message("Processing source '", focal_src, "'")
  
  # Subset data
  src_sub <- beta_v2 %>% 
    dplyr::filter(source == focal_src)
  
  # Loop across treatments
  for(focal_trt in unique(src_sub$cage.treatment_orig)){
    # focal_trt <- "F"
    
    # Subset again
    trt_sub <- src_sub %>% 
      dplyr::filter(cage.treatment_orig == focal_trt)
    
    # Loop across study years
    for(focal_yr in unique(trt_sub$year)){
      # focal_yr <- "2021"
      
      # Subset again
      yr_sub <- trt_sub %>% 
        dplyr::filter(year == focal_yr)
      
      ## ------------------------ ##
      # Beta Disp for Design 1 ----
      ## ------------------------ ##
      
      # Loop across most granular level of experimental design
      for(focal_des1 in unique(yr_sub$exp.design.1)){
        # focal_des1 <- "CSF__1"
        
        # Subset yet again
        des1_sub <- yr_sub %>% 
          dplyr::filter(exp.design.1 == focal_des1)
        
        # Calculate beta dispersion
        des1_beta <- calc_betadisp(df = des1_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = pref_dist_method, result_prefix = "exp.design.1") %>% 
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
        # focal_des2 <- "CSF"
        
        # Subset yet again
        des2_sub <- yr_sub %>% 
          dplyr::filter(exp.design.2 == focal_des2)
        
        # Calculate beta dispersion
        des2_beta <- calc_betadisp(df = des2_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = pref_dist_method, result_prefix = "exp.design.2") %>% 
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
        # focal_des3 <- "CSF"
        
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
                                   dist_method = pref_dist_method, result_prefix = "exp.design.3") %>% 
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
        # focal_des4 <- "CSF"
        
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
                                   dist_method = pref_dist_method, result_prefix = "exp.design.4") %>% 
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
                                   dist_method = pref_dist_method, result_prefix = "exp.name") %>% 
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

# Unlist them to create an 'all scales' table
beta_allscales <- purrr::list_rbind(x = beta_deslists)

# Check structure
dplyr::glimpse(beta_allscales)

## ------------------------------------------- ##
# Identify 'Finest Scale' of Beta Dispersion ----
## ------------------------------------------- ##

# Ultimately we want only the best dispersion available in a given dataset
## Regardless of which design level that is for that experiment

# Make a list for storing outputs
beta_finelist <- list()

# Pare down the 'all scales' output slightly (to only instances with beta dispersion
beta_fine_v1 <- beta_allscales %>% 
  dplyr::filter(is.na(betadisp.comm.dist) != T)

# To do this, we'll loop across sources and experiments
for(finest_src in sort(unique(beta_allscales$source))){
  
  # Subset to that source
  beta_fine_src <- beta_fine_v1 %>% 
    dplyr::filter(source == finest_src)
  
  for(finest_name in sort(unique(beta_fine_src$exp.name))){
    # finest_name <- "Palmas_Exposed-Cool"
    
    # Progress message
    message("Identifying finest scale for '", finest_name, "'")
    
    # Subset the beta dispersion table to only this source
    beta_fine_sub <- beta_fine_src %>% 
      dplyr::filter(exp.name == finest_name)
    
    # Make another subset for each design level
    beta_fine_sub_des1 <- dplyr::filter(beta_fine_sub, betadisp.design.level == "exp.design.1")
    beta_fine_sub_des2 <- dplyr::filter(beta_fine_sub, betadisp.design.level == "exp.design.2")
    beta_fine_sub_des3 <- dplyr::filter(beta_fine_sub, betadisp.design.level == "exp.design.3")
    beta_fine_sub_des4 <- dplyr::filter(beta_fine_sub, betadisp.design.level == "exp.design.4")
    beta_fine_sub_name <- dplyr::filter(beta_fine_sub, betadisp.design.level == "exp.name")
    
    # Work through the design levels sequentially (lowest to highest)
    ## And add the lowest one with beta dispersion for both standardized cage treatments to the output list
    if(all(c("caged", "uncaged") %in% unique(beta_fine_sub_des1$cage.treatment_std))){
      
      # Add to list
      beta_finelist[[finest_name]] <- beta_fine_sub_des1
      
      # Print a message too
      message("For '", finest_name, "' exp.design.1 was the finest level with beta dispersion for both treatments") }
    
    # Do the same for design 2
    if(all(c("caged", "uncaged") %in% unique(beta_fine_sub_des2$cage.treatment_std))){
      beta_finelist[[finest_name]] <- beta_fine_sub_des2
      message("For '", finest_name, "' exp.design.2 was the finest level with beta dispersion for both treatments") }
    
    # And design 3
    else if(all(c("caged", "uncaged") %in% unique(beta_fine_sub_des3$cage.treatment_std))){
      beta_finelist[[finest_name]] <- beta_fine_sub_des3
      message("For '", finest_name, "' exp.design.3 was the finest level with beta dispersion for both treatments") }
    
    # And design 4
    else if(all(c("caged", "uncaged") %in% unique(beta_fine_sub_des4$cage.treatment_std))){
      beta_finelist[[finest_name]] <- beta_fine_sub_des4
      message("For '", finest_name, "' exp.design.4 was the finest level with beta dispersion for both treatments") }
    
    # And the experiment name
    else if(all(c("caged", "uncaged") %in% unique(beta_fine_sub_name$cage.treatment_std))){
      beta_finelist[[finest_name]] <- beta_fine_sub_name
      message("For '", finest_name, "' exp.name was the finest level with beta dispersion for both treatments") }
    
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist the list that we just created
beta_fine_v2 <- purrr::list_rbind(beta_finelist)

# Check structure
dplyr::glimpse(beta_fine_v2)

# Did we lose any sources?
supportR::diff_check(old = unique(beta_allscales$source), new = unique(beta_fine_v2$source))

# Did we lose any experiments?
## From sources that were not dropped
beta_test <- dplyr::filter(beta_allscales, source %in% beta_fine_v2$source)
supportR::diff_check(old = unique(beta_test$exp.name), new = unique(beta_fine_v2$exp.name))

## ------------------------------------------- ##
# Diagnose Lost Sources ----
## ------------------------------------------- ##

# Identify lost sources
dropped_sources <- setdiff(x = unique(beta_allscales$source), y = unique(beta_fine_v2$source))

# Create a nice diagnostic output for sources that we do lose in this process
for(lost_src in dropped_sources){
  
  # Subset the 'all scales' output to just this source
  beta_lost <- dplyr::filter(beta_allscales, source == lost_src)
  
  # Generate a file name
  lost_file <- paste0("beta-disp-calc-failure-diagnostic_", lost_src)
  
  # Export locally
  write.csv(x = beta_lost, na = '', row.names = F,
            file = file.path("data", "diagnostic", lost_file))
  
} # Close loop

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
beta_v99 <- beta_fine_v2

# How many sources and exp.name got through the pipeline?
unique(beta_v99$source) # 110
unique(beta_v99$exp.name) # 297

# Identify tidy file name / path
beta_name <- "05-A_caged_beta-disp"
beta_path <- file.path("data", paste0(beta_name, "_finest-scales.csv"))

# Export locally
write.csv(x = beta_v99, row.names = F, na = '', file = beta_path)

# Re-check 'all scales' structure
dplyr::glimpse(beta_allscales)

# Export locally
write.csv(x = beta_allscales, na = '', row.names = F,
          file = file.path("data", paste0(beta_name, "_all-scales.csv")))

# End ----
