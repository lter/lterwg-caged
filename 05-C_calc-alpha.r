## --------------------------------------------------------------- ##
# CAGED Alpha Diversity Calculation
## --------------------------------------------------------------- ##
# Purpose:
## Calculate alpha diversity (i.e., richness) within experiment and within caging treatment

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
alpha_v1 <- read.csv(file.path("data", "04_caged_zero-filled.csv"))

# Check structure
dplyr::glimpse(alpha_v1)

# Check what data made it through 04
unique(alpha_v1$source) # 121
unique(alpha_v1$exp.name) # 363
# same as 03, nothing was dropped, just zero filled so it makes sense

## ------------------------------------------- ##
# Data Preparation ----
## ------------------------------------------- ##

# Bray-Curtis doesn't work for 2 or more replicates with a total abundance of zero
## For datasets with that problem then, we need to pick one (at random) and drop the other
zero_abun <- alpha_v1 %>% 
  # Calculate total abundance within design 1
  dplyr::group_by(source, exp.name, exp.design.4, exp.design.3,
                  exp.design.2, exp.design.1, cage.treatment_std) %>% 
  dplyr::summarize(tot_abundance = sum(abundance, na.rm = T),
                    .groups = "drop") %>% 
# Filter to only rows with a total abundance of zero
  dplyr::filter(tot_abundance == 0) %>% 
  # Identify datasets with more than one of these zero abundance replicates
  dplyr::group_by(source, exp.name, exp.design.4, exp.design.3,
                  exp.design.2, cage.treatment_std) %>% 
  dplyr::mutate(rep_ct = seq_along(along.with = unique(exp.design.1))) %>% 
  dplyr::ungroup() %>%
  # Filter to only instances with more than one replicate with a total abundance of 0
  dplyr::filter(rep_ct > 1)

# Check structure
dplyr::glimpse(zero_abun)

# Create a dataframe of fake abundances for those replicates
fake_abun <- alpha_v1 %>%
  # Filter to only reps flagged in above pipe
  dplyr::filter(source %in% c(zero_abun$source) & exp.name %in% c(zero_abun$exp.name)) %>%
  # Overwrite the taxa and abundance columns with hard-coded values
  dplyr::mutate(taxa = "FAKE.SPECIES_added.to.solve.BC.algebra.problem",
                abundance = 0.01) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(fake_abun)

# Attach the fake abundances to the real data
## This means experiments that would have had two (or more) zero-abundance reps now have one non-zero abundance
## (i.e., the fake species/abundance that we just added!)
alpha_v2 <- dplyr::bind_rows(alpha_v1, fake_abun)

# Re-check structure
dplyr::glimpse(alpha_v2)

# How many fake abundances were inserted?
message(nrow(fake_abun), " fake abundances added (", 
        round(nrow(fake_abun) / nrow(alpha_v2) * 100, digits = 2), 
        "% of the total)")

## ------------------------------------------- ##
# Calculate Beta Dispersion ----
## ------------------------------------------- ##

# Create a list for storing outputs
alpha_des1_list <- list()
alpha_des2_list <- list()
alpha_des3_list <- list()
alpha_des4_list <- list()
alpha_name_list <- list()

# Loop across original data source
for(focal_src in setdiff(x = sort(unique(alpha_v2$source)),
                         # Manually (temporarily) removing datasets as/if needed
                         y = c(""))){
  # focal_src <- "gilson_southafrica_intertidalexclusion_2021_grazers_inverts.csv"
  
  # Progress message
  message("Processing source '", focal_src, "'")
  
  # Subset data
  src_sub <- alpha_v2 %>% 
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
        alpha_des1_list[[paste0(focal_src, focal_trt, focal_des1)]] <- des1_beta
        
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
        alpha_des2_list[[paste0(focal_src, focal_trt, focal_des2)]] <- des2_beta
        
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
                           .groups = "drop")
        
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
                             .groups = "drop")
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        des3_beta <- calc_betadisp(df = des3_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = pref_dist_method, result_prefix = "exp.design.3") %>% 
          # Wrangle that output slightly
          tidy_betadisp(beta = ., result_prefix = "exp.design.3")
        
        # Add to list
        alpha_des3_list[[paste0(focal_src, focal_trt, focal_des3)]] <- des3_beta
        
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
                          .groups = "drop")
        
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
                            .groups = "drop")
          
        } # Close conditional aggregation
        
        # Re-identify the number of psuedoreplicates at design level 2
        des4_pseudorep_ct <- des4_sub %>% 
          dplyr::group_by(exp.design.3) %>% 
          dplyr::summarize(des2.ct = length(unique(exp.design.2)),
                          .groups = "drop")
        
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
                            .groups = "drop")
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        des4_beta <- calc_betadisp(df = des4_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = pref_dist_method, result_prefix = "exp.design.4") %>% 
          # Wrangle that output slightly
          tidy_betadisp(beta = ., result_prefix = "exp.design.4")
        
        # Add to list
        alpha_des4_list[[paste0(focal_src, focal_trt, focal_des4)]] <- des4_beta
        
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
                          .groups = "drop")
        
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
                            .groups = "drop")
          
        } # Close conditional aggregation
        
        # Re-identify the number of psuedoreplicates at design level 2
        name_pseudorep_ct <- name_sub %>% 
          dplyr::group_by(exp.design.4, exp.design.3) %>% 
          dplyr::summarize(des2.ct = length(unique(exp.design.2)),
                          .groups = "drop")
        
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
                            .groups = "drop")
          
        } # Close conditional aggregation
        
        # Re-re-identify the number of psuedoreplicates at design level 3
        name_pseudorep_ct <- name_sub %>% 
          dplyr::group_by(exp.design.4) %>% 
          dplyr::summarize(des3.ct = length(unique(exp.design.3)),
                          .groups = "drop")
        
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
                              .groups = "drop")
          
        } # Close conditional aggregation
        
        # Calculate beta dispersion
        name_beta <- calc_betadisp(df = name_sub, floor = min_reps,
                                   taxa_col = "taxa", abun_col = "abundance",
                                   dist_method = pref_dist_method, result_prefix = "exp.name") %>% 
          # Wrangle that output slightly
          tidy_betadisp(beta = ., result_prefix = "exp.name")
        
        # Add to list
        alpha_name_list[[paste0(focal_src, focal_trt, focal_name)]] <- name_beta
        
      } # Close "exp.name" loop
    } # Close year loop
  } # Close treatment loop
} # Close source loop

## ------------------------------------------- ##
# Process Calculation Output Lists ----
## ------------------------------------------- ##

# Do some minor processing on the output lists and unlist them
## Design 1 Betas
alpha_des1 <- purrr::list_rbind(x = alpha_des1_list)
## Design 2 Betas
alpha_des2 <- purrr::list_rbind(x = alpha_des2_list)
## Design 3 Betas
alpha_des3 <- purrr::list_rbind(x = alpha_des3_list)
## Design 4 Betas
alpha_des4 <- purrr::list_rbind(x = alpha_des4_list)
## Exp Name Betas
alpha_expname <- purrr::list_rbind(x = alpha_name_list)

# Check the structure of one
dplyr::glimpse(alpha_des1)

# Add these to a list (useful later)
alpha_deslists <- list(alpha_des1, alpha_des2, alpha_des3, alpha_des4, alpha_expname)

# Unlist them to create an 'all scales' table
alpha_allscales <- purrr::list_rbind(x = alpha_deslists)

# Check structure
dplyr::glimpse(alpha_allscales)

## ------------------------------------------- ##
# Identify 'Finest Scale' of Beta Dispersion ----
## ------------------------------------------- ##

# Ultimately we want only the best dispersion available in a given dataset
## Regardless of which design level that is for that experiment

# Make a list for storing outputs
alpha_finelist <- list()

# Pare down the 'all scales' output slightly (to only instances with beta dispersion
alpha_fine_v1 <- alpha_allscales %>% 
  dplyr::filter(is.na(betadisp.comm.dist) != T)

# To do this, we'll loop across sources and experiments
for(finest_src in sort(unique(alpha_allscales$source))){
  
  # Subset to that source
  alpha_fine_src <- alpha_fine_v1 %>% 
    dplyr::filter(source == finest_src)
  
  for(finest_name in sort(unique(alpha_fine_src$exp.name))){
    # finest_name <- "Palmas_Exposed-Cool"
    
    # Progress message
    message("Identifying finest scale for '", finest_name, "'")
    
    # Subset the beta dispersion table to only this source
    alpha_fine_sub <- alpha_fine_src %>% 
      dplyr::filter(exp.name == finest_name)
    
    # Make another subset for each design level
    alpha_fine_sub_des1 <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.design.1")
    alpha_fine_sub_des2 <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.design.2")
    alpha_fine_sub_des3 <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.design.3")
    alpha_fine_sub_des4 <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.design.4")
    alpha_fine_sub_name <- dplyr::filter(alpha_fine_sub, betadisp.design.level == "exp.name")
    
    # Work through the design levels sequentially (lowest to highest)
    ## And add the lowest one with beta dispersion for both standardized cage treatments to the output list
    if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_des1$cage.treatment_std))){
      
      # Add to list
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_des1
      
      # Print a message too
      message("For '", finest_name, "' exp.design.1 was the finest level with beta dispersion for both treatments") }
    
    # Do the same for design 2
    else if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_des2$cage.treatment_std))){
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_des2
      message("For '", finest_name, "' exp.design.2 was the finest level with beta dispersion for both treatments") }
    
    # And design 3
    else if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_des3$cage.treatment_std))){
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_des3
      message("For '", finest_name, "' exp.design.3 was the finest level with beta dispersion for both treatments") }
    
    # And design 4
    else if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_des4$cage.treatment_std))){
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_des4
      message("For '", finest_name, "' exp.design.4 was the finest level with beta dispersion for both treatments") }
    
    # And the experiment name
    else if(all(c("caged", "uncaged") %in% unique(alpha_fine_sub_name$cage.treatment_std))){
      alpha_finelist[[paste0(finest_src, finest_name)]] <- alpha_fine_sub_name
      message("For '", finest_name, "' exp.name was the finest level with beta dispersion for both treatments") }
    
  } # Close 'exp.name' loop
} # Close 'source' loop

# Unlist the list that we just created
alpha_fine_v2 <- purrr::list_rbind(alpha_finelist)

# Check structure
dplyr::glimpse(alpha_fine_v2)

# Did we lose any sources?
# these are datasets that don't have both caging treatments for at least one design level
supportR::diff_check(old = unique(alpha_allscales$source), new = unique(alpha_fine_v2$source))
#now alderson is getting dropped here because it only has n=3 now that we split it up

# Did we lose any experiments?
## From sources that were not dropped
alpha_test <- dplyr::filter(alpha_allscales, source %in% alpha_fine_v2$source)
supportR::diff_check(old = unique(alpha_test$exp.name), new = unique(alpha_fine_v2$exp.name))

## ------------------------------------------- ##
# Diagnose Lost Sources ----
## ------------------------------------------- ##

# Identify lost sources
dropped_sources <- setdiff(x = unique(alpha_allscales$source), y = unique(alpha_fine_v2$source))

# Create a nice diagnostic output for sources that we do lose in this process
for(lost_src in dropped_sources){
  
  # Subset the 'all scales' output to just this source
  alpha_lost <- dplyr::filter(alpha_allscales, source == lost_src)
  
  # Generate a file name
  lost_file <- paste0("source_betadisp-failure_", lost_src)
  
  # Export locally
  write.csv(x = alpha_lost, na = '', row.names = F,
            file = file.path("data", "diagnostic", lost_file))
  
}

## ------------------------------------------- ##
# Diagnose Lost Experiment Names ----
## ------------------------------------------- ##

# Identify lost experiments
dropped_names <- setdiff(x = unique(alpha_test$exp.name), y = unique(alpha_fine_v2$exp.name))

# Create a nice diagnostic output for sources that we do lose in this process
for(lost_exp in dropped_names){
  
  # Subset the 'all scales' output to just this source
  alpha_lost_tmp <- dplyr::filter(alpha_allscales, !source %in% unique(dropped_sources))
  alpha_lost <- dplyr::filter(alpha_lost_tmp, exp.name == lost_exp)

  # Generate a file name
  lost_file <- paste0("expname_betadisp-failure_", lost_exp, ".csv")
  
  # Export locally
  write.csv(x = alpha_lost, na = '', row.names = F,
            file = file.path("data", "diagnostic", lost_file)) 
}

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
alpha_v99 <- alpha_fine_v2

# How many sources and exp.name got through the pipeline?
unique(alpha_v99$source) # 117
unique(alpha_v99$exp.name) # 346

# Identify tidy file name / path
alpha_name <- "05-C_caged_alpha-div"
alpha_path <- file.path("data", paste0(alpha_name, "_finest-scales.csv"))

# Export locally
write.csv(x = alpha_v99, row.names = F, na = '', file = alpha_path)

# Re-check 'all scales' structure
dplyr::glimpse(alpha_allscales)

# Export locally
write.csv(x = alpha_allscales, na = '', row.names = F,
          file = file.path("data", paste0(alpha_name, "_all-scales.csv")))

# End ----

