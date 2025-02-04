## --------------------------------------------------------------- ##
# CAGED Filtering
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, magrittr, ltertools, vegan)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
beta_v1 <- read.csv(file.path("data", "03_caged_filtered.csv"))

# Check structure
dplyr::glimpse(beta_v1)

## ------------------------------------------- ##
# Calculate Beta Dispersion ----
## ------------------------------------------- ##

# Create a list for storing outputs
beta_des1_list <- list()
beta_des2_list <- list()
beta_des3_list <- list()

# Loop across original data source
for(focal_src in unique(beta_v1$source)){
  
  # Progress message
  message("Processing source '", focal_src, "'")
  
  # Subset data
  src_sub <- beta_v1 %>% 
    dplyr::filter(source == focal_src)
  
  # Loop across treatments
  for(focal_trt in unique(src_sub$original.treatment)){
    
    # Subset again
    trt_sub <- src_sub %>% 
      dplyr::filter(original.treatment == focal_trt)
    
    # Loop across most granular level of experimental design
    for(focal_des1 in unique(trt_sub$exp.design.1)){
      
      # Subset yet again
      des1_sub <- trt_sub %>% 
        dplyr::filter(exp.design.1 == focal_des1)
      
      # Prepare output (post-calculation)
      des1_sub_out <- des1_sub %>% 
        dplyr::select(source:distance.from.source) %>% 
        dplyr::distinct()
      
      # Pivot to wide format & drop all non-taxa columns
      des1_sub_wide <- des1_sub %>% 
        tidyr::pivot_wider(names_from = original.taxa,
                           values_from = abundance,
                           values_fill = 0) %>% 
        dplyr::select(-source:-distance.from.source)
      
      # Get distance/dissimilarity matrix
      des1_sub_dist <- vegan::vegdist(x = des1_sub_wide, method = "bray")
      
      # Skip beta dispersion calculation if no distance found (n = 1)
      if(length(des1_sub_dist) > 0){
        
      # Calculate beta dispersion
      des1_sub_beta <- vegan::betadisper(d = des1_sub_dist,
                                         group = as.factor(rep(x = "x", 
                                                               times = nrow(des1_sub_wide))),
                                         type = "centroid", bias.adjust = F, 
                                         sqrt.dist = F, add = F)
      }
      
      # Finalize outputs
      des1_sub_out %<>%
        dplyr::mutate(
          exp.design.1.n = nrow(des1_sub_wide),
          exp.design.1.betadisp = ifelse(length(des1_sub_dist) > 0,
                                         yes = des1_sub_beta$distances,
                                         no = NA_real_))
      
      # Add to list
      beta_des1_list[[paste0(focal_src, focal_trt, focal_des1)]] <- des1_sub_out
      
    } # Close "exp.design.1" loop
    
    # Loop across experimental design level 2
    for(focal_des2 in unique(trt_sub$exp.design.2)){
      
      # Subset yet again
      des2_sub <- trt_sub %>% 
        dplyr::filter(exp.design.2 == focal_des2) %>% 
        # Summarizing across more granular spatial scale(s)
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(setdiff(x = names(trt_sub), 
                                  y = c("exp.design.1", "abundance"))))) %>% 
        dplyr::summarize(abundance = mean(abundance, na.rm = T),
                         .groups = "keep") %>% 
        dplyr::ungroup()
      
      # Prepare output (post-calculation)
      des2_sub_out <- des2_sub %>% 
        dplyr::select(source:distance.from.source) %>% 
        dplyr::distinct()
      
      # Pivot to wide format & drop all non-taxa columns
      des2_sub_wide <- des2_sub %>% 
        tidyr::pivot_wider(names_from = original.taxa,
                           values_from = abundance,
                           values_fill = 0) %>% 
        dplyr::select(-source:-distance.from.source)
      
      # Get distance/dissimilarity matrix
      des2_sub_dist <- vegan::vegdist(x = des2_sub_wide, method = "bray")
      
      # Skip beta dispersion calculation if no distance found (n = 1)
      if(length(des2_sub_dist) > 0){
        
        # Calculate beta dispersion
        des2_sub_beta <- vegan::betadisper(d = des2_sub_dist,
                                           group = as.factor(rep(x = "x", 
                                                                 times = nrow(des2_sub_wide))),
                                           type = "centroid", bias.adjust = F, 
                                           sqrt.dist = F, add = F)
      }
      
      # Finalize outputs
      des2_sub_out %<>%
        dplyr::mutate(
          exp.design.2.n = nrow(des2_sub_wide),
          exp.design.2.betadisp = ifelse(length(des2_sub_dist) > 0,
                                         yes = des2_sub_beta$distances,
                                         no = NA_real_))
      
      # Add to list
      beta_des2_list[[paste0(focal_src, focal_trt, focal_des2)]] <- des2_sub_out
      
    } # Close "exp.design.2" loop
    
    # Loop across experimental design level 3
    for(focal_des3 in unique(trt_sub$exp.design.3)){
      
      # Subset yet again
      des3_sub <- trt_sub %>% 
        dplyr::filter(exp.design.3 == focal_des3) %>% 
        # Summarizing across more granular spatial scale(s)
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(setdiff(x = names(trt_sub), 
                                  y = c("exp.design.2", "exp.design.1", "abundance"))))) %>% 
        dplyr::summarize(abundance = mean(abundance, na.rm = T),
                         .groups = "keep") %>% 
        dplyr::ungroup()
      
      # Prepare output (post-calculation)
      des3_sub_out <- des3_sub %>% 
        dplyr::select(source:distance.from.source) %>% 
        dplyr::distinct()
      
      # Pivot to wide format & drop all non-taxa columns
      des3_sub_wide <- des3_sub %>% 
        tidyr::pivot_wider(names_from = original.taxa,
                           values_from = abundance,
                           values_fill = 0) %>% 
        dplyr::select(-source:-distance.from.source)
      
      # Get distance/dissimilarity matrix
      des3_sub_dist <- vegan::vegdist(x = des3_sub_wide, method = "bray")
      
      # Skip beta dispersion calculation if no distance found (n = 1)
      if(length(des3_sub_dist) > 0){
        
        # Calculate beta dispersion
        des3_sub_beta <- vegan::betadisper(d = des3_sub_dist,
                                           group = as.factor(rep(x = "x", 
                                                                 times = nrow(des3_sub_wide))),
                                           type = "centroid", bias.adjust = F, 
                                           sqrt.dist = F, add = F)
      }
      
      # Finalize outputs
      des3_sub_out %<>%
        dplyr::mutate(
          exp.design.3.n = nrow(des3_sub_wide),
          exp.design.3.betadisp = ifelse(length(des3_sub_dist) > 0,
                                         yes = des3_sub_beta$distances,
                                         no = NA_real_))
      
      # Add to list
      beta_des3_list[[paste0(focal_src, focal_trt, focal_des3)]] <- des3_sub_out
      
    } # Close "exp.design.3" loop
  } # Close treatment loop
} # Close source loop

# Unlist the output lists
beta_des1 <- purrr::list_rbind(x = beta_des1_list)
beta_des2 <- purrr::list_rbind(x = beta_des2_list)
beta_des3 <- purrr::list_rbind(x = beta_des3_list)

# Combine them!
beta_v2 <- beta_des1 %>% 
  dplyr::left_join(y = beta_des2)

beta_v2 <- purrr::list_rbind(x = beta_list)

# Check structure
dplyr::glimpse(beta_v2)


str(trt_sub)


# The group suggests the following function:
?vegan::betadisper





# Create new object (post-betadisp calculation)
beta_v2 <- beta_v1

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
beta_v99 <- beta_v2

# Identify tidy file name / path
beta_name <- "04_caged_beta-disp.csv"
beta_path <- file.path("data", beta_name)

# Export locally
write.csv(x = beta_v99, row.names = F, na = '', file = beta_path)

# Upload to Drive
googledrive::drive_upload(media = beta_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
