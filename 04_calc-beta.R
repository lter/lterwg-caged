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
## Note though that streamlining treatment / taxa information will likely make this number non-zero

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

# Loop across original data source
for(focal_src in unique(beta_v2$source)){
  # focal_src <- "cain_australia_herbexclusion_2021_macropod_plants.csv"
  
  # Progress message
  message("Processing source '", focal_src, "'")
  
  # Subset data
  src_sub <- beta_v2 %>% 
    dplyr::filter(source == focal_src)
  
  # Loop across treatments
  for(focal_trt in unique(src_sub$cage.treatment)){
    # focal_trt <- "Exclosure"
    
    # Subset again
    trt_sub <- src_sub %>% 
      dplyr::filter(cage.treatment == focal_trt)
    
    # Loop across study years
    for(focal_yr in unique(trt_sub$year)){
      
      # Subset again
      yr_sub <- trt_sub %>% 
        dplyr::filter(year == focal_yr)
      
      # Loop across most granular level of experimental design
      for(focal_des1 in unique(yr_sub$exp.design.1)){
        # focal_des1 <- "A2"
        
        # Subset yet again
        des1_sub <- yr_sub %>% 
          dplyr::filter(exp.design.1 == focal_des1)
        
        # Calculate beta dispersion
        des1_sub_out <- calc_betadisp(df = des1_sub, floor = min_reps,
                                      taxa_col = "taxa", abun_col = "abundance",
                                      dist_method = "bray", result_prefix = "exp.design.1")
        
        # Add to list
        beta_des1_list[[paste0(focal_src, focal_trt, focal_des1)]] <- des1_sub_out
        
      } # Close "exp.design.1" loop
      
      # Loop across experimental design level 2
      for(focal_des2 in unique(yr_sub$exp.design.2)){
        # focal_des2 <- "herbexclusion"
        
        # Subset yet again
        des2_sub <- yr_sub %>% 
          dplyr::filter(exp.design.2 == focal_des2)
        
        # Calculate beta dispersion
        des2_sub_beta <- calc_betadisp(df = des2_sub, floor = min_reps,
                                       taxa_col = "taxa", abun_col = "abundance",
                                       dist_method = "bray", result_prefix = "exp.design.2")
        
        # Drop finer experimental design level(s)
        des2_sub_out <- des2_sub_beta %>% 
          dplyr::select(-exp.design.1) %>% 
          dplyr::distinct()
        
        # Add to list
        beta_des2_list[[paste0(focal_src, focal_trt, focal_des2)]] <- des2_sub_out
        
      } # Close "exp.design.2" loop
      
      # Loop across experimental design level 3
      for(focal_des3 in unique(yr_sub$exp.design.3)){
        # focal_des3 <- "herbexclusion"
        
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
        des3_sub_beta <- calc_betadisp(df = des3_sub, floor = min_reps,
                                       taxa_col = "taxa", abun_col = "abundance",
                                       dist_method = "bray", result_prefix = "exp.design.3")
        
        # Drop finer experimental design level(s)
        des3_sub_out <- des3_sub_beta %>% 
          dplyr::select(-dplyr::ends_with(c("exp.design.1", "exp.design.2"))) %>% 
          dplyr::distinct()
        
        # Add to list
        beta_des3_list[[paste0(focal_src, focal_trt, focal_des3)]] <- des3_sub_out
        
      } # Close "exp.design.3" loop
    } # Close year loop
  } # Close treatment loop
} # Close source loop

# Unlist the output lists
beta_des1 <- purrr::list_rbind(x = beta_des1_list)
beta_des2 <- purrr::list_rbind(x = beta_des2_list)
beta_des3 <- purrr::list_rbind(x = beta_des3_list)

# Combine them!
beta_v3 <- beta_des1 %>% 
  dplyr::left_join(y = beta_des2,
                   by = c("source", "organization", "site", 
                          "experiment.name", "sampling.years", "excluded.group", 
                          "measured.group", "region", "lter.site", "ecosystem", 
                          "consumer.taxa", "resource.taxa", 
                          "cage.treatment", "exclosure.age", "year", 
                          "exp.name", "exp.design.3", "exp.design.2",
                          "distance.from.surface", "distance.from.source")) %>% 
  dplyr::left_join(y = beta_des3,
                   by = c("source", "organization", "site", 
                          "experiment.name", "sampling.years", "excluded.group", 
                          "measured.group", "region", "lter.site", "ecosystem", 
                          "consumer.taxa", "resource.taxa", 
                          "cage.treatment", "exclosure.age", "year", 
                          "exp.name", "exp.design.3",
                          "distance.from.surface", "distance.from.source")) %>% 
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
# Export ----
## ------------------------------------------- ##

# Create final object name
beta_v99 <- beta_v4

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
    exp.design.1.n.bin = dplyr::case_when(
      exp.design.1.n == 1 ~ "N = 1",
      exp.design.1.n > 1 & exp.design.1.n <= 5 ~ "N = 2-5",
      exp.design.1.n > 5 & exp.design.1.n <= 15 ~ "N = 6-15",
      exp.design.1.n > 15 & exp.design.1.n <= 30 ~ "N = 16-30",
      exp.design.1.n > 30 ~ "N > 30"),
    exp.design.2.n.bin = dplyr::case_when(
      exp.design.2.n == 1 ~ "N = 1",
      exp.design.2.n > 1 & exp.design.2.n <= 5 ~ "N = 2-5",
      exp.design.2.n > 5 & exp.design.2.n <= 15 ~ "N = 6-15",
      exp.design.2.n > 15 & exp.design.2.n <= 30 ~ "N = 16-30",
      exp.design.2.n > 30 ~ "N > 30"),
    exp.design.3.n.bin = dplyr::case_when(
      exp.design.3.n == 1 ~ "N = 1",
      exp.design.3.n > 1 & exp.design.3.n <= 5 ~ "N = 2-5",
      exp.design.3.n > 5 & exp.design.3.n <= 15 ~ "N = 6-15",
      exp.design.3.n > 15 & exp.design.3.n <= 30 ~ "N = 16-30",
      exp.design.3.n > 30 ~ "N > 30")
  ) %>% 
  dplyr::mutate(
    dplyr::across(.cols = dplyr::ends_with(".n.bin"),
                  .fns = ~ factor(x = ., levels = c("N = 1", "N = 2-5", 
                                                    "N = 6-15", "N = 16-30", 
                                                    "N > 30"))))

# Re-check structure
dplyr::glimpse(beta_viz)

# Design level 1 graph
ggplot(beta_viz, aes(x = organization, y = exp.design.1.betadisp,
                     fill = exp.design.1.n.bin)) +
  geom_violin() +
  labs(x = "Organization", y = "Beta Dispersion (Design Level 1)") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "04_betadisp-violins_exp-design-1.png"),
       width = 10, height = 5, units = "in")

# Design level 2 graph
ggplot(beta_viz, aes(x = organization, y = exp.design.2.betadisp,
                     fill = exp.design.2.n.bin)) +
  geom_violin() +
  labs(x = "Organization", y = "Beta Dispersion (Design Level 2)") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "04_betadisp-violins_exp-design-2.png"),
       width = 10, height = 5, units = "in")

# Design level 3 graph
ggplot(beta_viz, aes(x = organization, y = exp.design.3.betadisp,
                     fill = exp.design.3.n.bin)) +
  geom_violin() +
  labs(x = "Organization", y = "Beta Dispersion (Design Level 3)") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "04_betadisp-violins_exp-design-3.png"),
       width = 10, height = 5, units = "in")


# End ----
