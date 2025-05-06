#################################################


## ------------------------------------------- ##
# Data Preparation (Within Design Levels) ----
## ------------------------------------------- ##

# Identify local data files
beta_files <- dir(path = file.path("data"), pattern = "05_caged_beta-disp_exp-")

# Output list
beta_list <- list()

# Loop across needed data files
for(focal_file in sort(unique(beta_files))){
  
  # Progress message
  message("Processing file: ", focal_file)
  
  # Read in the data
  caged_wdes_v1 <- read.csv(file = file.path("data", focal_file)) %>% 
    # Remove missing beta dispersion
    dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
    # Keep only good treatments
    dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
    # Remove unwanted columns
    dplyr::select(source, cage.treatment_std, betadisp.design.level, 
                  betadisp.comm.dist) %>% 
    # Summarize within treatments
    dplyr::group_by(source, betadisp.design.level, cage.treatment_std) %>% 
    dplyr::summarize(betadisp = mean(betadisp.comm.dist, na.rm = T),
                     .groups = "keep") %>% 
    dplyr::ungroup() %>% 
    # Pivot to treatment into wide format
    tidyr::pivot_wider(names_from = cage.treatment_std, values_from = betadisp) %>% 
    # Calculate difference
    dplyr::mutate(diff = uncaged - caged)
  
  # Add to list
  beta_list[[focal_file]] <- caged_wdes_v1
  
}

# Unlist output
caged_wdes_v2 <- purrr::list_rbind(x = beta_list)

# Check structure
dplyr::glimpse(caged_wdes_v2)

### prepare for analysis

