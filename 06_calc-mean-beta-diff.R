## --------------------------------------------------------------- ##
# CAGED *Mean* Difference in Beta Dispersion Calculation
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Calculate Mean Difference ----
## ------------------------------------------- ##

# Identify any beta dispersion outputs
(beta_outs <- dir(path = file.path("data"), pattern = "05-A_caged_beta-disp"))

# Loop across these to be more interpretable than purrr-style functional programming
for(focal_beta in beta_outs){
  
  # Progress message
  message("Calculating mean difference / summary stats for ", focal_beta)
  
  # Read the file in
  
  
  
}


# Read 'em in
diff_v1 <- purrr::map(.x = beta_outs,
                      .f = ~ read.csv(file = file.path("data", .x)))

# Check the structure of one
dplyr::glimpse(diff_v1[[1]])

# Do needed preparing / calculating
diff_v2 <- diff_v1 %>% 
  # Remove missing beta dispersion
  purrr::map(.f = ~ dplyr::filter(.data = .x, !is.na(betadisp.comm.dist))) %>% 
  # Keep only 'good' treatments
  purrr::map(.f = ~ dplyr::filter(.data = .x, cage.treatment_std %in% c("caged", "uncaged"))) %>% 
  # # Drop unwanted columns
  # purrr::map(.f = ~ dplyr::select(.data = .x, -dplyr::starts_with("exp.design"),
  #                                 -cage.treatment_orig, -betadisp.median, -betadisp.sample.size)) %>% 
  # Summarize within treatments / etc.
  purrr::map(.f = ~ dplyr::group_by(.data = .x, 
                                    dplyr::across(dplyr::all_of(
                                      # setdiff(x = names(.x), y = c("betadisp.median"))
                                      c("source", "organization", "site", 
                                        "excluded.group", "measured.group", 
                                        "exp.name", "cage.treatment_std", "year", 
                                        "betadisp.design.level")
                                    )))) %>% 
  purrr::map(.f = ~ dplyr::summarize(.data = .x,
                                     betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
                                     betadisp.sd = sd(betadisp.comm.dist, na.rm = T),
                                     betadisp.n = dplyr::n(),
                                     betadisp.se = betadisp.sd / sqrt(betadisp.n),
                                     .groups = "keep")) %>% 
  purrr::map(.f = ~ dplyr::ungroup(x = .x)) %>% 
  # Flip to wide format
  # Actually calculate difference in mean betadisp
  purrr::map(.f = ~ dplyr::mutate(.data = .x, betadisp.mean.diff = ))
  
glimpse(diff_v2[[1]])
 
  
  # Summarize
  # Summarize within treatments
  dplyr::group_by(source, betadisp.design.level, cage.treatment_std) %>% 
  dplyr::summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Pivot to treatment into wide format
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = betadisp.mean) %>% 
  # Calculate difference
  dplyr::mutate(diff = uncaged - caged)
  

# Re-check structure
dplyr::glimpse(diff_v2[[1]])


# Do needed preparing of data
diffv2 <- diffv1 %>% 
  # Remove missing beta dispersion
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  # Keep only good treatments
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  # Summarize
  # Summarize within treatments
  dplyr::group_by(source, betadisp.design.level, cage.treatment_std) %>% 
  dplyr::summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Pivot to treatment into wide format
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = betadisp.mean) %>% 
  # Calculate difference
  dplyr::mutate(diff = uncaged - caged)

# Re-check structure
dplyr::glimpse(diffv2)


# Read in data
gamma_v1 <- read.csv(file.path("data", "04_diffzero-filled.csv"))

# Check structure
dplyr::glimpse(gamma_v1)

## ------------------------------------------- ##
# Calculate Gamma Richness ----
## ------------------------------------------- ##

# Do needed wrangling
gamma_v2 <- gamma_v1 %>% 
  # Drop unwanted columns
  dplyr::select(-dplyr::starts_with(c("exp.design.", "cage.treatment")), -abundance) %>% 
  # Group by only desired columns & count number of unique taxa
  dplyr::group_by(dplyr::across(
    dplyr::all_of(setdiff(x = names(.), y = c("year", "taxa", "abundance"))) 
  )) %>% 
  dplyr::summarize(gamma.richness = length(unique(taxa)),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Do we have the expected number of values?
nrow(gamma_v2) == length(unique(paste(gamma_v2$source, gamma_v2$exp.name)))

# Check structure
dplyr::glimpse(gamma_v2)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
gamma_v99 <- gamma_v2

# Identify tidy file name / path
gamma_name <- "05-B_caged_gamma-rich.csv"
gamma_path <- file.path("data", gamma_name)

# Export locally
write.csv(x = gamma_v99, row.names = F, na = '', file = gamma_path)

# # Upload to Drive
# googledrive::drive_upload(media = gamma_path, overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
