## --------------------------------------------------------------- ##
# Diagnosis Script - Failure to Calculate Beta Dispersion
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Create needed folder(s)
dir.create(path = file.path("data", "diagnostic"), showWarnings = F, recursive = T)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Data Prep ----
## ------------------------------------------- ##

# Read in the beta dispersion (all scales) dataset
beta_v1 <- read.csv(file = file.path("data", "05-A_caged_beta-disp_all-scales.csv"))

# Check structure
dplyr::glimpse(beta_v1)

# Pare down to bare minimum content
beta_v2 <- beta_v1 %>% 
  dplyr::select(source, exp.name:cage.treatment_std, 
                betadisp.design.level, betadisp.sample.size) %>% 
  dplyr::distinct()

# Re-check structure
dplyr::glimpse(beta_v2)

## ------------------------------------------- ##
# Create Diagnostic Files ----
## ------------------------------------------- ##

# For which files do we want to do this diagnosis?
diag_files <- c("aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv",
                "alberti_patagonia_grasslands_2016-2024_guanaco_vegetation.csv",
                "burkepile_florida_herbvr_2009-2012_fish_benthic.csv",
                "duran_floridacoralreef_successiontiles_2016_fish_mcaroalgae.csv",
                "gex_argentina-elpalmer_arggradient_2002_grazers_plants.csv",
                "gex_argentina-quebrada_arggradient_2002_grazers_plants.csv",
                "gex_argentina-relincho_arggradient_2002_grazers_plants.csv",
                "gex_beevermojave_mojave_2002_burrows&cattle_plants.csv",
                "lter-harvard_newengland_plantcover_2008-2019_moose_treeseedling.csv",
                "porensky_wyoming_nex_2015-2024_prairiedogs_vegetation.csv",
                "soler_argentina_native-alienplants_2015-2020_herbivores_vegetation.csv",
                "villar_brazil_car-cbo-ita_2009-2016_tapirs_forest.csv",
                "wang_mongolia_cattlesheepgrazersupp_2018_ruminant_plants.csv")

# Iterate across these
for(focal_file in diag_files){
  
  # Progress message
  message("Making diagnostic output for: ", focal_file)
  
  # Subset data
  diag_df <- dplyr::filter(beta_v2, source == focal_file)
  
  # Export
  write.csv(x = diag_df, row.names = F, na = '',
            file = file.path("data", "diagnostic", paste0("DIAGNOSE_", focal_file)))
  
}

# End ----
