## --------------------------------------------------------------- ##
# Diagnosis Script - Sources Dropped
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

# Manually identify files of interest
want_src <- c("gilson_southafrica_intertidalexclusion_2021_grazers_algae.csv",
              "porensky_wyoming_nex_2015-2024_prairiedogs_vegetation.csv",
              "parker_wetlands_carpgrass_2005_crayfish_plants.csv",
              "lamb_galapagos_consumermobility_2017_fish-urchins_algae.csv", # (exp.name = camano protected warm)
              "lter-sevilleta_newmexico_sev-project_1995-2005_smallmammals_vegetation.csv",
              "gex_beevermojave_mojave_2002_burrows&cattle_plants.csv",
              "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv" #(exp.name = COL)
)

## ------------------------------------------- ##
# Load Data ----
## ------------------------------------------- ##

# Read in the beta dispersion values
beta_v1 <- read.csv(file = file.path("data", "05-A_caged_beta-disp_finest-scales.csv"))

# Check structure
dplyr::glimpse(beta_v1)

# Read in the final pre-stats data too
pre.stats_v1 <- read.csv(file = file.path("data", "07_caged_w.meta_finest-scales.csv"))

# Check structure
dplyr::glimpse(pre.stats_v1)

## ------------------------------------------- ##
# Prepare Data ----
## ------------------------------------------- ##

# Streamline beta dispersion df to just needed info for diagnostics
beta_v2 <- beta_v1 %>% 
  # Immediately filter to desired sources
  dplyr::filter(source %in% want_src) %>% 
  dplyr::filter(source != "lamb_galapagos_consumermobility_2017_fish-urchins_algae.csv" |
                  (source == "lamb_galapagos_consumermobility_2017_fish-urchins_algae.csv" & exp.name == "Camano_protected-Warm")) %>% 
  dplyr::filter(source != "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv" |
                  (source == "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv" & exp.name == "COL")) %>% 
  # Pare down to just needed columns
  dplyr::select(source, dplyr::starts_with("exp."), cage.treatment_std, year,
                dplyr::starts_with("betadisp")) %>% 
  # Pare down to unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(beta_v2)

# Process the other df too
pre.stats_v2 <- pre.stats_v1  %>% 
  # Immediately filter to desired sources
  dplyr::filter(source %in% want_src) %>% 
  dplyr::filter(source != "lamb_galapagos_consumermobility_2017_fish-urchins_algae.csv" |
                  (source == "lamb_galapagos_consumermobility_2017_fish-urchins_algae.csv" & exp.name == "Camano_protected-Warm")) %>% 
  dplyr::filter(source != "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv" |
                  (source == "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv" & exp.name == "COL")) %>% 
  # Pare down to just needed columns
  dplyr::select(source, dplyr::starts_with("exp."), cage.treatment_std, year,
                dplyr::starts_with(c("betadisp", "within.cage.treat"))) %>% 
  # Pare down to unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(pre.stats_v2)

# End ----
