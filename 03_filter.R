## --------------------------------------------------------------- ##
                        # CAGED Filtering
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, Tyler Coverdale, Jamie McDevitt-Irwin, Kelly Speare ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
sub_v1 <- read.csv(file.path("data", "02_caged_tidied.csv"))

# Check structure
dplyr::glimpse(sub_v1)

# Check what data made it through 01
unique(sub_v1$source) # 127
unique(sub_v1$exp.name) # 369

## ------------------------------------------- ##
# Drop Zero-Abundance Samples ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v1)

# Identify total abundance at design 1 and within cage treatment
sub_v2 <- sub_v1 %>% 
  dplyr::group_by(source, exp.name, exp.design.4, exp.design.3,
                  exp.design.2, exp.design.1, cage.treatment_std) %>% 
  dplyr::mutate(tot_abundance = sum(abundance, na.rm = T)) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(sub_v2)

# Any zero-abundance design levels?
zero_abun <- sub_v2 %>% 
  dplyr::filter(tot_abundance == 0)

# Check structure
dplyr::glimpse(zero_abun)

# Remove 'exp.design.1' levels without any abundance
sub_v3 <- sub_v2 %>% 
  # Average abundance withing experimental design level 1
  dplyr::group_by(
    dplyr::across(dplyr::all_of(setdiff(x = names(.),
                                        y = c("taxa", "abundance"))))
  ) %>% 
  dplyr::mutate(avg.abun = mean(abundance, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # Drop any rows where the average is 0 (i.e., no observations of any taxon)
  dplyr::filter(avg.abun > 0) %>% 
  # Ditch column used to do this subsetting
  dplyr::select(-avg.abun)

# Check number of lost rows
message(nrow(sub_v2) - nrow(sub_v3), " rows lost")

# Identify any datasets dropped entirely (shouldn't be any)
setdiff(x = unique(sub_v2$source), y = unique(sub_v3$source))
setdiff(x = unique(sub_v2$exp.name), y = unique(sub_v3$exp.name))

# Re-check structure
dplyr::glimpse(sub_v3)

# Re-attach zero-abundance rows (if any)
sub_v4 <- dplyr::bind_rows(sub_v3, zero_abun)

# Re-check structure
dplyr::glimpse(sub_v4)

## ------------------------------------------- ##
# Handle Sub-Annual Sampling ----
## ------------------------------------------- ##
# This is for any datasets that have sampling points within each year
# We need to identify what is the last sampling point within that year

# Check structure
dplyr::glimpse(sub_v4)

# Do needed processing
sub_v5 <- sub_v4 %>% 
  # Identify cases with more than one sampling point within dataset/year
  dplyr::group_by(source, year) %>% 
  dplyr::mutate(time.ct = length(unique(sampling.point)),
                times = paste(sort(unique(sampling.point)), collapse = "; ")) %>% 
  dplyr::ungroup()

# Identify any sources with more than one time point
multi.times <- sub_v5 %>% 
  dplyr::filter(time.ct != 1) %>% 
  dplyr::select(source, year, time.ct, times) %>% 
  dplyr::distinct()

# Check that out
as.data.frame(multi.times)
## View(multi.times)

# Do desired subsetting
sub_v6 <- sub_v5 %>% 
  dplyr::filter(
    # Keep any datasets with only one sampling event per year
    time.ct == 1 |
      # OR keep the manually-identified last time point for the following datasets
      ## A
      (source == "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv" &
         year == "2010" & sampling.point == "11/23/2010") |
      (source == "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv" &
         year == "2011" & sampling.point == "8/14/2011") |
      (source == "alberti_argentina_saltmarshexclosure_2007-2024_guineapigs_plants.csv" &
         year == "year" & sampling.point == "2024") |
      (source == "alberti_netherlands_floodplainsgrassland_1994-2001_cattle_vegetation.csv" &
         year == "year" & sampling.point == "14") |
      (source == "alberti_patagonia_grasslands_2016-2024_guanaco_vegetation.csv" &
         sampling.point %in% c("2024-11-08", "2024-11-09")) |
      ## B
      (source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" &
         year == "2009" & sampling.point == "December-09") |
      (source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" &
         year == "2010" & sampling.point == "Nov-10") |
      (source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" &
         year == "2011" & sampling.point == "November-11") |
      (source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" &
         year == "2012" & sampling.point == "August-12") |
      ## D
      (source == "diaz_longyearbyen_sedimentexclusionexp_2017_epibenthicpredators_benthic.csv" &
         sampling.point == "2017-08-23T00:00") |
      (source == "diaz_thiisbukta_sedimentexclusionexp_2017_epibenthicpredators_benthic.csv" &
         sampling.point == "2017-08-08T00:00") |
      (source == "duran_floridacoralreef_successiontiles_2016_fish_mcaroalgae.csv" &
         sampling.point == "June") |
      (source == "duran_florida-keys_established_2012_fishes_algae.csv" &
         sampling.point == "June") |
      (source == "duran_florida-keys_succession_2012_fishes_algae.csv" &
         sampling.point == "June") |
      ## E
      (source == "emry_britishcolumbia_intertidalexclusion_2011_herbivores_intertidal.csv" &
         exp.name == "Low" & sampling.point == "7/6/11") |
      (source == "emry_britishcolumbia_intertidalexclusion_2011_herbivores_intertidal.csv" &
         exp.name == "High" & sampling.point == "6/29/11") |
      ## G
      (source == "gilson_southafrica_intertidalexclusion_2021_grazers_algae.csv" & 
         sampling.point == "9") | 
      (source == "gilson_southafrica_intertidalexclusion_2021_grazers_inverts.csv" & 
         sampling.point == "9") |   
      ## H
      (source == "hensel_georgia_brackishhogs_2013-2015_hogs_plants.csv" & 
         year == "2013" & sampling.point == "7/5/13") | 
      (source == "hensel_georgia_brackishhogs_2013-2015_hogs_plants.csv" & 
         year == "2014" & sampling.point == "7/1/14") | 
      ## L
      # i think this one actually only has one sampling time point in 1985? 
      (source == "lter-cdr_cedarcreek_herbivorenutrients_1984-1985_herbivores_vegetation.csv" &
          year == "1985") |
      ## LTER Andrews
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1980" & sampling.point == "6/15/80") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1981" & sampling.point == "6/30/81") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1982" & sampling.point == "6/27/82") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1983" & sampling.point == "7/19/83") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1984" & sampling.point == "6/22/84") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1986" & sampling.point == "6/18/86") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1988" & sampling.point == "6/28/88") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1992" & sampling.point == "6/5/92") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "1996" & sampling.point == "6/26/96") |
      (source == "lter-andrewsforest_oregon_elkeclosure_1979-2007_elk_herbs.csv" &
         year == "2007" & sampling.point == "6/9/07") |
      ## M
      (source == "mclaren_canada_borealforestexcover_1990-1999_herbivore_plants.csv" &
         sampling.point == "2") |
      ## P
      (source == "parker_wetlands_chattahoocheeriver_2004_beavers_freshwaterplants.csv" & 
         sampling.point %in% c("7/20/2004", "7/22/2004")) |
      (source == "pelinson_brazil_predatorisolationcomm_2017_tilapia_insects.csv" & 
         sampling.point == "3") |
      ## S
      (source == "samper-villarreal_costarica_seagrass_2018-2019_seaturtle_seagrass.csv" & 
         year == "2018" & sampling.point == "9") |
      (source == "samper-villarreal_costarica_seagrass_2018-2019_seaturtle_seagrass.csv" & 
         year == "2019" & sampling.point == "13") |

      (source == "shantz_florida_partialcages_2013-2014_fish_benthic.csv" & 
         year == "2013" & sampling.point == "Sep_13") |
      (source == "shantz_florida_partialcages_2013-2014_fish_benthic.csv" & 
         year == "2014" & sampling.point == "Sep_14") |
      (source == "spiecker_newzealand_intertidalexclosure_2017-2018_herbivores_intertidal.csv" &
         year == "2017" & sampling.point == "11") |
      (source == "spiecker_newzealand_intertidalexclosure_2017-2018_herbivores_intertidal.csv" &
         year == "2018" & sampling.point == "3") |
      # Sellers
      # sellers has multi annual sampling within source but not within exp.name so its causing it to be dropped
      # had to change data key so check is the sampling point so we can just take check = 2 
      (source == "sellers_panama_coastalupwellingseasonality_2017-2018_mollusc_microalgae.csv" &
         sampling.point == "2") |
      (source == "villar_brazil-est_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2004" & sampling.point == "8") |
      (source == "villar_brazil-est_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2005" & sampling.point == "21") |
      (source == "villar_brazil-est_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2006" & sampling.point == "33") |
      (source == "villar_brazil-est_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2007" & sampling.point == "45") |
      (source == "villar_brazil-est_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2008" & sampling.point == "57") |
      (source == "villar_brazil-taq_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2004" & sampling.point == "8") |
      (source == "villar_brazil-taq_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2005" & sampling.point == "21") |
      (source == "villar_brazil-taq_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2006" & sampling.point == "33") |
      (source == "villar_brazil-taq_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2007" & sampling.point == "45") |
      (source == "villar_brazil-taq_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         year == "2008" & sampling.point == "57") |
      ## W
      (source == "wang_mongolia_cattlesheepgrazersupp_2018_ruminant_plants.csv" &
         sampling.point %in% c("2018.9.13", "2018.9.7")) |
      (source == "wang_mongolia_sheepgrazersupp_2014-2018_ruminant_plants.csv" &
         sampling.point %in% c("921", "922"))
    # (source == "" &
    #    year == "" & sampling.point == "")
  )

# Check number of lost rows (hopefully few rows but understandable if some/many)
message(nrow(sub_v5) - nrow(sub_v6), " rows lost")

# Identify any datasets dropped entirely (shouldn't be any)
setdiff(x = unique(sub_v5$source), y = unique(sub_v6$source))
# i think this fixed cedar creek! 

setdiff(x = unique(sub_v5$exp.name), y = unique(sub_v6$exp.name))
## fixed the sellers issue!

# Re-check sampling point for same datasets that previously had more than 1
multi.times_v2 <- sub_v6 %>% 
  dplyr::bind_rows(dplyr::filter(sub_v5, !source %in% sub_v6$source)) %>% 
  dplyr::filter(source %in% multi.times$source) %>% 
  dplyr::select(source, year, sampling.point) %>% 
  dplyr::distinct()

# Check that out
dplyr::glimpse(multi.times_v2)
## View(multi.times_v2)

# Drop the temp columns once everything looks good
sub_v7 <- sub_v6 %>% 
  # Drop "sampling.point" column plus any temporary columns
  dplyr::select(-sampling.point, -time.ct, -times)

# Re-check structure
dplyr::glimpse(sub_v7)

## ------------------------------------------- ##
# Handle Multi-Annual Sampling ----
## ------------------------------------------- ##
# These are datasets that have more than one year of data
# we just want to take the final year


# How many datasets have more than one year of data?
sub_v7 %>% 
  dplyr::group_by(source, exp.name) %>% 
  dplyr::summarize(yr_ct = length(unique(year)), .groups = "keep") %>% 
  dplyr::filter(yr_ct > 1) %>% 
  as.data.frame()

# List for outputs
sub_list <- list()

# Iterate across datasets
for(focal_src in sort(unique(sub_v7$source))){
   # focal_src <- "gex_tibet1-25_exrainfallgradient_2009-2010_grazers_plants.csv"

   # Progress message
   message("Working on file ", focal_src)

   # Subset data
   focal_src_df <- dplyr::filter(.data = sub_v7, source == focal_src)

   # Now loop across experiments
   for(focal_exp in sort(unique(focal_src_df$exp.name))){

      # Progress message
      message("Working on experiment ", focal_exp)

      # Subset data again
      focal_df <- dplyr::filter(.data = focal_src_df, exp.name == focal_exp)

      # Count number of years of data within that dataset
      yr_ct <- length(unique(focal_df$year))

      # If just one year, return that
      if(yr_ct == 1){ focal_out <- focal_df

      # Otherwise...
      } else {

         # Identify the last year
         last_yr <- sort(unique(focal_df$year))[yr_ct]

         # Subset the data
         focal_out <- dplyr::filter(.data = focal_df, year == last_yr)

         # Print message
         print(paste0(yr_ct, " years identified. ", last_yr, " identified as the last."))

      } # Close conditional

   # Add outputs to list
   sub_list[[paste0(focal_src, focal_exp)]] <- focal_out } # Close exp.name loop
} # Close source loop

# Unlist outputs
sub_v8 <- purrr::list_rbind(x = sub_list)

# Any full datasets lost?
supportR::diff_check(old = unique(sub_v7$source), new = unique(sub_v8$source))
supportR::diff_check(old = unique(sub_v7$exp.name), new = unique(sub_v8$exp.name))

# Re-check multi-annual data
sub_v8 %>% 
  dplyr::group_by(source, exp.name) %>% 
  dplyr::summarize(yr_ct = length(unique(year)), .groups = "keep") %>% 
  dplyr::filter(yr_ct > 1) %>% 
  as.data.frame()

# Re-check structure more generally
dplyr::glimpse(sub_v8)

## ------------------------------------------- ##
# Remove Particular Datasets ----
## ------------------------------------------- ##

# Remove any unwanted datasets by hand
sub_v9 <- sub_v8 %>% 
  # Jamie says this dataset is really the last year of a different dataset so should be removed
  dplyr::filter(source != "mcdevittirwin_palmyra_palmyratiles_2014_fish_benthic.csv") %>% 
  # Jamie says this dataset is the 4 month version while another dataset is the same but 12-month
  dplyr::filter(source != "lter-mcr_moorea_grazingintensity_2010_fish_benthic.csv") %>% 
  # Jamie says this dataset has only one replicate per treatment
  dplyr::filter(source != "lter-harvard_newengland_plantcover_2008-2019_moose_treeseedling.csv") %>%
  # JMI & KS decided these should be excluded because they are more like enclosures (check google docs meeting notes)
  dplyr::filter(!source %in% c("wang_mongolia_cattlesheepgrazersupp_2018_ruminant_plants.csv",
   "wang_mongolia_sheepgrazersupp_2014-2018_ruminant_plants.csv"))

# Double check only unwanted data are lost
supportR::diff_check(old = unique(sub_v8$source), new = unique(sub_v9$source))

# Check structure
dplyr::glimpse(sub_v9)

## ------------------------------------------- ##
# Remove Non-Living Taxa ----
## ------------------------------------------- ##

# Check current taxa
sort(unique(sub_v9$taxa))

# Remove non-living ones
sub_v10 <- sub_v9 %>%
  dplyr::filter(!taxa %in% c("LITT", "Bare", "Dead Barnacle", "Amphipod tube", "bare", 
                             "Mud Tube", "Jingle shell", "Sand tube", "Little Black tubes", 
                             "Mud tube", "Branch", "rock", "BARE", "Litter", 
                             "Bareground", "cactus__dead_", "QUERCUS DOUGSEED", "QUERCUS DOUGLASII_SEED", 
                             "SEED2 SPECIES", "SEED1 SPECIES", "QUERCUS AGRIFOLIA_SEED", 
                             "QUERCUS AG_SEED", "ZZZZ general codes", "#N/A", "per.bare", 
                             "litter", "standing dead Betula nana", "caribou feces", 
                             "frost boil", "animal litter", "Squirrel feces", "vole trail", 
                             "vole litter", "human trail", "vole hole", "vole trail", 
                             "Mixed dead litter", "Bare soil", "Standing Dead Betula nana", 
                             "Soil Frost boil", "Standing Dead Salix pulchra", "Ledum palustre-Dead", 
                             "Miscellaneous litter", "Pine needles", "Radulations", 
                             "Bare.cropped.substrate", "Rubble", "Sand", "SOIL", 
                             "sediment", "substrate", "Rock", "Dung"))


# Check for lost files
supportR::diff_check(old = unique(sub_v9$source), new = unique(sub_v10$source))
supportR::diff_check(old = unique(sub_v9$exp.name), new = unique(sub_v10$exp.name))

# How many lost rows?
message(nrow(sub_v9) - nrow(sub_v10), " rows lost")

# Full structure check
dplyr::glimpse(sub_v10)

## ------------------------------------------- ##
# Remove Confounding Treatments ----
## ------------------------------------------- ##

# For this paper, some treatments are likely confounding the effect of exclosures
sub_v11 <- sub_v10 %>% 
  # Don't want insecticided plots
  dplyr::filter(!treat.insecticide %in% c("Sprayed")) %>%
  # Don't want Nitrogen addition
  dplyr::filter(!treat.nitrogen.addition %in% c(16, 50)) %>%
  # Don't want prairie dog disturbance
   dplyr::filter(!treat.disturbance %in% c("prairie dog")) %>%
  # Don't want certain nutrient 
  dplyr::filter(!treat.nutrients %in% c("Nutrient Pollution", "enriched",
                                        "NP", "N", "P", 1:9))

# How many rows lost?
message(nrow(sub_v10) - nrow(sub_v11), " rows lost")

# Lose any full datasets (we shouldn't)?
supportR::diff_check(old = unique(sub_v10$source), new = unique(sub_v11$source))
# [1] "lter-cdr_cedarcreek_herbivorenutrients_1984-1985_herbivores_vegetation.csv"
# [2] "lter-cdr_cedarcreekecosystem_herbivorybyN_1982-2011_deer_vegetation.csv" 

# Lose any experiment names?
supportR::diff_check(old = unique(sub_v10$exp.name), new = unique(sub_v11$exp.name))
# [1] "lter-cdr_cedarcreek_herbivorenutrients_1984-1985_herbivores_vegetation.csv"
# [2] "lter-cdr_cedarcreekecosystem_herbivorybyN_1982-2011_deer_vegetation.csv"
# yes its ok both of these are lost - its because they have Nitrogen values 1-9 and we filter out all nutrient addition

# Check structure
dplyr::glimpse(sub_v11)

## ------------------------------------------- ##
# Drop Unwanted Columns ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v11)

# Drop any columns we know we don't want at the outset
sub_v12 <- sub_v11 %>% 
  # total abundance column used for filter double-checking at start of this script
  dplyr::select(-tot_abundance) %>% 
  # Superseded "original" columns (standardized in QC script)
  dplyr::select(-dplyr::starts_with("treat.")) %>% 
  # 'Distance from' column(s)
  dplyr::select(-dplyr::starts_with("distance.from.")) %>% 
  # Exclosure age
  dplyr::select(-exclosure.age) %>% 
  # Any columns that are entirely empty
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0)))

# Double check gained/lost columns
supportR::diff_check(old = names(sub_v11), new = names(sub_v12))

# Re-check structure
dplyr::glimpse(sub_v12)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
sub_v99 <- sub_v12

# What sources/experiments made it? 
sort(unique(sub_v99$source)) #121
sort(unique(sub_v99$exp.name)) #361

unique(sub_v99$year) # "year" is in there, thats what causing this to be a character
# its from villar but the raw data looks like it shoudl be fine

# Identify tidy file name / path
filter_name <- "03_caged_filtered.csv"
filter_path <- file.path("data", filter_name)

# Export locally
write.csv(x = sub_v99, row.names = F, na = '', file = filter_path)

# End ----
