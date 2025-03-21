## --------------------------------------------------------------- ##
                        # CAGED Filtering
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools)

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
sub_v1 <- read.csv(file.path("data", "02_caged_tidied.csv"))

# Check structure
dplyr::glimpse(sub_v1)

## ------------------------------------------- ##
# Drop Unwanted Columns ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v1)

# Drop any columns we know we don't want at the outset
sub_v2 <- sub_v1 %>% 
  # Superseded "original" columns (standardized in QC script)
  dplyr::select(-dplyr::starts_with("treat.")) %>% 
  # Drop unstandardized cage treatments too
  dplyr::select(-cage.treatment_orig) %>% 
  # 'Distance from' column(s)
  dplyr::select(-distance.from.surface, -distance.from.source) %>% 
  # Exclosure age
  dplyr::select(-exclosure.age)

# Double check gained/lost columns
supportR::diff_check(old = names(sub_v1), new = names(sub_v2))

# Re-check structure
dplyr::glimpse(sub_v2)

## ------------------------------------------- ##
# Drop Zero-Abundance Samples ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v2)

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

# Re-check structure
dplyr::glimpse(sub_v3)

## ------------------------------------------- ##
# Handle Sub-Annual Sampling ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(sub_v3)

# Do needed processing
sub_v3b <- sub_v3 %>% 
  # Identify cases with more than one sampling point within dataset/year
  dplyr::group_by(source, year) %>% 
  dplyr::mutate(time.ct = length(unique(sampling.point)),
                times = paste(sort(unique(sampling.point)), collapse = "; ")) %>% 
  dplyr::ungroup()

# Identify any sources with more than one time point
multi.times <- sub_v3b %>% 
  dplyr::filter(time.ct != 1) %>% 
  dplyr::select(source, year, time.ct, times) %>% 
  dplyr::distinct()

# Check that out
as.data.frame(multi.times)
## View(multi.times)

# Do desired subsetting
sub_v4 <- sub_v3b %>% 
  dplyr::filter(
    # Keep any datasets with only one sampling event per year
    time.ct == 1 |
      # OR keep the manually-identified last time point for the following datasets
      ## B
      (source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" &
         year == "2009" & sampling.point == "Fall 2009") |
      (source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" &
         year == "2010" & sampling.point == "Fall 2010") |
      (source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" &
         year == "2011" & sampling.point == "Fall 2011") |
      (source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" &
         year == "2012" & sampling.point == "Winter 2012") |
      ## D
      (source == "diaz_longyearbyen_sedimentexclusionexp_2017_epibenthicpredators_benthic.csv" &
         sampling.point == "2017-08-23T00:00") |
      (source == "diaz_thiisbukta_sedimentexclusionexp_2017_epibenthicpredators_benthic.csv" &
         sampling.point == "2017-08-08T00:00") |
      ## E
      (source == "emry_britishcolumbia_intertidalexclusion_2011_herbivores_intertidal.csv" &
         sampling.point == "7/6/11") |
      ## G
      (source == "gilson_southafrica_intertidalexclusion_2021_grazers_algae.csv" & 
         sampling.point == "12") | 
      (source == "gilson_southafrica_intertidalexclusion_2021_grazers_inverts.csv" & 
         sampling.point == "12") |   
      ## H
      (source == "hensel_georgia_brackishhogs_2013-2015_hogs_plants.csv" & 
         year == "2013" & sampling.point == "7/5/13") | 
      (source == "hensel_georgia_brackishhogs_2013-2015_hogs_plants.csv" & 
         year == "2014" & sampling.point == "7/1/14") | 
      ## L
      (source == "lamb_galapagos_consumermobility_2017_fish-urchins_algae.csv" &
         year == "2017" & sampling.point == "Warm") |
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
      ## LTER Cedar Creek
      (source == "lter-cdr_cedarcreek_herbivorenutrients_1984-1985_herbivores_vegetation.csv" &
         year == "1984" & sampling.point == "840724") |
      (source == "lter-cdr_cedarcreek_herbivorenutrients_1984-1985_herbivores_vegetation.csv" &
         year == "1985" & sampling.point == "850829") |
      ## LTER Sevilleta
      (source == "lter-sevilleta_newmexico_sev-project_1995-2005_smallmammals_vegetation.csv" &
         year == "2005" & sampling.point == "11/29/05") |
      ## P
      (source == "pelinson_brazil_predatorisolationcomm_2017_tilapia_insects.csv" & 
         sampling.point == "3") |
      ## S
      (source == "spiecker_newzealand_intertidalexclosure_2017-2018_herbivores_intertidal.csv" &
         year == "2017" & sampling.point == "11") |
      (source == "spiecker_newzealand_intertidalexclosure_2017-2018_herbivores_intertidal.csv" &
         year == "2018" & sampling.point == "3") |
      ## V
      (source == "villar_brazil_car_2009-2016_tapirs_forest.csv" &
         sampling.point == "T73") |
      (source == "villar_brazil_cbo_2009-2016_tapirs_forest.csv" &
         sampling.point == "T87") |
      (source == "villar_brazil_ita_2009-2016_tapirs_forest.csv" &
         sampling.point == "T74") |
      (source == "villar_brazil-est_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         sampling.point == "105") |
      (source == "villar_brazil-taq_largewildherbivores_2004-2014_largeherbivores_plants.csv" &
         sampling.point == "105")
      # (source == "" &
      #    year == "" & sampling.point == "")
  ) %>% 
  # Drop "sampling.point" column plus any temporary columns
  dplyr::select(-sampling.point, -time.ct, -times)

# Check number of lost rows (hopefully few rows but understandable if some/many)
message(nrow(sub_v3) - nrow(sub_v4), " rows lost")

# Identify any datasets dropped entirely (shouldn't be any)
setdiff(x = unique(sub_v3$source), y = unique(sub_v4$source))

# Re-check structure
dplyr::glimpse(sub_v4)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Create final object name
sub_v99 <- sub_v4

# Identify tidy file name / path
filter_name <- "03_caged_filtered.csv"
filter_path <- file.path("data", filter_name)

# Export locally
write.csv(x = sub_v99, row.names = F, na = '', file = filter_path)

# Upload to Drive
googledrive::drive_upload(media = filter_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
