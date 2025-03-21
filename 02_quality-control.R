## --------------------------------------------------------------- ##
                # CAGED Wrangling & Quality Control
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
tidy_v1 <- read.csv(file.path("data", "01_caged_harmonized.csv"))

# Check structure
dplyr::glimpse(tidy_v1)

## ------------------------------------------- ##
# Standardize Cage Treatment Values ----
## ------------------------------------------- ##

# Check current treatments
tidy_v1 %>% 
  dplyr::select(organization, treat.cage) %>% 
  dplyr::distinct()

# Perform needed standardization
tidy_v2 <- tidy_v1 %>% 
  # Make treatment lowercase
  dplyr::mutate(cage.tmp = tolower(treat.cage)) %>% 
  # Actually do standardization
  dplyr::mutate(
    cage.treatment_std = dplyr::case_when(
      ## Confident changes
      ### Cage Present
      cage.tmp %in% c("full", "exclosure", "start", "exclusion", 
                        "fenced", "caged", "2.full.cage",
                        "full nitex", "full quarter",
                        "control small fenced", "np small fenced",
                        "small fenced no fertilizer", "full exclosure",
                        "nodeer", "total_excl", "closed") ~ "caged",
      ### Partial cage
      cage.tmp %in% c("partial", "3.part.cage", 
                        "partial nitex", "partial quarter",
                        "partial exclosure", "livest_excl") ~ "partial",
      ### No cage
      cage.tmp %in% c("none", "open", "end/control", "control", 
                        "unfenced", "uncaged", "1.open.ctrl",
                        "control unfenced", "np unfenced",
                        "deer", "grazed") ~ "uncaged",
      ## If treatment isn't known, leave it that way
      tolower(cage.tmp) == "no cage treatment identified" ~ "unknown",
      ## If not covered by prior conditions, just flag it as uncertain
      T ~ "uncertain"), .before = treat.cage) %>% 
  # Drop temporary lowercase cage column
  dplyr::rename(cage.treatment_orig = treat.cage) %>% 
  dplyr::select(-cage.tmp)

# Any gained / lost columns?
supportR::diff_check(old = names(tidy_v1), new = names(tidy_v2))

# Look at all uncertain treatments
tidy_v2 %>% 
  dplyr::filter(cage.treatment_std == "uncertain") %>% 
  dplyr::select(organization, cage.treatment_orig) %>% 
  dplyr::distinct()

# Re-check structure
dplyr::glimpse(tidy_v2)

## ------------------------------------------- ##
# Handle Missing Experimental Design Facets ----
## ------------------------------------------- ##

# Check experimental design columns
tidy_v2 %>% 
  dplyr::select(organization, exp.name, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

# Do needed standardization
tidy_v3 <- tidy_v2 %>% 
  # Fill any missing values with experiment name
  ## (All have 'exp.design.1' but not necessarily all have higher levels)
  dplyr::mutate(
    exp.design.2 = ifelse(nchar(exp.design.2) == 0 | is.na(exp.design.2),
                          yes = project.name, no = exp.design.2),
    exp.design.3 = ifelse(nchar(exp.design.3) == 0 | is.na(exp.design.3),
                          yes = project.name, no = exp.design.3),
    exp.design.4 = ifelse(nchar(exp.design.4) == 0 | is.na(exp.design.4),
                          yes = project.name, no = exp.design.4),
    ## If 'exp.name' is missing, fill with full dataset filename
    exp.name = ifelse(nchar(exp.name) == 0 | is.na(exp.name),
                      yes = source, no = exp.name)
  )

# Re-check
tidy_v3 %>% 
  dplyr::select(organization, exp.name, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

## ------------------------------------------- ##
# Clarify Experimental Design Facets ----
## ------------------------------------------- ##

# Check experimental design columns
tidy_v3 %>% 
  dplyr::select(organization, exp.name, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

# Ccheck unique 'exp.design.1' values (across datasets)
sort(unique(tidy_v3$exp.design.1))

# Do needed processing
tidy_v4 <- tidy_v3 %>% 
  # Combine design 3 and 4 if not the same
  dplyr::mutate(exp.design.3 = ifelse(exp.design.4 == exp.design.3,
                                      yes = exp.design.3, 
                                      no = paste(exp.design.4, exp.design.3, sep = "__"))) %>% 
  # Combine 2 and 3 if not the same
  dplyr::mutate(exp.design.2 = ifelse(exp.design.3 == exp.design.2,
                                      yes = exp.design.2, 
                                      no = paste(exp.design.3, exp.design.2, sep = "__"))) %>% 
  # Combine 1 and 2 if not the same
  dplyr::mutate(exp.design.1 = ifelse(exp.design.2 == exp.design.1,
                                      yes = exp.design.1, 
                                      no = paste(exp.design.2, exp.design.1, sep = "__")))

# Re-check unique 'exp.design.1' values
sort(unique(tidy_v4$exp.design.1))

# How many new ones gained?
length(unique(tidy_v4$exp.design.1)) - length(unique(tidy_v3$exp.design.1))

# Check experimental design columns
tidy_v4 %>% 
  dplyr::select(organization, exp.name, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

## ------------------------------------------- ##
# Standardize Taxon Names ----
## ------------------------------------------- ##

# Check current taxa names
sort(unique(tidy_v4$original.taxa))

# Do desired wrangling
tidy_v5 <- tidy_v4 %>% 
  dplyr::mutate(taxa = dplyr::case_when(
    
    # No wrangling done here (yet)
    
    T ~ original.taxa), .after = original.taxa) %>% 
  # Drop original taxa name
  dplyr::select(-original.taxa)

# Re-check taxa names
sort(unique(tidy_v5$taxa))

## ------------------------------------------- ##
# Standardize Study Years ----
## ------------------------------------------- ##

# Check current years
tidy_v5 %>% 
  dplyr::filter(is.na(year) | !stringr::str_count(string = year, pattern = "\\d{4}")) %>% 
  dplyr::group_by(source, sampling.years) %>% 
  dplyr::summarize(years = paste(unique(year), collapse = ", "),
                   .groups = "keep")

# Fill in missing years as appropriate
tidy_v6 <- tidy_v5 %>% 
  dplyr::mutate(year = dplyr::case_when(
    !is.na(year) ~ as.character(year),
    source == "hensel_georgia_brackishhogs_2013-2015_hogs_plants.csv" ~ paste0("20", stringr::str_sub(sampling.point, start = nchar(sampling.point) - 1, end = nchar(sampling.point))),
    source == "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv" ~ sampling.years,
    source == "burkepile_florida_herbvr_2009-2012_fish_benthic.csv" ~ sampling.point,
    source == "clausing_newzealand_intertidalexclosure_2010-2012_grazers_algae.csv" ~ sampling.years,
    ## Put in placeholder years for datasets without year info
    source == "lter-arc_alaska_acidictussock_1996-1999_vertebrates_plants.csv" ~ "1999",
    source == "lter-harvard_newengland_plantcover_2008-2019_moose_plants.csv" ~ "2019",
    ## If year from file name has four digits, use that
    nchar(sampling.years) == 4 ~ sampling.years,
    T ~ NA)) %>% 
  # Do any needed post-processing
  ## Drop season names
  dplyr::mutate(year = gsub(pattern = "Fall |Spring |Summer |Winter ", 
                            replacement = "", x = year))

# Re-check years
tidy_v6 %>% 
  dplyr::filter(is.na(year) | !stringr::str_count(string = year, pattern = "\\d{4}")) %>% 
  dplyr::group_by(source, sampling.years) %>% 
  dplyr::summarize(years = paste(unique(year), collapse = ", "),
                   .groups = "keep")

## ------------------------------------------- ##
# Standardize Misc. Other Variables ----
## ------------------------------------------- ##

# Re-check structure
dplyr::glimpse(tidy_v6)

# Do desired standardization
tidy_v7 <- tidy_v6 %>% 
  # Standardize casing for distance from surface
  dplyr::mutate(distance.from.surface = tolower(distance.from.surface))

# Re-check structure
dplyr::glimpse(tidy_v7)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v7

# Check structure
dplyr::glimpse(tidy_v99)

# Identify tidy file name / path
tidy_name <- "02_caged_tidied.csv"
tidy_path <- file.path("data", tidy_name)

# Export locally
write.csv(x = tidy_v99, row.names = F, na = '', file = tidy_path)

# Upload to Drive
googledrive::drive_upload(media = tidy_path, overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
