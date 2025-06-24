## --------------------------------------------------------------- ##
                # CAGED Wrangling & Quality Control
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
      cage.tmp %in% c("full", "exclosure", "exclusion", 
                        "fenced", "caged", "2.full.cage",
                        "full nitex", "full quarter", "cage",
                        "control small fenced", "np small fenced",
                        "small fenced no fertilizer", "full exclosure",
                        "nodeer", "total_excl", "ungrazed", "closed", 
                        "oui", "in", "caribou and small mammal fence", "caribou fence", 
                        "livest_excl", "Macropod-grazed") ~ "caged",
      ### Partial cage
      cage.tmp %in% c("partial", "3.part.cage", 
                        "partial nitex", "partial quarter",
                        "partial exclosure") ~ "partial",
      ### No cage
      cage.tmp %in% c("none", "open", "end/control", "control", 
                        "unfenced", "uncaged", "1.open.ctrl",
                        "control unfenced", "np unfenced",
                        "deer", "grazed", "non", "out", "open-grazed", 
                        "nitrogen phosphorus unfenced", "no cage", 
                        "no fence") ~ "uncaged",
      ## Organization/data source-dependent changes
      ### A
      organization == "ashton" & cage.tmp == "4.cage.expo" ~ "partial",
      organization == "alberti" & cage.tmp %in% c(1:2, "ng") ~ "caged",
      organization == "alberti" & cage.tmp %in% c(3, "g") ~ "uncaged",
      organization == "alberti" & cage.tmp == "cc" ~ "partial",
      ### B
      organization == "burkepile" & cage.tmp == "Exclosure control" ~ "uncaged",
      ### C
      organization == "chen" & cage.tmp %in% c("ungrazed", "g") ~ "caged",
      organization == "chen" & cage.tmp %in% c("hares", "hares & geese", "c") ~ "uncaged",
      organization == "clausing" & cage.tmp == "removal" ~ "caged",
      organization == "clausing" & cage.tmp == "ambient" ~ "uncaged",
      organization == "cper" & cage.tmp == "ah" ~ "uncaged", # AH = all herbivores
      organization == "cper" & cage.tmp %in% c("ce", "cre", "re") ~ "caged", #_E = _ exclosure
      ### D
      organization == "diaz" & cage.tmp == "artefact" ~ "partial",
      source == "duran_floridacoralreef_successiontiles_2016_fish_mcaroalgae.csv" &
        cage.tmp == "e" ~ "caged",
      source == "duran_floridacoralreef_successiontiles_2016_fish_mcaroalgae.csv" &
        cage.tmp == "h" ~ "uncaged",
      ### G
      organization == "gex" & cage.tmp %in% c("g", "gg") ~ "uncaged", # G = grazed
      organization == "gex" & cage.tmp %in% c("u", "uu") ~ "caged", # U = ungrazed
      source == "gex_queenslandaus1-5_Silcock_2009_grazers_plants.csv" &
        cage.tmp == "macropod-grazed" ~ "caged",
      source == "gex_bakker-cedarcreek_bakker-cedarcreek_year_deer_plants.csv" &
        cage.tmp == "gs" ~ "uncaged", # Note gex "GS" differs between datasets!
      source == "gex_bakker-sgs_bakker-sgs_2001_cattle&lagomorphs_plants.csv" &
        cage.tmp == "gs" ~ "caged",
      organization == "gilson" & cage.tmp == "f" ~ "caged",
      organization == "gilson" & cage.tmp %in% c("h", "c") ~ "uncaged",
      ### L
      organization == "lamb" & cage.tmp %in% c("roof", "fence") ~ "caged",
      organization == "lter-arc" & cage.tmp %in% c("lfct", "lfnp", "sfct", "sfnp", 
                                                   "lfct17", "sfct17", "mfct17") ~ "caged",
      organization == "lter-arc" & cage.tmp %in% c("nfct", "nfnp", "ct", 
                                                   "np", "n", "p") ~ "uncaged",
      source == "lter-cdr_cedarcreek_herbivorenutrients_1984-1985_herbivores_vegetation.csv" &
        cage.tmp %in% c(1:4, 7) ~ "caged",
      source == "lter-cdr_cedarcreek_herbivorenutrients_1984-1985_herbivores_vegetation.csv" &
        cage.tmp %in% c(5, 6, 8) ~ "uncaged",
      source == "lter-cdr_cedarcreekecosystem_herbivorybyN_1982-2011_deer_vegetation.csv" &
        cage.tmp == "1" ~ "caged",
      source == "lter-cdr_cedarcreekecosystem_herbivorybyN_1982-2011_deer_vegetation.csv" &
        cage.tmp == "0" ~ "uncaged",
      source == "lter-cdr_cedarcreekecosystem_plantabovegroundbiomass_1991_grasshoppers_vegetation.csv" &
        cage.tmp == "1" ~ "uncaged",
      source == "lter-cdr_cedarcreekecosystem_plantabovegroundbiomass_1991_grasshoppers_vegetation.csv" &
        cage.tmp %in% c(2:8) ~ "caged",
      organization == "lter-mcr" & cage.tmp == "cage control" ~ "uncaged",
      organization == "lter-mcr" & stringr::str_detect(string = cage.tmp, pattern = "x") ~ "caged",
      organization == "lter-sevilleta" & cage.tmp %in% c("l", "r") ~ "caged",
      organization == "lter-sevilleta" & cage.tmp == "c" ~ "uncaged",
      ### M
      organization == "mclaren" & cage.tmp %in% c(0, "c") ~ "uncaged",
      organization == "mclaren" & cage.tmp %in% c(1, "e") ~ "caged",
      ### N
      organization == "nopp-mayer" & cage.tmp == "0" ~ "uncaged",
      organization == "nopp-mayer" & cage.tmp == "1" ~ "caged",
      ### P
      organization == "parker" & cage.tmp == "cage control" ~ "partial",
      organization == "pascual" & cage.tmp == "cage control" ~ "partial",
      organization == "pelinson" & cage.tmp == "present" ~ "uncaged",
      organization == "pelinson" & cage.tmp == "absent" ~ "caged",
      organization == "porensky" & cage.tmp %in% c("y__livestock ex", "y__ungulate ex") ~ "caged",
      organization == "porensky" & cage.tmp %in% c("n__out", "y__out") ~ "uncaged",
      organization == "porensky" & cage.tmp %in% c("n__livestock ex", "n__ungulate ex") ~ "partial",
      ### R
      organization == "royo" & cage.tmp == "1" ~ "caged",
      organization == "royo" & cage.tmp == "0" ~ "uncaged",
      ### S
      organization == "spiecker" & cage.tmp %in% c("b", "l", "lu", "u") ~ "caged",
      organization == "spiecker" & cage.tmp %in% c("h", "hl", "hlu", "hu") ~ "uncaged",
      ### V
      organization == "villar" & cage.tmp == "a" ~ "caged",
      organization == "villar" & cage.tmp == "c" ~ "uncaged",      
      ### W
      organization == "wang" & cage.tmp %in% c("cg", "sg", "ng", "mg", "lg") ~ "caged",
      organization == "wang" & cage.tmp %in% c("csg", "hg") ~ "uncaged",
      ## If treatment isn't known, leave it that way
      cage.tmp == "no cage treatment identified" ~ "unknown",
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

# Should a diagnostic CSV be exported summarizing that information?
diagnostic_export <- TRUE

# Generate / export a diagnostic if desired
if(diagnostic_export == TRUE){
  
  # Generate
  diagnose_treats <- tidy_v2 %>% 
    dplyr::select(source, cage.treatment_std, cage.treatment_orig) %>% 
    dplyr::group_by(source, cage.treatment_std) %>% 
    # dplyr::summarize(original.treatments = paste(unique(cage.treatment_orig), collapse = "; "),
    #                  .groups = "keep") %>% 
    dplyr::distinct() %>% 
    dplyr::filter(cage.treatment_std %in% c("caged", "uncaged", "partial") != T)
  
  # Export
  write.csv(x = diagnose_treats, na = '', row.names = F,
            file = file.path("data", "cage-treatment-standardization.csv"))
  
}

# Re-check structure
dplyr::glimpse(tidy_v2)

## ------------------------------------------- ##
# Standardize Other Treatments
## ------------------------------------------- ##
## At the start of the project, only cage treatments matter
## But, eventually others may matter too so makes sense to tidy those up somewhat

# Do needed standardization
tidy_v3 <- tidy_v2 %>% 
  # Handle composite cage + nutrient treatment from ARC
  dplyr::mutate(treat.nutrients = dplyr::case_when(
    source != "lter-arc_alaskatundra_nutrientsandexclosures_2005_vertebrates_vegetation.csv" ~ treat.nutrients,
    stringr::str_detect(string = cage.treatment_orig, pattern = "NP") ~ "NP",
    cage.treatment_orig == "P" ~ "P",
    cage.treatment_orig == "N" ~ "N",
    stringr::str_detect(string = cage.treatment_orig, pattern = "CT") ~ "none",
    T ~ treat.nutrients)) # %>% 
  # # Handle composite cage + shading treatment from Spiecker
  # dplyr::mutate(treat.canopy = dplyr::case_when(
  #   source != "spiecker_newzealand_intertidalexclosure_2017-2018_herbivores_intertidal.csv" ~ treat.canopy,
  #   stringr::str_detect(string = cage.treatment_orig, pattern = "L") == T ~ "not shaded",
  #   stringr::str_detect(string = cage.treatment_orig, pattern = "L") != T ~ "shaded",
  #   T ~ treat.canopy))

# Check structure
dplyr::glimpse(tidy_v3)

## ------------------------------------------- ##
# Contextualize Experiment Names ----
## ------------------------------------------- ##

# Need to fill missing experiment names and attach relevant context
tidy_v3b <- tidy_v3 %>% 
  # Make all empty design levels truly empty
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with(c("exp.name", "exp.design.")),
                              .fns = ~ ifelse(nchar(.) == 0,
                                              yes = NA, no = .))) %>% 
  ## If 'exp.name' is missing, fill with full dataset filename
  dplyr::mutate(exp.name = ifelse(is.na(exp.name), yes = source, no = exp.name))

# Check for 'new' experiment names
supportR::diff_check(old = unique(tidy_v3$exp.name), new = unique(tidy_v3b$exp.name))

# Then, if there is a fire treatment, we want to add that to the experiment name
tidy_v4 <- tidy_v3b %>%
  dplyr::mutate(exp.name = ifelse(nchar(treat.fire) == 0 | is.na(treat.fire),
                      yes = exp.name,
                      no = paste(exp.name, treat.fire, sep = "--")) )

# Check again
supportR::diff_check(old = unique(tidy_v3b$exp.name), new = unique(tidy_v4$exp.name))

# Check structure
dplyr::glimpse(tidy_v4)

## ------------------------------------------- ##
# Fill Missing Experimental Design Levels ----
## ------------------------------------------- ##

# Check experimental design columns
tidy_v4 %>% 
  dplyr::select(organization, exp.name, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

# Do needed standardization
tidy_v5 <- tidy_v4 %>% 
  dplyr::mutate(
    ## Fill any missing design level values with experiment name
    exp.design.1 = ifelse(is.na(exp.design.1), yes = exp.name, no = exp.design.1),
    exp.design.2 = ifelse(is.na(exp.design.2), yes = exp.name, no = exp.design.2),
    exp.design.3 = ifelse(is.na(exp.design.3), yes = exp.name, no = exp.design.3),
    exp.design.4 = ifelse(is.na(exp.design.4), yes = exp.name, no = exp.design.4)
  ) %>% 
  # Fix problem with Ashton dataset
  ## Blocks are accidentally uniquely identified by trailing period + number
  dplyr::mutate(exp.design.2 = ifelse(source == "ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv",
                                      yes = gsub(pattern = paste0("\\.", 1:9, collapse = "|"),
                                                 replacement = "",
                                                 x = exp.design.2),
                                      no = exp.design.2))

# Re-check
tidy_v5 %>% 
  dplyr::select(organization, exp.name, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

## ------------------------------------------- ##
# Clarify Experimental Design Facets ----
## ------------------------------------------- ##

# Check experimental design columns
tidy_v5 %>% 
  dplyr::select(organization, exp.name, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

# Ccheck unique 'exp.design.1' values (across datasets)
sort(unique(tidy_v5$exp.design.1))

# Do needed processing
tidy_v6 <- tidy_v5 %>% 
  # Combine experiment name and design 4 if not the same
  dplyr::mutate(exp.design.4 = ifelse(exp.name == exp.design.4,
                                      yes = exp.design.4, 
                                      no = paste(exp.name, exp.design.4, sep = "__"))) %>% 
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
sort(unique(tidy_v6$exp.design.1))

# How many new ones gained?
message(length(unique(tidy_v6$exp.design.1)) - length(unique(tidy_v5$exp.design.1)), " unique 'exp.design.1' levels gained")

# Check experimental design columns
tidy_v6 %>% 
  dplyr::select(organization, exp.name, dplyr::starts_with("exp.design.")) %>% 
  dplyr::distinct() %>% 
  dplyr::glimpse()

## ------------------------------------------- ##
# Standardize Taxon Names ----
## ------------------------------------------- ##

# Check current taxa names
sort(unique(tidy_v6$original.taxa))

# Do desired wrangling
tidy_v7 <- tidy_v6 %>% 
  dplyr::mutate(taxa = dplyr::case_when(
    # removing "bleached" from species names to lump with living taxa
    source == "spiecker_newzealand_intertidalexclosure_2017-2018_herbivores_intertidal.csv" & 
      original.taxa %in% c("Bleached Crustose", "Bleached Jointed Calcareous", 
                           "Bleached Sheet", "Bleached Coarsely Branched") ~ gsub(pattern = "Bleached ", replacement = "", x = original.taxa),
    # Otherwise, keep original name
    T ~ original.taxa), .after = original.taxa) %>% 
  # Drop original taxa name
  dplyr::select(-original.taxa)

# Check difference
supportR::diff_check(old = unique(tidy_v6$original.taxa), new = unique(tidy_v7$taxa))

# Re-check taxa names
sort(unique(tidy_v7$taxa))

## ------------------------------------------- ##
# Standardize Study Years ----
## ------------------------------------------- ##

# Identify datasets where 'year' is not provided or isn't a number
malformed_years <- tidy_v7 %>% 
  dplyr::filter(is.na(supportR::force_num(.$year))) %>%
  dplyr::pull(source) %>% unique()

# Check current years
tidy_v7 %>% 
  dplyr::filter(source %in% malformed_years) %>% 
  dplyr::group_by(source, sampling.years) %>% 
  dplyr::summarize(years = paste(unique(year), collapse = ", "),
                   points = paste(unique(sampling.point), collapse = ", "),
                   .groups = "keep") %>% 
  as.data.frame()

# Fill in missing years as appropriate
tidy_v8 <- tidy_v7 %>% 
  dplyr::mutate(year = dplyr::case_when(
    ## If year is a non-NA number
    !is.na(supportR::force_num(.$year)) ~ as.character(year),
    ## Nopp-Mayer, 'year' is number of years after 1989
    source == "nopp-mayer_austria_ungulateherbivory_1989-2007_ungulates_trees.csv" ~ as.character(as.numeric(year) + 1989), 
    ## Sampling point is year
    source %in% c(
      "alberti_argentina_saltmarshexclosure_2007-2024_guineapigs_plants.csv",
      "burkepile_florida_herbvr_2009-2012_fish_benthic.csv",
      "chen_netherlands_saltmarsh_1972-2019_cattle_plants.csv",
      "lter-arc_DHTundra_nutrientsandexclosures_2005-2013-2017_vertebrates_vegetation.csv",
      "lter-arc_MATundra_nutrientsandexclosures_2005-2015-2017_vertebrates_vegetation.csv"
      ) ~ sampling.point,
    # Take from file name
    source %in% c("ashton_coastalamerica_marinepredexcl_2017-2019_predators_benthic.csv",
                  "clausing_newzealand_intertidalexclosure_2010-2012_grazers_algae.csv") ~ sampling.years,
    ## Date in mm/dd/yy format (2-digit year)
    source %in% c(
      "hensel_georgia_brackishhogs_2013-2015_hogs_plants.csv"
    )  ~ paste0("20", stringr::str_sub(sampling.point, 
                                       start = nchar(sampling.point) - 1, 
                                       end = nchar(sampling.point))),
    ## Date in mm/dd/yyyy format (4-digit year)
    source %in% c(
      "aguilera_chile_rockyintertidal_2010-2011_mollusc_kelp.csv"
    ) ~ stringr::str_sub(sampling.point, 
                         start = nchar(sampling.point) - 3, 
                         end = nchar(sampling.point)),
    ## Date is yyyy/mm/dd format (4-digit year at start)
    source %in% c(
      "alberti_patagonia_grasslands_2016-2024_guanaco_vegetation.csv"
    ) ~ stringr::str_sub(sampling.point, start = 1, end = 4),
    ## If year from file name has four digits, use that
    nchar(sampling.years) == 4 ~ sampling.years,
    T ~ "year")) %>% 
  # Do any needed post-processing
  ## Drop season names
  dplyr::mutate(year = gsub(pattern = "Fall |Spring |Summer |Winter ", 
                            replacement = "", x = year))

# Re-check years
tidy_v8 %>% 
  dplyr::filter(source %in% malformed_years) %>% 
  dplyr::mutate(fixed = ifelse(is.na(supportR::force_num(x = .$year)) != T,
                             yes = T, no = F)) %>% 
  dplyr::group_by(source, sampling.years, fixed) %>% 
  dplyr::summarize(years = paste(unique(year), collapse = ", "),
                   points = paste(unique(sampling.point), collapse = ", "),
                   .groups = "keep") %>% 
  dplyr::filter(fixed != T) # %>% view()

## ------------------------------------------- ##
# Standardize Misc. Other Variables ----
## ------------------------------------------- ##

# Re-check structure
dplyr::glimpse(tidy_v8)

# Do desired standardization
tidy_v9 <- tidy_v8
# No such wrangling needed (yet)

# Re-check structure
dplyr::glimpse(tidy_v9)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v9

# Check structure
dplyr::glimpse(tidy_v99)

# Identify tidy file name / path
tidy_name <- "02_caged_tidied.csv"
tidy_path <- file.path("data", tidy_name)

# Export locally
write.csv(x = tidy_v99, row.names = F, na = '', file = tidy_path)

# Upload to Drive
# googledrive::drive_upload(media = tidy_path, overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# End ----
