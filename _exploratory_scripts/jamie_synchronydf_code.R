# Code for Synchrony Proposal DF
# Dependency: need to run 03_filter script first 


# How many timepoints per source/exp.name
sub_v5 <- sub_v4 %>% 
  # Identify cases with more than one sampling point 
  dplyr::group_by(source, exp.name) %>% 
  dplyr::mutate(time.ct = length(unique(sampling.point)),
                times = paste(sort(unique(sampling.point)), collapse = "; ")) %>% 
  dplyr::ungroup()

multi.times <- sub_v5 %>% 
  # how many datasets have >3 years of time points
  dplyr::filter(time.ct >2) %>% 
  dplyr::select(source, exp.name, year, time.ct, times) %>% 
  dplyr::distinct()



# How many years per source/exp.name
multi.year <- sub_v7 %>% 
  dplyr::group_by(source, exp.name) %>% 
  dplyr::summarize(yr_ct = length(unique(year)), .groups = "keep") %>% 
  # how many datasets have more than 3 years of data 
  dplyr::filter(yr_ct > 2) %>% 
  as.data.frame() 


# Metadata (ecosystem, lat/long)
meta_v1 <- read.csv(file.path("data", "07_tidy-sitelevel-metadata.csv"))

# Now join all together
library(dplyr)

result <- multi.times %>%
  full_join(multi.year, by = join_by(source, exp.name)) %>%
  left_join(meta_v1, by = join_by(source, exp.name)) %>%
  select(source, exp.name, lat, long, var_aq.or.terr, time.ct, yr_ct) %>%
  # filter out any i think are not relevant based on metadata notes
  filter(!source %in% c("lter-cdr_cedarcreek_herbivorenutrients_1984-1985_herbivores_vegetation.csv",
                        "villar_brazil_car-cbo-ita_2009-2016_tapirs_forest.csv",
                        "villar_brazil_car-cbo-ita_2009-2016_tapirs_forest.csv",
                        "villar_brazil_car-cbo-ita_2009-2016_tapirs_forest.csv",
                        "lter-cdr_cedarcreekecosystem_herbivorybyN_1982-2011_deer_vegetation.csv"
                        )) %>%
  write_csv("data/synchrony_df.csv")
 




