## --------------------------------------------------------------- ##
# Exploratory Temporal Replicate Checks
## --------------------------------------------------------------- ##
# Purpose:
## Identify how many experiments have more than one time point within/across years

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Create needed folders
source(file = file.path("00_setup.R"))
dir.create(path = file.path("graphs", "bad"), showWarnings = FALSE)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
multi_v1 <- read.csv(file.path("data", "02_caged_tidied.csv"))

# Check structure
dplyr::glimpse(multi_v1)

## ------------------------------------------- ##
# Summarize the Data
## ------------------------------------------- ##

# Count number of years and sample points
multi_v2 <- multi_v1 %>% 
  dplyr::group_by(source, exp.name) %>% 
  dplyr::summarize(
    year_ct = length(unique(year)),
    time_ct = length(unique(sampling.point)),
    time_cross.yr_ct = length(unique(paste(year, sampling.point))),
    .groups = "drop")

# Check structure
dplyr::glimpse(multi_v2)

## ------------------------------------------- ##
# Make Graphs
## ------------------------------------------- ##
# Graph number of reps with more than one sample point
multi_v2 %>% 
  dplyr::filter(time_ct > 1) %>% 
  ggplot(data = ., aes(x = 'x', y = time_ct)) +
  geom_violin() +
  geom_jitter() +
  labs(y = "Number of Time Points Within Year") +
  supportR::theme_lyon() +
  theme(axis.text.x = element_blank(),
    axis.title.x = element_blank())

ggsave("2026-06-03_multi-times-within-year.png", width = 6, height = 6, units = "in")

ggplot(multi_v2, aes(x = "x", y = time_cross.yr_ct))+
   geom_violin() +
   geom_jitter() +
   labs(y = "Number of Time Points Across Years") +
   supportR::theme_lyon() +
   theme(axis.text.x = element_blank(),
      axis.title.x = element_blank())

ggsave("2026-06-03_multi-times-across-year.png", width = 6, height = 6, units = "in")

ggplot(multi_v2, aes(x = year_ct, y = time_cross.yr_ct))+
   geom_point(alpha = 0.1) +
   labs(y = "Number of Time Points Across Years") +
   supportR::theme_lyon()

ggsave("2026-06-03_multi-times-year-by-within-year.png", width = 6, height = 6, units = "in")

# End ----
