## --------------------------------------------------------------- ##
# CAGED Beta Dispersion Calculation
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Create needed folder(s)
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
caged_v1 <- read.csv(file.path("data", "05_caged_beta-disp.csv"))

# Check structure
dplyr::glimpse(caged_v1)

## ------------------------------------------- ##
# Exploratory Graphs ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(caged_v1)

# Do some pre-visualization wrangling
beta_viz <- caged_v1 %>% 
  dplyr::mutate(
    betadisp.n.bin = dplyr::case_when(
      betadisp.sample.size == 1 ~ "N = 1",
      betadisp.sample.size > 1 & betadisp.sample.size <= 5 ~ "N = 2-5",
      betadisp.sample.size > 5 & betadisp.sample.size <= 15 ~ "N = 6-15",
      betadisp.sample.size > 15 & betadisp.sample.size <= 30 ~ "N = 16-30",
      betadisp.sample.size > 30 ~ "N > 30",
      T ~ NA)) %>% 
  dplyr::mutate(betadisp.n.bin = factor(x = betadisp.n.bin, 
                                        levels = c("N = 1", "N = 2-5", 
                                                   "N = 6-15", "N = 16-30", 
                                                   "N > 30")))
# Re-check structure
dplyr::glimpse(beta_viz)

# Exploratory graph
ggplot(beta_viz, aes(x = cage.treatment_std, y = betadisp.comm.dist)) +
  geom_jitter(aes(fill = cage.treatment_std), width = 0.15,
              alpha = 0.3, size = 1, pch = 21) +
  facet_wrap(. ~ source) +
  labs(x = "Cage Treatment", y = "Beta Dispersion") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1))

# Export locally
ggsave(filename = file.path("graphs", "06_betadisp-violins.png"),
       width = 12, height = 12, units = "in")

# End ----
