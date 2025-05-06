

librarian::shelf(tidyverse, googledrive, supportR)



## ------------------------------------------- ##
# Data Preparation (Across Design Levels) ----
## ------------------------------------------- ##

# Read in the data
dt1 <- read.csv(file = file.path("data", "06_caged_with-metadata.csv"))

# calculate the difference metric
dt2 <- dt1 %>% 
  # Remove missing beta dispersion
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  # Keep only good treatments
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  # Summarize
  # Summarize within treatments
  dplyr::group_by(ecotype1, source, betadisp.design.level, cage.treatment_std) %>% 
  dplyr::summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Pivot to treatment into wide format
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = betadisp.mean) %>% 
  # Calculate difference
  dplyr::mutate(diff = uncaged - caged)

unique(dt2$ecotype1)

# make a boxplot showing difference between caged and uncaged for each ecotype1
dt2 %>% filter(ecotype1 != "") %>% 
ggplot(aes(x = ecotype1, y = diff)) +
  geom_boxplot(aes(fill = ecotype1), alpha = 0.4) +
  geom_jitter(aes(fill = ecotype1), width = 0.15,
              size = 2.5, pch = 21) #+
  #facet_wrap(. ~ source) +
  labs(x = "Ecotype1", y = "Uncaged - Caged Beta Dispersion",
       title = paste0("Graph created on ", Sys.Date())) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()

