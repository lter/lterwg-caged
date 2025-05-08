
## --------------------------------------------------------------- ##
# CAGED Beta disp.diff figs
## --------------------------------------------------------------- ##
# Written by: Ryan Rogers

#Figure(s) of ecosystem types x difference (bets diversity, dispersion)

#Aquatic vs terrestiral comaprisons, 
#Caged treatment standardization, consumer richness.category, lat

#################################################
#Housekeeping
librarian::shelf(tidyverse, magrittr, ltertools, vegan, supportR, ggplot2, update_all= TRUE)

# Create needed folder(s)
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in the data
caged_v1 <- read.csv(file = file.path("data", "06_caged_with-metadata_finest-scales.csv"))

# Check structure
dplyr::glimpse(caged_v1)

head(caged_v1)

# Do needed preparing of data
caged_v2 <- caged_v1 %>% 
  # Remove missing beta dispersion
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  # Keep only good treatments
  dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
  # Summarize
  # Summarize within treatments
  dplyr::group_by(source, betadisp.design.level, cage.treatment_std) %>% 
  dplyr::summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Pivot to treatment into wide format
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = betadisp.mean) %>% 
  # Calculate difference
  dplyr::mutate(diff = uncaged - caged)

# Re-check structure
dplyr::glimpse(caged_v2)

## ------------------------------------------- ##
# Nick's Create Graph (Across Design Levels) ----
## ------------------------------------------- ##

# Create desired graph
ggplot(caged_v2, aes(x = diff, y = reorder(source, dplyr::desc(-diff)), 
                     color = betadisp.design.level)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = 3) +
  labs(x = "Uncaged - Caged Beta Dispersion",
       y = "Dataset Source") +
  supportR::theme_lyon() +
  theme(axis.text.y = element_blank())

#### re-attach metadata ####

caged_v3<-left_join(caged_v2, caged_v1, by = c("source"))

caged_v3$natural.vs.artificial.substrate <- as.factor(caged_v3$natural.vs.artificial.substrate)

dplyr::glimpse(caged_v3)

#### Exploratory plots, diff x gamma rich (?), ecosystem type, cage size, herbivore rich, latitude

### Figure 1 Successional stage ####

ggplot(caged_v3, aes(x = natural.vs.artificial.substrate, y = diff)) +
  geom_boxplot(aes(fill = natural.vs.artificial.substrate), alpha = 0.4) +
  #geom_jitter(aes(fill = natural.vs.artificial.substrate), width = 0.15,
              #size = 2.5, pch = 21) +
  labs(x = "Natural vs. artificial substrate (Succession)", y = "diff",
       title = paste0("Graph created on ", Sys.Date())) +
  #scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
  #                            "unknown" = "gray", "uncertain" = "gray20")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()

### Figure 2 Ecosystem type (coarse) - Terrestrial/Aquatic ####

ggplot(caged_v3)


#### Figure 3 Ecosystem type (fine) - Ecosystem types ####

### Figure 4 Gamma richness ####

#### Figure 5 Herbivore richness ####

#### Figure 6 Exclusion time #####

#### Figure 7 Size of cage ####











