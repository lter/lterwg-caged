#################################################
## luisa genes exploratory analysis script

##successional stage for:
#difference between caged and uncaged
#beta dispersion


## ------------------------------------------- ##
# Nick's Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Create needed folder(s)
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()


## ------------------------------------------- ##
# Nick's modified Data Preparation (Across Design Levels) ----
## ------------------------------------------- ##

# Read in the data
caged_v1 <- read.csv(file = file.path("data", "06_caged_with-metadata.csv"))

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
  dplyr::group_by(
    dplyr::across(
      dplyr::all_of(setdiff(x = names(fill_v1), y = "betadisp.median")))) %>% 
  dplyr::summarize(abundance = mean(abundance, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()
  
# Pivot to treatment into wide format
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = betadisp.mean) %>% 
  # Calculate difference
  dplyr::mutate(diff = uncaged - caged)

# Re-check structure
dplyr::glimpse(caged_v2)

caged_v3 <- inner_join(caged_v2, caged_v1)

caged_v3$natural.vs.artificial.substrate <- as.factor(caged_v3$natural.vs.artificial.substrate)

###
#exploratory boxplots

ggplot(caged_v3, aes(x = natural.vs.artificial.substrate, y = diff)) +
  geom_boxplot(aes(fill = natural.vs.artificial.substrate), alpha = 0.4) +
  geom_jitter(aes(fill = natural.vs.artificial.substrate), width = 0.15,
              size = 2.5, pch = 21) +
  labs(x = "Natural vs. artificial substrate", y = "diff",
       title = paste0("Graph created on ", Sys.Date())) +
  #scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
   #                            "unknown" = "gray", "uncertain" = "gray20")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()

#### terrestrial vs. aquatic

#exploratory boxplots
ggplot(caged_v3, aes(x = aq.or.terr, y = diff)) +
  geom_boxplot(aes(fill = aq.or.terr), alpha = 0.4) +
  geom_jitter(aes(fill = aq.or.terr), width = 0.15,
              size = 2.5, pch = 21) +
  labs(x = "", y = "diff",
       title = paste0("Graph created on ", Sys.Date())) +
  #scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
  #                            "unknown" = "gray", "uncertain" = "gray20")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()

#### terrestrial vs. aquatic - raw values
ggplot(caged_v3, aes(x = aq.or.terr, y = betadisp.comm.dist)) +
  geom_boxplot(aes(fill = aq.or.terr), alpha = 0.4) +
  geom_jitter(aes(fill = aq.or.terr), width = 0.15,
              size = 2.5, pch = 21) +
  labs(x = "", y = "betadisp.comm.dist",
       title = paste0("Graph created on ", Sys.Date())) +
  scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
                               "uncertain" = "gray20")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()

ggplot(caged_v3, aes(x = aq.or.terr, y = betadisp.comm.dist)) +
  geom_boxplot(aes(fill = aq.or.terr), alpha = 0.4) +
  geom_jitter(aes(fill = aq.or.terr), width = 0.15,
              size = 2.5, pch = 21) +
  labs(x = "", y = "betadisp.comm.dist",
       title = paste0("Graph created on ", Sys.Date())) +
  scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
                               "uncertain" = "gray20")) +
  facet_wrap(~ cage.treatment_std) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()



#caged treatment standardized

ggplot(caged_v3, aes(x = cage.treatment_std, y = betadisp.comm.dist)) +
  geom_boxplot(aes(fill = cage.treatment_std), alpha = 0.4) +
  geom_jitter(aes(fill = cage.treatment_std), width = 0.15,
              size = 2.5, pch = 21) +
  labs(x = "", y = "betadisp.comm.dist",
       title = paste0("Graph created on ", Sys.Date())) +
  #scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
  #                            "unknown" = "gray", "uncertain" = "gray20")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()


#consumer richness .category



#latitude



#trophic level

#exploratory boxplots
ggplot(caged_v3, aes(x = consumer.trophic.level, y = diff)) +
  geom_boxplot(aes(fill = consumer.trophic.level), alpha = 0.4) +
  geom_jitter(aes(fill = consumer.trophic.level), width = 0.15,
              size = 2.5, pch = 21) +
  labs(x = "", y = "diff",
       title = paste0("Graph created on ", Sys.Date())) +
  #scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
  #                            "unknown" = "gray", "uncertain" = "gray20")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()

###



## ------------------------------------------- ##
# Nick's modified Data Preparation (Within Design Levels) ----
## ------------------------------------------- ##



# Identify local data files
beta_files <- dir(path = file.path("data"), pattern = "06_caged_with-metadata.csv")

# Output list
beta_list <- list()

# Loop across needed data files
for(focal_file in sort(unique(beta_files))){
  
  # Progress message
  message("Processing file: ", focal_file)
  
  # Read in the data
  caged_wdes_v1 <- read.csv(file = file.path("data", focal_file)) %>% 
    # Remove missing beta dispersion
    dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
    # Keep only good treatments
    dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
    # Summarize within treatments
    #dplyr::group_by(source, betadisp.design.level, cage.treatment_std) %>% 
    dplyr::summarize(betadisp = mean(betadisp.comm.dist, na.rm = T),
                     .groups = "keep") %>% 
    dplyr::ungroup() %>% 
    # Pivot to treatment into wide format
    #tidyr::pivot_wider(names_from = cage.treatment_std, values_from = betadisp) %>% 
    # Calculate difference
    #dplyr::mutate(diff = uncaged - caged)
  
  # Add to list
  #beta_list[[focal_file]] <- caged_wdes_v1
  
#}

# Unlist output
caged_wdes_v2 <- purrr::list_rbind(x = beta_list)

# Check structure
dplyr::glimpse(caged_wdes_v2)

# test



