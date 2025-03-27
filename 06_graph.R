## --------------------------------------------------------------- ##
# CAGED Beta Dispersion Calculation
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Create needed folder(s)
dir.create(path = file.path("graphs"), showWarnings = F)
dir.create(path = file.path("graphs", "per-dataset-boxplots"), showWarnings = F)

# Authorize GoogleDrive
googledrive::drive_auth()

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

# Make some exploratory graphs!
for(focal_src in sort(unique(beta_viz$source))){
  # focal_src <- "soler_argentina_native-alienplants_2015-2020_herbivores_vegetation.csv"
  
  # Progress message
  message("Making exploratory boxplots for file: '", focal_src, "'")
  
  # Subset data
  focal_sub <- dplyr::filter(.data = beta_viz, source == focal_src)
 
  # Create graph
  ggplot(focal_sub, aes(x = cage.treatment_std, y = betadisp.comm.dist)) +
    geom_boxplot(aes(fill = cage.treatment_std), alpha = 0.4) +
    geom_jitter(aes(fill = cage.treatment_std), width = 0.15,
                size = 2.5, pch = 21) +
    facet_wrap(. ~ source) +
    labs(x = "Cage Treatment", y = "Beta Dispersion",
         title = paste0("Graph created on ", Sys.Date())) +
    scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
                                 "unknown" = "gray", "uncertain" = "gray20")) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          strip.text = element_text(size = 8),
          axis.text.x = element_text(angle = 35, hjust = 1)) +
    supportR::theme_lyon()
  
  # Create nice file name/path
  focal_name <- paste0("06_betadisp-boxplots_", gsub(".csv", "", focal_src), ".png")
  focal_path <- file.path("graphs", "per-dataset-boxplots", focal_name)
  
  # Save locally
  ggsave(filename = focal_path, width = 6, height = 4, units = "in")
 
  # Upload to Drive
  googledrive::drive_upload(media = focal_path, overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1bZiSTpRbcGU3L0_Krdfd9MPGjVeVbKLj"))
   
}

# End ----
