## --------------------------------------------------------------- ##
# CAGED Exploration - Abundance Histograms (by Dataset)
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR, update_all= TRUE)

# Create needed folder(s)
dir.create(path = file.path("graphs"), showWarnings = F)
dir.create(path = file.path("graphs", "per-dataset-abun-viols"), showWarnings = F)

# Authorize GoogleDrive
googledrive::drive_auth()

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
caged_v1 <- read.csv(file.path("data", "02_caged_tidied.csv"))

# Check structure
dplyr::glimpse(caged_v1)

## ------------------------------------------- ##
# Exploratory Graphs ----
## ------------------------------------------- ##

# Check structure
dplyr::glimpse(caged_v1)

# Do any needed pre-visualization wrangling
abun_viz <- caged_v1
  ## Currently no needed wrangling

# Re-check structure
dplyr::glimpse(abun_viz)

# Make some exploratory graphs!
for(focal_src in sort(unique(abun_viz$source))){
  # focal_src <- "soler_argentina_native-alienplants_2015-2020_herbivores_vegetation.csv"
  
  # Progress message
  message("Making exploratory boxplots for file: '", focal_src, "'")
  
  # Subset data
  focal_sub <- dplyr::filter(.data = abun_viz, source == focal_src)
  
  # Create graph
  ggplot(focal_sub, aes(x = exp.name, y = abundance)) +
    # geom_boxplot(alpha = 0.4, fill = "#00aa50") +
    geom_jitter(alpha = 0.1, width = 0.15, size = 2.5, pch = 21, fill = "#000000") +
    geom_violin(alpha = 0.8, fill = "#00aa50") +
    facet_wrap(. ~ source) +
    labs(x = "Experiment Name", y = "Abundance",
         title = paste0("Graph created on ", Sys.Date())) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          strip.text = element_text(size = 8)) +
    supportR::theme_lyon()
  
  # Create nice file name/path
  focal_name <- paste0("02a_abun-violins_", gsub(".csv", "", focal_src), ".png")
  focal_path <- file.path("graphs", "per-dataset-abun-viols", focal_name)
  
  # Save locally
  ggsave(filename = focal_path, width = 6, height = 4, units = "in")
  
  # Upload to Drive
  googledrive::drive_upload(media = focal_path, overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1kejYoO5GD_di6VxUX2rvrkDTWAruKT-4"))
  
}

# End ----
