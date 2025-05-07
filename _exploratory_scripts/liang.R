## --------------------------------------------------------------- ##
# CAGED site map Mega Figure
## --------------------------------------------------------------- ##
# Written by: Maowei Liang, ...

# Purpose: to make a map of all sites based on meta_v2
## 

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR, raster, ncdf4, 
                 dplyr, sp, stringr, magrittr, reshape2, tools, 
                 maps, ggplot2, cowplot, ggspatial, ggrepel,
                 rnaturalearth, rnaturalearthdata)

# Create needed folder(s)
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Metadata ----
## ------------------------------------------- ##

# Identify the relevant GoogleSheet
meta_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AFR2XIdw_sKbUk9PVA")) %>% 
  dplyr::filter(name == "sitelevel-metadata")

# Check that worked
meta_drive

# Download it
googledrive::drive_download(file = meta_drive$id, type = "csv", overwrite = T,
                            path = file.path("data", meta_drive$name))

# Read it in
meta_v1 <- read.csv(file = file.path("data", "sitelevel-metadata.csv"))

# Check structure
dplyr::glimpse(meta_v1)

## ------------------------------------------- ##
# Standardize Lat/Long Format ----
## ------------------------------------------- ##

# Check current lat/long formats
sort(unique(meta_v1$lat))

# Do needed repairs
meta_v2 <- meta_v1 %>% 
  # Rename & duplicate original lat/long cols
  dplyr::mutate(lat.orig = lat,
                long.orig = long) %>% 
  # Replace degree symbol with period
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "°|º", replacement = ".", x = .))) %>% 
  # Remove unwanted characters
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "’|'|′|\\\"", replacement = "", x = .))) %>% 
  # Replace N/S and E/W with negative symbols as needed
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ ifelse(stringr::str_detect(string = ., pattern = "S"),
                                              yes = paste0("-", .), no = .))) %>% 
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ ifelse(stringr::str_detect(string = ., pattern = "W"),
                                              yes = paste0("-", .), no = .))) %>% 
  # Then remove superseded cardinal direction letters
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "N|S|E|W", replacement = "", x = .))) %>% 
  # Remove spaces after periods
  dplyr::mutate(dplyr::across(.cols = lat:long,
                              .fns = ~ gsub(pattern = "\\. ", replacement = "\\.", x = .))) %>% 
  # Split based on periods
  tidyr::separate_wider_delim(cols = lat, delim = ".", names = c("tmp__lat", "tmp__lat2"),
                              too_many = "merge", too_few = "align_start") %>% 
  tidyr::separate_wider_delim(cols = long, delim = ".", names = c("tmp__long", "tmp__long2"),
                              too_many = "merge", too_few = "align_start") %>% 
  # Remove periods from all four temp columns
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("tmp__"),
                              .fns = ~ gsub(pattern = "\\.", replacement = "", x = .))) %>% 
  # Recombine temp columns with period between first and second
  dplyr::mutate(lat = ifelse(!is.na(tmp__lat) & !is.na(tmp__lat2),
                             yes = paste0(tmp__lat, ".", tmp__lat2),
                             no = "")) %>% 
  dplyr::mutate(long = ifelse(!is.na(tmp__long) & !is.na(tmp__long2),
                              yes = paste0(tmp__long, ".", tmp__long2),
                              no = "")) %>% 
  # Remove temp columns
  dplyr::select(-dplyr::starts_with("tmp__")) %>% 
  # Reorder some other columns
  dplyr::relocate(lat.orig:long, .after = exp.name)

# Re-check formats
sort(unique(meta_v2$lat))

# Check structure more generally
dplyr::glimpse(meta_v2)



## ------------------------------------------- ##
# Obtaining climate data ----
## ------------------------------------------- ##

# Load the CRU TS precipitation dataset into R 
pre <- brick("E:/UMN/Research/Collaborations/CAGED_lter/code_cru_climate/cru_ts4.09.1901.2024.pre.dat.nc", varname="pre")
tmp <- brick("E:/UMN/Research/Collaborations/CAGED_lter/code_cru_climate/cru_ts4.09.1901.2024.tmp.dat.nc", varname="tmp")

# Create an ID column by combining a numeric sequence with the 'exp.name' column
meta_v2$site_id <- paste0(seq_len(nrow(meta_v2)), "_", meta_v2$exp.name)

# Convert the lat and long to numeric variables as lat_num and long_num
meta_v2$lat_num <- as.numeric(meta_v2$lat)
meta_v2$long_num <- as.numeric(meta_v2$long)

# Check the updated data frame
head(meta_v2)

# Select just the lat/long and site ID (3-letter code for site) columns and make a df
caged_sites <- meta_v2 %>%
  dplyr::select(site_id, long_num, lat_num) %>%
  dplyr::rename(lon=long_num, lat=lat_num)

# This can be removed after acess all NA values
caged_sites$lon[is.na(caged_sites$lon)] <- 0
caged_sites$lat[is.na(caged_sites$lat)] <- 0

caged_sites <- as.data.frame(caged_sites)
variable.names(caged_sites)

# Make sure caged_sites has only two columns (lon and lat) for extraction
coordinates(caged_sites) <- c('lon', 'lat')

# Extract climate data from the RasterBrick as a data.frame
pre.sites <- data.frame(raster::extract(pre, caged_sites))
tmp.sites <- data.frame(raster::extract(tmp, caged_sites))

# Add site_id as a new column to the extracted data
pre.sites$site_id <- caged_sites$site_id
tmp.sites$site_id <- caged_sites$site_id

# Reshape to long format
pre.long <- melt(pre.sites, varnames = c("variable"), value.name = "precip_mm")
tmp.long <- melt(tmp.sites, varnames = c("variable"), value.name = "temp_C")

# Extract year from variable name
pre.long$year <- as.numeric(substr(pre.long$variable, 2, 5))
tmp.long$year <- as.numeric(substr(tmp.long$variable, 2, 5))

# Sum monthly precipitation per year per site
annual.pre <- pre.long %>%
  group_by(site_id, year) %>%
  summarize(total_precip_mm = sum(precip_mm, na.rm = TRUE)) %>%
  summarize(mean_total_precip_mm = mean(total_precip_mm), 
            sd_total_precip_mm = sd(total_precip_mm))

# Average monthly temperature per year per site
annual.tmp <- tmp.long %>%
  group_by(site_id, year) %>%
  summarize(mean_temp_C = mean(temp_C, na.rm = TRUE)) %>%
  summarize(mean_annual_temp_C = mean(mean_temp_C), 
            sd_annual_temp_C = sd(mean_temp_C))

# Merge the two annual summaries
annual.climate <- merge(annual.pre, annual.tmp, by = c("site_id"))

# Merge with caged_sites using site_id as the key
meta_v2_merged_clim <- merge(meta_v2, annual.climate, by = "site_id")

head(meta_v2_merged_clim)

## ------------------------------------------- ##
# Making a map ----
## ------------------------------------------- ##

# Get a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Make a map
sites.map <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  annotation_scale(location = "bl", 
                   pad_x = unit(0.4, "in"), 
                   pad_y = unit(0.7, "in"),
                   height = unit(0.2, "cm"), 
                   width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), 
                         pad_y = unit(0.8, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = meta_v2_merged_clim, aes(x = long_num, y = lat_num, size = mean_annual_temp_C, color = aq.or.terr), 
             shape = 19, alpha = 0.5) +
  scale_color_manual(values=c('#0072B2','#D55E00'),
                     breaks = c("aquatic",
                                "terrestrial"),
                     labels = c("Aquatic",
                                "Terrestrial")) +
  guides(color=guide_legend(bquote(paste("Systems")))) +
  guides(size=guide_legend("Temperature (ºC)")) +
  #labs(tag = "A") +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme(#legend.position = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10, family = "Arial", color = "black"),
    panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", size = 0.2), 
    panel.background = element_rect(fill = "aliceblue"), 
    panel.border = element_rect(fill = NA),
    axis.text.y = element_text(size = 12, family = "Arial", color = "black"),
    axis.text.x = element_text(size = 12, family = "Arial", color = "black"),
    axis.title = element_text(size = 14, family = "Arial", color = "black"))
