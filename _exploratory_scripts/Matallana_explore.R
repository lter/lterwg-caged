#CAGED NCEAS working group
#Nico Matallana exploratory script


# Load Packages -----------------------------------------------------------
librarian::shelf(tidyverse, googledrive, supportR, raster, ncdf4, 
                 dplyr, sp, ,sf, stringr, magrittr, reshape2, tools, 
                 maps, ggplot2, cowplot, ggspatial, ggrepel,
                 rnaturalearth, rnaturalearthdata, httpuv, geodata)

#Set-up google drive connection ----
googledrive::drive_auth(email = "nicomatamej@gmail.com")

#import tidy metadata
meta.drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")) %>% 
  dplyr::filter(name == "07_caged_w.meta_finest-scales")

# Check that worked
meta.drive

# Download it
googledrive::drive_download(file = meta.tidy$id, type = "csv", overwrite = T,
                            path = file.path("data", meta.tidy$name))

# Read it in
meta.tidy <- read.csv(file = file.path("data", "07_caged_w.meta_finest-scales.csv"))

### REMOVE NEXT 2 CHUNKS ONCE THE RIGHT TIDY METADATA FILE IS IDENTIFIED

## ------------------------------------------- ##
# Metadata processing ----
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
# Standardize Lat/Long Format Y prep for global climate models ----
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
sort(unique(meta_v2$long))

#My code:

#Create column with as.numeric lat/longs
meta_v2$lat_num = as.numeric(meta_v2$lat)
meta_v2$long_num = as.numeric(meta_v2$long)

meta_latlong = meta_v2 %>%
  dplyr::select(source, lat.orig, long.orig, lat, long, lat_num, long_num) #subset vars

meta_v2$exp.id = 1:length(meta_v2$exp.name) #create ID column for re-joining later
meta_v2 = meta_v2 %>% dplyr::relocate(exp.id) #move to first column

meta_export = meta_v2 %>%
  dplyr::select(exp.id, lat_num, long_num) #subset basic gps data for uploading to model websites

write.csv(meta_export, "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/meta_export1.csv")

#Write full metadata fill with numeric lat/long columns
write.csv(meta_v2, "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/USGS Productivity Dataset/meta_v2_num.lat.long.csv")

## -------------------------------------------- ##
# Import & explore productivity dataset ----
## -------------------------------------------- ##

npp = read.csv("C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/USGS Productivity Dataset/Productivity-MOD17A3HGF-061-results.csv")

hist(npp$MOD17A3HGF_061_Npp_500m)

npp_avg = npp %>%
  group_by(ID, Latitude, Longitude) %>%
  summarise(npp.mean = mean(MOD17A3HGF_061_Npp_500m)) %>%
  ungroup()

meta.npp = meta_v2 %>%
  left_join(npp_avg, by = c("exp.id" = "ID"))
  
write.csv(meta.npp, "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/meta.npp.csv")

npp_filt = npp_avg %>%
  filter(npp.mean > 15000) #%>%
  dplyr::select(-npp.mean)

write.csv(npp_filt, "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/meta_export2.csv")

#import dataset with outlier locations re-downloaded
npp_filt.avg = read.csv("C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/USGS Productivity Dataset/Npp-sub-MOD17A3HGF-061-results.csv")
  
# NPP dataset has 65 nonsensical values, abandoning pursuit 5/7/2025


# Import & explore WorldClim datasets ----

#Download data monthly data... consider ignoring
wc.tmin.10min.1970_2000 = worldclim_global(var = "tmin", res = 10, path = "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/worldclim/tmin.10min.1970_200") 
wc.tmax.10min.1970_2000 = worldclim_global(var = "tmax", res = 10, path = "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/worldclim/tmax.10min.1970_200") 
wc.prec.10min.1970_2000 = worldclim_global(var = "prec", res = 10, path = "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/worldclim/prec.10min.1970_200") 

#bio datasets use average across time period (1 value per year for each variable, ex: average yearly temperature)
#bio vars: https://www.worldclim.org/data/bioclim.html
#resolution: 10 minute x 10 minute cells (18.5km2)
wc.bio.10min.1970_2000 = worldclim_global(var = "bio", res = 10, path = "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/worldclim/tavg.10min.1970_200")
wc.crs = crs(wc.bio.10min.1970_2000) #save CRS for later use

#plot
plot(wc.bio.10min.1970_2000$wc2.1_10m_bio_1)

#Prepare metadata for spatial points conversion
meta.pre.sp = meta_v2 %>%
  dplyr::select(long_num, lat_num, exp.id,source,exp.name) %>% #simplify df
  drop_na() #drop rows with lat/long NAs
  
#Turn dataframe into a spatial object
meta.sp = st_as_sf(meta.pre.sp, coords = c("long_num", "lat_num"), crs = wc.crs) 

#check plots
plot(wc.bio.10min.1970_2000$wc2.1_10m_bio_3)
plot(meta.sp, pch = 20, size = 8, col = "red", add = TRUE)

#extract worldclim raster values at points
tavg.pts = extract(wc.tavg.10min.1970_2000$wc2.1_10m_bio_1, meta.sp) #extract yearly average temperature at each point
t.sd.pts = extract(wc.bio.10min.1970_2000$wc2.1_10m_bio_4, meta.sp) #extract yearly temperature standard deviation x 100 at each point
ppt.pts = extract(wc.bio.10min.1970_2000$wc2.1_10m_bio_12, meta.sp) #extract yearly total precipitation in mm

#cbind simple metadata with worldclim data
meta.wc.extracts = cbind(meta.pre.sp, tavg.pts, t.sd.pts, ppt.pts) %>%
  dplyr::select(-ID) %>% #clean up variables
  left_join(meta_v2[,c(1,10)], by = "exp.id") %>% #join in "aquatic or terrestrial" variable
  filter(var_aq.or.terr == "terrestrial") %>% #filter out aquatic points
  rename("t.avg.C" = "wc2.1_10m_bio_1") %>%
  rename("t.sd.C" = "wc2.1_10m_bio_4") %>%
  rename("ppt.mm" = "wc2.1_10m_bio_12")

## Quick plots & correlations ##
#histrograms
par(mfrow=c(2,2))
hist(meta.wc.extracts$t.avg.C, main = "Avg. Temp. C")
hist(meta.wc.extracts$t.sd.C, main = "Temp S.D. * 100")
hist(meta.wc.extracts$ppt.mm, main = "Precip. mm") #Some outliers above ~2,000
hist(meta.wc.extracts[meta.wc.extracts$ppt.mm < 2000,]$ppt.mm, main = "precip < 2000") #precip under 2000mm cutoff

#Correlations
pairs(meta.wc.extracts[,c(1,2,6:8)])
par(mfrow=c(2,2))
plot(meta.wc.extracts$lat_num, meta.wc.extracts$t.avg.C, main = "Temp ~ lat")
plot(meta.wc.extracts$lat_num, meta.wc.extracts$t.sd.C, main = "Temp SD ~ lat")
plot(meta.wc.extracts$lat_num, meta.wc.extracts$ppt.mm, main = "precip ~ lat")
plot(meta.wc.extracts$t.avg.C, meta.wc.extracts$ppt.mm, main = "ppt ~ temp")

### Visualize maps ----

# Get a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Sites colored by aq & ter ----
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
  geom_point(data = meta_v2, aes(x = long_num, y = lat_num, color = var_aq.or.terr), #
             shape = 19, alpha = 0.5, size = 4) +
  scale_color_manual(values=c('#0072B2','#D55E00'),
                    breaks = c("aquatic","terrestrial"),
                   labels = c("Aquatic","Terrestrial")) +
  guides(color=guide_legend(bquote(paste("Systems")))) +
  guides(size=guide_legend("Yearly avg Npp (kgC/m2)")) +
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

sites.map

# Sites colored by Avg. temp ----
wc.map.temp <- ggplot() +
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
  geom_point(data = meta.wc.extracts, aes(x = long_num, y = lat_num, color = t.avg.C), #replace color var with t.avg.C, t.sd.C & ppt.mm to inspect
             shape = 19, alpha = 0.5, size = 4) +
  scale_color_gradientn(colours = rainbow(5), trans = 'reverse') +
  guides(color=guide_colourbar(bquote(paste("Avg. Temp (C)")))) +
  #guides(size=guide_legend("Average Yearly Temperature (C)")) +
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

wc.map.temp



# Sites colored by yearly temp s.d. ----
wc.map.temp.sd <- ggplot() +
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
  geom_point(data = meta.wc.extracts, aes(x = long_num, y = lat_num, color = t.sd.C), #replace color var with t.avg.C, t.sd.C & ppt.mm to inspect
             shape = 19, alpha = 0.8, size = 4) +
  scale_color_gradient(low = "white", high = "blue") +
  guides(color=guide_colourbar(bquote(paste("Average yearly Temp\nStandard Deviation x 100")))) +
  #guides(size=guide_legend("Average Yearly Temperature (C)")) +
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

wc.map.temp.sd

# Sites colored by yearly total precip mm ----
wc.map.ppt <- ggplot() + #NOTE: high precip outliers removed!
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
  geom_point(data = meta.wc.extracts[meta.wc.extracts$ppt.mm < 2000,], aes(x = long_num, y = lat_num, color = ppt.mm), #precip values above 2000 removed
             shape = 19, alpha = 0.5, size = 4) +
  scale_color_gradientn(colours = rainbow(5)) +
  guides(color=guide_colourbar(bquote(paste("Avg. Total Yearly precip (mm)")))) +
  #guides(size=guide_legend("Average Yearly Temperature (C)")) +
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

wc.map.ppt


# End ----