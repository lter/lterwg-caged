## CAGED NCEAS working group ##
#Nico Matallana exploratory script

#Collection, analysis and visualization of climatic variables and NPP for
#CAGED study site locations

# Load Packages -----------------------------------------------------------
librarian::shelf(tidyverse, googledrive, supportR, raster, ncdf4, 
                 dplyr, sp, ,sf, stringr, magrittr, reshape2, tools, 
                 maps, ggplot2, cowplot, ggspatial, ggrepel,
                 rnaturalearth, rnaturalearthdata, httpuv, geodata,
                 QBMS)

#Set-up google drive connection & import metadata ----
googledrive::drive_auth(email = "nicomatamej@gmail.com")

#import tidy metadata
meta.drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od")) %>% 
  dplyr::filter(name == "07_caged_w.meta_finest-scales.csv")

# Check that worked
meta.drive

# Download it
googledrive::drive_download(file = meta.drive$id, type = "csv", overwrite = T,
                            path = file.path("data", meta.drive$name))

# Read it in (latest run: 5/12/2025)
meta.tidy.full <- read.csv(file = file.path("data", "07_caged_w.meta_finest-scales.csv"))

# Extract basic row identifiers & lat/long
meta.tidy.simple = meta.tidy.full %>%
  dplyr::select(source, exp.name, var_aq.or.terr, lat, long) %>%
  distinct() %>% #collapse duplicates
  drop_na() #Remove sites with no lat/longs

# NPP data ----

#export csv of lat/long & ID column to extract npp values from AppEEARS data access portal.

meta.ID = meta.tidy.simple %>%
  mutate(ID = 1:length(.$source)) %>%
  relocate(ID)

meta.npp.export = meta.ID %>%
  dplyr::select(ID, lat, long)

write.csv(meta.npp.export, "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/meta_export1.csv")

#Coordinates file uploaded to AppEARS data access portal to extract values at locations (https://appeears.earthdatacloud.nasa.gov/task/point)
#Data is kg of carbon per m2 per year (500m resolution) between 2000-02-18 to 2024-12-31
#Data source: MOD17A3HGF Version 6.1, https://lpdaac.usgs.gov/products/mod17a3hgfv061/
   # Citation: Running, S., Zhao, M. (2021). MODIS/Terra Net Primary Production Gap-Filled Yearly L4 Global 500m SIN Grid V061. NASA EOSDIS Land Processes Distributed Active Archive Center. Accessed 2025-05-12 from https://doi.org/10.5067/MODIS/MOD17A3HGF.061. Accessed May 12, 2025.

#read in npp data file from google drive and join with meta.ID
#Download from Google Drive
drive_npp.data = googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1WmT8QsIJoty1aSvu1TTWQCkRKFOzBdTi")) %>% 
  dplyr::filter(name == "npp-updated-points-5-12-25-MOD17A3HGF-061-results.csv")

googledrive::drive_download(file = drive_npp.data$id, type = "csv", overwrite = T,
                            path = file.path("data", drive_npp.data$name))

#read in and join
meta.npp.import <- read.csv(file = file.path("data", "npp-updated-points-5-12-25-MOD17A3HGF-061-results.csv")) %>%
  group_by(ID) %>%
  summarise(npp.avg = round(mean(MOD17A3HGF_061_Npp_500m), 3)) %>% #average across years
  left_join(meta.ID, by = "ID")

#inspect data
par(mfrow = c(1,1))
hist(meta.npp.import$npp.avg)
count(meta.npp.import[meta.npp.import$npp.avg > 20000,]$npp.avg) #54 sites with nonsensical npp values (mostly aquatic)
hist(meta.npp.import[meta.npp.import$npp.avg < 20000,]$npp.avg) #sites with <20000 npp

length(meta.tidy.simple[meta.tidy.simple$var_aq.or.terr == "aquatic",]$source) #64 sites aquatic

# WorldClim data ----

#bio datasets use average value across 1970 - 2000 (1 value per year for each variable, ex: average yearly temperature)
#bio vars: https://www.worldclim.org/data/bioclim.html
#resolution: 10 minute x 10 minute cells (18.5km2)
wc.bio.10min.1970_2000 = worldclim_global(var = "bio", res = 10, path = "C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/worldclim/tavg.10min.1970_200")
wc.crs = crs(wc.bio.10min.1970_2000) #save CRS for later use

#plot
plot(wc.bio.10min.1970_2000$wc2.1_10m_bio_1)
  
#Turn dataframe into a spatial object

meta.sp = st_as_sf(meta.tidy.simple, coords = c("long", "lat"), crs = wc.crs) 

#check mapping
par(mfrow=(c(1,1)))
plot(wc.bio.10min.1970_2000$wc2.1_10m_bio_3)
plot(meta.sp, pch = 20, size = 8, col = "red", add = TRUE)

#extract worldclim raster values at points
tavg.pts = extract(wc.bio.10min.1970_2000$wc2.1_10m_bio_1, meta.sp) #extract yearly average temperature at each point
t.sd.pts = extract(wc.bio.10min.1970_2000$wc2.1_10m_bio_4, meta.sp) #extract yearly temperature standard deviation x 100 at each point
ppt.pts = extract(wc.bio.10min.1970_2000$wc2.1_10m_bio_12, meta.sp) #extract yearly total precipitation in mm

#cbind simple metadata with worldclim data
meta.wc.extracts = cbind(meta.tidy.simple, tavg.pts, t.sd.pts, ppt.pts) %>%
  dplyr::select(-ID) %>% #clean up variables
  rename("t.avg.C" = "wc2.1_10m_bio_1") %>% #rename vars
  rename("t.sd.C" = "wc2.1_10m_bio_4") %>%
  rename("ppt.mm" = "wc2.1_10m_bio_12") %>%
  filter(var_aq.or.terr == "terrestrial") #filter out aquatic points

## Inspect data ##
#histrograms
par(mfrow=c(2,2))
hist(meta.wc.extracts$t.avg.C, main = "Avg. Temp. C")
hist(meta.wc.extracts$t.sd.C, main = "Temp S.D. * 100")
hist(meta.wc.extracts$ppt.mm, main = "Precip. mm") #Some outliers above ~2,000
hist(meta.wc.extracts[meta.wc.extracts$ppt.mm < 2000,]$ppt.mm, main = "precip < 2000") #precip under 2000mm cutoff

# TerraClim data ----

#Terraclim dataset: https://www.climatologylab.org/terraclimate.html
#Uses WorldClim & other datasets to interpolate from 1958 up to the present

#prep data
meta.terr = meta.tidy.simple %>%
  mutate(ID = 1:length(.$source)) %>%
  relocate(ID) %>%
  filter(var_aq.or.terr == "terrestrial")

### tmax

#Prepare for-loop for tmax extraction from Terraclim

var = "tmax"

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")

#tmax
nc <- nc_open(baseurlagg)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

#ALWAYS RUN THIS BEFORE RE-RUNNING FOR LOOP!!
tmax.df = data.frame(ID = c(), year = c(), mo = c(), tmax = c()) #create blank dataframe to build in for-loop

#Initialize progress bar
pb <- txtProgressBar(min = 0, max = length(meta.terr$ID), style = 3)

###For loop to extract monthly average tmax values from 1958 - 2025 at each site.
#This takes a long time. 169 points to extract from.
#Make sure to run tmax.df = data.frame(...) above before re-running for-loop!

for (i in 1:length(meta.terr$ID)){ 
  
  x = c(meta.terr[i,6], meta.terr[i,5]) #select coordinates
  
  #Code to turn coordinates into index for data extraction
  flat = match(abs(lat - x[2]) < 1/48, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(lon - x[1]) < 1/48, 1)
  lonindex = which(flon %in% 1)
  start <- c(lonindex, latindex, 1)
  count <- c(1, 1, -1)
  
  
  # read in the full period of record using aggregated files
  data = as.numeric(ncvar_get(nc, varid = var,start = start, count))
  
  #Create df for each site
  sub <- data.frame(ID = rep(meta.terr[i,1], times = length(data)),
                          year = rep(c(1958:2025), each = 12, length.out = length(data)),
                          mo = rep(1:12, length.out = length(data)),
                          tmax = data)
  
  tmax.df = rbind(tmax.df, sub) #rbind all data together
  
  #Print progress
  setTxtProgressBar(pb, i)

}

### DO NOT RE-RUN: Troubleshoot ID 163

#163: 47.8200000, 13.500000, longitude comes up as NA in lonindex
#Trying lat/lon 47.82, 13.45 - worked! changed in original google drive sitelevel-metadata file

#Change in current metadata object
meta.terr$long = if_else(meta.terr$ID == 163, 13.45, meta.terr$long)

#Extract data at new coordinates and rbind to full dataset
x = c(meta.terr[meta.terr$ID == 163, 6], meta.terr[meta.terr$ID == 163, 5])

#Code to turn coordinates into index for data extraction
flat = match(abs(lat - x[2]) < 1/48, 1)
latindex = which(flat %in% 1)
flon = match(abs(lon - x[1]) < 1/48, 1)
lonindex = which(flon %in% 1)
start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)


# read in the full period of record using aggregated files
data = as.numeric(ncvar_get(nc, varid = var,start = start, count))


#When new coordinate has been fixed & data pulled, add to full dataset
sub <- data.frame(ID = rep(163, times = length(data)),
                  year = rep(c(1958:2025), each = 12, length.out = length(data)),
                  mo = rep(1:12, length.out = length(data)),
                  tmax = data)

tmax.df = rbind(tmax.df, sub)


### DO NOT RE-RUN: Trouble shoot ID 178
#original coordinates lat/lon: 32.01195, 85.40892
#test
x = c(meta.terr[meta.terr$ID == 178, 6], meta.terr[meta.terr$ID == 178, 5])

#Code to turn coordinates into index for data extraction
flat = match(abs(lat - x[2]) < 1/48, 1)
latindex = which(flat %in% 1)
flon = match(abs(lon - x[1]) < 1/48, 1)
lonindex = which(flon %in% 1)
start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)

# read in the full period of record using aggregated files
data = as.numeric(ncvar_get(nc, varid = var,start = start, count))

#extraction worked? not sure why for-loop didn't want to work for this guy
sub <- data.frame(ID = rep(178, times = length(data)),
                  year = rep(c(1958:2025), each = 12, length.out = length(data)),
                  mo = rep(1:12, length.out = length(data)),
                  tmax = data)

tmax.df = rbind(tmax.df, sub) #Added to dataset

hist(tmax.df$tmax)


### tmin
#Prepare for-loop for tmin extraction from Terraclim

var = "tmin"

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")

#tmin
nc <- nc_open(baseurlagg)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

#Initialize progress bar
pb <- txtProgressBar(min = 0, max = length(meta.terr$ID), style = 3)

#ALWAYS RUN THIS BEFORE RE-RUNNING FOR LOOP!!
tmin.df = data.frame(ID = c(), year = c(), mo = c(), tmin = c()) #create blank dataframe to build in for-loop

###For loop to extract monthly average tmmin values from 1958 - 2025 at each site.
for (i in 1:length(meta.terr$ID)){ 
  
  x = c(meta.terr[i,6], meta.terr[i,5]) #select coordinates
  
  #Code to turn coordinates into index for data extraction
  flat = match(abs(lat - x[2]) < 1/48, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(lon - x[1]) < 1/48, 1)
  lonindex = which(flon %in% 1)
  start <- c(lonindex, latindex, 1)
  count <- c(1, 1, -1)
  
  
  # read in the full period of record using aggregated files
  data = as.numeric(ncvar_get(nc, varid = var,start = start, count))
  
  #Create df for each site
  sub = data.frame(ID = rep(meta.terr[i,1], times = length(data)),
                    year = rep(c(1958:2025), each = 12, length.out = length(data)),
                    mo = rep(1:12, length.out = length(data)),
                    tmin = data)
  
  tmin.df = rbind(tmin.df, sub) #rbind all data together
  
  #Print progress
  setTxtProgressBar(pb, i)
  
}

hist(tmin.df$tmin)


###ppt
#Prepare for-loop for ppt extraction from Terraclim

var = "ppt"

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")

#ppt
nc <- nc_open(baseurlagg)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

#Initialize progress bar
pb <- txtProgressBar(min = 0, max = length(meta.terr$ID), style = 3)

#ALWAYS RUN THIS BEFORE RE-RUNNING FOR LOOP!!
ppt.df = data.frame(ID = c(), year = c(), mo = c(), ppt = c()) #create blank dataframe to build in for-loop

###For loop to extract monthly total ppt values from 1958 - 2025 at each site.
for (i in 1:length(meta.terr$ID)){ 
  
  x = c(meta.terr[i,6], meta.terr[i,5]) #select coordinates
  
  #Code to turn coordinates into index for data extraction
  flat = match(abs(lat - x[2]) < 1/48, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(lon - x[1]) < 1/48, 1)
  lonindex = which(flon %in% 1)
  start <- c(lonindex, latindex, 1)
  count <- c(1, 1, -1)
  
  
  # read in the full period of record using aggregated files
  data = as.numeric(ncvar_get(nc, varid = var,start = start, count))
  
  #Create df for each site
  sub = data.frame(ID = rep(meta.terr[i,1], times = length(data)),
                   year = rep(c(1958:2025), each = 12, length.out = length(data)),
                   mo = rep(1:12, length.out = length(data)),
                   ppt = data)
  
  ppt.df = rbind(ppt.df, sub) #rbind all data together
  
  #Print progress
  setTxtProgressBar(pb, i)
  
}


### DO NOT RE-RUN:Troubleshoot ID 189
id.189 = meta.worldclim.full %>%
  filter(ID == 189)

#GPS point is in ocean: try adjusting longitude to put it on land
#original lat/long: 35.5	-121.1. Trying -121.0, 35.5
#Worked! Changed lat/long in google drive site.level metadata

#for loop to extract 3 terraclimate variables for ID 189 and add to full data

x = c(-121.0, 35.5) #select coordinates, Long/lat

var = c("tmax", "tmin", "ppt")

#Create DF to cbind data into
id.189 = list(ID = rep(189, times = 804),
              year = rep(c(1958:2025), each = 12, length.out = 804),
              mo = rep(1:12, length.out = 804))
  
  data.frame(ID = rep(189, times = 804),
           year = rep(c(1958:2025), each = 12, length.out = 804),
           mo = rep(1:12, length.out = 804))

for(i in var){
  # enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
  baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",i),"_1958_CurrentYear_GLOBE.nc")
  
  nc <- nc_open(baseurlagg)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")

  #Code to turn coordinates into index for data extraction
  flat = match(abs(lat - x[2]) < 1/48, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(lon - x[1]) < 1/48, 1)
  lonindex = which(flon %in% 1)
  start <- c(lonindex, latindex, 1)
  count <- c(1, 1, -1)


  # read in the full period of record using aggregated files
  id.189[[i]] = as.numeric(ncvar_get(nc, varid = i,start = start, count))
  
  #data = assign(paste(i), as.numeric(ncvar_get(nc, varid = i,start = start, count)))

  #id.189 = cbind(id.189, data)

}

id.189 = as.data.frame(id.189)




#troubleshoot Zero ppt values
ppt.check = ppt.df %>%
  filter(ppt == 0) %>%
  group_by(ID, year) %>%
  summarise(count = n())
#No location has a full 12 months of zeros, at most 6 months of zeros
#proceed with caution

#Combine datasets 
terraclim.full = tmax.df %>%
  left_join(tmin.df, by = c("ID", "year", "mo")) %>%
  left_join(ppt.df, by = c("ID", "year", "mo")) %>%
  drop_na() %>% #remove old ID 189 rows with NAs
  rbind(id.189) #rbind new ID 189 rows with data


#Calculate aggregated values
terraclim.avgs = terraclim.full %>%
  filter(year > 1994) %>% #filter to the last 30 years of the dataset (up until 2024)
  mutate(tmid = (tmax + tmin)/2) %>% #Average tmax and tmid
  group_by(ID, year) %>% 
  summarise(t.avg.yr = mean(tmid), #First find average yearly temperature, within-year s.d. and yearly total ppt
            t.sd.yr = sd(tmid),
            ppt.total.yr = sum(ppt)) %>%
  ungroup() %>%
  group_by(ID) %>%
  summarise(t.avg = mean(t.avg.yr), #Total average temperature
            t.avg.sd = mean(t.sd.yr), #Average yearly standard deviation in temperature
            ppt.avg = mean(ppt.total.yr)) %>% #average yearly ppt
  left_join(meta.terr, by = "ID") #Join in basic site metadata
  
#
# Correlations ----
pairs(meta.wc.extracts[meta.wc.extracts$ppt.mm < 2000,c(4:8)])

#inspect temp ~ temp.sd
ggplot(meta.wc.extracts, aes(x = t.avg.C, y = t.sd.C)) +
  geom_point() +
  geom_smooth(method = "lm")

temp.sd.model = lm(t.sd.C ~ t.avg.C, data = meta.wc.extracts)
summary(temp.sd.model) #R2 = .57

#inspect vars ~ lat, temp ~ ppt
par(mfrow=c(2,2))
plot(meta.wc.extracts$lat, meta.wc.extracts$t.avg.C, main = "Temp ~ lat")
plot(meta.wc.extracts$lat, meta.wc.extracts$t.sd.C, main = "Temp SD ~ lat")
plot(meta.wc.extracts$lat, meta.wc.extracts$ppt.mm, main = "precip ~ lat")
plot(meta.wc.extracts[meta.wc.extracts$ppt.mm < 2000,]$t.avg.C, meta.wc.extracts[meta.wc.extracts$ppt.mm < 2000,]$ppt.mm, main = "ppt ~ temp")


## Check correlation with npp data ##
wc.npp.meta = meta.wc.extracts %>%
  left_join(meta.npp.import[,2:4], by = c("source", "exp.name")) %>%
  filter(npp.avg < 20000) %>%
  filter(ppt.mm < 2000)
pairs(wc.npp.meta[,4:9])
pairs(wc.npp.meta[,6:9])

#model NPP from climate vars
npp.temp.tempsd.ppt.lm = lm(npp.avg ~ t.avg.C + ppt.mm + t.sd.C, data = wc.npp.meta) #all climate vars
summary(npp.temp.tempsd.ppt.lm) #R2 = 0.31

npp.tempsd.lm.ppt.lm = lm(npp.avg ~ ppt.mm + t.sd.C, data = wc.npp.meta) #subtract avg. temp
summary(npp.tempsd.lm.ppt.lm) #R2 = 0.28 


### Check correlations with terraclimate data
#Join terraclimate with worldclim and npp data

meta.wc.npp.terraclim = wc.npp.meta %>%
  left_join(terraclim.avgs, by = c("source", "exp.name")) %>%
  dplyr::select(-ID, -lat.y, -long.y, -var_aq.or.terr.y)

#Visualize correlations

pairs(meta.wc.npp.terraclim[,4:12])

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
  geom_point(data = meta.tidy.simple, aes(x = long, y = lat, color = var_aq.or.terr),
             shape = 19, alpha = 0.5, size = 4) +
  scale_color_manual(values=c('#0072B2','#D55E00'),
                    breaks = c("aquatic","terrestrial"),
                   labels = c("Aquatic","Terrestrial")) +
  guides(color=guide_legend(bquote(paste("Systems")))) +
  #guides(size=guide_legend("")) +
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

# Sites colored by NPP ----
wc.map.npp <- ggplot() +
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
  geom_point(data = meta.npp.import[meta.npp.import$npp.avg < 20000 & meta.npp.import$var_aq.or.terr == "terrestrial",],
             aes(x = long, y = lat, color = npp.avg), #outliers >20k filtered out
             shape = 19, alpha = 0.5, size = 4) +
  scale_color_gradientn(colours = rainbow(5), trans = 'reverse') +
  guides(color=guide_colourbar(bquote(paste("Avg. NPP\nkgC/m2/yr")))) +
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

wc.map.npp


# Sites colored by WorldClim Avg. temp ----
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
  geom_point(data = meta.wc.extracts, aes(x = long, y = lat, color = t.avg.C),
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



# Sites colored by WorldClim yearly temp s.d. ----
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
  geom_point(data = meta.wc.extracts, aes(x = long, y = lat, color = t.sd.C), #replace color var with t.avg.C, t.sd.C & ppt.mm to inspect
             shape = 19, alpha = 0.8, size = 4) +
  scale_color_gradient(low = "red", high = "blue") +
  guides(color=guide_colourbar(bquote(paste("Average yearly temp\nstandard deviation x 100")))) +
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

# Sites colored by WorldClim yearly total precip mm ----
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
  geom_point(data = meta.wc.extracts[meta.wc.extracts$ppt.mm < 2000,], aes(x = long, y = lat, color = ppt.mm), #precip values above 2000 removed
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

# Sites colored by Terraclim vars ----

tc.map <- ggplot() +
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
  geom_point(data = terraclim.avgs, aes(x = long, y = lat, color = t.avg),
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

tc.map





# Next steps ----

# Acquire & analyze ocean temperature dataset for marine sites
# Identify variable names for export
# Export data into appropriate google drive folder
# upload code into appropriate github file or folder
# incorporate data into modeling workflows

# End ----
