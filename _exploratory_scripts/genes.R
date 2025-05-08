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


###### Exploratory Plots ######

#consumer richness .category



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


###start here thursday ####


##### canibalizing jamie's and marc's script to explore ####

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, lme4, 
                 performance, lubridate, car, 
                 njlyon0/supportR, sjPlot, emmeans, ggpubr) #, update_all= TRUE)

# Create needed folder(s)
#dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
alldata_v1 <- read.csv(file.path("data", "06_caged_with-metadata_finest-scales.csv"))


## ------------------------------------------- ##
# Data Wrangling
## ------------------------------------------- ##
glimpse(alldata_v1) # 11,875 rows, 51 columns

supportR::num_check(data = alldata_v1, col = "exp.name.spatialextent.category")
sort(unique(alldata_v1$betadisp.design.level))


#get years and richness into number form
alldata_v1$year.start.exclosure <- year(as.Date(as.character(alldata_v1$year.start.exclosure), format = "%Y"))
alldata_v1$year.end.exclosure <- year(as.Date(as.character(alldata_v1$year.end.exclosure), format = "%Y"))
#alldata_v1$consumer.richness <- as.numeric(alldata_v1$consumer.richness) #some stupid shit like ">10" in here, so this gives NA
alldata_v1$lat <- as.numeric(alldata_v1$lat)

#### lg

#standardize exclusion.duration column
alldata_v1$exclusion.duration #many different formats, needs to be cleaned - I'm not sure if thats the best way to go

alldata_v1 <- alldata_v1 %>%
  mutate( 
    exclusion.digits = as.numeric(str_extract(exclusion.duration, "\\d+")),
    exclusion.duration.clean = case_when(
      # Convert years to months
      str_detect(exclusion.duration, "year") ~ exclusion.digits * 12,
      
      # Convert weeks to months (approximate)
      str_detect(exclusion.duration, "week") ~ exclusion.digits / 4.345,
      
      # Keep months as-is
      str_detect(exclusion.duration, "month") ~ exclusion.digits,
      
      # Plain numbers assumed to be months
     # str_detect(exclusion.duration, "^\\d+$") ~ as.numeric(exclusion.duration), ####risky
      
      # Everything else becomes NA
      TRUE ~ NA_real_
    )
  ) %>% select(-exclusion.digits)

#checking
sort(unique(alldata_v1$exclusion.duration.clean))


## ------------------------------------------- ##
# Exploratory Figures 
## ------------------------------------------- ##

#subset only caged uncaged
alldata_v2 <- alldata_v1 %>%
  filter(cage.treatment_std %in% c("caged", "uncaged")) 

### Figure 1 Successional stage ####

### Figure 2 Aquatic vs. Terrestrial ####
library(supportR)
unique(alldata_v1$aq.or.terr)
sort(unique(alldata_v3$betadisp.comm.dist))
num_check(alldata_v3, col="betadisp.comm.dist") #check if it's number or NA

alldata_v3 <- alldata_v2 %>%
  filter(aq.or.terr %in% c("aquatic", "terrestrial")) 

unique(alldata_v3$aq.or.terr)

aq.terr.boxplot <-
  alldata_v3 %>% 
  ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  geom_boxplot() +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  labs(y = expression("Beta dispersion"), x = "") +
  theme_bw(base_size=12)  +
  theme() +
  facet_wrap(~aq.or.terr)
  
aq.terr.boxplot

#### Figure 3 Eco type #####

#remove NAs for now
alldata_v3 <- alldata_v2 %>%
  filter(!is.na(ecotype1) & ecotype1 != "")

ecotype.boxplot <-
  alldata_v3 %>% 
  ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  geom_boxplot() +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  labs(y = expression("Beta dispersion"), x = "") +
  theme_bw(base_size=12)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank()) +
  facet_wrap(~ecotype1)

ecotype.boxplot

#save locally

### Figure 4 Gamma diversity #### - do later

#### Figure 5 Herbivore richness ####
#need new clean metadata for continuous herb richness

unique(alldata_v2$consumer.richness.category)
alldata_v5 <- alldata_v2 %>%
  filter(consumer.richness.category %in% c("low", "high")) 

consumer.richness <-
  alldata_v5 %>% 
  ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  geom_boxplot() +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  labs(y = expression("Beta dispersion"), x="") +
  theme_bw(base_size=12)  +
  theme() +
  ggtitle("Consumer richness")+
  facet_wrap(~consumer.richness.category)

consumer.richness

##split aquatic terrestrial

consumer.richness <-
  alldata_v5 %>% 
  ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist)) + 
  geom_boxplot() +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  labs(y = expression("Beta dispersion"), x="") +
  theme_bw(base_size=12)  +
  theme() +
  scale_fill_manual(values = c("aquatic" = "royalblue", "terrestrial" = "green4")) + 
  ggtitle("Consumer richness")+
  facet_wrap(~consumer.richness.category + aq.or.terr)

consumer.richness

alldata_v5 %>% 
  ggplot(data = ., aes(x = cage.treatment_std, y = betadisp.comm.dist, fill=aq.or.terr)) + 
  geom_boxplot() +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  labs(y = expression("Beta dispersion"), x="") +
  theme_bw(base_size=12)  +
  theme() +
  scale_fill_manual(values = c("aquatic" = "royalblue", "terrestrial" = "green4")) + 
  ggtitle("Consumer richness")+
  facet_wrap(~aq.or.terr + consumer.richness.category )



#### Figure 6 Exclusion time #####

ggplot(alldata_v2, aes(x = exclusion.duration.clean, y =betadisp.comm.dist, color = aq.or.terr)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = T) +
  labs(x = "Exclusion Duration (months)", y = "beta.disp.comm.dist") +
  scale_color_manual(values = c("aquatic" = "darkblue", "terrestrial" = "green4")) + 
  facet_wrap(~ cage.treatment_std)+
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


#subset time

alldata_v3 <- alldata_v2 %>%
  filter(exclusion.duration.clean < 250)

ggplot(alldata_v3, aes(x = exclusion.duration.clean, y =betadisp.comm.dist, color = aq.or.terr)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = T) +
  labs(x = "Exclusion Duration (months)", y = "beta.disp.comm.dist") +
  scale_color_manual(values = c("aquatic" = "darkblue", "terrestrial" = "green4")) + 
  facet_wrap(~ cage.treatment_std)+
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


#### Figure 7 Size of cage #### -- needs lots of standardizing

#### Figure 8 Latitude ####

alldata_v3 <- alldata_v2$lat 
  
alldata_v3 <- alldata_v2 %>%
  filter(aq.or.terr %in% c("aquatic", "terrestrial")) 

ggplot(alldata_v3, aes(x = lat, y =betadisp.comm.dist, color=aq.or.terr)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "latitude", y = "beta.disp.comm.dist") +
  scale_color_manual(values = c("aquatic" = "darkblue", "terrestrial" = "green4")) + 
  facet_wrap(~ aq.or.terr)+
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggplot(alldata_v2, aes(x = lat, y =betadisp.comm.dist)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "latitude", y = "beta.disp.comm.dist") +
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

alldata_v2

#absolute latitude
alldata_v3$absolute_lat <- abs(alldata_v3$lat)

ggplot(alldata_v3, aes(x = absolute_lat, y =betadisp.comm.dist, color=aq.or.terr)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "absolute latitude", y = "beta.disp.comm.dist") +
  scale_color_manual(values = c("aquatic" = "darkblue", "terrestrial" = "green4")) + 
  facet_wrap(~ aq.or.terr)+
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


#bins for climate zones
alldata_v3$climate.zone

#reorder
alldata_v3$climate.zone <- 
  factor(alldata_v3$climate.zone, levels = c("polar", "subpolar", "temperate", "subtropical", "tropical"))

  
climate.zone <-
  alldata_v3 %>% 
  ggplot(data = ., aes(x = climate.zone, y = betadisp.comm.dist, fill = aq.or.terr)) + 
  geom_boxplot() +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  labs(y = expression("Beta dispersion"), x = "") +
  scale_fill_manual(values = c("aquatic" = "royalblue", "terrestrial" = "green4")) + 
  theme_bw(base_size=12)  +
  theme() +
  facet_wrap(~aq.or.terr)
climate.zone


### Figure 9 productivity #### - later

### Figure 10 temperature #### - later

####Figure 11 Herbivore size #### - later





