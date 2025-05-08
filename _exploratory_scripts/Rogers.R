
## --------------------------------------------------------------- ##
# CAGED Beta disp.diff figs
## --------------------------------------------------------------- ##
#Cannibalized from Nick, Jamie, Luisa, Marc
#Written by: Ryan Rogers

#Figure(s) beta diversity mean difference

#Aquatic vs terrestrial comparisons, 
#Caged treatment standardization, consumer richness.category, lat

#################################################
#Housekeeping
librarian::shelf(tidyverse, magrittr, ltertools, vegan, supportR, ggplot2, update_all= TRUE)

# Create needed folder(s)
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in the data
caged_v1 <- read.csv(file = file.path("data", "07_caged_w.meta_finest-scales.csv"))

# Check structure
dplyr::glimpse(caged_v1)

head(caged_v1)

# Do needed preparing of data
caged_v2 <- caged_v1 %>% 
  # Remove missing beta dispersion
  dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
  # Summarize
  # Summarize within treatments
  dplyr::group_by(source, exp.name, cage.treatment_std,
                  lat, ecotype1, aq.or.terr, exclusion.duration,
                  climate.zone, consumer.richness.category, natural.vs.artificial.substrate, gamma.richness) %>% 
  dplyr::summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Pivot to treatment into wide format
  tidyr::pivot_wider(names_from = cage.treatment_std,
                     values_from = betadisp.mean) %>% 
  # Calculate difference
  dplyr::mutate(diff = uncaged - caged)

#standardize exclusion.duration column
caged_v2$exclusion.duration #many different formats, needs to be cleaned - I'm not sure if thats the best way to go

caged_v3 <- caged_v2 %>%
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

alldata_v2 <- alldata_v1 %>%
  filter(cage.treatment_std %in% c("caged", "uncaged")) 

#checking
sort(unique(caged_v3$exclusion.duration.clean))

# Re-check structure
dplyr::glimpse(caged_v3)

###########
#Graphs/Plots Below

#### Exploratory plots, diff x gamma rich (?), ecosystem type, cage size, herbivore rich, latitude

### Figure 1 Successional stage ####

succdiffplot <- ggplot(caged_v3, aes(x = natural.vs.artificial.substrate, y = diff)) +
  geom_boxplot(aes(fill = natural.vs.artificial.substrate), alpha = 0.4) +
  #geom_jitter(aes(fill = natural.vs.artificial.substrate), width = 0.15,
             # size = 2.5, pch = 21) +
  labs(x = "Natural vs. artificial substrate (Succession)", y = "Beta diff (Uncaged-Caged)",
       title = paste0("Graph created on ", Sys.Date())) +
  #scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
  #                            "unknown" = "gray", "uncertain" = "gray20")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()

succdiffplot

### Figure 2 Ecosystem type (coarse) - Terrestrial/Aquatic ####

coarecodifplot <- ggplot(caged_v3, aes(x = aq.or.terr, y = diff)) +
  geom_boxplot(aes(fill = aq.or.terr), alpha = 0.4) +
  #geom_jitter(aes(fill = natural.vs.artificial.substrate), width = 0.15,
  # size = 2.5, pch = 21) +
  labs(x = "Ecosystem type (coarse)", y = "Beta dis. diff (uncaged - caged)",
       title = paste0("Graph created on ", Sys.Date())) +
  #scale_fill_manual(values = c("caged" = "red", "uncaged" = "blue", "partial" = "purple", 
  #                            "unknown" = "gray", "uncertain" = "gray20")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  supportR::theme_lyon()

coarecodifplot

#### Figure 3 Ecosystem type (fine) - Ecosystem types ####

ecotypediff.boxplot <-
  caged_v3 %>% 
  ggplot(data = ., aes(x = ecotype1, y = diff)) + 
  geom_boxplot() +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  labs(y = expression("Beta dis diff (uncaged - caged)"), x = "", 
       title = paste0("Graph created on ", Sys.Date())) +
  theme_bw(base_size=12)  +
  theme(plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank())

ecotypediff.boxplot

### Figure 4 Gamma richness ####

caged_v3$aq.or.terr <- as.factor(caged_v3$aq.or.terr)

caged_v3<- caged_v3 %>%
  filter(aq.or.terr %in% c("aquatic", "terrestrial")) 


gammarichplot <-
  ggplot(caged_v3, aes(x = gamma.richness, y = diff, color = aq.or.terr)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = T) +
  labs(x = "Gamma diversity", y = "Beta dis diff (uncaged - caged)") +
  scale_color_manual(values = c("aquatic" = "darkblue", "terrestrial" = "green4")) 

gammarichplot

#### Figure 5 Herbivore (consumer) richness ####

conrichdifplot <-
  caged_v3 %>% 
  ggplot(data = ., aes(x = consumer.richness.category, y = diff, fill=aq.or.terr)) + 
  geom_boxplot() +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  labs(y = expression("Beta dis diff (uncaged - caged)"), x= "Consumer richness", 
       title = paste0("Graph created on ", Sys.Date())) + 
  theme_bw(base_size=12)  +
  theme() +
  ggtitle("Consumer richness")+
  facet_wrap(~aq.or.terr)

conrichdifplot

#### Figure 6 Exclusion time #####

extimeplot <- ggplot(caged_v3, aes(x = exclusion.duration.clean, y = diff, color = aq.or.terr)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = T) +
  labs(x = "Exclusion duration (months)", y ="Beta dis diff (uncaged - caged)") +
  scale_color_manual(values = c("aquatic" = "darkblue", "terrestrial" = "green4")) 

extimeplot

#### Figure 7 Size of cage ####


#### Figure 8 Latitude, binned and continous

latdifplot <- 
  ggplot(caged_v3, aes(x = lat, y = diff, color=aq.or.terr)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "latitude", y = "Beta dis diff (uncaged - caged)") +
  scale_color_manual(values = c("aquatic" = "darkblue", "terrestrial" = "green4")) 

latdifplot


caged_v3$absolute_lat <- abs(caged_v3$lat)

abslatdifplot <-
  ggplot(caged_v3, aes(x = absolute_lat, y = diff, color=aq.or.terr)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "absolute latitude", y = "Beta dis diff (uncaged - caged)") +
  scale_color_manual(values = c("aquatic" = "darkblue", "terrestrial" = "green4")) 
  
abslatdifplot
