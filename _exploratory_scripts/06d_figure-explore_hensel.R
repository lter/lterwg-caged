## --------------------------------------------------------------- ##
# CAGED Data Exploring via Figures
## --------------------------------------------------------------- ##
# Written by: Marc Hensel

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, ltertools, lme4, performance, lubridate, car, njlyon0/supportR, visreg) #, update_all= TRUE)

# Create needed folder(s)
#dir.create(path = file.path("data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in data
#BUT might want to put nicks new "pull from google drive" code here
marc.modeldata_v1
#alldata_v1 <- read.csv(file.path("data", "06_caged_with-metadata.csv"))

#lets see what we are dealing with
glimpse(marc.modeldata_v1)

 ecotype.fig = 
  marc.modeldata_v1 %>% 
  #filter(!Treatment == "Cage Control") %>% 
  #filter(Species == 'Mud crab') %>%
  ggplot(data = ., aes(x = ecotype1, y = betadisp.comm.dist)) + 
   geom_violin(stat = "ydensity", position = "dodge", draw_quantiles = c(0.25, 0.5, 0.75)) +
  # geom_jitter(height = 0, width = 0.1, size = .5, alpha = 0.4) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_normal", 
               size = 1, position = position_dodge(width = .1), color = "blue") +
 # labs(y = expression("Mud crab predation \n(Prop. consumed)"), x = "") +
  theme_bw(base_size=16)  +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank()) 

 geom_violin(
   mapping = NULL,
   data = NULL,
   stat = "ydensity",
   position = "dodge",
   ...,
   draw_quantiles = NULL,
   trim = TRUE,
   bounds = c(-Inf, Inf),
   scale = "area",
   na.rm = FALSE,
   orientation = NA,
   show.legend = NA,
   inherit.aes = TRUE
 )
 ecotype.fig
 