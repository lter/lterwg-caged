## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, njlyon0/supportR,
                 ggpubr) #, update_all= TRUE) 

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ----
# these dfs were created in script 08 script
## ------------------------------------------- ##
caged_beta_for_plots <- read.csv(file.path("data", "08_caged_prepped-beta-dispersion.csv"))


## ------------------------------------------- ##
# Plots ---- 
## ------------------------------------------- ##
colnames(caged_beta_for_plots)

## I am looking at cols 15-19
#15 var_succ.vs.late
#16 var_exclusion.duration.continuousyears
#17 var_taxonomic.level
#18 var_exclosure.area.m2
#19 var_whereisthecage

# Sample size of data for aquatic and terrestrial by the number of days 

# Number of experiments in aquatic vs terrestrial
caged_beta_for_plots %>%
  dplyr::count(exp.name, var_aq.or.terr) %>%
  filter(var_aq.or.terr != "") %>%
  ggplot(aes(x= var_aq.or.terr,
             fill=var_aq.or.terr)) +
  geom_bar() +
  theme_pubr(base_size=16) +
  scale_fill_manual(values= c("turquoise",
                               "darkgreen")) +
  labs(x= "Biome",
       y="Number of experiments",
       fill = "Ecotype")


# Proportion of experiments in each ecosystem type
caged_beta_for_plots %>%
  dplyr::count(exp.name, var_aq.or.terr, var_ecotype1) %>%
  filter(var_aq.or.terr != "") %>%
  filter(var_ecotype1 != "") %>%
  ggplot(aes(x=var_aq.or.terr, 
             fill=var_ecotype1)) +
  geom_bar(position= "fill") +
  theme_pubr(base_size=16) +
  labs(x= "Biome",
       y="Proportion",
       fill = "Ecotype")


# Sample size by taxonomic level 
caged_beta_for_plots %>%
  filter(var_aq.or.terr != "", 
        var_taxonomic.level != "") %>%
  ggplot(aes(x = var_taxonomic.level,
             y = betadisp.sample.size,
             fill = var_aq.or.terr,
            color = var_aq.or.terr)) +   
  geom_boxplot(size = 1.1, alpha = 0.7) +
  stat_summary(                                        
    fun.data = function(x) data.frame(y = max(x) + 1,
                                      label = paste0("n=", length(x))),
    geom = "text",
    position = position_dodge(width = 0.75),
    size = 4,
    color = "black",
    show.legend = FALSE
  ) +
  geom_point(position = position_jitter(), alpha = 0.05) +
  theme_pubr(base_size = 16) +
  scale_fill_manual(values = c("royalblue", "darkturquoise")) +
  scale_color_manual(values = c("royalblue", "darkturquoise")) +
  labs(x = "Taxonomic level",
       y = "Sample size",
       fill = "Habitat")

# Sample size by taxonomic level 
caged_beta_for_plots %>%
  filter(var_aq.or.terr != "", 
        var_taxonomic.level != "") %>%
  ggplot(aes(x = var_taxonomic.level,
             y = betadisp.sample.size,
             fill = var_aq.or.terr,
             color = var_aq.or.terr)) +   
  geom_boxplot(size = 1.1, alpha = 0.7) +
  stat_summary(
    fun.data = function(x) data.frame(y = max(x) + 1,
                                      label = paste0("n=", length(x))),
    geom = "text",
    position = position_dodge(width = 0.75),
    size = 4,
    color = "black",
    show.legend = FALSE
  ) +
  geom_point(position = position_jitter(), alpha = 0.05) +
  theme_pubr(base_size = 16) +
  scale_fill_manual(name = "Habitat",                             
                    values = c("royalblue", "darkturquoise")) +
  scale_color_manual(name = "Habitat",                           
                     values = c("royalblue", "darkturquoise")) +
  labs(x = "Taxonomic level",
       y = "Sample size")                                         

# Get one row per unique exp.name (deduplicate) and remove missing taxonomic levels
caged_beta_for_plots %>%
  distinct(`exp.name`, .keep_all = TRUE) %>%
  filter(!is.na(var_taxonomic.level), var_taxonomic.level != "") %>%
  mutate(
    var_taxonomic.level = factor(
      var_taxonomic.level,
      levels = c("functional.groups", "genus", "species"),
      labels = c("Functional groups", "Genus", "Species")
    ),
    var_aq.or.terr = factor(
      var_aq.or.terr,
      levels = c("aquatic", "terrestrial"),
      labels = c("Aquatic", "Terrestrial")
    )
  )
 
# Plot
ggplot(caged_beta_for_plots, aes(x = var_taxonomic.level, y = betadisp.sample.size, fill = var_aq.or.terr)) +
  geom_boxplot(
    outlier.shape  = 21,
    outlier.size   = 2,
    outlier.stroke = 0.4,
    alpha          = 0.7,
    width          = 0.6,
    position       = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(
    values = c("Aquatic" = "#378ADD", "Terrestrial" = "#1D9E75"),
    name   = NULL
  ) +
  labs(
    x     = "Taxonomic level",
    y     = "Sample size",
    title = "Sample size by taxonomic level",
    subtitle = "Grouped by aquatic vs. terrestrial"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "top",
    plot.title       = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )
 
ggsave("boxplot_sample_size.png", width = 7, height = 5, dpi = 300)
