

librarian::shelf(tidyverse, googledrive, supportR)



## ------------------------------------------- ##
# Data Preparation (Across Design Levels) ----
## ------------------------------------------- ##

# Read in the data
#dt1 <- read.csv(file = file.path("data", "07_caged_w.meta_finest-scales.csv"))

dt1 <- read.csv(file = file.path("data", "08_caged_prepped-beta-dispersion.csv"))

colnames(dt1)

# var_succ.vs.late
# var_ecotype1
# var_consumer.richness.number
# gamma.richness
# consumer.trophic.level
# var_max.consumer.size.category
# var_taxonomic.level
# var_exclosure.area.m2
# var_exclusion.duration.continuousyears
# spatial.analysis.levels.yn
# var_consumer.native.domestic

# for each variable, group by aquatic vs. terrestrial and caged vs. uncaged, then calculate
# the sample sizes, then calculate the average sample size between caged and uncaged for
# aquatic and terrestrial

#colnames(dt1)[10:14]

#dt2 <- dt1 %>% select(var_succ.vs.late, var_aq.or.terr, cage.treatment_std) %>% group_by(var_aq.or.terr, cage.treatment_std) %>% summarize(sample_size = n())%>% group_by(var_aq.or.terr) %>% summarize(sample_size_mean = mean(sample_size))


#View(dt2)

# var_consumer.richness.number
#class(dt1$var_consumer.richness.number)

#dt2 <- dt1 %>% select(var_consumer.richness.number, var_aq.or.terr, cage.treatment_std) %>% group_by(var_aq.or.terr, cage.treatment_std) %>% summarize(sample_size = n())%>% group_by(var_aq.or.terr) %>% summarize(sample_size_mean = mean(sample_size))

# box plot with sample sizes for consumer richness
dt2 <- dt1 %>% select(var_consumer.richness.number, var_aq.or.terr, cage.treatment_std) %>% rename(var_choice = var_consumer.richness.number) %>% filter(var_choice != "") %>% group_by(var_aq.or.terr, cage.treatment_std) %>% mutate(sample_size = n())%>% ungroup() %>%  group_by(var_aq.or.terr) %>% mutate(sample_size_mean = mean(sample_size)) %>% ungroup()

#View(dt2) 

dt3 <- dt2 %>% select(var_aq.or.terr, sample_size_mean) %>% unique()

#dt3

dt2 %>%
  ggplot(aes(x= var_aq.or.terr,
             y=var_choice)) +
  geom_boxplot(aes(fill = var_aq.or.terr), size=1.1, alpha = 0.5) +
  geom_point(aes(color = var_aq.or.terr), position= position_jitter(), alpha= 0.05) +
  scale_fill_manual(values= c("turquoise",
                              "darkgreen")) +
  scale_color_manual(values= c("turquoise",
                               "darkgreen")) +
  geom_text(data = dt3, aes(var_aq.or.terr, Inf, label = round(sample_size_mean)), vjust = 1) +
  labs(x= "var_aq.or.terr",
       y="Consumer richness",
       fill = "var_aq.or.terr")#+
#supportR::theme_lyon() +


# barplot for consumer richness category
dt2 <- dt1 %>% select(var_consumer.richness.category, var_aq.or.terr, cage.treatment_std) %>% rename(var_choice = var_consumer.richness.category) %>% filter(var_choice != "") %>% group_by(var_aq.or.terr, cage.treatment_std, var_choice) %>% summarize(sample_size = n()) %>%  group_by(var_aq.or.terr, var_choice) %>% summarize(sample_size_mean = mean(sample_size)) %>% mutate(label_text = sample_size_mean)
# mutate(label_text = paste0("n = ", sample_size_mean))

#View(dt2)

dt2 %>% 
  ggplot(aes(fill=var_choice, y=sample_size_mean, x=var_aq.or.terr)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(x= "var_aq.or.terr",
       y="Sample size",
       fill = "Consumer richness")+
  scale_fill_manual(values= c("#993404", "#EC7014", "#FEC44F")) +
  #supportR::theme_lyon() +
  geom_text(
    aes(label = label_text), 
    vjust = -0.5,
    size = 4,
    position = position_dodge(width = 0.9)
  )



# calculate the difference metric
# dt2 <- dt1 %>% 
#   # Remove missing beta dispersion
#   dplyr::filter(!is.na(betadisp.comm.dist)) %>% 
#   # Keep only good treatments
#   dplyr::filter(cage.treatment_std %in% c("caged", "uncaged")) %>% 
#   # Summarize
#   # Summarize within treatments
#   dplyr::group_by(ecotype1, aq.or.terr, source, betadisp.design.level, cage.treatment_std) %>% 
#   dplyr::summarize(betadisp.mean = mean(betadisp.comm.dist, na.rm = T),
#                    .groups = "keep") %>% 
#   dplyr::ungroup() %>% 
#   # Pivot to treatment into wide format
#   tidyr::pivot_wider(names_from = cage.treatment_std,
#                      values_from = betadisp.mean) %>% 
#   # Calculate difference
#   dplyr::mutate(diff = uncaged - caged)
# 
# unique(dt2$ecotype1)
# unique(dt2$aq.or.terr)

# make a boxplot showing difference between caged and uncaged for each ecotype1
# dt2 %>% filter(ecotype1 != "") %>% 
# ggplot(aes(x = ecotype1, y = diff)) +
#   geom_boxplot(aes(fill = ecotype1), alpha = 0.4) +
#   geom_jitter(aes(fill = ecotype1), width = 0.15,
#               size = 2.5, pch = 21) #+
#   #facet_wrap(. ~ source) +
#   labs(x = "Ecotype1", y = "Uncaged - Caged Beta Dispersion",
#        title = paste0("Graph created on ", Sys.Date())) +
#   theme(legend.position = "none",
#         legend.title = element_blank(),
#         strip.text = element_text(size = 8),
#         axis.text.x = element_text(angle = 35, hjust = 1)) +
#   supportR::theme_lyon()
#   
#   # make a boxplot showing difference between caged and uncaged for aquatic vs terrestrial
#   dt2 %>% filter(aq.or.terr != "") %>% 
#     ggplot(aes(x = aq.or.terr, y = diff)) +
#     geom_boxplot(aes(fill = aq.or.terr), alpha = 0.4) +
#     geom_jitter(aes(fill = aq.or.terr), width = 0.15,
#                 size = 2.5, pch = 21) +
#   #facet_wrap(. ~ source) +
#   scale_fill_manual(values = c("aquatic" = "lightblue", "terrestrial" = "tan")) +
#   labs(x = "aq.or.terr", y = "Uncaged - Caged Beta Dispersion",
#        title = paste0("Graph created on ", Sys.Date())) +
#     theme(legend.position = "none",
#           legend.title = element_blank(),
#           strip.text = element_text(size = 8),
#           axis.text.x = element_text(angle = 35, hjust = 1)) +
#     supportR::theme_lyon()

