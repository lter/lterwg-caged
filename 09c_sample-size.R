## --------------------------------------------------------------- ##
# CAGED Sample size by aquatic vs terrestrial data figures
## --------------------------------------------------------------- ##
# Written by: Nico Matallana, Raine Detmer, Hillary Krumbholz

## ------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, njlyon0/supportR,
                 ggpubr, cowplot) #, update_all= TRUE) 

# Create needed folders
source(file = file.path("00_setup.R"))

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Load Data ----
# these dfs were created in script 08 script
## ------------------------------------------- ##
caged_beta <- read.csv(file.path("data", "08_caged_prepped-beta-dispersion.csv"))


# View(caged_beta) 
dim(caged_beta)# 12905  rows

## ------------------------------------------- ##
# Check  ---- 
## ------------------------------------------- ##
colnames(caged_beta)

# List the predictors
caged_beta %>% select(starts_with("var")) %>% names()

# [1] "var_upper.source"                       "var_climate.zone"                      
# [3] "var_aq.or.terr"                         "var_ecotype1"                          
# [5] "var_consumer.taxonomy"                  "var_consumer.metabolism"               
# [7] "var_resource.type.category"             "var_consumer.richness.number"          
# [9] "var_consumer.richness.category"         "var_max.consumer.size.category"        
#[11] "var_dominant.consumer.species.category" "var_consumer.native.domestic"          
#[13] "var_succ.vs.late"                       "var_exclusion.duration.continuousyears"
#[15] "var_taxonomic.level"                    "var_exclosure.area.m2"                 
#[17] "var_whereisthecage"

# Check which columns are categorical and numerical 
caged_beta %>% 
  select(starts_with("var")) %>% 
  sapply(class)

# draft for loop for predictor sample sizes ----


bd.prep = caged_beta[,c(2, 5, 20, 4, 6:19)] #simplify and reorder dataframe for plotting

# vector of colors for categorical variables
col_vector <- c("#CC6677", "#332288", "#DDCC77", "#88CCEE", "#117733","#882255", "#44AA99", "#999933", "#AA4499", "#CCDDAA", "#555555", "#FFCCCC", "#DDDDDD")

#colnames(beta.disper)

colnames(bd.prep)

fig.list = list()

#hist(bd.temp$var_exclosure.area.m2)

for(i in colnames(bd.prep)[4:length(colnames(bd.prep))]){
  
  #diagnostics, hash out before running
  #i = colnames(bd.prep)[17]
  
  if(i == "var_exclosure.area.m2"){
    bd.temp = bd.prep %>%
      dplyr::select(exp.name, var_aq.or.terr, cage.treatment_std, all_of(i)) %>% # filter to relevant variables
      drop_na() %>% # drop NAs
      filter(.[[i]] != "") %>%
      filter(.[[i]] != "unknown") %>%
      mutate(var_exclosure.area.m2 = as.numeric(var_exclosure.area.m2))
    
  } else {
    bd.temp = bd.prep %>%
      dplyr::select(exp.name, var_aq.or.terr, cage.treatment_std, all_of(i)) %>% # filter to relevant variables
      drop_na() %>% # drop NAs
      filter(i != "") %>%
      filter(i != "unknown") %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.integer), as.numeric))
  }
  
  
  class(bd.temp[[i]])
  
  if(is.factor(bd.temp[[i]])){
    
    bd.factor.df = bd.temp %>%
      group_by(var_aq.or.terr, cage.treatment_std, .[i]) %>%
      summarise(n = n()) %>%
      group_by(var_aq.or.terr, .[i]) %>%
      summarise(avg.n = mean(n)) %>% 
      ungroup()
    
    # number of categories for colors
    #n_cols <- bd.factor.df %>% select(.[i]) %>% unique() %>% nrow()
    n_cols <- length(unique(bd.factor.df[[i]]))
    
    fig.list[[i]] = ggplot(bd.factor.df) +
      geom_bar(aes(y = avg.n, x = var_aq.or.terr, fill = .data[[i]]), stat = "identity", position = position_dodge()) +
      #labs(y = "Number of studies", fill = "Ecotype") +
      theme(panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey30"),
            axis.title.x = element_blank() 
      ) +
      scale_fill_manual(values= col_vector[1:n_cols]) +
    fig.list[[i]]
    
  } else {
    
    #i = colnames(bd.prep)[8]
    
    bd.num.df = bd.temp %>%
      group_by(var_aq.or.terr, cage.treatment_std) %>%
      mutate(sample_size = n()) %>%
      ungroup() %>%
      group_by(var_aq.or.terr) %>%
      mutate(sample_size_mean = mean(sample_size)) %>%
      ungroup()
    
    bd.num.sum = bd.num.df %>%
      dplyr::select(var_aq.or.terr, sample_size_mean) %>%
      distinct()
    
    fig.list[[i]] = ggplot(bd.num.df) +
      geom_boxplot(aes(x = var_aq.or.terr, y = .data[[i]], fill = var_aq.or.terr), alpha = .5) +
      #geom_point(aes(x = var_aq.or.terr, y = .data[[i]], color = var_aq.or.terr),
      #position = position_jitter(), alpha = .05) +
      scale_fill_manual(values = c("turquoise",
                                   "darkgreen")) +
      scale_color_manual(values = c("turquoise",
                                    "darkgreen")) +
      geom_text(data = bd.num.sum, aes(var_aq.or.terr, Inf, label = round(sample_size_mean)), vjust = 1) +
      labs(x = "var_aq.or.terr", fill = "var_aq.or.terr")+
      theme(panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey30"),
            axis.title.x = element_blank())
    
    fig.list[[i]]
    
  }
  
}

plot_grid(plotlist = fig.list, ncol = 3, nrow = 5)

length(fig.list)

    
    plot_grid(fig.list[[1]], fig.list[[2]], fig.list[[3]], ncol = 3, nrow = 1)
    plot_grid(fig.list[[4]], fig.list[[5]], fig.list[[6]], ncol = 3, nrow = 1)
    plot_grid(fig.list[[7]], fig.list[[8]], fig.list[[9]], ncol = 3, nrow = 1)
    plot_grid(fig.list[[10]], fig.list[[11]], fig.list[[12]], ncol = 3, nrow = 1)
    plot_grid(fig.list[[13]], fig.list[[14]], fig.list[[15]], ncol = 3, nrow = 1)

  

#grid.list[[1]]

#
