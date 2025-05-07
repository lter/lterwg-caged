



marc.modeldata_v1

 ecotype.fig = 
  marc.modeldata_v1 %>% 
  #filter(!Treatment == "Cage Control") %>% 
  #filter(Species == 'Mud crab') %>%
  ggplot(data = ., aes(x = ecotype1, y = betadisp.comm.dist)) + 
   geom_point(size = 2, alpha = .6, 
              position = position_dodge(width = .2)) +
  stat_summary(geom = "violin", fun.data = "mean_cl_normal", 
               size = 1.5, position = position_dodge(width = .1), color = "blue", 
               violinwidth = 1) +
 # stat_summary(geom = "line", aes(group = measured.group), 
 #              fun.data = "mean_cl_normal", position = position_dodge(width = .1), linewidth = 1) +
  #scale_shape_manual(labels = c("Adult", "Juvenile"), values=c(5, 16)) + 
  #scale_x_discrete(labels = c('Nekton Predator\nExclusion', 'Uncaged\nControl')) +
 # labs(y = expression("Mud crab predation \n(Prop. consumed)"), x = "") +
 # ylim(0, 1) +
  theme_bw(base_size=16)  +
  theme(#plot.margin = unit(c(1,1,1,1), "cm"), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "", legend.title = element_blank()) 

 ecotype.fig
 