#### POSET per anys ####

all_hdi_long$Level <- ordered(all_hdi_long$hdicode, levels = c("Very High", "High", "Medium", "Low")) 
Years <- c(1991, 2001, 2011, 2021)
countries <- c()
incomparabilities <- c()
average_heights <- list()
palette(brewer.pal(n =4, name = "RdYlBu")) 

for(i in 1:length(Years)){
  data_noNA <- all_hdi_long %>% 
    filter(Year == Years[i]) %>% 
    select(-country, -region, -hdicode) %>% 
    drop_na()
  
  data_noNANum <- data_noNA %>% 
    select(-Level)
  
  X <- data_noNANum$iso3
  data_noNANum <- column_to_rownames(data_noNANum, var = "iso3") 
  
  Z <- get_poset(data_noNANum, X)
  print(summary(poset_from_incidence(Z)))
  
  png(file=paste("POSET_HDI_", Years[i], ".png", sep = ""), width=1100, height=860)
  plot(Z, col = as.factor(data_noNA$Level), pch = 16)
  legend('bottomright', legend = levels(as.factor(data_noNA$Level)), col = 1:4, pch = 16)
  dev.off()
  

  data_noNA_SOM <- all_hdi_long %>% 
    filter(Year == Years[i]) %>% 
    select(-country, -region, -hdicode, -Year,-Level) %>% 
    drop_na() %>% 
    column_to_rownames(var = "iso3") 
  
  dataPoset.train <- as.matrix(scale(data_noNA_SOM)) # make a SOM grid 
  set.seed(100) 
  dataPoset.grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
  
  set.seed(100) 
  dataPoset.model <- som(dataPoset.train, dataPoset.grid, rlen = 500, radius = 2.5, keep.data = TRUE)
  
  # str(dataPoset.model) 
  #data.frame(country = rownames(dataPosetNum), cell = dataPoset.model$unit.classif) %>% arrange(desc(cell))
  DT::datatable(data.frame(country = rownames(data_noNA_SOM), cell = dataPoset.model$unit.classif))
  #plot(dataPoset.model, type = "changes")
  
  png(file=paste("../30 years of HDI/SOM_HDI", Years[i], "_mapping.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "mapping", pchs = 19, shape = "straight", col = as.factor(data_noNA$Level)) 
  legend('bottomright', legend = levels(as.factor(dataPoset$Level)), col = 1:4, cex = 0.8, pch = 16)
  dev.off()
  
  png(file=paste("../30 years of HDI/SOM_HDI", Years[i], "_codesplot.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "codes", main = "Codes Plot", shape="s")
  dev.off()
  
  png(file=paste("../30 years of HDI/SOM_HDI", Years[i], "_neighbours.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "dist.neighbours", shape = "s")
  dev.off()
  
  set.seed(100) 
  fviz_nbclust(dataPoset.model$codes[[1]], kmeans, method = "wss")
  
  set.seed(100) 
  clust <- kmeans(dataPoset.model$codes[[1]], centers = 3) #clust <- kmeans(dataPoset.model$codes[[1]], 9) clust
  
  dataPoset.cluster <- data.frame(data_noNA_SOM, cluster = clust$cluster[dataPoset.model$unit.classif]) 
  #tail(dataPoset.cluster, 10) 
  # clustering using hierarchial # cluster.som <- cutree(hclust(dist(dataPoset.model$codes[[1]])), 6)
  
  #Cluster boundaries plot(dataPoset.model, type = "codes", bgcol = rainbow(1)[clust$cluster], main = "Cluster SOM", shape = "s") add.cluster.boundaries(dataPoset.model, clust$cluster)
  
  png(file=paste("../30 years of HDI/SOM_HDI", Years[i], "_cluster.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "mapping", pchs = 19, shape = "straight", col = as.factor(data_noNA$Level)) 
  legend('bottomright', legend = levels(as.factor(dataPoset$Level)), col = 1:4, cex = 0.8, pch = 16) 
  add.cluster.boundaries(dataPoset.model, clust$cluster)
  dev.off()
  
  png(file=paste("../30 years of HDI/SOM_HDI", Years[i], "_clustermap.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "codes", shape = "s", bgcol = rainbow(4)[clust$cluster], main = "Cluster Map")
  add.cluster.boundaries(dataPoset.model, clust$cluster)
  dev.off()
  
  png(file=paste("../30 years of HDI/SOM_HDI", Years[i], "_clusterlevels.png", sep = ""), width=1100, height=860)
  palette(brewer.pal(n =4, name = "RdYlBu")) 
  plot(Z, col = as.factor(dataPoset.cluster$cluster), pch = 16)
  legend('bottomright', legend = levels(as.factor(dataPoset.cluster$cluster)), col = 1:4, pch = 16)
  dev.off()
  
  som_grid <- dataPoset.model[[4]]$pts %>%
    as_tibble %>% 
    mutate(id=row_number())
  
  som_pts <- tibble(id = dataPoset.model[[2]],
                    dist = dataPoset.model[[2]],
                    type = data_noNA$Level,
                    countryISO3 = data_noNA$iso3)
  
  som_pts <- som_pts %>% left_join(som_grid,by="id")
  som_grid %>% 
    ggplot(aes(x0=x,y0=y))+
    geom_circle(aes(r=0.5))+
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom") +
    ggrepel::geom_text_repel(data=som_pts,aes(x, y, label = countryISO3, col = type), position = position_jitter(), size = 4)+
    scale_color_manual(values=c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6"),name="IDH Level")
  ggsave(filename = paste("../30 years of HDI/SOM_HDI", Years[i], "_mapping_labels.png", sep = ""), 
         width=4500, height=3500, units = "px", bg="white")
}
