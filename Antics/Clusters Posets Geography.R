library(POSetR) 
library(tidyverse) 
library(parsec) 
library(RColorBrewer) 
hdi_regions <- read_delim("../HDI2022.txt", delim = "\t")

regions <- unique(hdi_regions$Region) 

regions <- sort(regions)
optim_centers <- c(4, 7, 8, 9, 6)
centers <- 6

png(file="../Geography of HDI/POSet_HDI23_Clusters2.png", res = 240, height = 5000, width = 3600)
par(mfrow = c(5,3))

for(i in 1:length(regions)) {
  print(regions[i])
  set.seed(19810815)
  hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t",show_col_types = FALSE)
  
  hdi_regions_filtered <- hdi_regions_filtered %>% 
    mutate(DevRegion = as.factor(DevRegion))
  
  palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "Set3")) 
  
  hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[i])
  
  X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
  hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
  Z <- get_poset(hdi_regions_filtered_Num, X)

  plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16, shape = "equispaced")
  
  
  print("STABLE CLUSTERS")
  
  km_clusters <- kmeans(x = hdi_regions_filtered_Num, centers = centers, nstart = 100)
  
  Z <- get_poset(km_clusters$centers, 1:nrow(km_clusters$centers))
  plot(Z, main = regions[i])
  print(km_clusters$centers)
  print(km_clusters$cluster %>% sort())
  print(km_clusters$betweenss/km_clusters$totss)
  
  # Analisi estabilitat
  resumen <- list(cluster_comp = matrix(nrow = 10, ncol = centers))
  for (j in 1:10) {
    km_clusters_estab <- kmeans(x = hdi_regions_filtered_Num, centers = centers, nstart = 100)
    #  resumen$clusters[i,] <- km_clusters$cluster %>% sort()
    resumen$cluster_comp[j,] <- km_clusters_estab$size %>% sort()

  }
  print(apply(resumen$cluster_comp, 2, sd))
  print("OPTIMAL CENTERS")
  # optimal clusters
  km_clusters <- kmeans(x = hdi_regions_filtered_Num, centers = optim_centers[i], nstart = 100)
  print(km_clusters$centers)
  print(km_clusters$cluster %>% sort())
  print(km_clusters$betweenss/km_clusters$totss)
  Z <- get_poset(km_clusters$centers, 1:nrow(km_clusters$centers))
  plot(Z)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = levels(as.factor(hdi_regions_filtered$DevRegion)),
       col = 1:7, lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, pch = 16, seg.len=1)
dev.off()
 
# #optimal clusters
# 
# fviz_nbclust(x = hdi_regions_filtered_Num, FUNcluster = kmeans, method = "wss", k.max = 25, 
#              diss = get_dist(hdi_regions_Num, method = "euclidean"), nstart = 50)
# 
# fviz_nbclust(x = hdi_regions_filtered_Num, FUNcluster = kmeans, method = "silhouette", k.max = 25, 
#              diss = get_dist(hdi_regions_filtered_Num, method = "euclidean"), nstart = 50)
# 
# gap_stat <- cluster::clusGap(hdi_regions_filtered_Num, FUN = kmeans, nstart = 25,
#                     K.max = 15, B = 50)
# 
# fviz_gap_stat(gap_stat)

centers <- 4
iteracions <- 9
resumen <- list(perc_var = numeric(),
                cluster_comp = matrix(nrow = iteracions, ncol = centers),
                cluster_centers = list())



hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t",show_col_types = FALSE)
hdi_regions_filtered <- hdi_regions_filtered %>% 
  filter(Region==regions[5])

X <- hdi_regions_filtered$Code
hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 

hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
  select(-Level, -Country, -Region, -SubRegion, -DevRegion)


par(mfrow = c(3,3))
for (i in 1:iteracions) {
  km_clusters <- kmeans(x = hdi_regions_filtered_Num, centers = centers, nstart = 100)
  resumen$perc_var[i] <- km_clusters$betweenss/km_clusters$totss
#  resumen$clusters[i,] <- km_clusters$cluster %>% sort()
  resumen$cluster_comp[i,] <- km_clusters$size %>% sort()
  resumen$cluster_centers[[i]] <- km_clusters$centers
  
  Z <- get_poset(km_clusters$centers, 1:nrow(km_clusters$centers))
  plot(Z)
}
resumen$perc_var

resumen$cluster_comp
apply(resumen$cluster_comp, 2, sd)


# dend <- hdi_regions_filtered_Num %>% dist %>% hclust %>% as.dendrogram
# # and plot it:
# dend %>% plot
# 

