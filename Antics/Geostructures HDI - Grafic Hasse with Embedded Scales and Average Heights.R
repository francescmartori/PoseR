# Generació dels average heights amb els embedded plots
png(file="../Geography of HDI/4.1-POSET_HDI23_Embedded_AverageHeights.png", res = 240, height = 2600, width = 2000)
par(mfrow = c(3,2))
par(mar = c(1,2, 1, 0))

for(i in 1:length(regions)) {
  hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t")
  hdi_regions_filtered <- rbind(hdi_regions_filtered, pp)
  
  hdi_regions_filtered <- hdi_regions_filtered %>% 
    mutate(DevRegion = as.factor(DevRegion))
  
  palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "Set3")) 
  
  hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region %in% c(regions[i],"Benchmark" ))
  
  X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
  hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
  Z <- get_poset(hdi_regions_filtered_Num, X)

  #### POSET amb altures mitjana ####
  M <- MRP(Z,method = "approx")
  avg_heigths_full <- colSums(M)
  
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov, shape = "equispaced")
  
  V$y <- avg_heigths_full
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1
  
  plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
       col = as.factor(hdi_regions_filtered$DevRegion), pch = 16,
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white", 
       main = regions[i])
  
  axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd=2)
  text(V, labels = rownames(Z.cov), cex = 0.75)

}

## and now the legend
plot.new() 
hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t")
hdi_regions_filtered <- rbind(hdi_regions_filtered, pp)

hdi_regions_filtered <- hdi_regions_filtered %>% 
  mutate(DevRegion = as.factor(DevRegion))
legend("center", legend = levels(hdi_regions_filtered$DevRegion), col = 1:8, pch = 16, cex = 1.4)

dev.off()


#plot the world
hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t")
hdi_regions_filtered <- rbind(hdi_regions_filtered, pp)

hdi_regions_filtered <- hdi_regions_filtered %>% 
  mutate(DevRegion = as.factor(DevRegion))

X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
Z <- get_poset(hdi_regions_filtered_Num, X)

  #### POSET amb altures mitjana ####
M <- MRP(Z,method = "approx")
avg_heigths_full <- colSums(M)
  
Z.cov <- incidence2cover(Z)
V <- -vertices(Z.cov, shape = "equispaced")
  
V$y <- avg_heigths_full
xlim <- c(min(V$x), max(V$x)) * 1.3
ylim <- c(min(V$y), max(V$y)) * 1
par(mar = c(0,2, 0, 0))
plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
     col = as.factor(hdi_regions_filtered$DevRegion), pch = 16,
     ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white")

axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
grid(lwd=2)
text(V, labels = rownames(Z.cov), cex = 0.75)
legend("bottomright", legend = levels(hdi_regions_filtered$DevRegion), col = 1:8, pch = 16, cex = 0.85)
