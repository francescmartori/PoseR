
# generate the embedded plots
png(file="../Geography of HDI/3.2-POSET_HDI23_Embedded.png", res = 240, height = 2600, width = 2800)
par(mfrow = c(3,2))
par(mar = c(0,0, 0, 0))
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

  plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16, shape = "equispaced")
  
}
hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t")
hdi_regions_filtered <- rbind(hdi_regions_filtered, pp)

hdi_regions_filtered <- hdi_regions_filtered %>% 
  mutate(DevRegion = as.factor(DevRegion))
plot.new() 
 legend("center", legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:8, pch = 16, cex = 1.4)

dev.off()

## plot the world
par(mar = c(0,0, 0, 0))
hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t")
hdi_regions_filtered <- rbind(hdi_regions_filtered, pp)

hdi_regions_filtered <- hdi_regions_filtered %>% 
  mutate(DevRegion = as.factor(DevRegion))

palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "Set3")) 

X <- hdi_regions_filtered$Code
hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 

hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
  select(-Level, -Country, -Region, -SubRegion, -DevRegion)

Z <- get_poset(hdi_regions_filtered_Num, X)

plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16, shape = "equispaced")
legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:8, pch = 16, cex = 0.9)
