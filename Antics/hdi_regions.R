library(POSetR) 
library(tidyverse) 
library(parsec) 
library(RColorBrewer) 
library(readr)
hdi_regions <- read_delim("../HDI2022.txt", delim = "\t")

regions <- unique(hdi_regions$Region) 
#Overall analysis
X <- hdi_regions$Code
hdi_regions <- column_to_rownames(hdi_regions, var = "Code") 

hdi_regions_Num <- hdi_regions %>% 
  select(-Level, -Country, -Region, -SubRegion, -DevRegion)

Z <- get_poset(hdi_regions_Num, X)

palette(brewer.pal(n =7, name = "RdYlBu")) 
#par(mar = c(bottom, left, top, right)) 
def_margin <- c(5.1, 4.1, 4.1, 2.1)
par(mar = c(1,0, 0, 2))
plot(Z, col = as.factor(hdi_regions$DevRegion), pch = 16)

legend('bottomright', legend = levels(as.factor(hdi_regions$DevRegion)), col = 1:7, pch = 16)

palette(brewer.pal(n =length(regions), name = "Accent")) 
plot(Z, col = as.factor(hdi_regions$Region), pch = 16)

legend('bottomright', legend = levels(as.factor(hdi_regions$Region)), col = 1:length(regions), pch = 16)


M <- MRP(Z,method = "approx")
avg_heigths_full <- colSums(M)

#Sensitivity based on Bruggeman Patil, chapter 4, p48
dfSensitivity <- data.frame(columns = colnames(hdi_regions_Num), sens = numeric(length(colnames(hdi_regions_Num))))

Z <- get_poset(hdi_regions_Num[], X)
for(i in 1:length(colnames(hdi_regions_Num))) {
  dfSensitivity$sens[i] = sum(get_poset(hdi_regions_Num[,-i], X) - Z)
}

dfSensitivity %>% 
  ggplot(aes(columns, sens)) + geom_bar(stat = "identity") + ylab("Sensitivity") + xlab("Variables") + ggtitle("Sensitivity analysis")

dfSensitivity %>% arrange(desc(sens))

# Analysis by regions


for(i in 1:length(regions)) {
  hdi_regions_filtered <- read_delim("../HDI2022.txt", delim = "\t")
  
  hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[i])
  
  X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
  hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
  Z <- get_poset(hdi_regions_filtered_Num, X)
  palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "RdYlBu")) 
  
  png(file=paste("../Geography of HDI/POSet_HDI22_", regions[i], "_byDevRegion.png", sep = ""), width=1100, height=860)
  plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16)
  legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:7, pch = 16)
  dev.off()
  
  print(regions[i])
  print(summary(poset_from_incidence(Z)))
  
  #### POSET amb altures mitjana ####
  M <- MRP(Z,method = "approx")
  avg_heigths_full <- colSums(M)
  
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov)
  
  V$y <- avg_heigths_full
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1
  
  png(file=paste("../Geography of HDI/POSet_HDI22_avgHeight_", regions[i], ".png", sep = ""), width=1100, height=860)
  
  plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
       col = as.factor(hdi_regions_filtered$DevRegion), pch = 16,
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white")
  
  axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd=2)
  text(V, labels = rownames(Z.cov), cex = 0.75)
  legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:7, pch = 16)
  
  dev.off()
  
  #Sensitivity
  
  dfSensitivity <- data.frame(columns = colnames(hdi_regions_filtered_Num), sens = numeric(length(colnames(hdi_regions_filtered_Num))))
  for(j in 1:length(colnames(hdi_regions_filtered_Num))) {
    dfSensitivity$sens[j] = sum(get_poset(hdi_regions_filtered_Num[,-j], X) - Z)
  }
  print(dfSensitivity)
 
  ggplot(dfSensitivity, aes(columns, sens)) + geom_bar(stat = "identity") + 
    ylab("Sensitivity") + xlab("Variables") + 
    ggtitle(paste("Sensitivity analysis from ", regions[i], " HDI data", sep = ""))
   ggsave(filename = paste("../Geography of HDI/Sensitivity_HDI22_", regions[i], ".png", sep = ""), 
          width=1100, height=860, units = "px")
   
}  
  
## Tabla Sensibilidades

dfSensitivity <- data.frame(
  Continent = c("Asia", "Europa", "Africa", "Americas", "Oceania"),
  LE = c(109, 81, 248, 57, 14),
  EYS = c(83, 84, 91, 46, 0),
  MYS = c(140, 150, 96, 69, 1),
  GNIpC = c(49, 38, 85, 54, 5)
)

dfSensitivity %>% 
  pivot_longer(-Continent, names_to = "Variables", values_to = "Changes") %>% 
  ggplot(aes(Variables, Changes)) + geom_bar(stat="identity") + facet_wrap(~ Continent)

cbind(Continent = dfSensitivity$Continent,dfSensitivity[,2:5]/ rowSums(dfSensitivity[,2:5])) %>% 
  pivot_longer(-Continent, names_to = "Variables", values_to = "Changes") %>% 
  ggplot(aes(Variables, Changes)) + geom_bar(stat="identity") + facet_wrap(~ Continent) +
  scale_y_continuous(breaks = seq(0, 0.8, by = .2), labels = scales::percent)


### Dominance and incomparability

hdi_regions_filtered <- read_delim("../HDI2022.txt", delim = "\t")
hdi_regions$Region %>% unique
hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[2])

X <- hdi_regions_filtered$Code
hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 

hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
  select(-Level, -Country, -Region, -SubRegion, -DevRegion)

Z <- get_poset(hdi_regions_filtered_Num, X)


# dominance  and incomparability ####

M <- MRP(as.matrix(Z), method = "approx")
dominance <- abs(svd(M)$v[,1])

inc <- matrix(ncol = ncol(M), nrow = nrow(M))
for(i in 1:nrow(M)) {
  for(j in 1:ncol(M)) {
    if(i==j) inc[i,j] = 0 else inc[i,j] = 2 * min(M[i,j], M[j,i])
    
  }
}

incomparability <- abs(eigen(inc)$vectors[,1])

plot(incomparability, dominance, type="n")
text(incomparability ,     dominance , labels = X, cex = 0.75)

## prova modelitzar


### comparativa clusters i poset original #### 

for(i in 1:length(regions)) {
  hdi_regions_filtered <- read_delim("../HDI2022.txt", delim = "\t",show_col_types = FALSE)
  
  hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[i])
  
  X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
  hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
  Z <- get_poset(hdi_regions_filtered_Num, X)
  palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "RdYlBu")) 
  
  plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16)
  legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), 
         col = 1:length(unique(hdi_regions_filtered$DevRegion)), 
         pch = 16)

  km_clusters <- kmeans(x = hdi_regions_filtered_Num, centers = 10, nstart = 50)
  print(km_clusters$betweenss/km_clusters$totss)
  print(km_clusters$cluster) %>% sort()
  table(km_clusters$cluster)
  
  print(km_clusters$centers)
  
  Z <- get_poset(km_clusters$centers, 1:nrow(km_clusters$centers))
  plot(Z)
  
}


### inclusió de perfils comparables ####

pp1 <- data.frame(Country = "PP1",
                  Code = "MAX",
                  LE = max(hdi_regions$LE),
                  EYS = max(hdi_regions$EYS),
                  MYS = max(hdi_regions$MYS),
                  GNIpC = max(hdi_regions$GNIpC),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp2 <- data.frame(Country = "PP2",
                  Code = "Q3",
                  LE = quantile(hdi_regions$LE, 0.75),
                  EYS = quantile(hdi_regions$EYS, 0.75),
                  MYS = quantile(hdi_regions$MYS, 0.75),
                  GNIpC = quantile(hdi_regions$GNIpC, 0.75),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp3 <- data.frame(Country = "PP3",
                  Code = "Q2",
                  LE = quantile(hdi_regions$LE, 0.5),
                  EYS = quantile(hdi_regions$EYS, 0.5),
                  MYS = quantile(hdi_regions$MYS, 0.5),
                  GNIpC = quantile(hdi_regions$GNIpC, 0.5),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp4 <- data.frame(Country = "PP4",
                  Code = "Q1",
                  LE = quantile(hdi_regions$LE, 0.25),
                  EYS = quantile(hdi_regions$EYS, 0.25),
                  MYS = quantile(hdi_regions$MYS, 0.25),
                  GNIpC = quantile(hdi_regions$GNIpC, 0.25),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp5 <- data.frame(Country = "PP5",
                  Code = "MIN",
                  LE = min(hdi_regions$LE),
                  EYS = min(hdi_regions$EYS),
                  MYS = min(hdi_regions$MYS),
                  GNIpC = min(hdi_regions$GNIpC),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp <- rbind(pp1, pp2, pp3, pp4, pp5) 
rownames(pp) <- NULL


for(i in 1:length(regions)) {
  hdi_regions_filtered <- read_delim("../HDI2022.txt", delim = "\t")
  
  hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[i])
  
  hdi_regions_filtered <- rbind(hdi_regions_filtered, pp)
  
  X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
  hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
  
  
  Z <- get_poset(hdi_regions_filtered_Num, X)
  palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "RdYlBu")) 
  
  png(file=paste("../Geography of HDI/POSet_HDI22_", regions[i], "_EmbeddedScale.png", sep = ""), width=1100, height=860)
  
  plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16)
  legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:7, pch = 16)
  dev.off()
  
  print(regions[i])
  print(summary(poset_from_incidence(Z)))
  
  #### POSET amb altures mitjana ####
  M <- MRP(Z,method = "approx")
  avg_heigths_full <- colSums(M)
  
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov)
  
  V$y <- avg_heigths_full
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1
  
  png(file=paste("../Geography of HDI/POSet_HDI22_", regions[i], "_EmbeddedScale_Avgheights.png", sep = ""), width=1100, height=860)
  
  plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
       col = as.factor(hdi_regions_filtered$DevRegion), pch = 16,
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white")
  
  axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd=2)
  text(V, labels = rownames(Z.cov), cex = 0.75)
  legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:7, pch = 16)
  dev.off()
}

  
