library(POSetR) 
library(tidyverse) 
library(parsec) 
library(RColorBrewer) 

## Load the data. 
# this case is for tab separated data. if different change delim
poset_data <- read_delim("../HDI2023.txt", delim = "\t")

## Get ready your data to do the poset
X <- poset_data$Code # you need one variable that contains the names/codes for the objects in the poset
poset_data <- column_to_rownames(poset_data, var = "Code") 

# The dataframe with which you will calculate the "poset" must only have numerical variables
# we are discarding the qualitative variables here in select
poset_data_num <- poset_data %>% 
  select(-Level, -Country, -Region, -SubRegion, -Level, -DevRegion)

Z <- get_poset(poset_data_num, X) ## where the magic happens

#par(mar = c(bottom, left, top, right)) 
def_margin <- c(5.1, 4.1, 4.1, 2.1)
par(mar = c(0,0, 0, 0))

plot(Z)


# Calcul de ratios del poset
Z.cov <- incidence2cover(Z)
V <- vertices(Z.cov, shape = "equispaced")

n <- nrow(Z)
total_pairs <- n * (n - 1) / 2
comparable <- sum(Z[upper.tri(Z)] == 1 | t(Z)[upper.tri(Z)] == 1)
incomparable <- total_pairs - comparable
ic_ratio <- round(incomparable / comparable, 2)
h <- length(unique(V$y))
w <- max(table(V$y))
wh_ratio <- round(w / h, 2)

cat("Region:", regions[i], "| N:", n, "| Height:", h, "| Width:", w, 
    "| C:", comparable, "| I:", incomparable, 
    "| I/C:", ic_ratio, "| W/H:", wh_ratio, "\n")

#Sensitivity based on Bruggeman Patil, chapter 4, p48
dfSensitivity <- data.frame(columns = colnames(poset_data_num), sens = numeric(length(colnames(poset_data_num))))

Z <- get_poset(poset_data_num[], X)


for(i in 1:length(colnames(poset_data_num))) {
  dfSensitivity$sens[i] = sum(get_poset(poset_data_num[,-i], X) - Z)
}

dfSensitivity %>% 
  ggplot(aes(columns, sens)) + geom_bar(stat = "identity") + ylab("Sensitivity") + xlab("Variables") + ggtitle("Sensitivity analysis")

dfSensitivity %>% arrange(desc(sens))

# Analysis by regions
regions <- unique(hdi_regions$Region) 

for(i in 1:length(regions)) {
  hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t",show_col_types = FALSE)
  
  hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[i])
  
  X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
  hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
  Z <- get_poset(hdi_regions_filtered_Num, X)
  
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov)
  
  n <- nrow(Z)
  total_pairs <- n * (n - 1) / 2
  comparable <- sum(Z[upper.tri(Z)] == 1 | t(Z)[upper.tri(Z)] == 1)
  incomparable <- total_pairs - comparable
  ic_ratio <- round(incomparable / comparable, 2)
  h <- length(unique(V$y))
  w <- max(table(V$y))
  wh_ratio <- round(w / h, 2)
  
  cat("Region:", regions[i], "| N:", n, "| Height:", h, "| Width:", w, 
      "| C:", comparable, "| I:", incomparable, 
      "| I/C:", ic_ratio, "| W/H:", wh_ratio, "\n")
#### POSET amb altures mitjana ####
  M <- MRP(Z,method = "approx")
  avg_heigths_full <- colSums(M)
  
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov)
  
  V$y <- avg_heigths_full
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1
  
  # png(file=paste("../Geography of HDI/POSet_HDI23_avgHeight_", regions[i], ".png", sep = ""), width=1100, height=860)
  
  plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
       col = as.factor(hdi_regions_filtered$DevRegion), pch = 16,
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white")
  
  axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd=2)
  text(V, labels = rownames(Z.cov), cex = 0.75)
  legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:7, pch = 16)
  
  # dev.off()
  
  #Sensitivity
  
  dfSensitivity <- data.frame(columns = colnames(hdi_regions_filtered_Num), sens = numeric(length(colnames(hdi_regions_filtered_Num))))
  for(j in 1:length(colnames(hdi_regions_filtered_Num))) {
    dfSensitivity$sens[j] = sum(get_poset(hdi_regions_filtered_Num[,-j], X) - Z)
  }
  print(dfSensitivity)
  
  ggplot(dfSensitivity, aes(columns, sens)) + geom_bar(stat = "identity") + 
    ylab("Sensitivity") + xlab("Variables") + 
    ggtitle(paste("Sensitivity analysis from ", regions[i], " HDI data", sep = ""))
  ggsave(filename = paste("../Geography of HDI/Sensitivity_HDI23_", regions[i], ".png", sep = ""), 
         width=1100, height=860, units = "px")
}  
  

## Tabla Sensibilidades
## Omplim manualment dels prints del for anterior
dfSensitivity <- data.frame(
  Continent = c("Asia", "Europa", "Africa", "Americas", "Oceania", "World"),
  LE = c(98, 83, 251, 60, 30, 1405),
  EYS = c(81, 282, 92, 56, 1, 1210),
  MYS = c(147, 158, 127, 43, 6, 1446),
  GNIpc = c(46, 25, 83, 61, 7, 687)
)

dfSensitivity %>% 
  pivot_longer(-Continent, names_to = "Variables", values_to = "Changes") %>% 
  ggplot(aes(Variables, Changes)) + geom_bar(stat="identity") + facet_wrap(~ Continent)

cbind(Continent = dfSensitivity$Continent,dfSensitivity[,2:5]/ rowSums(dfSensitivity[,2:5])) %>% 
  pivot_longer(-Continent, names_to = "Variables", values_to = "Changes") %>% 
  ggplot(aes(Variables, Changes)) + geom_bar(stat="identity") + facet_wrap(~ Continent) +
  scale_y_continuous(breaks = seq(0, 0.8, by = .2), labels = scales::percent)


### Dominance and incomparability Synthesis of Multi-indicator System Over Time: A Poset-based Approach
# August 2021Social Indicators Research 157(3):1-23

hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t")
poset_data$Region %>% unique
hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[2])

X <- hdi_regions_filtered$Code
hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 

hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
  select(-Level, -Country, -Region, -SubRegion, -DevRegion)

Z <- get_poset(hdi_regions_filtered_Num, X)

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
text(incomparability ,dominance , labels = X, cex = 0.75)

## prova modelitzar


### comparativa clusters i poset original #### 

for(i in 1:length(regions)) {
  hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t",show_col_types = FALSE)
  
  hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[i])
  
  X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
  hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
  Z <- get_poset(hdi_regions_filtered_Num, X)
  palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "RdYlBu")) 
  
  plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16, shape = "equispaced")
  legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), 
         col = 1:length(unique(hdi_regions_filtered$DevRegion)), 
         pch = 16)
  
  km_clusters <- kmeans(x = hdi_regions_filtered_Num, centers = 6, nstart = 50)
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
                  LE = max(poset_data$LE),
                  EYS = max(poset_data$EYS),
                  MYS = max(poset_data$MYS),
                  GNIpC = max(poset_data$GNIpC),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp2 <- data.frame(Country = "PP2",
                  Code = "Q3",
                  LE = quantile(poset_data$LE, 0.75),
                  EYS = quantile(poset_data$EYS, 0.75),
                  MYS = quantile(poset_data$MYS, 0.75),
                  GNIpC = quantile(poset_data$GNIpC, 0.75),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp3 <- data.frame(Country = "PP3",
                  Code = "Q2",
                  LE = quantile(poset_data$LE, 0.5),
                  EYS = quantile(poset_data$EYS, 0.5),
                  MYS = quantile(poset_data$MYS, 0.5),
                  GNIpC = quantile(poset_data$GNIpC, 0.5),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp4 <- data.frame(Country = "PP4",
                  Code = "Q1",
                  LE = quantile(poset_data$LE, 0.25),
                  EYS = quantile(poset_data$EYS, 0.25),
                  MYS = quantile(poset_data$MYS, 0.25),
                  GNIpC = quantile(poset_data$GNIpC, 0.25),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp5 <- data.frame(Country = "PP5",
                  Code = "MIN",
                  LE = min(poset_data$LE),
                  EYS = min(poset_data$EYS),
                  MYS = min(poset_data$MYS),
                  GNIpC = min(poset_data$GNIpC),
                  Level  = "Benchmark",
                  Region = "Benchmark",
                  SubRegion = "Benchmark",
                  DevRegion = "Benchmark")

pp <- rbind(pp1, pp2, pp3, pp4, pp5) 
rownames(pp) <- NULL

# generate the embedded plots
png(file="../Geography of HDI/3-POSET_HDI23_Embedded.png", res = 240, height = 3600, width = 4200)
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
  # palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "RdYlBu")) 
  # 
  plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16)
  # legend('bottom', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:7, pch = 16)
  
}

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

plot(Z, col = as.factor(hdi_regions_filtered$DevRegion), pch = 16)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = levels(as.factor(hdi_regions_filtered$DevRegion)),
       col = 1:7, lwd = 5, xpd = TRUE, horiz = TRUE, cex = 0.8, pch = 16, seg.len=1)
#

dev.off()


# Generació dels average heights amb els embedded plots
png(file="../Geography of HDI/4-POSET_HDI23_Embedded_AverageHeights.png", res = 240, height = 4200, width = 2700)
par(mfrow = c(3,2))
for(i in 1:length(regions)) {
  hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t")
  
  hdi_regions_filtered <- hdi_regions_filtered %>% filter(Region==regions[i])
  
  hdi_regions_filtered <- rbind(hdi_regions_filtered, pp)
  
  X <- hdi_regions_filtered$Code
  hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 
  
  hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
    select(-Level, -Country, -Region, -SubRegion, -DevRegion)
  
  Z <- get_poset(hdi_regions_filtered_Num, X)
  palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "RdYlBu")) 

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
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white")
  
  axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd=2)
  text(V, labels = rownames(Z.cov), cex = 0.75)
  legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:7, pch = 16)
  
}

## and now the world
hdi_regions_filtered <- read_delim("../HDI2023.txt", delim = "\t")

hdi_regions_filtered <- rbind(hdi_regions_filtered, pp)

X <- hdi_regions_filtered$Code
hdi_regions_filtered <- column_to_rownames(hdi_regions_filtered, var = "Code") 

hdi_regions_filtered_Num <- hdi_regions_filtered %>% 
  select(-Level, -Country, -Region, -SubRegion, -DevRegion)

Z <- get_poset(hdi_regions_filtered_Num, X)
palette(brewer.pal(n =length(unique(hdi_regions_filtered$DevRegion)), name = "RdYlBu")) 

#### POSET amb altures mitjana ####
M <- MRP(Z,method = "approx")
avg_heigths_full <- colSums(M)

Z.cov <- incidence2cover(Z)
V <- -vertices(Z.cov)

V$y <- avg_heigths_full
xlim <- c(min(V$x), max(V$x)) * 1.3
ylim <- c(min(V$y), max(V$y)) * 1

plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
     col = as.factor(hdi_regions_filtered$DevRegion), pch = 16,
     ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white")

axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
grid(lwd=2)
text(V, labels = rownames(Z.cov), cex = 0.75)
legend('bottomright', legend = levels(as.factor(hdi_regions_filtered$DevRegion)), col = 1:7, pch = 16)

dev.off()

world_comparison <- cbind(hdi_regions_filtered, avg_heigths_full)

world_comparison <- world_comparison %>% 
  mutate(misclassification = ifelse(avg_heigths_full>world_comparison[world_comparison$Country == "PP2",]$avg_heigths_full, "Q1",
                                    ifelse(avg_heigths_full>world_comparison[world_comparison$Country == "PP3",]$avg_heigths_full, "Q2",
                                           ifelse(avg_heigths_full>world_comparison[world_comparison$Country == "PP4",]$avg_heigths_full, "Q3","Q4"))))

table(world_comparison$misclassification, world_comparison$Level)


#### Example POSET ####

countries <- c("Switzerland", "United States", "Austria", "Mexico", "Paraguay")

hdi_regions <- read_delim("../HDI2023.txt", delim = "\t")
hdi_regions <- hdi_regions %>% filter(Country %in% countries)

X <- hdi_regions$Country
hdi_regions <- column_to_rownames(hdi_regions, var = "Country") 

hdi_regions_Num <- hdi_regions %>% 
  select(-Level, -Code, -Region, -SubRegion, -DevRegion)

Z <- get_poset(hdi_regions_Num, X)

#par(mar = c(bottom, left, top, right)) 
def_margin <- c(5.1, 4.1, 4.1, 2.1)
par(mar = c(1,0, 0, 2))
plot(Z, shape = "equispaced")
