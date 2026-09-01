# ----------------------------------------------------------------------------
# Original file: poset_i_SOM_all_hdi.R
# 30 years of HDI: poset of the full HDI time series, sensitivity,
# SOM maps and clusters, per-year posets, correlation of average
# heights with the official HDI ranking.
#
# Notes / known issues (code kept as in the original):
#  - expects data/HDR23-24_Composite_indices_complete_time_series.csv (UNDP)
#  - FIXME: iso3 == 'SLV' is El Salvador; Slovenia is 'SVN'
#  - FIXME: in the per-year loops, colors use all_hdi_noNA$Level (the global dataset) instead of the year subset, so point colors can be misaligned
#  - FIXME: the last sections use palette.name/nvars/codes defined in sandbox/som_codes_hack.R
# ----------------------------------------------------------------------------

# Load the support functions (run from the repository root, e.g. after
# opening the .Rproj file in RStudio):
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))


library(POSetR) 
library(tidyverse) 
library(parsec) 
library(countrycode) 
library(RColorBrewer) 
library(kohonen)
library(factoextra) 

all_hdi <- read_csv("data/HDR23-24_Composite_indices_complete_time_series.csv")
#“Source: UNDP (United Nations Development Programme). 2022. Human Development Report 2021/2022: Uncertain Times, Unsettled Lives: Shaping our Future in a Transforming World. New York.”

#### Preparació de les dades ####
names(all_hdi)

all_hdi <- all_hdi %>% 
  filter(!is.na(hdicode)) %>% 
  select(iso3, country, hdicode, region, starts_with("le_"), starts_with("eys_"), starts_with("mys_"), starts_with("gnipc_")) %>% 
  select(-starts_with("le_m"), -starts_with("le_f"),
         -starts_with("eys_m"), -starts_with("eys_f"), 
         -starts_with("mys_m"), -starts_with("mys_f"))
  
all_eys <- all_hdi %>%
  select(iso3, country, hdicode, region, starts_with("eys")) %>% 
  pivot_longer(
    cols = eys_1990:eys_2022,
    names_to = c("Year"),
    names_pattern = "eys_(.*)",
    values_to = "EYS"
  ) 
  
all_mys <- all_hdi %>%
  select(iso3, country, hdicode, region, starts_with("mys")) %>% 
  pivot_longer(
    cols = mys_1990:mys_2022,
    names_to = c("Year"),
    names_pattern = "mys_(.*)",
    values_to = "MYS"
  ) 

all_gnipc <- all_hdi %>%
  select(iso3, country, hdicode, region, starts_with("gnipc")) %>% 
  pivot_longer(
    cols = gnipc_1990:gnipc_2022,
    names_to = c("Year"),
    names_pattern = "gnipc_(.*)",
    values_to = "GNIpC"
  ) %>% 
  mutate(GNIpC = log(GNIpC))

all_le <- all_hdi %>%
  select(iso3, country, hdicode, region, starts_with("le")) %>% 
  pivot_longer(
    cols = le_1990:le_2022,
    names_to = c("Year"),
    names_pattern = "le_(.*)",
    values_to = "LE"
  ) 

all_hdi_long <- full_join(all_eys, all_mys) %>% 
  full_join(all_le) %>% 
  full_join(all_gnipc)
  
#### POSET ####
all_hdi$Level <- ordered(all_hdi$hdicode, levels = c("Very High", "High", "Medium", "Low")) 
all_hdi_noNA <- all_hdi %>% 
  select(-country, -region, -hdicode) %>% 
  drop_na()

all_hdi_noNANum <- all_hdi_noNA %>% 
  select(-Level) %>% 
  mutate_at(vars(matches("gni")), log)

X <- all_hdi_noNANum$iso3
all_hdi_noNANum <- column_to_rownames(all_hdi_noNANum, var = "iso3") 

Z <- get_poset(all_hdi_noNANum, X)
print(summary(poset_from_incidence(Z)))

#Sensitivity based on Bruggeman Patil, chapter 4, p48
dfSensitivity <- data.frame(columns = colnames(all_hdi_noNANum), sens = numeric(length(colnames(all_hdi_noNANum))))

Z <- get_poset(all_hdi_noNANum[], X)
for(i in 1:length(colnames(all_hdi_noNANum))) {
  cat(i/length(colnames(all_hdi_noNANum)))
  dfSensitivity$sens[i] = sum(get_poset(all_hdi_noNANum[,-i], X) - Z)
}

dfSensitivity %>%
  filter(sens > 1) %>% 
  ggplot(aes(columns, sens)) + geom_bar(stat = "identity") + 
  ylab("Number of ordinal changes caused by deleting a given variable") + ggtitle("Sensitivity analysis") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dfSensitivity %>% 
  mutate(shortVar = substr(columns, 1, 3)) %>% 
  group_by(shortVar) %>% 
  summarise(totalSens = sum(sens)) %>% 
  ggplot(aes(shortVar, totalSens)) + geom_bar(stat = "identity") + 
  ylab("Number of ordinal changes caused by deleting a given variable") + xlab(NULL) + ggtitle("Sensitivity analysis") +
  theme_minimal() 


dfSensitivity %>% arrange(desc(sens))

palette(brewer.pal(n =4, name = "RdYlBu")) 

png(file=paste("output/POSet_allHDI.png", sep = ""), width=1100, height=860)
plot(Z, col = as.factor(all_hdi_noNA$Level), pch = 16)

legend('bottomright', legend = levels(as.factor(all_hdi_noNA$Level)), col = 1:4, pch = 16)
dev.off()


M <- MRP(Z,method = "approx")
avg_heigths_full <- colSums(M)

#### POSET amb altures mitjana ####
Z.cov <- incidence2cover(Z)
V <- -vertices(Z.cov)

V$y <- avg_heigths_full
xlim <- c(min(V$x), max(V$x)) * 1.3
ylim <- c(min(V$y), max(V$y)) * 1

png(file=paste("output/POSet_allHDI_avgHeights.png", sep = ""), width=1100, height=860)

plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
     ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white", pch = 16, col = as.factor(all_hdi_noNA$Level))

axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
grid(lwd=2)
text(V, labels = rownames(Z.cov), cex = 0.75)
dev.off()

#### SOM Map i Clusters ####
dataPoset.train <- as.matrix(scale(all_hdi_noNANum)) # make a SOM grid 
set.seed(100) 
dataPoset.grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")

set.seed(100) 
dataPoset.model <- som(dataPoset.train, dataPoset.grid, rlen = 500, radius = 2.5, keep.data = TRUE)

# str(dataPoset.model) 
#data.frame(country = rownames(dataPosetNum), cell = dataPoset.model$unit.classif) %>% arrange(desc(cell))
DT::datatable(data.frame(country = rownames(all_hdi_noNANum), cell = dataPoset.model$unit.classif))
#plot(dataPoset.model, type = "changes")

png(file=paste("output/SOM_allHDI_mapping.png", sep = ""), width=1100, height=860)
plot(dataPoset.model, type = "mapping", pchs = 19, shape = "straight", col = as.factor(all_hdi_noNA$Level)) 
legend('bottomright', legend = levels(as.factor(all_hdi_noNA$Level)), col = 1:4, cex = 0.8, pch = 16)
dev.off()

png(file=paste("output/SOM_allHDI_codesplot.png", sep = ""), width=1100, height=860)
plot(dataPoset.model, type = "codes", main = "Codes Plot", shape="s")
dev.off()

png(file=paste("output/SOM_allHDI_neighbours.png", sep = ""), width=1100, height=860)
plot(dataPoset.model, type = "dist.neighbours", shape = "s")
dev.off()

set.seed(100) 
fviz_nbclust(dataPoset.model$codes[[1]], kmeans, method = "wss")

set.seed(100) 
clust <- kmeans(dataPoset.model$codes[[1]], centers = 3) #clust <- kmeans(dataPoset.model$codes[[1]], 9) clust

dataPoset.cluster <- data.frame(all_hdi_noNA, cluster = clust$cluster[dataPoset.model$unit.classif]) 
#tail(dataPoset.cluster, 10) 
# clustering using hierarchial # cluster.som <- cutree(hclust(dist(dataPoset.model$codes[[1]])), 6)

#Cluster boundaries plot(dataPoset.model, type = "codes", bgcol = rainbow(1)[clust$cluster], main = "Cluster SOM", shape = "s") add.cluster.boundaries(dataPoset.model, clust$cluster)

png(file=paste("output/SOM_allHDI_cluster.png", sep = ""), width=1100, height=860)
plot(dataPoset.model, type = "mapping", pchs = 19, shape = "straight", col = as.factor(all_hdi_noNA$Level)) 
legend('bottomright', legend = levels(as.factor(all_hdi_noNA$Level)), col = 1:4, cex = 0.8, pch = 16) 
add.cluster.boundaries(dataPoset.model, clust$cluster)
dev.off()

png(file=paste("output/SOM_allHDI_clustermap.png", sep = ""), width=1100, height=860)
plot(dataPoset.model, type = "codes", shape = "s", bgcol = terrain.colors(4)[clust$cluster], main = "Cluster Map")
add.cluster.boundaries(dataPoset.model, clust$cluster)
dev.off()

png(file=paste("output/SOM_allHDI_clusterlevels.png", sep = ""), width=1100, height=860)
palette(brewer.pal(n =4, name = "RdYlBu")) 
plot(Z, col = as.factor(dataPoset.cluster$cluster), pch = 16)
legend('bottomright', legend = levels(as.factor(dataPoset.cluster$cluster)), col = 1:4, pch = 16)
dev.off()


#### POSET per anys ####

all_hdi_long$Level <- ordered(all_hdi_long$hdicode, levels = c("Very High", "High", "Medium", "Low")) 
Years <- unique(all_hdi_long$Year)
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
    select(-Level, -Year)
  
  X <- data_noNANum$iso3
  data_noNANum <- column_to_rownames(data_noNANum, var = "iso3") 
  
  Z <- get_poset(data_noNANum, X)
  print(summary(poset_from_incidence(Z)))
  
  png(file=paste("output/POSET_HDI_", Years[i], ".png", sep = ""), width=1100, height=860)
  plot(Z, col = as.factor(data_noNA$Level), pch = 16)
  legend('bottomright', legend = levels(as.factor(data_noNA$Level)), col = 1:4, pch = 16)
  dev.off()
  
  M <- MRP(Z,method = "approx")
  avg_heigths_full <- colSums(M)
  average_heights[[paste("avg_heights_", Years[i], sep="")]] <- colSums(M)
  
  #Sensitivity based on Bruggeman Patil, chapter 4, p48
  dfSensitivity <- data.frame(columns = colnames(data_noNANum), sens = numeric(length(colnames(data_noNANum))))
  for(j in 1:length(colnames(data_noNANum))) {
    dfSensitivity$sens[j] = sum(get_poset(data_noNANum[,-j], X) - Z)
  }
  #print(dfSensitivity)
  dfSensitivity %>% 
    ggplot(aes(columns, sens)) + geom_bar(stat = "identity") + 
    ylab("Number of ordinal changes caused by deleting a given variable") + ggtitle(paste("Sensitivity analysis from ", Years[i], " HDI data", sep = ""))
  ggsave(filename = paste("output/Sensitivity_HDI_", Years[i], ".png", sep = ""), 
         width=1100, height=860, units = "px")
  
  #### POSET amb altures mitjana ####
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov)

  V$y <- avg_heigths_full
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1

  png(file=paste("output/POSET_HDI_heights_", Years[i], ".png", sep = ""), width=1100, height=860)

  plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "",
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white", pch = 16, col = as.factor(data_noNA$Level))

  axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd=2)
  text(V, labels = rownames(Z.cov), cex = 0.75)
  legend('bottomright', legend = levels(as.factor(data_noNA$Level)), col = 1:4, pch = 16)

  dev.off()

  countries <- append(countries, summary(Z)$number.of.elements)
  incomparabilities <- append(incomparabilities, summary(Z)$number.of.comparability)
}

# SOMs per Years
#### SOM Map i Clusters ####
for(i in 1:length(Years)){
  
  data_noNA <- all_hdi_long %>% 
    filter(Year == Years[i]) %>% 
    select(-country, -region, -hdicode, -Year, -Level) %>% 
    drop_na() %>% 
    column_to_rownames(var = "iso3") 
    

  dataPoset.train <- as.matrix(scale(data_noNA)) # make a SOM grid 
  set.seed(100) 
  dataPoset.grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
  
  set.seed(100) 
  dataPoset.model <- som(dataPoset.train, dataPoset.grid, rlen = 500, radius = 2.5, keep.data = TRUE)
  
  # str(dataPoset.model) 
  #data.frame(country = rownames(dataPosetNum), cell = dataPoset.model$unit.classif) %>% arrange(desc(cell))
  DT::datatable(data.frame(country = rownames(data_noNA), cell = dataPoset.model$unit.classif))
  #plot(dataPoset.model, type = "changes")
  
  png(file=paste("output/SOM_HDI", Years[i], "_mapping.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "mapping", pchs = 19, shape = "straight", col = as.factor(all_hdi_noNA$Level)) 
  legend('bottomright', legend = levels(as.factor(all_hdi_noNA$Level)), col = 1:4, cex = 0.8, pch = 16)
  dev.off()
  
  png(file=paste("output/SOM_HDI", Years[i], "_codesplot.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "codes", main = "Codes Plot", shape="s")
  dev.off()
  
  png(file=paste("output/SOM_HDI", Years[i], "_neighbours.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "dist.neighbours", shape = "s")
  dev.off()
  
  set.seed(100) 
  fviz_nbclust(dataPoset.model$codes[[1]], kmeans, method = "wss")
  
  set.seed(100) 
  clust <- kmeans(dataPoset.model$codes[[1]], centers = 3) #clust <- kmeans(dataPoset.model$codes[[1]], 9) clust
  
  dataPoset.cluster <- data.frame(data_noNA, cluster = clust$cluster[dataPoset.model$unit.classif]) 
  #tail(dataPoset.cluster, 10) 
  # clustering using hierarchial # cluster.som <- cutree(hclust(dist(dataPoset.model$codes[[1]])), 6)
  
  #Cluster boundaries plot(dataPoset.model, type = "codes", bgcol = rainbow(1)[clust$cluster], main = "Cluster SOM", shape = "s") add.cluster.boundaries(dataPoset.model, clust$cluster)
  
  png(file=paste("output/SOM_HDI", Years[i], "_cluster.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "mapping", pchs = 19, shape = "straight", col = as.factor(all_hdi_noNA$Level)) 
  legend('bottomright', legend = levels(as.factor(all_hdi_noNA$Level)), col = 1:4, cex = 0.8, pch = 16) 
  add.cluster.boundaries(dataPoset.model, clust$cluster)
  dev.off()
  
  png(file=paste("output/SOM_HDI", Years[i], "_clustermap.png", sep = ""), width=1100, height=860)
  plot(dataPoset.model, type = "codes", shape = "s", bgcol = rainbow(4)[clust$cluster], main = "Cluster Map")
  add.cluster.boundaries(dataPoset.model, clust$cluster)
  dev.off()
  
  png(file=paste("output/SOM_HDI", Years[i], "_clusterlevels.png", sep = ""), width=1100, height=860)
  palette(brewer.pal(n =4, name = "RdYlBu")) 
  plot(Z, col = as.factor(dataPoset.cluster$cluster), pch = 16)
  legend('bottomright', legend = levels(as.factor(dataPoset.cluster$cluster)), col = 1:4, pch = 16)
  dev.off()
}

# Convertir llista avg_heights a dataframe

dfHeights <- data.frame('2022' = average_heights[[length(names(average_heights))]])
dfHeights$X2022 <- NULL

for(i in 1:length(names(average_heights))) {
  dfHeights[,paste("t", i, sep="")] <- average_heights[[i]][match(row.names(dfHeights), names(average_heights[[i]]))]
}

names(dfHeights) <- substr(names(average_heights), max(nchar(names(average_heights))) - 3, 100)


#### Preparació de les dades ####
rank_all_hdi <- read_csv("data/HDR23-24_Composite_indices_complete_time_series.csv")

rank_all_hdi <- rank_all_hdi %>% 
  filter(!is.na(hdicode)) %>% 
  select(iso3, country, hdicode, region, starts_with("hdi_")) %>% 
  select(-starts_with("hdi_f"), -starts_with("hdi_m"), -starts_with("hdi_rank_2022"))
 
# Gràfics de correlació entre hdi rank i average heigh del POSET
rank_all_hdi$Level <- ordered(rank_all_hdi$hdicode, levels = c("Very High", "High", "Medium", "Low")) 

for (i in 1:length(names(dfHeights))) {
  print(cor(dfHeights[, i], rank(rank_all_hdi[, 4+i]), use = "p", method = "spearman"))
  plot(dfHeights[, i], rank(rank_all_hdi[, 4+i], na.last = "keep"), main=paste("Year", names(dfHeights)[i]), 
       xlab = "Average Heights from POSET", ylab = "Actual HDI rank", col = as.factor(rank_all_hdi$Level), pch = 16)
  abline(a=0, b=1)
  legend('bottomright', legend = levels(as.factor(rank_all_hdi$Level)), col = 1:4, pch = 16)
  
}

difHeightRank <- rank_all_hdi %>% 
  filter(!is.na(hdicode)) %>% 
  select(iso3, country, Level, region)
 
for (i in 1:length(names(dfHeights))) {
  difHeightRank[,names(dfHeights)[i]] <- dfHeights[, i] - rank(rank_all_hdi[, 4+i], na.last = "keep") # negatiu vol dir que el poset sobreestima el resultat del rank

}
difHeightRank <- difHeightRank %>% 
  rowwise() %>% 
  mutate(mean_diff = mean(c_across(where(is.numeric)), na.rm=TRUE))
difHeightRank %>% ggplot(aes(mean_diff, fill = Level)) + geom_density(alpha = 0.2) + theme_minimal() 
ggsave(filename = paste("output/trend_avg_heigths.png", sep = ""), 
       width=1800, height=1060, units = "px")

#### SIMPLIFICACIÓ SOM ####

simpleSOM <- data.frame(gni = dataPoset.model$codes %>% as.data.frame() %>% select(starts_with("gni")) %>% apply(1, mean),
                        le = dataPoset.model$codes %>% as.data.frame() %>% select(starts_with("le")) %>% apply(1, mean),
                        mys = dataPoset.model$codes %>% as.data.frame() %>% select(starts_with("mys")) %>% apply(1, mean),
                        eys = dataPoset.model$codes %>% as.data.frame() %>% select(starts_with("eys")) %>% apply(1, mean))


stars(simpleSOM, locations = dataPoset.model$grid$pts, labels = NULL,  
      len = 0.4, col.segments = palette.name(nvars), 
      draw.segments = TRUE)
legend("topright", legend = colnames(codes), 
       ncol = 1, fill = palette.name(nvars))
add.cluster.boundaries(dataPoset.model, clust$cluster)

#### INCLUSIÓ AVG HEIGHTS SOM ####

simpleSOM_complete <- data.frame(country = rownames(all_hdi_noNANum), 
                                 cell = dataPoset.model$unit.classif, 
                                 avg_height = avg_heigths_full, 
                                 cluster = dataPoset.cluster$cluster, 
                                 gni = all_hdi_noNANum %>% select(starts_with("gni")) %>% apply(1, mean),
                                 le = all_hdi_noNANum %>% select(starts_with("le")) %>% apply(1, mean),
                                 mys = all_hdi_noNANum %>% select(starts_with("mys")) %>% apply(1, mean),
                                 eys = all_hdi_noNANum %>% select(starts_with("eys")) %>% apply(1, mean)
)

simpleSOM_complete %>% 
  group_by(cell) %>% 
  summarise(mean(exp(gni)),
            mean(le),
            mean(mys),
            mean(eys),
            mean(avg_height),
            median(avg_height),
            sd(avg_height),
            n(),
            mean(cluster),)

data.frame(country = rownames(all_hdi_noNANum), 
           cell = dataPoset.model$unit.classif, 
           avg_height = avg_heigths_full, 
           cluster = dataPoset.cluster$cluster) %>% 
  ggplot(aes(x=as.factor(cluster), y=avg_height, group=cell, col=as.factor(cluster))) + geom_boxplot()

#### Posets Countries time

all_hdi <- read_csv("data/HDR23-24_Composite_indices_complete_time_series.csv")
#“Source: UNDP (United Nations Development Programme). 2022. Human Development Report 2021/2022: Uncertain Times, Unsettled Lives: Shaping our Future in a Transforming World. New York.”
all_countries <- full_join(all_eys, all_gnipc) %>% 
  full_join(all_le) %>% full_join(all_mys)

Venezuela <- all_countries %>% filter(iso3 == "VEN") %>% 
  select(-iso3, -country, -hdicode, -region) %>% 
  mutate_at(vars(matches("gni")), log)

X <- Venezuela$Year
Venezuela <- column_to_rownames(Venezuela, var = "Year") 

Z <- get_poset(Venezuela, X)
print(summary(poset_from_incidence(Z)))
par(mar=c(0,0,0,0))
plot(Z)

Slovenia <- all_countries %>% filter(iso3 == "SLV") %>% 
  select(-iso3, -country, -hdicode, -region) %>% 
  mutate_at(vars(matches("gni")), log)

X <- Slovenia$Year
Slovenia <- column_to_rownames(Slovenia, var = "Year") 

Z <- get_poset(Slovenia, X)
print(summary(poset_from_incidence(Z)))
plot(Z)


Turkey <- all_countries %>% filter(iso3 == "TUR") %>% 
  select(-iso3, -country, -hdicode, -region) %>% 
  mutate_at(vars(matches("gni")), log)

X <- Turkey$Year
Turkey <- column_to_rownames(Turkey, var = "Year") 

Z <- get_poset(Turkey, X)
print(summary(poset_from_incidence(Z)))
plot(Z)


Albania <- all_countries %>% filter(iso3 == "ALB") %>% 
  select(-iso3, -country, -hdicode, -region) %>% 
  mutate_at(vars(matches("gni")), log)

X <- Albania$Year
Albania <- column_to_rownames(Albania, var = "Year") 

Z <- get_poset(Albania, X)
print(summary(poset_from_incidence(Z)))
plot(Z)
