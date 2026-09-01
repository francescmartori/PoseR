# ----------------------------------------------------------------------------
# Original file: Proves_poset.R
# Exploratory: SOM and t-SNE experiments on POSET environment data.
#
# Notes / known issues (code kept as in the original):
#  - expects data/DATA POSET ENVIRONMENT TEXT 9 JULY V2.txt
#  - exploratory code, not maintained
# ----------------------------------------------------------------------------

library(kohonen)
library(tidyverse)
library(Rtsne)
dataPoset <- read_delim("data/DATA POSET ENVIRONMENT TEXT 9 JULY V2.txt", 
                        delim = "\t", escape_double = FALSE, 
                        col_types = cols(...8 = col_skip(), ...9 = col_skip()), 
                        trim_ws = TRUE)

dataPoset <- dataPoset %>% 
  mutate(Countries = as.factor(Countries)) %>% 
  glimpse()

# make a train data sets that scaled and convert them to be a matrix cause kohonen function accept numeric matrix
dataPoset.train <- as.matrix(scale(dataPoset[,c(-1,-2)]))

# make a SOM grid
set.seed(100)
dataPoset.grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")

# make a SOM model
set.seed(100)
dataPoset.model <- som(dataPoset.train, dataPoset.grid, 
                       rlen = 500, 
                       radius = 2.5, 
                       keep.data = TRUE,
                 dist.fcts = "sumofsquares")

str(dataPoset.model)
 
dataPoset.model$unit.classif 
plot(dataPoset.model, type = "mapping", pchs = 19, shape = "straight")
head(data.frame(dataPoset.train), 20)
plot(dataPoset.model, type = "codes", main = "Codes Plot", shape="s")
plot(dataPoset.model, type = "changes")

plot(dataPoset.model, type = "dist.neighbours")

heatmap.som <- function(model){
  for (i in 1:10) {
    plot(model, shape = "straight", type = "property", property = getCodes(model)[,i], 
         main = colnames(getCodes(model))[i]) 
  }
}
heatmap.som(dataPoset.model)

library(factoextra)
set.seed(100)
fviz_nbclust(dataPoset.model$codes[[1]], kmeans, method = "wss")


set.seed(100)
clust <- kmeans(scale(data), 9)
#clust <- kmeans(dataPoset.model$codes[[1]], 9)
clust

# know cluster each data
dataPoset.cluster <- data.frame(dataPoset, cluster = clust$cluster[dataPoset.model$unit.classif])
tail(dataPoset.cluster, 10)
# clustering using hierarchial
# cluster.som <- cutree(hclust(dist(dataPoset.model$codes[[1]])), 6)

#Cluster boundaries
plot(dataPoset.model, type = "codes", bgcol = rainbow(9)[clust$cluster], main = "Cluster SOM", shape = "s")
add.cluster.boundaries(dataPoset.model, clust$cluster)

### tSNE
dataPoset.tsne <- dataPoset.train %>%
  Rtsne(perplexity = 25, max_iter = 2000, check_duplicates = FALSE)

tibble(x = dataPoset.tsne$Y[,1],
       y = dataPoset.tsne$Y[,2], 
       cluster = dataPoset.cluster[,8],
       node = dataPoset.model$unit.classif, 
       country = dataPoset$Countries) %>% 
  ggplot(aes(x,y)) + geom_point( aes(color=as.factor(cluster))) +
  geom_text(aes(label=country), check_overlap = TRUE) +
  theme(legend.position = "bottom")+
  labs(x="",y="") + 
  facet_wrap(~cluster)
  
