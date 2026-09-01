# Seguint Visualizing Partially ordered sets for socioeconomic analysis
# Fattore and co, 2014

# Falta incloure els stars 
# Veure com podem controlar la ordenació dels nodes del poset


library(kohonen)
library(parsec)
library(tidyverse)
dataPoset <- read_delim("C:/Users/franc/OneDrive - IQS/IQS/Recerca/POSET/DATA POSET ENVIRONMENT TEXT 9 JULY V2.txt", 
                        delim = "\t", escape_double = FALSE, 
                        col_types = cols(...8 = col_skip(), ...9 = col_skip()), 
                        trim_ws = TRUE)

dataPoset <- dataPoset %>% 
  mutate(Countries = as.factor(Countries)) %>% 
  glimpse()

dataPoset <- column_to_rownames(dataPoset, var = "Countries")

# make a train data sets that scaled and convert them to be a matrix cause kohonen function accept numeric matrix
dataPoset.train <- as.matrix(scale(dataPoset))

# make a SOM grid
set.seed(100)
dataPoset.grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")

# make a SOM model
set.seed(100)
dataPoset.model <- som(dataPoset.train, dataPoset.grid, 
                       rlen = 500, 
                       radius = 2.5, 
                       keep.data = TRUE,
                       mode = "batch")
dev.off()
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


# POset on the SOM neurons

X <- 1:25

r <- function(x,y) all(dataPoset.model$codes[[1]][x,] <= dataPoset.model$codes[[1]][y,])
r <- Vectorize(r)
Z <- outer(X, X, FUN = r)

Z <- validate.partialorder.incidence(Z)
dimnames(Z) <- list(X, X)

#Haasse diagram

Z.cov <- incidence2cover(Z)
V <- -vertices(Z.cov)

V$y <- dataPoset.model$codes[[1]][,1]
xlim <- c(min(V$x), max(V$x)) * 1.3
ylim <- c(min(V$y), max(V$y)) * 1.3

plot(V, y = , panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
     ylab = colnames(dataPoset.model$codes[[1]])[1], xlim = xlim, ylim = ylim, cex = 4, bg = "white", pch = 21)

axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
grid(lwd=2)
text(V, labels = rownames(Z.cov), cex = 0.75)

