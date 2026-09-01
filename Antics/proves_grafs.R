library(igraph)
library(networkD3)
library(tidyverse)
library(parsec)

allButN <- function(x, y, n = 0) {
  if (length(x) != length(y)) {
    stop("Vectors must have the same length")
  }
  
  sum(x < y) == length(x) - n
}
Vectorize(allButN)

dFattore <- read_delim("C:/Users/franc/OneDrive - IQS/IQS/Recerca/POSET/Data_Fattore_PoseticToolsTutorial.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)
dFattore <- dFattore %>% 
  mutate(GrossDebt = -1 * GrossDebt)

X <- dFattore$State
dFattore <- column_to_rownames(dFattore, var = "State")
dFattore$State <- NULL
dFattore$Country <- NULL
r <- function(x,y) allButN(dFattore[x,], dFattore[y,], 0)
r <- Vectorize(r)
Z <- outer(X, X, FUN = r)
dimnames(Z) <- list(X, X)
Z
Z <- validate.partialorder.incidence(Z)
plot(Z)


!Z
net <- graph_from_adjacency_matrix(Z)
plot(net, edge.arrow.size=.2, edge.curved=0,
       vertex.color="orange", vertex.frame.color="#555555",
       vertex.label.color="black", vertex.label.cex=.7)




net3D <- as.data.frame(get.edgelist(net))

# Plot https://r-graph-gallery.com/network-interactive.html
p <- simpleNetwork(net3D, height="100px", width="100px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   arrows = T,
                   zoom = T,                    # Can you zoom on the figure?
                   )
p

URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
              "master/JSONdata/miserables.json")
MisJson <- jsonlite::fromJSON(URL)
ValjeanInds <- which(MisJson$links == 11, arr = TRUE)[, 1]
ValjeanCols <- ifelse(1:nrow(MisJson$links) %in% ValjeanInds, "#bf3eff", "#666")

forceNetwork(Links = MisLinks, 
             Nodes = MisNodes, 
             Source = "source", 
             Target = "target", 
             Value = "value", 
             NodeID = "name", 
             Group = "group", 
             opacity = 0.8, 
             arrows = TRUE, 
             zoom = TRUE)
N