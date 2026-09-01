# ----------------------------------------------------------------------------
# Original file: Fattore_PoseticTools_Tutorial.R
# Reproduction of the example in Fattore's 'PoseticTools' tutorial:
# poset construction, Hasse diagram, average heights and eigenvector
# dominance.
#
# Notes / known issues (code kept as in the original):
#  - expects data/Data_Fattore_PoseticToolsTutorial.txt
# ----------------------------------------------------------------------------

library(tidyverse)
library(parsec)


dFattore <- read_delim("data/Data_Fattore_PoseticToolsTutorial.txt", 
                                                delim = "\t", escape_double = FALSE, 
                                                trim_ws = TRUE)
dFattore <- dFattore %>% 
  mutate(GrossDebt = -1 * GrossDebt)

X <- dFattore$State
dFattore <- column_to_rownames(dFattore, var = "State")
dFattore$State <- NULL
dFattore$Country <- NULL

r <- function(x,y) all(dFattore[x,] <= dFattore[y,])
r <- Vectorize(r)
Z <- outer(X, X, FUN = r)
dimnames(Z) <- list(X, X)
Z <- validate.partialorder.incidence(Z)
#Haasse diagram
plot(Z)



rownames(Z) <- X

M <- MRP(Z)
rownames(M) <- X

avr_height <- colSums(M) 
eigenvector <- abs(svd(M)$v[,1])

plot(rep(1,length(avr_height)),avr_height, xlim = c(0,4))
plot(rep(3,length(avr_height)), eigenvector)
