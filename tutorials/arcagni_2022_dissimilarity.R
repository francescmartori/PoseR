# ----------------------------------------------------------------------------
# Original file: Arcagni2022.R
# Reproduction of the Arcagni (2022) example: MRP-based dissimilarity
# between component posets and their synthesis.
#
# Notes / known issues (code kept as in the original):
#  - see R/poset_dissimilarity.R for the function extracted from this script
# ----------------------------------------------------------------------------

### Arcagni 2022

library(tidyverse) 
library(parsec) 

pi1 <- matrix(c(1,0,0,0,0,1,1,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,1), ncol = 5, byrow = TRUE)
pi1 <- validate.partialorder.incidence(pi1)
ppi1 <- MRP(pi1, method = "exact")

pi2 <- matrix(c(1,0,1,1,0,1,1,1,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,1), ncol = 5, byrow = TRUE)
pi2 <- validate.partialorder.incidence(pi2)
ppi2 <- MRP(pi2, method = "exact")

pi3 <- matrix(c(1,0,0,1,0,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,0,1,1,1), ncol = 5, byrow = TRUE)
pi3 <- validate.partialorder.incidence(pi3)
ppi3 <- MRP(pi3, method = "exact")

pi0 <- pi1 * pi2 * pi3
ppi0 <- MRP(pi0, method = "exact")

l1Norm <- function(matrix) sum(abs(matrix))

dissim <- mean(c(l1Norm(ppi0 - ppi1)/l1Norm(ppi1),
                 l1Norm(ppi0 - ppi2)/l1Norm(ppi2),
                 l1Norm(ppi0 - ppi3)/l1Norm(ppi3)))



piac <- matrix(c(1, 0, 1, 0, 0,
                 1, 1, 1, 0, 0,
                 0, 0, 1, 0, 0,
                 0, 0, 0, 1, 0,
                 0, 0, 0, 1, 1),
               ncol = 5, byrow = TRUE)

piac <- validate.partialorder.incidence(piac)
plot(piac)
ppiac <- MRP(piac)

mean(c(l1Norm(piac - ppi1)/l1Norm(ppi1),
       l1Norm(piac - ppi2)/l1Norm(ppi2),
       l1Norm(piac - ppi3)/l1Norm(ppi3)))
