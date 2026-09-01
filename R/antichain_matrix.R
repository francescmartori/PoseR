# antichainMatrix() -----------------------------------------------------------
# Antichain analysis of a data matrix, following:
#   Bruggemann, R. and Voigt, K. (2012). "Antichains in Partial Order,
#   Example: Pollution in a German Region by Lead, Cadmium, Zinc and Sulfur".
#   MATCH Commun. Math. Comput. Chem. 67, 731-744.
#
# For every pair of columns (indicators) and every pair of rows (elements),
# it flags whether the two indicators disagree on the ordering of the two
# elements (i.e. generate an incomparability).
#
# Value: a list with
#   RAC      per indicator pair: how many incomparabilities it generates
#   RAC_Norm RAC normalized by ncol^2 / 4
#   CAC      per element pair: how incomparable the pair is
#   CAC_Norm CAC normalized by the number of element pairs
#   antiMat  the full pair-by-pair 0/1 matrix
#
# Depends on: magrittr pipe (loaded with tidyverse)

antichainMatrix <- function(df) {

  preMat <- apply(df, 2, function(y) combn(y, 2, function(x) x[1] <= x[2])) %>%
    as.data.frame()
  colnames(preMat) <- colnames(df)

  antiMat <- do.call(cbind, combn(colnames(preMat), 2,
                                  FUN = function(x) list(as.numeric(preMat[x[1]] != preMat[x[2]]))))

  colnames(antiMat) <- combn(colnames(df), 2, FUN = function(x) paste(x[1], "-", x[2], sep = ""))
  rownames(antiMat) <- combn(rownames(df), 2, FUN = function(x) paste(x[1], "-", x[2], sep = ""))

  RAC <- colSums(antiMat)  # the larger, the more incomparabilities that indicator pair generates
  RAC_Norm <- RAC / (ncol(df)^2 / 4)
  CAC <- rowSums(antiMat)  # the larger, the more incomparable that pair of elements is
  CAC_Norm <- CAC / (nrow(df) * (nrow(df) - 1) / 2)

  list(RAC = RAC,
       RAC_Norm = RAC_Norm,
       CAC = CAC,
       CAC_Norm = CAC_Norm,
       antiMat = antiMat)
}
