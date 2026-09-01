# poset_dominance() -----------------------------------------------------------
# Dominance and incomparability scores of the elements of a poset, based on
# the mutual ranking probability (MRP) matrix. Follows the approach in:
#   "Synthesis of Multi-indicator System Over Time: A Poset-based Approach",
#   Social Indicators Research 157(3), 2021.
#
# dominance:        first right singular vector of the MRP matrix (absolute
#                   values).
# incomparability:  leading eigenvector of the symmetric matrix
#                   2 * min(M_ij, M_ji) (absolute values).
#
# Arguments:
#   Z       validated incidence matrix (from get_poset()).
#   method  method passed to parsec::MRP ("approx" or "exact").
#
# Value: data.frame(element, dominance, incomparability).
#
# Depends on: parsec (MRP)

poset_dominance <- function(Z, method = "approx") {
  M <- MRP(as.matrix(Z), method = method)

  dominance <- abs(svd(M)$v[, 1])

  inc <- matrix(0, nrow = nrow(M), ncol = ncol(M))
  for (i in seq_len(nrow(M))) {
    for (j in seq_len(ncol(M))) {
      if (i != j) inc[i, j] <- 2 * min(M[i, j], M[j, i])
    }
  }

  incomparability <- abs(eigen(inc)$vectors[, 1])

  data.frame(element = rownames(Z),
             dominance = dominance,
             incomparability = incomparability)
}
