# poset_dissimilarity() -------------------------------------------------------
# Dissimilarity between a synthesis poset and a set of component posets,
# based on the L1 norm of the differences between their mutual ranking
# probability (MRP) matrices. Reproduces the measure used in the
# Arcagni (2022) example (see tutorials/arcagni_2022_dissimilarity.R).
#
# Arguments:
#   Z0      incidence matrix of the synthesis / reference poset.
#   Zs      list of incidence matrices of the component posets.
#   method  method passed to parsec::MRP ("exact" or "approx").
#
# Value: the mean relative L1 distance between MRP(Z0) and each MRP(Zi).
#
# Depends on: parsec (MRP)

l1_norm <- function(m) sum(abs(m))

poset_dissimilarity <- function(Z0, Zs, method = "exact") {
  M0 <- MRP(Z0, method = method)
  mean(sapply(Zs, function(Zi) {
    Mi <- MRP(Zi, method = method)
    l1_norm(M0 - Mi) / l1_norm(Mi)
  }))
}
