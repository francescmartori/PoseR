# get_poset() ----------------------------------------------------------------
# Build the incidence matrix of a partial order from a data frame of
# (numeric) indicators: element x precedes element y iff x <= y on every
# column. The result is validated as a partial order with parsec.
#
# Arguments:
#   data  data frame (rows = elements, columns = numeric indicators).
#         Row order must match the identifiers in `X`.
#   X     vector of element identifiers (used as dimnames of the matrix).
#
# Value: a validated incidence matrix (class from parsec), ready for
#   plot(), MRP(), incidence2cover(), etc.
#
# Depends on: parsec (validate.partialorder.incidence)

get_poset <- function(data, X) {

  r <- function(x, y) all(data[x, ] <= data[y, ])
  r <- Vectorize(r)
  Z <- outer(X, X, FUN = r)
  dimnames(Z) <- list(X, X)
  Z <- validate.partialorder.incidence(Z)
  Z
}
