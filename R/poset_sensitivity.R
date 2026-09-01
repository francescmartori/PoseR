# poset_sensitivity() ---------------------------------------------------------
# Leave-one-indicator-out sensitivity analysis of a poset, following
# Bruggemann & Patil, "Ranking and Prioritization for Multi-indicator
# Systems" (chapter 4, p. 48).
#
# For each indicator, the poset is rebuilt without it and the number of
# changed entries in the incidence matrix is counted: the larger the count,
# the more that indicator shapes the partial order.
#
# This consolidates the sensitivity loops that were copy-pasted across the
# HDI analysis scripts.
#
# Arguments:
#   data     numeric data frame used to build the poset.
#   X        element identifiers (as in get_poset()).
#   Z        optional: the full poset, recomputed if not supplied.
#   verbose  print progress (useful for large posets).
#
# Value: data.frame(variable, sens), one row per indicator.

poset_sensitivity <- function(data, X, Z = NULL, verbose = FALSE) {
  if (is.null(Z)) Z <- get_poset(data, X)

  vars <- colnames(data)
  sens <- numeric(length(vars))

  for (i in seq_along(vars)) {
    if (verbose) cat(round(i / length(vars), 2), "\n")
    sens[i] <- sum(get_poset(data[, -i, drop = FALSE], X) - Z)
  }

  data.frame(variable = vars, sens = sens)
}
