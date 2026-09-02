# analyze_relations() ---------------------------------------------------------
# For every element of the poset, list which elements it is comparable and
# incomparable with, plus overall counts.
#
# Arguments:
#   Z      validated incidence matrix (from get_poset()).
#   input  kept for interface symmetry with plot_poset(); not used.
#
# Value: named list, one entry per element with $comparable and
#   $uncomparable, plus $summary with total_elements,
#   total_comparable_pairs and total_incomparable_pairs.
#
# Depends on: parsec (incidence2cover, validate.partialorder.incidence)

analyze_relations <- function(Z, input = "min") {
  Z.cov <- incidence2cover(Z)
  Z.poset <- validate.partialorder.incidence(Z)  # Validate full partial order

  points <- rownames(Z)
  n <- length(points)

  # Initialize symmetric relation matrix
  sym_comparable <- matrix(0, n, n, dimnames = list(points, points))

  for (i in seq_along(points)) {
    for (j in seq_along(points)) {
      if (i != j && Z.poset[points[i], points[j]] == 1) {
        sym_comparable[points[i], points[j]] <- 1
        sym_comparable[points[j], points[i]] <- 1
      }
    }
  }

  results <- list()
  for (i in seq_along(points)) {
    point <- points[i]
    comparable <- points[sym_comparable[point, ] == 1]
    uncomparable <- setdiff(points, c(comparable, point))

    results[[point]] <- list(
      comparable = comparable,
      uncomparable = uncomparable
    )
  }

  # --- Summary stats ---
  total_comparables <- sum(sym_comparable) / 2
  total_possible_pairs <- choose(n, 2)

  results$summary <- list(
    total_elements = n,
    total_comparable_pairs = total_comparables,
    total_incomparable_pairs = total_possible_pairs - total_comparables
  )

  results
}


# poset_stats() ---------------------------------------------------------------
# Print a verbose summary of the poset: the per-element relations (same
# information as summary(poset_from_incidence(Z))[3]) and the number of
# levels (length of the longest chain).
# For a compact one-row numeric summary (I/C and W/H ratios), see
# poset_ratios().

poset_stats <- function(Z) {
  relations <- analyze_relations(Z)
  print(relations)
  print_number_of_levels(Z)
}
