# poset_ratios() --------------------------------------------------------------
# Compact structural summary of a poset: size, height, width, comparable and
# incomparable pairs, and the I/C and W/H ratios used in the Geography of
# HDI analyses. (Named poset_ratios to avoid clashing with poset_stats(),
# the verbose per-element summary in R/analyze_relations.R.)
#
# Arguments:
#   Z  validated incidence matrix (from get_poset()).
#
# Value: one-row data.frame with
#   n, height, width, comparable, incomparable, ic_ratio (I/C),
#   wh_ratio (W/H).
#
# Depends on: parsec (incidence2cover, vertices)

poset_ratios <- function(Z) {
  Z.cov <- incidence2cover(Z)
  V <- vertices(Z.cov, shape = "equispaced")

  n <- nrow(Z)
  total_pairs <- n * (n - 1) / 2
  comparable <- sum(Z[upper.tri(Z)] == 1 | t(Z)[upper.tri(Z)] == 1)
  incomparable <- total_pairs - comparable
  h <- length(unique(V$y))
  w <- max(table(V$y))

  data.frame(n = n,
             height = h,
             width = w,
             comparable = comparable,
             incomparable = incomparable,
             ic_ratio = round(incomparable / comparable, 2),
             wh_ratio = round(w / h, 2))
}
