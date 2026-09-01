# benchmark_profiles() --------------------------------------------------------
# Build benchmark ("embedded scale") profiles for a numeric data frame:
# artificial elements at chosen quantiles of every indicator. Adding them to
# a dataset before building the poset embeds a fully comparable reference
# scale (MAX > Q3 > Q2 > Q1 > MIN) in the Hasse diagram.
#
# This generalizes the pp1..pp5 blocks that were duplicated in the HDI
# analysis scripts (where the profiles also carried the qualitative columns
# Region = "Benchmark" etc.; add those afterwards as needed).
#
# Arguments:
#   data   numeric data frame (same columns used to build the poset).
#   probs  named vector of quantile levels; names become the row names.
#
# Value: data.frame with one row per benchmark profile.

benchmark_profiles <- function(data,
                               probs = c(MAX = 1, Q3 = 0.75, Q2 = 0.5,
                                         Q1 = 0.25, MIN = 0)) {
  out <- t(sapply(probs, function(p) {
    sapply(data, quantile, probs = p, na.rm = TRUE)
  }))
  rownames(out) <- names(probs)
  colnames(out) <- colnames(data)
  as.data.frame(out)
}
