# find_longest_chain() --------------------------------------------------------
# Find the longest chain of the poset by depth-first search over the cover
# relation, starting from the minimal elements. From SupportFunctions_FINAL.R.
#
# NOTE: this enumerates all maximal chains, which is exponential in the
# worst case - fine for teaching-sized posets, do not use on posets with
# hundreds of elements.
#
# Arguments:
#   Z  validated incidence matrix (from get_poset()).
#
# Value: character vector with the elements of the longest chain, bottom to
#   top (suitable for plot_poset(..., chain = )).
#
# Depends on: parsec (incidence2cover)

find_longest_chain <- function(Z) {
  cover_mat <- incidence2cover(Z)
  nodes <- rownames(cover_mat)

  # Adjacency list from cover matrix
  adj_list <- lapply(nodes, function(n) {
    successors <- which(cover_mat[n, ] == 1)
    nodes[successors]
  })
  names(adj_list) <- nodes

  # Minimal elements (no predecessors)
  minimal_nodes <- nodes[colSums(cover_mat) == 0]

  dfs <- function(node, path) {
    successors <- adj_list[[node]]
    if (length(successors) == 0) {
      return(list(path))
    }

    chains <- list()
    for (succ in successors) {
      extended_paths <- dfs(succ, c(path, succ))
      chains <- c(chains, extended_paths)
    }
    chains
  }

  all_chains <- list()
  for (start_node in minimal_nodes) {
    chains <- dfs(start_node, c(start_node))
    all_chains <- c(all_chains, chains)
  }

  chain_lengths <- sapply(all_chains, length)
  all_chains[[which.max(chain_lengths)]]
}


# print_number_of_levels() ----------------------------------------------------
# Print the longest chain and the number of levels (its length).

print_number_of_levels <- function(Z) {
  chain <- find_longest_chain(Z)
  cat("Longest chain:\n", paste(chain, collapse = " -> "), "\n")
  cat("Number of levels:", length(chain), "\n")
  invisible(length(chain))
}
