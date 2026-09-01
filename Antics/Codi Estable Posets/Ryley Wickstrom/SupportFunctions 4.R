
get_poset <- function(data, X) {
  
  r <- function(x,y) all(data[x,] <= data[y,]) 
  r <- Vectorize(r) 
  Z <- outer(X, X, FUN = r)
  dimnames(Z) <- list(X, X) 
  Z <- validate.partialorder.incidence(Z)
  Z
}



antichainMatrix <- function(df){
  # from Antichains in Partial order, Example: Pollution in a German Region by Lead, Cadmium, Zinc ...
  # Rainer Bruggeman and Kristina Voigt
  # MATCH Commun. Math. Comput. Chem 67 (2012) 731-744
  
  preMat <- apply(df, 2, function(y) combn(y, 2, function(x) x[1] <= x[2])) %>% 
    as.data.frame()
  colnames(preMat) <- colnames(df)
  
  antiMat <- do.call(cbind,combn(colnames(preMat), 2, 
                                 FUN= function(x) list(as.numeric(preMat[x[1]] != preMat[x[2]]))))
  
  colnames(antiMat) <- combn(colnames(df), 2, FUN = function(x) paste(x[1], "-", x[2], sep=""))
  rownames(antiMat) <- combn(rownames(df), 2, FUN = function(x) paste(x[1], "-", x[2], sep=""))
  
  RAC <- colSums(antiMat) # com més gran, més incomparabilitats provoca aquell parell de variables
  RAC_Norm <- RAC / ( ncol(df) ^ 2 / 4 )
  CAC <- rowSums(antiMat) # com més gran, més incomparable és el parell d'objectes
  CAC_Norm <- CAC / ( nrow(df) * (nrow(df) - 1) / 2 )
  
  antiMatrix <- list(RAC = RAC, 
                     RAC_Norm = RAC_Norm, 
                     CAC = CAC, 
                     CAC_Norm = CAC_Norm,
                     antiMat = antiMat)
  return(antiMatrix)
  
}



plot_poset <- function(Z, input = "min", type = c("avgheight", "standard"),
                       method = "approx", color = "white", title = NULL, chain = NULL) {
  type <- match.arg(type)
  Z.cov <- if (input == "max") t(incidence2cover(Z)) else incidence2cover(Z)
  V <- if (input == "max") vertices(Z.cov) else -vertices(Z.cov) 
  V <- spread_overlapping_x(V, Z.cov = Z.cov)
  
  if (type == "avgheight") {
    plot_avgheight_poset(Z.cov, Z, V, method, color, title, input)
  } else {
    plot_standard_poset(Z.cov, V, color, title, input, chain)
  }
}



plot_avgheight_poset <- function(Z.cov, Z, V, method, color, title, input) {
  M <- MRP(Z, method = method)  # Compute multiple ranking profiles
  avg_heights <- colSums(M)     # Get average heights for each element
  
  # Define plot limits
  xrange <- range(V$x)
  yrange <- range(V$y)
  
  xpad <- diff(xrange) * 0.15
  ypad <- diff(yrange) * 0.15
  
  xlim <- c(xrange[1] - xpad, xrange[2] + xpad)
  ylim <- c(yrange[1] - ypad, yrange[2] + ypad)
  
  main_title <- if (is.null(title)) paste("Poset -", input, "(Average Heights)") else paste(title, "(Average Heights)")
  
  # Create empty plot area
  plot(V$x, V$y, type = "n", axes = FALSE, xlab = "", ylab = "average height", 
       xlim = xlim, ylim = ylim,
       main = main_title)
  
  # Y-axis labels
  axis.labels <- round(seq(min(V$y) - 2, max(V$y), length.out = 12), 1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd = 2)
  
  # Draw arrows for each cover relation
  edge_list <- which(Z.cov == 1, arr.ind = TRUE)
  for (i in seq_len(nrow(edge_list))) {
    from <- edge_list[i, 1]
    to <- edge_list[i, 2]
    arrows(V$x[from], V$y[from], V$x[to], V$y[to],
           length = 0.2, angle = 20, col = "gray30", lwd = 2, lty = 1)
  }
  
  # Plot nodes and their labels
  points(V$x, V$y, pch = 21, bg = color, cex = 4)
  text(V$x, V$y, labels = rownames(Z.cov), cex = 0.75)
}



#' Plot a basic Hasse diagram of a poset
plot_standard_poset <- function(Z.cov, V, color = "white", title = NULL, input = "min", chain = NULL) {
  xrange <- range(V$x)
  yrange <- range(V$y)
  
  xpad <- diff(xrange) * 0.15
  ypad <- diff(yrange) * 0.15
  
  xlim <- c(xrange[1] - xpad, xrange[2] + xpad)
  ylim <- c(yrange[1] - ypad, yrange[2] + ypad)
  main_title <- if (is.null(title)) "Standard Poset" else title
  
  plot(V$x, V$y, type = "n", axes = FALSE, xlab = "", ylab = "",
       xlim = xlim, ylim = ylim, asp = 1, main = main_title)
  
  edge_list <- which(Z.cov == 1, arr.ind = TRUE)
  
  for (i in seq_len(nrow(edge_list))) {
    from <- edge_list[i, 1]
    to <- edge_list[i, 2]
    
    from_label <- rownames(Z.cov)[from]
    to_label <- rownames(Z.cov)[to]
    
    # Check if this edge is part of the chain
    from_in_chain <- match(from_label, chain)
    to_in_chain <- match(to_label, chain)
    is_chain_edge <- !is.null(chain) &&
      !is.na(from_in_chain) &&
      !is.na(to_in_chain) &&
      abs(to_in_chain - from_in_chain) == 1
    
    segments(V$x[from], V$y[from], V$x[to], V$y[to],
             col = if (is_chain_edge) "purple" else "gray30",
             lwd = if (is_chain_edge) 3 else 2)
  }
  
  # Node coloring (highlight chain if present)
  node_colors <- if (!is.null(chain)) {
    ifelse(rownames(Z.cov) %in% chain, "purple", color)
  } else {
    color
  }
  
  points(V$x, V$y, pch = 21, bg = node_colors, cex = 5)
  text(V$x, V$y, labels = rownames(Z.cov), cex = 0.8)
}



analyze_relations <- function(Z, input = "min") {
  Z.cov <- incidence2cover(Z)
  Z.poset <- validate.partialorder.incidence(Z)  # Validate full partial order
  
  points <- rownames(Z)
  n <- length(points)
  
  # Initialize symmetric relation matrix
  sym_comparable <- matrix(0, n, n, dimnames = list(points, points))
  
  # Populate symmetric relationships
  for (i in seq_along(points)) {
    for (j in seq_along(points)) {
      if (i != j && Z.poset[points[i], points[j]] == 1) {
        sym_comparable[points[i], points[j]] <- 1
        sym_comparable[points[j], points[i]] <- 1
      }
    }
  }
  
  # Organize output per point
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
  total_elements <- n
  # Total symmetric comparables (each pair counted twice)
  total_comparables <- sum(sym_comparable) / 2
  total_possible_pairs <- choose(n, 2)
  total_incomparables <- total_possible_pairs - total_comparables
  
  # Add summary at the end
  results$summary <- list(
    total_elements = total_elements,
    total_comparable_pairs = total_comparables,
    total_incomparable_pairs = total_incomparables
  )
  
  return(results)
}



find_longest_chain <- function(Z) {
  # Convert incidence matrix to cover relation
  cover_mat <- incidence2cover(Z)
  nodes <- rownames(cover_mat)
  
  # Create adjacency list from cover matrix
  adj_list <- lapply(nodes, function(n) {
    successors <- which(cover_mat[n, ] == 1)
    nodes[successors]
  })
  names(adj_list) <- nodes
  
  # Find minimal elements (no predecessors)
  minimal_nodes <- nodes[colSums(cover_mat) == 0]
  
  # Helper function for DFS
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
    return(chains)
  }
  
  # Run DFS from all minimal nodes and track the longest chain
  all_chains <- list()
  for (start_node in minimal_nodes) {
    chains <- dfs(start_node, c(start_node))
    all_chains <- c(all_chains, chains)
  }
  
  # Find and return the longest chain
  chain_lengths <- sapply(all_chains, length)
  longest_index <- which.max(chain_lengths)
  return(all_chains[[longest_index]])
}



print_number_of_levels <- function(Z) {
  chain <- find_longest_chain(Z)
  cat("Longest chain:\n", paste(chain, collapse = " -> "), "\n")
  cat("Number of levels:", length(chain), "\n")
  return(invisible(length(chain)))
}



spread_overlapping_x <- function(V, spacing = 0.4, Z.cov = NULL, tolerance = 0.05) {
  key <- paste(V$x, V$y)
  duplicated_coords <- unique(key[duplicated(key)])
  
  # 1. Spread points with exact same (x, y)
  for (coord in duplicated_coords) {
    ids <- which(key == coord)
    n <- length(ids)
    offsets <- seq(-spacing * (n - 1) / 2, spacing * (n - 1) / 2, length.out = n)
    V$x[ids] <- V$x[ids] + offsets
  }
  
  # 2. Spread points near edges (if Z.cov provided)
  if (!is.null(Z.cov)) {
    edge_list <- which(Z.cov == 1, arr.ind = TRUE)
    
    for (i in seq_len(nrow(V))) {
      for (j in seq_len(nrow(edge_list))) {
        from <- edge_list[j, 1]
        to <- edge_list[j, 2]
        
        if (i %in% c(from, to)) next  # Skip endpoints
        
        # Get point and edge
        px <- V$x[i]
        py <- V$y[i]
        x1 <- V$x[from]; y1 <- V$y[from]
        x2 <- V$x[to];   y2 <- V$y[to]
        
        # Compute simple perpendicular distance (approximate)
        # Formula for point-line distance
        num <- abs((y2 - y1)*px - (x2 - x1)*py + x2*y1 - y2*x1)
        den <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
        dist <- if (den == 0) Inf else num / den
        
        if (dist < tolerance) {
          # Just nudge the point a little left or right
          V$x[i] <- V$x[i] + runif(1, -spacing, spacing)
          break  # Done with this point
        }
      }
    }
  }
  
  return(V)
}



poset_stats <- function(Z) {
  relations <- analyze_relations(Z)
  print(relations)
  print_number_of_levels(Z)
}