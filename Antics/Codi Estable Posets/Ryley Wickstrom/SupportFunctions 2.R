
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
                       method = "approx", color = "white", title = NULL, ...) {
  type <- match.arg(type)  # Ensure type is valid
  
  Z.cov <- incidence2cover(Z)  # Convert incidence to cover relation matrix
  
  # Compute node coordinates depending on input direction
  if (input == "max") {
    Z.cov <- t(Z.cov)        # Transpose for max perspective
    V <- vertices(Z.cov)
  } else {
    V <- -vertices(Z.cov)    # Negate for min perspective
  }
  
  # Add jitter to avoid node overlap
  set.seed(42)
  jitter_amount <- 0.1
  V_jittered <- V
  V_jittered$x <- V$x + runif(length(V$x), -jitter_amount, jitter_amount)
  V_jittered$y <- V$y + runif(length(V$y), -jitter_amount, jitter_amount)
  
  # Choose plotting method
  if (type == "avgheight") {
    plot_avgheight_poset(Z.cov, Z, V_jittered, method, color, title, input)
  } else {
    plot_standard_poset(Z.cov, V_jittered, color, title, input)
  }
}


plot_avgheight_poset <- function(Z.cov, Z, V, method, color, title, input) {
  M <- MRP(Z, method = method)  # Compute multiple ranking profiles
  avg_heights <- colSums(M)     # Get average heights for each element
  
  # Define plot limits
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1.1
  
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
           length = 0.5, angle = 20, col = "gray30", lwd = 2, lty = 1)
  }
  
  # Plot nodes and their labels
  points(V$x, V$y, pch = 21, bg = color, cex = 4)
  text(V$x, V$y, labels = rownames(Z.cov), cex = 0.75)
  
  # Optional legend if colored
  if (color != "white") {
    legend('bottomright', 
           legend = levels(as.factor(color)), 
           col = length(unique(color)), 
           pch = 16)
  }
}


#' Plot a basic Hasse diagram of a poset
plot_standard_poset <- function(Z.cov, V, color = "white", title = NULL, input = NULL) {
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1.1
  
  main_title <- if (is.null(title)) {
    if (!is.null(input)) paste("Poset -", input, "perspective") else "Poset"
  } else {
    title
  }
  
  # Initialize empty plot
  plot(V, type = "n", axes = FALSE, xlab = "", ylab = "", 
       xlim = xlim, ylim = ylim, asp = 1,
       main = main_title)
  
  # Draw lines for cover relations
  edge_list <- which(Z.cov == 1, arr.ind = TRUE)
  for (i in seq_len(nrow(edge_list))) {
    from <- edge_list[i, 1]
    to <- edge_list[i, 2]
    segments(V$x[from], V$y[from], V$x[to], V$y[to], col = "gray30", lwd = 2)
  }
  
  # Plot nodes and labels
  points(V$x, V$y, pch = 21, bg = color, cex = 5)
  text(V$x, V$y, labels = rownames(Z.cov), cex = 0.8)
}



analyze_relations <- function(Z, input = "min") {
  Z.cov <- incidence2cover(Z)
  Z.poset <- validate.partialorder.incidence(Z)  # Validate full partial order
  
  points <- rownames(Z)
  n <- length(points)
  
  # Initialize symmetric relation matrix
  sym_related <- matrix(0, n, n, dimnames = list(points, points))
  
  # Populate symmetric relationships
  for (i in seq_along(points)) {
    for (j in seq_along(points)) {
      if (i != j && Z.poset[points[i], points[j]] == 1) {
        sym_related[points[i], points[j]] <- 1
        sym_related[points[j], points[i]] <- 1
      }
    }
  }
  
  # Organize output
  results <- list()
  for (i in seq_along(points)) {
    point <- points[i]
    
    related <- points[sym_related[point, ] == 1]
    unrelated <- setdiff(points, c(related, point))
    
    results[[point]] <- list(
      related = related,
      unrelated = unrelated
    )
  }
  
  return(results)
}


plot_standard_poset_with_chain <- function(Z, chain = NULL, title = NULL, input = "min") {
  Z.cov <- incidence2cover(Z)
  
  if (is.null(chain)) {
    chain <- find_longest_chain(Z)  # User-defined function
  }
  
  # Orientation setup
  V <- if (input == "max") vertices(t(Z.cov)) else -vertices(Z.cov)
  
  # Add jitter to avoid node overlap
  set.seed(42)
  jitter_amount <- 0.1
  V_jittered <- V
  V_jittered$x <- V$x + runif(length(V$x), -jitter_amount, jitter_amount)
  V_jittered$y <- V$y + runif(length(V$y), -jitter_amount, jitter_amount)
  
  xlim <- c(min(V_jittered$x), max(V_jittered$x)) * 1.3
  ylim <- c(min(V_jittered$y), max(V_jittered$y)) * 1.1
  main_title <- paste(title, "-highlighted")
  
  # Initialize plot
  plot(V_jittered, type = "n", axes = FALSE, xlab = "", ylab = "", 
       xlim = xlim, ylim = ylim, asp = 1,
       main = main_title)
  
  # Draw edges, highlighting chain
  edge_list <- which(Z.cov == 1, arr.ind = TRUE)
  for (i in seq_len(nrow(edge_list))) {
    from <- edge_list[i, 1]
    to <- edge_list[i, 2]
    from_name <- rownames(Z.cov)[from]
    to_name <- rownames(Z.cov)[to]
    
    pos_from <- match(from_name, chain)
    pos_to <- match(to_name, chain)
    
    is_chain_edge <- !is.na(pos_from) && !is.na(pos_to) && (pos_to - pos_from == 1)
    
    segments(V_jittered$x[from], V_jittered$y[from], V_jittered$x[to], V_jittered$y[to], 
             col = if (is_chain_edge) "purple" else "gray30", 
             lwd = if (is_chain_edge) 3 else 2)
  }
  
  # Plot nodes, highlighting those in the chain
  node_colors <- ifelse(rownames(Z.cov) %in% chain, "purple", "white")
  points(V_jittered$x, V_jittered$y, pch = 21, bg = node_colors, cex = 5)
  text(V_jittered$x, V_jittered$y, labels = rownames(Z.cov), cex = 0.8)
}
