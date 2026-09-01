# plot_poset() ----------------------------------------------------------------
# Unified Hasse-diagram plotting for posets built with get_poset().
# Consolidated from SupportFunctions_FINAL.R (dispatcher + helpers), keeping
# the group-color legend of the earlier versions.
#
#   type = "standard"  : classic Hasse diagram (levels on the y axis),
#                        optionally highlighting a chain in purple.
#   type = "avgheight" : Hasse diagram with the y coordinate given by the
#                        average height of each element, computed from the
#                        mutual ranking probability matrix (parsec::MRP).
#   input = "min"/"max": perspective of the diagram (minimal or maximal
#                        elements at the bottom).
#
# Arguments:
#   Z       validated incidence matrix (from get_poset()).
#   input   "min" (default) or "max".
#   type    "avgheight" (default) or "standard".
#   method  method passed to parsec::MRP ("approx" or "exact").
#   color   node fill: a single color, or a vector/factor with one group per
#           element (a legend is added in that case).
#   title   plot title (optional).
#   chain   type = "standard" only: character vector of element names, bottom
#           to top, highlighted in purple (see find_longest_chain()).
#
# Depends on: parsec (incidence2cover, vertices, MRP)
#
# NOTE (consolidation, 2026-09): one bug fixed with respect to every
# historical version: in the "avgheight" plot, V$y is now actually replaced
# by the average heights (the originals computed them but never used them,
# so the plot silently showed level heights).

plot_poset <- function(Z, input = "min", type = c("avgheight", "standard"),
                       method = "approx", color = "white", title = NULL,
                       chain = NULL) {
  type <- match.arg(type)

  # Convert incidence to cover matrix (minimal set of relations)
  Z.cov <- if (input == "max") t(incidence2cover(Z)) else incidence2cover(Z)

  # Assign coordinates to each element
  V <- if (input == "max") vertices(Z.cov) else -vertices(Z.cov)
  V <- spread_overlapping_x(V, Z.cov = Z.cov)  # Adjust for overlapping coordinates

  # Draw the appropriate plot
  if (type == "avgheight") {
    plot_avgheight_poset(Z.cov, Z, V, method, color, title, input)
  } else {
    plot_standard_poset(Z.cov, V, color, title, input, chain)
  }
}


plot_avgheight_poset <- function(Z.cov, Z, V, method, color, title, input) {
  # Compute mutual ranking probabilities and average heights
  M <- MRP(Z, method = method)
  avg_heights <- colSums(M)
  V$y <- avg_heights  # bug fix: the originals computed this but never used it

  # Define plot limits with padding
  xrange <- range(V$x)
  yrange <- range(V$y)
  xpad <- diff(xrange) * 0.15
  ypad <- diff(yrange) * 0.15
  xlim <- c(xrange[1] - xpad, xrange[2] + xpad)
  ylim <- c(yrange[1] - ypad, yrange[2] + ypad)

  main_title <- if (is.null(title)) paste("Poset -", input, "(Average Heights)") else paste(title, "(Average Heights)")

  plot(V$x, V$y, type = "n", axes = FALSE, xlab = "", ylab = "average height",
       xlim = xlim, ylim = ylim, main = main_title)

  axis.labels <- round(seq(min(V$y) - 2, max(V$y), length.out = 12), 1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd = 2)

  # Draw arrows for cover relations
  edge_list <- which(Z.cov == 1, arr.ind = TRUE)
  for (i in seq_len(nrow(edge_list))) {
    from <- edge_list[i, 1]
    to <- edge_list[i, 2]
    arrows(V$x[from], V$y[from], V$x[to], V$y[to],
           length = 0.2, angle = 20, col = "gray30", lwd = 2, lty = 1)
  }

  points(V$x, V$y, pch = 21, bg = color, cex = 4)
  text(V$x, V$y, labels = rownames(Z.cov), cex = 0.75)

  add_group_legend(color)
}


plot_standard_poset <- function(Z.cov, V, color = "white", title = NULL,
                                input = "min", chain = NULL) {
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

    # Check if this edge is part of the highlighted chain
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

  # Determine node colors: highlight chain nodes
  node_colors <- if (!is.null(chain)) {
    ifelse(rownames(Z.cov) %in% chain, "purple", color)
  } else {
    color
  }

  points(V$x, V$y, pch = 21, bg = node_colors, cex = 5)
  text(V$x, V$y, labels = rownames(Z.cov), cex = 0.8)

  if (is.null(chain)) add_group_legend(color)
}


# Internal: add a bottom-right legend when `color` encodes groups
# (i.e. it is a vector/factor with more than one distinct value).
add_group_legend <- function(color) {
  if (length(color) > 1 && length(unique(color)) > 1) {
    groups <- as.factor(color)
    legend("bottomright",
           legend = levels(groups),
           col = seq_len(nlevels(groups)),
           pch = 16)
  }
}


# spread_overlapping_x() ------------------------------------------------------
# Nudge x coordinates so that coincident nodes, and nodes sitting on top of
# an edge, do not overlap in the Hasse diagram. Note: the near-edge nudge is
# random (runif), so plots are not pixel-reproducible; set.seed() first if
# that matters.

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

        px <- V$x[i]
        py <- V$y[i]
        x1 <- V$x[from]; y1 <- V$y[from]
        x2 <- V$x[to];   y2 <- V$y[to]

        # Point-line distance (approximate)
        num <- abs((y2 - y1) * px - (x2 - x1) * py + x2 * y1 - y2 * x1)
        den <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
        dist <- if (den == 0) Inf else num / den

        if (dist < tolerance) {
          V$x[i] <- V$x[i] + runif(1, -spacing, spacing)
          break
        }
      }
    }
  }

  V
}


# plot_poset_avgheight() ------------------------------------------------------
# Legacy wrapper kept for backward compatibility with older scripts and
# teaching material (Poset 101). Prefer plot_poset(Z, type = "avgheight").
# This is the original implementation based on parsec::drawedges.

plot_poset_avgheight <- function(Z, method = "approx", color = "white", ...) {
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov)

  M <- MRP(Z, method = method)
  avg_heights <- colSums(M)

  V$y <- avg_heights
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y))
  par(mar = c(1, 4.1, 0, 1))

  plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "",
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4,
       bg = "white", pch = 16, col = color)

  axis.labels <- round(seq(min(V$y) - 2, max(V$y), length.out = 12), 1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd = 2)
  text(V, labels = rownames(Z.cov), cex = 0.75)

  add_group_legend(color)
}
