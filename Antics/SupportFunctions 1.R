
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
                       method = "approx", color = "white", ...) {
  require(parsec)
  type <- match.arg(type)
  
  Z.cov <- incidence2cover(Z)
  if (input == "max") {
    Z.cov <- t(Z.cov)
    V <- vertices(Z.cov)
  } else {
    V <- -vertices(Z.cov)
  }
  
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * ifelse(type == "avgheight", 1, 1.1)
  par(mar = c(1, 4.1, 0, 1))
  
  if (type == "avgheight") {
    M <- MRP(Z, method = method)
    avg_heights <- colSums(M)
    
    plot(V, axes = FALSE, xlab = "", ylab = "average height", 
         xlim = xlim, ylim = ylim, cex = 4, bg = "white", pch = 16, col = color)
    
    axis.labels <- round(seq(min(V$y) - 2, max(V$y), length.out = 12), 1)
    axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
    grid(lwd = 2)
    
    edge_list <- which(Z.cov == 1, arr.ind = TRUE)
    for (i in seq_len(nrow(edge_list))) {
      from <- edge_list[i, 1]
      to <- edge_list[i, 2]
      arrows(V$x[from], V$y[from], V$x[to], V$y[to],
             length = 0.1, angle = 20, col = "gray30", lwd = 2, lty = 1)
    }
    
    text(V, labels = rownames(Z.cov), cex = 0.75)
    
    if (color != "white") {
      legend('bottomright', 
             legend = levels(as.factor(color)), 
             col = length(unique(color)), 
             pch = 16)
    }
    
  } else if (type == "standard") {
    plot(V, type = "n", axes = FALSE, xlab = "", ylab = "", 
         xlim = xlim, ylim = ylim, asp = 1)
    
    edge_list <- which(Z.cov == 1, arr.ind = TRUE)
    for (i in seq_len(nrow(edge_list))) {
      from <- edge_list[i, 1]
      to <- edge_list[i, 2]
      segments(V$x[from], V$y[from], V$x[to], V$y[to], col = "gray30", lwd = 2)
    }
    
    points(V$x, V$y, pch = 21, bg = "white", cex = 5)
    
    text(V$x, V$y, labels = rownames(Z.cov), cex = 0.8)
  }
}