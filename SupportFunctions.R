#### pHDI 2022 ####



get_poset <- function(data, X) {

  r <- function(x,y) all(data[x,] <= data[y,]) 
  r <- Vectorize(r) 
  Z <- outer(X, X, FUN = r)
  dimnames(Z) <- list(X, X) 
  Z <- validate.partialorder.incidence(Z)
  Z
}

antichainMatrix <- function(df){

  preMat <- apply(df, 2, function(y) combn(y, 2, function(x) x[1] <= x[2])) %>% 
    as.data.frame()
  colnames(preMat) <- colnames(df)
  
  antiMat <- do.call(cbind,combn(colnames(preMat), 2, 
                                 FUN= function(x) list(as.numeric(preMat[x[1]] != preMat[x[2]]))))
  
  colnames(antiMat) <- combn(colnames(perfil_num), 2, FUN = function(x) paste(x[1], "-", x[2], sep=""))
  rownames(antiMat) <- combn(rownames(perfil_num), 2, FUN = function(x) paste(x[1], "-", x[2], sep=""))
  
  RAC <- colSums(antiMat) # com més gran, més incomparabilitats provoca aquell parell de variables
  ACC <- rowSums(antiMat) # com més gran, més incomparable és el parell d'objectes
  
  antiMatrix <- list(RAC = RAC, ACC = ACC, antiMat = antiMat)

}


plot_poset_avgheight <- function(Z, method = "exact", ...) {
  #poset plot with y avg.height axis
  
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov)
  
  M <- MRP(Z,method = method)
  avg_heights <- colSums(M)
  
  V$y <- avg_heights
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1
  
  plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white", pch = 21)
  #,     col = as.factor(dataPosetpHDI2022$Level))
  
  axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd=2)
  text(V, labels = rownames(Z.cov), cex = 0.75)
  
}


