plot.centers(dataPoset.model, "bli")

main <- "The title"

palette.name <- terrain.colors

codes <- simpleSOM 
nvars <- ncol(codes)
maxlegendcols <- 3

codeRendering <- "segments"

margins <- rep(0.6, 4)

if (!is.null(main)) 
  margins[3] <- margins[3] + 2

par(mar = margins)

if (codeRendering == "segments" & nvars < 15 & !is.null(colnames(codes))) {
  plot(dataPoset.model$grid$pts, ylim = c(max(dataPoset.model$grid$pts[, 2]) + min(dataPoset.model$grid$pts[, 2]), -2))
  current.plot <- par("mfg")  # only needed when multiple plots
  plot.width <- diff(par("usr")[1:2])
  cex <- 1
  leg.result <- legend(x = mean(dataPoset.model$grid$pts[, 1]), xjust = 0.5, 
                       y = 0, yjust = 1, legend = colnames(codes), cex = cex, 
                       plot = FALSE, ncol = min(maxlegendcols, nvars), 
                       fill = palette.name(nvars))
  while (leg.result$rect$w > plot.width) {  # is it too wide?
    cex <- cex * 0.9
    leg.result <- legend(x = mean(dataPoset.model$grid$pts[, 1]), 
                         xjust = 0.5, y = 0, yjust = 1, legend = colnames(codes), 
                         cex = cex, plot = FALSE, ncol = min(maxlegendcols, nvars), 
                         fill = palette.name(nvars))
  }
  leg.result <- legend(x = mean(dataPoset.model$grid$pts[, 1]), xjust = 0.5, 
                       y = 0, yjust = 1, cex = cex, legend = colnames(codes), 
                       plot = FALSE, ncol = min(maxlegendcols, nvars), 
                       fill = palette.name(nvars))
  
  par(mfg = current.plot)
  plot(dataPoset.model$grid$pts, ylim = c(max(dataPoset.model$grid$pts[, 2]) + min(dataPoset.model$grid$pts[, 2]), -leg.result$rect$h))
  
  legend(x = mean(dataPoset.model$grid$pts[, 1]), xjust = 0.5, y = 0, 
         yjust = 1, cex = cex, plot = TRUE, legend = colnames(codes), 
         ncol = min(maxlegendcols, nvars), fill = palette.name(nvars))
} else {
  plot(dataPoset.model$grid$pts)
}

title.y <- max(dataPoset.model$grid$pts[, 2]) + 1.2
if (title.y > par("usr")[4] - 0.2) {
  title(main)
} else {
  text(mean(range(dataPoset.model$grid$pts[, 1])), title.y, main, 
       adj = 0.5, cex = par("cex.main"), font = par("font.main"))
}

bgcol <- "transparent"

symbols(dataPoset.model$grid$pts[, 1], dataPoset.model$grid$pts[, 2], 
        circles = rep(0.5, nrow(dataPoset.model$grid$pts)), inches = FALSE, add = TRUE, bg = bgcol)

codemins <- apply(codes, 2, min)
codes <- sweep(codes, 2, codemins)

switch(codeRendering, segments = {
  stars(codes, locations = dataPoset.model$grid$pts, labels = NULL, 
        len = 0.4, add = TRUE, col.segments = palette.name(nvars), 
        draw.segments = TRUE)
})


