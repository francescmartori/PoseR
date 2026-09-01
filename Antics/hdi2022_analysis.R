
library(tidyverse) 
library(parsec) 
library(countrycode) 
library(RColorBrewer) 
library(kohonen)
library(factoextra) 

dataPoset <- read_delim("C:/Users/franc/OneDrive - IQS/IQS/Recerca/POSET/HDI2022.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE) 

dataPoset$Level <- ordered(dataPoset$Level, levels = c("Very High", "High", "Medium", "Low")) 

dataPoset <- dataPoset %>% mutate(GNIpC = log(GNIpC))

X <- dataPoset$Code 
dataPoset <- column_to_rownames(dataPoset, var = "Code") 
dataPoset$Code <- NULL 
dataPoset$Country <- NULL

dataPosetNum <- dataPoset[, c("LE", "EYS", "MYS", "GNIpC")]

head(dataPosetNum)

r <- function(x,y) all(dataPosetNum[x,] <= dataPosetNum[y,]) 
r <- Vectorize(r)
Z <- outer(X, X, FUN = r)
dimnames(Z) <- list(X, X) 
Z <- validate.partialorder.incidence(Z)

palette(brewer.pal(n =4, name = "RdYlBu")) 
plot(Z, col = as.factor(dataPoset$Level), pch = 16)

legend('bottomright', legend = levels(as.factor(dataPoset$Level)), col = 1:4, pch = 16)

M <- MRP(Z,method = "approx")
avg_heigths_2022 <- colSums(M)


#poset plot with y avg.height axis
Z.cov <- incidence2cover(Z)
V <- -vertices(Z.cov)

V$y <- avg_heigths_2022
xlim <- c(min(V$x), max(V$x)) * 1.3
ylim <- c(min(V$y), max(V$y)) * 1

plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
     ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white", pch = 16, col = as.factor(dataPoset$Level))

axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
grid(lwd=2)
text(V, labels = rownames(Z.cov), cex = 0.75)


#####

dataPoset2019 <- read_delim("C:/Users/franc/OneDrive - IQS/IQS/Recerca/POSET/HDI2019.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE) 

dataPoset2019$Level <- ordered(dataPoset2019$Level, levels = c("Very High", "High", "Medium", "Low")) 

dataPoset2019 <- dataPoset2019 %>% mutate(GNIpC = log(GNIpC))

X <- dataPoset2019$Code 
dataPoset2019 <- column_to_rownames(dataPoset2019, var = "Code") 
dataPoset2019$Code <- NULL 
dataPoset2019$Country <- NULL

dataPoset2019Num <- dataPoset2019[, c("LE", "EYS", "MYS", "GNIpC")]

head(dataPoset2019)

r <- function(x,y) all(dataPoset2019Num[x,] <= dataPoset2019Num[y,]) 
r <- Vectorize(r)
Z <- outer(X, X, FUN = r)
dimnames(Z) <- list(X, X) 
Z <- validate.partialorder.incidence(Z)

palette(brewer.pal(n =4, name = "RdYlBu")) 
plot(Z, col = as.factor(dataPoset2019$Level), pch = 16)

legend('bottomright', legend = levels(as.factor(dataPoset2019$Level)), col = 1:4, pch = 16)

M <- MRP(Z,method = "approx")
avg_heigths_2019 <- colSums(M)

#poset plot with y avg.height axis
Z.cov <- incidence2cover(Z)
V <- -vertices(Z.cov)

V$y <- avg_heigths_2019
xlim <- c(min(V$x), max(V$x)) * 1.3
ylim <- c(min(V$y), max(V$y)) * 1

plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
     ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white", pch = 16, col = as.factor(dataPoset2019$Level))

axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
grid(lwd=2)
text(V, labels = rownames(Z.cov), cex = 0.75)

#### pHDI 2022 ####

dataPosetpHDI2022 <- read_delim("C:/Users/franc/OneDrive - IQS/IQS/Recerca/POSET/pHDI2022.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE) 
dataPosetpHDI2022
dataPosetpHDI2022$Level <- ordered(dataPosetpHDI2022$Level, levels = c("Very High", "High", "Medium", "Low")) 

dataPosetpHDI2022 <- dataPosetpHDI2022 %>% mutate(GNIpC = log(GNIpC))

X <- dataPosetpHDI2022$Code 
dataPosetpHDI2022 <- column_to_rownames(dataPosetpHDI2022, var = "Code") 
dataPosetpHDI2022$Code <- NULL 
dataPosetpHDI2022$Country <- NULL

dataPosetpHDI2022Num <- dataPosetpHDI2022[, c("CO2", "MFT", "LE", "EYS", "MYS", "GNIpC")]

head(dataPosetpHDI2022)

r <- function(x,y) all(dataPosetpHDI2022Num[x,] <= dataPosetpHDI2022Num[y,]) 
r <- Vectorize(r)
Z <- outer(X, X, FUN = r)
dimnames(Z) <- list(X, X) 
Z <- validate.partialorder.incidence(Z)

palette(brewer.pal(n =4, name = "RdYlBu")) 
plot(Z, col = as.factor(dataPosetpHDI2022$Level), pch = 16)

legend('bottomright', legend = levels(as.factor(dataPosetpHDI2022$Level)), col = 1:4, pch = 16)

M <- MRP(Z,method = "approx")
avg_heigths_2019 <- colSums(M)

#poset plot with y avg.height axis
Z.cov <- incidence2cover(Z)
V <- -vertices(Z.cov)

V$y <- avg_heigths_2019
xlim <- c(min(V$x), max(V$x)) * 1.3
ylim <- c(min(V$y), max(V$y)) * 1

plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
     ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, bg = "white", pch = 16, col = as.factor(dataPosetpHDI2022$Level))

axis.labels <- round(seq(min(V$y) -2 , max(V$y), length.out = 12),1)
axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
grid(lwd=2)
text(V, labels = rownames(Z.cov), cex = 0.75)
