

options(install.packages.check.source= "no")
pckgs <- c("parsec","readr")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {
  install.packages(pckg,repos="https://cloud.r-project.org/",
                   quiet=TRUE)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


get_poset <- function(data, X) {
  
  r <- function(x,y) all(data[x,] <= data[y,]) 
  r <- Vectorize(r) 
  Z <- outer(X, X, FUN = r)
  dimnames(Z) <- list(X, X) 
  Z <- validate.partialorder.incidence(Z)
  Z
}

all_hdi_noNANum <- read_csv("hdi_noNANum.csv") 
X <- rownames(all_hdi_noNANum)

#Sensitivity based on Bruggeman Patil, chapter 4, p48
dfSensitivity <- data.frame(columns = colnames(all_hdi_noNANum), 
                            sens = numeric(length(colnames(all_hdi_noNANum))))

Z <- get_poset(all_hdi_noNANum[], X)
for(i in 1:length(colnames(all_hdi_noNANum))) {
  cat(round(i/length(colnames(all_hdi_noNANum)),2), "\n")
  dfSensitivity$sens[i] = sum(get_poset(all_hdi_noNANum[,-i], X) - Z)
}

write_csv(dfSensitivity, "all_hdi_sensitivity.csv")