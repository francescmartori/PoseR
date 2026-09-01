# ----------------------------------------------------------------------------
# Original file: Exemple_loop_llarg.R
# 30 years of HDI: long-running leave-one-out sensitivity batch over
# the full HDI matrix; writes all_hdi_sensitivity.csv.
#
# Notes / known issues (code kept as in the original):
#  - expects data/hdi_noNANum.csv
#  - the inline get_poset() definition was removed: it now comes from R/get_poset.R (identical code)
# ----------------------------------------------------------------------------

# Load the support functions (run from the repository root, e.g. after
# opening the .Rproj file in RStudio):
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))



options(install.packages.check.source= "no")
pckgs <- c("parsec","readr")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {
  install.packages(pckg,repos="https://cloud.r-project.org/",
                   quiet=TRUE)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}



all_hdi_noNANum <- read_csv("data/hdi_noNANum.csv") 
X <- rownames(all_hdi_noNANum)

#Sensitivity based on Bruggeman Patil, chapter 4, p48
dfSensitivity <- data.frame(columns = colnames(all_hdi_noNANum), 
                            sens = numeric(length(colnames(all_hdi_noNANum))))

Z <- get_poset(all_hdi_noNANum[], X)
for(i in 1:length(colnames(all_hdi_noNANum))) {
  cat(round(i/length(colnames(all_hdi_noNANum)),2), "\n")
  dfSensitivity$sens[i] = sum(get_poset(all_hdi_noNANum[,-i], X) - Z)
}

write_csv(dfSensitivity, "output/all_hdi_sensitivity.csv")
