# ----------------------------------------------------------------------------
# Original file: Antichains__Bruggemann_Voigt_2014.R
# Fragment: HDI poset colored by region (related to R/antichain_matrix.R).
#
# Notes / known issues (code kept as in the original):
#  - FIXME: uses `regions` before defining it - does not run as is
#  - exploratory code, not maintained
# ----------------------------------------------------------------------------

# Load the support functions (run from the repository root, e.g. after
# opening the .Rproj file in RStudio):
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

library(POSetR) 
library(tidyverse) 
library(parsec) 
library(RColorBrewer) 
library(readr)

hdi_regions <- read_delim("data/HDI2022.txt", delim = "\t")
#Overall analysis
X <- hdi_regions$Code
hdi_regions <- column_to_rownames(hdi_regions, var = "Code") 

hdi_regions_Num <- hdi_regions %>% 
  select(-Level, -Country, -Region, -SubRegion)


Z <- get_poset(hdi_regions_Num, X)

palette(brewer.pal(n =length(regions), name = "Accent")) 
plot(Z, col = as.factor(hdi_regions$Region), pch = 16)

legend('bottomright', legend = levels(as.factor(hdi_regions$Region)), col = 1:length(regions), pch = 16)

