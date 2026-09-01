library(POSetR) 
library(tidyverse) 
library(parsec) 
library(RColorBrewer) 
library(readr)

hdi_regions <- read_delim("../HDI2022.txt", delim = "\t")
#Overall analysis
X <- hdi_regions$Code
hdi_regions <- column_to_rownames(hdi_regions, var = "Code") 

hdi_regions_Num <- hdi_regions %>% 
  select(-Level, -Country, -Region, -SubRegion)


Z <- get_poset(hdi_regions_Num, X)

palette(brewer.pal(n =length(regions), name = "Accent")) 
plot(Z, col = as.factor(hdi_regions$Region), pch = 16)

legend('bottomright', legend = levels(as.factor(hdi_regions$Region)), col = 1:length(regions), pch = 16)

