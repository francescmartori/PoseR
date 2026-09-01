# it is likely that you do not have this libraries
# install.packages(c("POSetR", "tidyverse", "parsec")
library(POSetR) 
library(tidyverse) 
library(parsec) 

### Support functions
source("SupportFunctions.R") # this file should be in the same folder as this script

# Load data. it is assumed that it is in the same folder as the script
# In this case, data is tab separated. If different, change delim accordingly ("," ";" ...)

poset_data <- data.frame(
  Code = c("a","b","c","d","e","f"),
  LE = c(80, 70, 60, 50, 85, 81),
  GDP = c(75000, 75000, 60000, 40000, 45000, 45000),
  MYS = c(15, 10, 8, 6, 10, 5)
  )

# Getting data ready
X <- poset_data$Code
poset_data <- column_to_rownames(poset_data, var = "Code") 

# Discard all non-numeric variables before calculating the poset (if needed)
poset_data_num <- poset_data

# Calculate poset
Z <- get_poset(poset_data_num, X) # Where the magic happens

par(mar = c(0,0, 0, 0)) # in order for the Hasse to fit better

## plot Hasse Diagram
plot(Z, shape = "equispaced")

### Get poset with average heights
plot_poset_avgheight(Z)


