# it is likely that you do not have this libraries
# install.packages(c("POSetR", "tidyverse", "parsec")
library(POSetR) 
library(tidyverse) 
library(parsec) 

### Support functions
source("SupportFunctions.R") # this file should be in the same folder as this script

# Load data. it is assumed that it is in the same folder as the script
# In this case, data is tab separated. If different, change delim accordingly ("," ";" ...)
#load("poset_data.RData") # Load the data from a .RData file

#poset_data <- poset_data %>% rownames_to_column(var = "Code")
poset_data <- data.frame(
  x = c(7, 5, 3, 9),
  y = c(5, 5, 3, 4),
  z = c(8, 3, 3, 7)
)

input <- readline(prompt="Would you prefer the maximal or minimal perspective(max or min):")

rownames(poset_data) <- rownames(poset_data) <- c("A1", "A2", "A3", "A4")

X <- rownames(poset_data)



# Getting data ready
#X <- poset_data$Code
#poset_data <- column_to_rownames(poset_data, var = "Code") 

# Discard all non-numeric variables before calculating the poset (if needed)
poset_data_num <- poset_data


# Calculate poset
Z <- get_poset(poset_data_num, X) # Where the magic happens

par(mar = c(0,0, 0, 0)) # in order for the Hasse to fit better

## plot Hasse Diagram

#plot(Z, shape = "equispaced")
plot_poset(Z, input, "standard")

### Get poset with average heights
plot_poset(Z, input, "avgheight")


