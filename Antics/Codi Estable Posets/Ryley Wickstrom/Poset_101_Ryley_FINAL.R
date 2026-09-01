### it is likely that you do not have this libraries
### install.packages(c("POSetR", "tidyverse", "parsec")
library(POSetR) 
library(tidyverse) 
library(parsec) 

### Support functions
source("SupportFunctions.R") # this file should be in the same folder as this script



### Load data. it is assumed that it is in the same folder as the script, or manually added

### In this case, data is tab separated. If different, change delim accordingly ("," ";" ...)
#load("poset_data.RData") # Load the data from a .RData file
#poset_data <- poset_data %>% rownames_to_column(var = "Code")

### This data is manually created 
### Data 1 - simple test
poset_data <- data.frame(
  x = c(7, 5, 3, 9),
  y = c(5, 5, 3, 4),
  z = c(8, 3, 3, 7)
)
rownames(poset_data) <- rownames(poset_data) <- c("A1", "A2", "A3", "A4")

### Data 2 tesxt with complicated Hasse
#poset_data <- data.frame(
#  Var1 = c(1, 2, 2, 3, 3, 4, 5, 5, 6, 7),
#  Var2 = c(5, 3, 4, 2, 4, 3, 6, 7, 6, 8),
#  Var3 = c(2, 1, 2, 3, 2, 4, 5, 4, 5, 6)
#)
#rownames(poset_data) <- paste0("P", 1:10)




### Getting data ready
X <- rownames(poset_data)
#poset_data <- column_to_rownames(poset_data, var = "Code") #for imported code 
poset_data_num <- poset_data #Discard all non-numeric variables before calculating (if needed)


### Getting input on the type of graph and the name 
input <- readline(prompt="Would you prefer the maximal or minimal perspective(max or min):")
title_input <- readline(prompt="What would you like to title your graphs:")




### Calculate poset
Z <- get_poset(poset_data_num, X) # Where the magic happens


### Plot Hasse Diagram
par(mar = c(1, 4.1, 3, 1)) # this line is responsible for the margins (bottom, left, top, right), use before calling any plot and it will change, only have to call once. 

#plot(Z, shape = "equispaced") # generic plotting

### Get poset standard plot
plot_poset(Z, input, "standard", title = title_input) #calls the custom plot methods, pass in the data, the input (graph direction), type and title

### Get poset with average heights
plot_poset(Z, input, "avgheight", title = title_input) #calls the custom plot methods, pass in the data, the input (graph direction), type and title

### Plot with a specific chain highlighted - list from the bottom of the chain to the top - make sure you indicate a chain
my_chain <- c("A2", "A1")  # Your custom chain - must be listed from the bottom up to follow the relationships correctly
#my_chain <- find_longest_chain(Z) #To make the highlighted chain be the longest chain
plot_poset(Z, input, "standard", title = title_input, chain = my_chain) # calls the custom plot with highlight method, passes in the data, the chain the title and the direction. 

### To show the summary statistics of the poset including #elements, #comparabalities, #incomparabalities, what elements are related, the numebr of levels
poset_stats(Z)


