# it is likely that you do not have this libraries
# install.packages(c("POSetR", "tidyverse", "parsec")
library(POSetR) 
library(tidyverse) 
library(parsec) 

### Support functions
source("SupportFunctions.R") # this file should be in the same folder as this script

# Load data. it is assumed that it is in the same folder as the script
# In this case, data is tab separated. If different, change delim accordingly ("," ";" ...)
poset_data <- read_delim("C:/Users/francesc.martori/Downloads/LNOB_MICS.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)
poset_data <- poset_data %>% select(InternetAcc_AbsGap, `InternetAcc_D-Index`, InternetAcc_AbsGap, Code)

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


