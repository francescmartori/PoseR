# ----------------------------------------------------------------------------
# Poset 101 - a first contact with posets for multidimensional indicators
#
# Consolidates the former teaching scripts Poset_101_Rallou.R,
# Poset_101_Ryley.R, Poset_101_Ryley 1.R, Poset_101_Ryley_FINAL.R and
# presentacio_Intro2Posets.R.
#
# Run from the repository root (open the .Rproj file in RStudio first).
# ----------------------------------------------------------------------------

# You may need to install these packages first:
# install.packages(c("POSetR", "tidyverse", "parsec"))
library(POSetR)
library(tidyverse)
library(parsec)

# Load the support functions
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))


## 1. A minimal example ------------------------------------------------------
# Five profiles described by two indicators. P1 dominates P3, P4 and P5;
# P1 and P2 are incomparable (each is better on one indicator), etc.

a <- data.frame(PIB = c(75000, 75000, 50000, 40000, 40000),
                EV  = c(80, 75, 60, 70, 65))
rownames(a) <- c("P1", "P2", "P3", "P4", "P5")

Z <- get_poset(a, rownames(a))
plot(Z)  # Hasse diagram (parsec's plot method)


## 2. A small worked example --------------------------------------------------

poset_data <- data.frame(
  Code = c("a", "b", "c", "d", "e", "f"),
  LE   = c(80, 70, 60, 50, 85, 81),
  GDP  = c(75000, 75000, 60000, 40000, 45000, 45000),
  MYS  = c(15, 10, 8, 6, 10, 5)
)

# Getting the data ready: one column must identify the elements ...
X <- poset_data$Code
poset_data <- column_to_rownames(poset_data, var = "Code")

# ... and the data frame used for the poset must be all numeric
poset_data_num <- poset_data

# Calculate the poset (where the magic happens)
Z <- get_poset(poset_data_num, X)

par(mar = c(0, 0, 0, 0))  # so the Hasse diagram fits better

# Hasse diagram (parsec's generic plotting)
plot(Z, shape = "equispaced")

# Custom Hasse diagram, with a title
plot_poset(Z, input = "min", type = "standard", title = "My first poset")

# Hasse diagram with average heights on the y axis
plot_poset(Z, type = "avgheight", title = "My first poset")

# Same, from the maximal perspective
# (input can also be asked interactively:
#  input <- readline(prompt = "maximal or minimal perspective (max or min): "))
plot_poset(Z, input = "max", type = "avgheight")

# Highlight a chain (list it from the bottom of the chain to the top),
# or highlight the longest chain of the poset
my_chain <- c("d", "c", "b", "a")
plot_poset(Z, type = "standard", chain = my_chain)
plot_poset(Z, type = "standard", chain = find_longest_chain(Z))

# Verbose summary: per-element comparabilities and number of levels
poset_stats(Z)

# Compact structural summary: height, width, I/C and W/H ratios
poset_ratios(Z)


## 3. Loading your own data ---------------------------------------------------
# For tab-separated data (change delim to "," or ";" if needed):
# poset_data <- read_delim("data/your_file.txt", delim = "\t",
#                          escape_double = FALSE, trim_ws = TRUE)
# Then proceed exactly as in section 2.
