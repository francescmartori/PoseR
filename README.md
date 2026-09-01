# Posets for Multidimensional Social Indicators (PoseR)

R functions and analysis scripts for applying Partially Ordered Set (POSET)
methodology to multidimensional social indicators (HDI, deprivation and
socioeconomic evaluation), building on the
[parsec](https://cran.r-project.org/package=parsec) and
[POSetR](https://cran.r-project.org/package=POSetR) packages.

The code supports the poset-based analyses published by Francesc Martori and
collaborators (IQS School of Management, Universitat Ramon Llull), and is
being consolidated here with a view to becoming an R package in the future.

## Repository structure

```
R/          Reusable functions (the future package)
tutorials/  Self-contained teaching scripts and reproductions of
            examples from the literature
analysis/   Scripts behind specific research projects
              geography-of-hdi/   Geography of HDI (2022/2023 data)
              30-years-of-hdi/    HDI time-series, SOM and posets
sandbox/    Exploratory code, kept for reference, not maintained
data/       Input data (not distributed, see data/README.md)
output/     Generated figures and tables (git-ignored)
```

## The functions in `R/`

| Function | Purpose |
|---|---|
| `get_poset(data, X)` | Build a validated incidence matrix from a numeric data frame (componentwise dominance) |
| `plot_poset(Z, input, type)` | Hasse diagram, standard or with average heights on the y axis, from the min or max perspective, with optional title and chain highlighting |
| `plot_poset_avgheight(Z)` | Legacy average-height Hasse plot (kept for backward compatibility) |
| `poset_stats(Z)` | Verbose summary: per-element comparable/incomparable lists and number of levels |
| `poset_ratios(Z)` | Compact structural summary: height, width, comparabilities, I/C and W/H ratios |
| `analyze_relations(Z)` | Per-element comparability structure (used by `poset_stats`) |
| `find_longest_chain(Z)` | Longest chain of the poset (DFS over the cover relation) |
| `print_number_of_levels(Z)` | Longest chain and number of levels |
| `poset_sensitivity(data, X)` | Leave-one-indicator-out sensitivity (Bruggemann & Patil) |
| `poset_dominance(Z)` | Dominance and incomparability scores from the MRP matrix |
| `benchmark_profiles(data)` | Quantile benchmark profiles (embedded scales: MAX, Q3, Q2, Q1, MIN) |
| `antichainMatrix(df)` | Antichain analysis (Bruggemann & Voigt 2012) |
| `poset_dissimilarity(Z0, Zs)` | MRP-based dissimilarity between posets (Arcagni 2022) |

## Getting started

```r
install.packages(c("parsec", "POSetR", "tidyverse"))

# from the repository root:
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

a <- data.frame(GDP = c(75000, 75000, 50000, 40000, 40000),
                LE  = c(80, 75, 60, 70, 65))
rownames(a) <- c("P1", "P2", "P3", "P4", "P5")

Z <- get_poset(a, rownames(a))
plot(Z)                          # Hasse diagram
plot_poset(Z, type = "avgheight")
poset_stats(Z)
```

`tutorials/poset_101.R` is the recommended entry point. Open the project
(`.Rproj`) in RStudio so all paths resolve from the repository root.

## Data

The analysis scripts expect input files under `data/` which are not
distributed with this repository (UNDP Human Development Report data and
other third-party sources). See `data/README.md` for the expected files and
where to obtain them.

## Citing

If you use this code, please cite it via the repository's citation file
(`CITATION.cff`; GitHub shows a "Cite this repository" button generated
from it).

## License

GPL-3. See [LICENSE](LICENSE).
