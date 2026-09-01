###############################################################################
## Geostructures of inequality: a poset analysis
## Unified analysis script
## All figures and tables for the paper
###############################################################################

# ===========================================================================
# 0. LIBRARIES AND SETUP
# ===========================================================================

library(POSetR) 
library(tidyverse) 
library(parsec) 
library(RColorBrewer) 

source("SupportFunctions.R")  # contains get_poset() function

# ===========================================================================
# 1. LOAD DATA AND BASIC SETUP
# ===========================================================================

poset_data <- read_delim("../HDI2023.txt", delim = "\t", show_col_types = FALSE)

regions <- sort(unique(poset_data$Region))
# regions: Africa, Americas, Asia, Europe, Oceania

# Optimal number of stable clusters per region (determined iteratively)
# Correspondence: Africa=4, Americas=7, Asia=8, Europe=9, Oceania=6
optim_centers <- c(4, 7, 8, 9, 6)

# Numeric variables for poset analysis
num_vars <- c("LE", "EYS", "MYS", "GNIpC")
cat_vars <- c("Level", "Country", "Region", "SubRegion", "DevRegion")

# ===========================================================================
# 2. HELPER FUNCTIONS
# ===========================================================================

# Prepare data for poset: set row names and select numeric columns
prepare_poset_data <- function(df, id_var = "Code") {
  X <- df[[id_var]]
  df <- column_to_rownames(df, var = id_var)
  df_num <- df %>% select(all_of(num_vars))
  list(X = X, df = df, df_num = df_num)
}

# Compute I/C and W/H ratios from a poset incidence matrix
compute_poset_ratios <- function(Z) {
  Z.cov <- incidence2cover(Z)
  V <- vertices(Z.cov, shape = "equispaced")
  
  n <- nrow(Z)
  total_pairs <- n * (n - 1) / 2
  comparable <- sum(Z[upper.tri(Z)] == 1 | t(Z)[upper.tri(Z)] == 1)
  incomparable <- total_pairs - comparable
  ic_ratio <- round(incomparable / comparable, 2)
  h <- length(unique(V$y))
  w <- max(table(V$y))
  wh_ratio <- round(w / h, 2)
  
  data.frame(N = n, Height = h, Width = w, 
             Comparable = comparable, Incomparable = incomparable,
             IC_ratio = ic_ratio, WH_ratio = wh_ratio)
}

# Compute sensitivity analysis for a poset
compute_sensitivity <- function(df_num, X, Z) {
  dfSens <- data.frame(
    columns = colnames(df_num), 
    sens = numeric(length(colnames(df_num)))
  )
  for (j in 1:ncol(df_num)) {
    dfSens$sens[j] <- sum(get_poset(df_num[, -j], X) - Z)
  }
  dfSens
}

# Plot average heights Hasse diagram
plot_avg_heights <- function(Z, df, title = "", show_legend = TRUE) {
  M <- MRP(Z, method = "approx")
  avg_heights <- colSums(M)
  
  Z.cov <- incidence2cover(Z)
  V <- -vertices(Z.cov, shape = "equispaced")
  V$y <- avg_heights
  
  xlim <- c(min(V$x), max(V$x)) * 1.3
  ylim <- c(min(V$y), max(V$y)) * 1
  
  plot(V, panel.first = drawedges(Z.cov, V), axes = FALSE, xlab = "", 
       col = as.factor(df$DevRegion), pch = 16,
       ylab = "average height", xlim = xlim, ylim = ylim, cex = 4, 
       bg = "white", main = title)
  
  axis.labels <- round(seq(min(V$y) - 2, max(V$y), length.out = 12), 1)
  axis(2, at = axis.labels, labels = axis.labels, cex.axis = 0.75)
  grid(lwd = 2)
  text(V, labels = rownames(Z.cov), cex = 0.75)
  
  if (show_legend) {
    legend('bottomright', 
           legend = levels(as.factor(df$DevRegion)), 
           col = 1:length(levels(as.factor(df$DevRegion))), pch = 16)
  }
  
  avg_heights
}

# Load and filter HDI data by region (reloads fresh each time)
load_region_data <- function(region = NULL, add_benchmarks = FALSE) {
  df <- read_delim("../HDI2023.txt", delim = "\t", show_col_types = FALSE)
  if (add_benchmarks) df <- rbind(df, pp)
  df <- df %>% mutate(DevRegion = as.factor(DevRegion))
  if (!is.null(region)) {
    if (add_benchmarks) {
      df <- df %>% filter(Region %in% c(region, "Benchmark"))
    } else {
      df <- df %>% filter(Region == region)
    }
  }
  df
}

# ===========================================================================
# 3. BENCHMARK PROFILES
# ===========================================================================

# Need poset_data with row names for computing quantiles
prep <- prepare_poset_data(poset_data)

pp1 <- data.frame(Country = "PP1", Code = "MAX",
                  LE = max(poset_data$LE), EYS = max(poset_data$EYS),
                  MYS = max(poset_data$MYS), GNIpC = max(poset_data$GNIpC),
                  Level = "Benchmark", Region = "Benchmark", 
                  SubRegion = "Benchmark", DevRegion = "Benchmark")

pp2 <- data.frame(Country = "PP2", Code = "Q3",
                  LE = quantile(poset_data$LE, 0.75), EYS = quantile(poset_data$EYS, 0.75),
                  MYS = quantile(poset_data$MYS, 0.75), GNIpC = quantile(poset_data$GNIpC, 0.75),
                  Level = "Benchmark", Region = "Benchmark", 
                  SubRegion = "Benchmark", DevRegion = "Benchmark")

pp3 <- data.frame(Country = "PP3", Code = "Q2",
                  LE = quantile(poset_data$LE, 0.5), EYS = quantile(poset_data$EYS, 0.5),
                  MYS = quantile(poset_data$MYS, 0.5), GNIpC = quantile(poset_data$GNIpC, 0.5),
                  Level = "Benchmark", Region = "Benchmark", 
                  SubRegion = "Benchmark", DevRegion = "Benchmark")

pp4 <- data.frame(Country = "PP4", Code = "Q1",
                  LE = quantile(poset_data$LE, 0.25), EYS = quantile(poset_data$EYS, 0.25),
                  MYS = quantile(poset_data$MYS, 0.25), GNIpC = quantile(poset_data$GNIpC, 0.25),
                  Level = "Benchmark", Region = "Benchmark", 
                  SubRegion = "Benchmark", DevRegion = "Benchmark")

pp5 <- data.frame(Country = "PP5", Code = "MIN",
                  LE = min(poset_data$LE), EYS = min(poset_data$EYS),
                  MYS = min(poset_data$MYS), GNIpC = min(poset_data$GNIpC),
                  Level = "Benchmark", Region = "Benchmark", 
                  SubRegion = "Benchmark", DevRegion = "Benchmark")

pp <- rbind(pp1, pp2, pp3, pp4, pp5)
rownames(pp) <- NULL

# ===========================================================================
# 4. FIGURE 1: Example Hasse diagram
# ===========================================================================

countries <- c("Switzerland", "United States", "Austria", "Mexico", "Paraguay")
df_example <- read_delim("../HDI2023.txt", delim = "\t", show_col_types = FALSE) %>%
  filter(Country %in% countries)

X_ex <- df_example$Country
df_example <- column_to_rownames(df_example, var = "Country")
df_example_num <- df_example %>% select(all_of(num_vars))

Z_example <- get_poset(df_example_num, X_ex)

par(mar = c(1, 0, 0, 2))
plot(Z_example, shape = "equispaced")

# ===========================================================================
# 5. FIGURE 2: Global Hasse diagram coloured by regions
# ===========================================================================

df_global <- load_region_data()
prep_global <- prepare_poset_data(df_global)

Z_global <- get_poset(prep_global$df_num, prep_global$X)

palette(brewer.pal(n = length(unique(prep_global$df$Region)), name = "Set3"))
par(mar = c(0, 0, 0, 0))
plot(Z_global, col = as.factor(prep_global$df$Region), pch = 16, shape = "equispaced")
legend('bottomright', legend = levels(as.factor(prep_global$df$Region)),
       col = 1:length(unique(prep_global$df$Region)), pch = 16)

# Global poset ratios
cat("\n=== GLOBAL POSET RATIOS ===\n")
print(compute_poset_ratios(Z_global))

# ===========================================================================
# 6. FIGURE 3: Global Hasse diagram coloured by developing regions
# ===========================================================================

palette(brewer.pal(n = length(unique(prep_global$df$DevRegion)), name = "Set3"))
par(mar = c(0, 0, 0, 0))
plot(Z_global, col = as.factor(prep_global$df$DevRegion), pch = 16, shape = "equispaced")
legend('bottomright', legend = levels(as.factor(prep_global$df$DevRegion)),
       col = 1:length(unique(prep_global$df$DevRegion)), pch = 16)

# ===========================================================================
# 7. GLOBAL SENSITIVITY ANALYSIS
# ===========================================================================

sensitivity_global <- compute_sensitivity(prep_global$df_num, prep_global$X, Z_global)
cat("\n=== GLOBAL SENSITIVITY ===\n")
print(sensitivity_global %>% arrange(desc(sens)))

# ===========================================================================
# 8. REGIONAL ANALYSIS LOOP
#    - Hasse diagrams (Fig. 4 col 1)
#    - Poset ratios (for Table)
#    - Average heights (Fig. 5)
#    - Sensitivity (Fig. 7)
# ===========================================================================

ratios_table <- data.frame()
sensitivity_all <- data.frame()

for (i in 1:length(regions)) {
  cat("\n=== REGION:", regions[i], "===\n")
  
  df_reg <- load_region_data(regions[i])
  prep_reg <- prepare_poset_data(df_reg)
  
  Z_reg <- get_poset(prep_reg$df_num, prep_reg$X)
  
  # --- Hasse diagram ---
  palette(brewer.pal(n = length(unique(prep_reg$df$DevRegion)), name = "RdYlBu"))
  plot(Z_reg, col = as.factor(prep_reg$df$DevRegion), pch = 16, shape = "equispaced")
  legend('bottomright', legend = levels(as.factor(prep_reg$df$DevRegion)),
         col = 1:length(unique(prep_reg$df$DevRegion)), pch = 16)
  
  # --- Poset ratios ---
  ratios_reg <- compute_poset_ratios(Z_reg)
  ratios_reg$Region <- regions[i]
  ratios_reg$Type <- "Original"
  ratios_table <- rbind(ratios_table, ratios_reg)
  cat("Ratios:\n")
  print(ratios_reg)
  
  # --- Average heights ---
  palette(brewer.pal(n = length(unique(prep_reg$df$DevRegion)), name = "RdYlBu"))
  avg_h <- plot_avg_heights(Z_reg, prep_reg$df, title = regions[i])
  
  # --- Sensitivity ---
  sens_reg <- compute_sensitivity(prep_reg$df_num, prep_reg$X, Z_reg)
  sens_reg$Region <- regions[i]
  sensitivity_all <- rbind(sensitivity_all, sens_reg)
  cat("Sensitivity:\n")
  print(sens_reg)
}

# Add global ratios to table
ratios_global <- compute_poset_ratios(Z_global)
ratios_global$Region <- "World"
ratios_global$Type <- "Original"
ratios_table <- rbind(ratios_table, ratios_global)

cat("\n=== ALL POSET RATIOS ===\n")
print(ratios_table)

# ===========================================================================
# 9. SENSITIVITY SUMMARY TABLE AND PLOT (Fig. 7)
# ===========================================================================

# Add global sensitivity
sensitivity_global$Region <- "World"
sensitivity_all <- rbind(sensitivity_all, sensitivity_global)

# Percentage plot
sensitivity_all %>%
  group_by(Region) %>%
  mutate(pct = sens / sum(sens)) %>%
  ggplot(aes(columns, pct)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ Region) +
  ylab("Changes") + xlab("Variables") +
  scale_y_continuous(breaks = seq(0, 0.8, by = .2), labels = scales::percent)

# ===========================================================================
# 10. FIGURE 4 (cols 2-3): CLUSTER ANALYSIS
# ===========================================================================

centers_fixed <- 6
cluster_ratios <- data.frame()

png(file = "../Geography of HDI/POSet_HDI23_Clusters.png", res = 240, height = 5000, width = 3600)
par(mfrow = c(5, 3))

for (i in 1:length(regions)) {
  cat("\n=== CLUSTERS:", regions[i], "===\n")
  set.seed(19810815)
  
  df_reg <- load_region_data(regions[i])
  df_reg <- df_reg %>% mutate(DevRegion = as.factor(DevRegion))
  palette(brewer.pal(n = length(unique(df_reg$DevRegion)), name = "Set3"))
  
  prep_reg <- prepare_poset_data(df_reg)
  Z_reg <- get_poset(prep_reg$df_num, prep_reg$X)
  
  # Col 1: Original Hasse
  plot(Z_reg, col = as.factor(prep_reg$df$DevRegion), pch = 16, shape = "equispaced")
  
  # Col 2: Fixed 6 clusters
  cat("-- Fixed 6 clusters --\n")
  km_6 <- kmeans(x = prep_reg$df_num, centers = centers_fixed, nstart = 100)
  cat("BCSS/TSS:", km_6$betweenss / km_6$totss, "\n")
  cat("Centers:\n"); print(km_6$centers)
  cat("Cluster composition:\n"); print(km_6$cluster %>% sort())
  
  Z_km6 <- get_poset(km_6$centers, 1:nrow(km_6$centers))
  plot(Z_km6, main = paste(regions[i], "- 6 clusters"))
  
  # Ratios for 6-cluster poset
  ratios_km6 <- compute_poset_ratios(Z_km6)
  ratios_km6$Region <- regions[i]
  ratios_km6$Type <- "Cluster_6"
  cluster_ratios <- rbind(cluster_ratios, ratios_km6)
  
  # Stability check for 6 clusters
  resumen_6 <- matrix(nrow = 10, ncol = centers_fixed)
  for (j in 1:10) {
    km_tmp <- kmeans(x = prep_reg$df_num, centers = centers_fixed, nstart = 100)
    resumen_6[j, ] <- km_tmp$size %>% sort()
  }
  cat("Stability (sd):", apply(resumen_6, 2, sd), "\n")
  
  # Col 3: Optimal stable clusters
  cat("-- Optimal stable clusters:", optim_centers[i], "--\n")
  km_opt <- kmeans(x = prep_reg$df_num, centers = optim_centers[i], nstart = 100)
  cat("BCSS/TSS:", km_opt$betweenss / km_opt$totss, "\n")
  cat("Centers:\n"); print(km_opt$centers)
  cat("Cluster composition:\n"); print(km_opt$cluster %>% sort())
  
  Z_km_opt <- get_poset(km_opt$centers, 1:nrow(km_opt$centers))
  plot(Z_km_opt, main = paste(regions[i], "-", optim_centers[i], "clusters"))
  
  # Ratios for optimal-cluster poset
  ratios_km_opt <- compute_poset_ratios(Z_km_opt)
  ratios_km_opt$Region <- regions[i]
  ratios_km_opt$Type <- paste0("Cluster_", optim_centers[i])
  cluster_ratios <- rbind(cluster_ratios, ratios_km_opt)
}

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', xaxt = 'n', yaxt = 'n')
legend('bottom', legend = levels(as.factor(df_reg$DevRegion)),
       col = 1:7, lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, pch = 16, seg.len = 1)
dev.off()

cat("\n=== ALL CLUSTER RATIOS ===\n")
print(cluster_ratios)

# Combined ratios table (original + clusters)
all_ratios <- rbind(ratios_table, cluster_ratios)
cat("\n=== COMPLETE RATIOS TABLE ===\n")
print(all_ratios)

# ===========================================================================
# 11. FIGURE 8: Hasse by regions with embedded scales
# ===========================================================================

png(file = "../Geography of HDI/3.2-POSET_HDI23_Embedded.png", res = 240, height = 2600, width = 2800)
par(mfrow = c(3, 2))
par(mar = c(0, 0, 0, 0))

embedded_ratios <- data.frame()

for (i in 1:length(regions)) {
  df_reg <- load_region_data(regions[i], add_benchmarks = TRUE)
  palette(brewer.pal(n = length(unique(df_reg$DevRegion)), name = "Set3"))
  
  prep_reg <- prepare_poset_data(df_reg)
  Z_reg <- get_poset(prep_reg$df_num, prep_reg$X)
  
  plot(Z_reg, col = as.factor(prep_reg$df$DevRegion), pch = 16, shape = "equispaced")
  
  # Ratios for embedded scales poset
  ratios_emb <- compute_poset_ratios(Z_reg)
  ratios_emb$Region <- regions[i]
  ratios_emb$Type <- "Embedded"
  embedded_ratios <- rbind(embedded_ratios, ratios_emb)
}
# Legend panel
df_leg <- load_region_data(add_benchmarks = TRUE)
plot.new()
legend("center", legend = levels(df_leg$DevRegion), col = 1:8, pch = 16, cex = 1.4)

dev.off()

cat("\n=== EMBEDDED SCALES RATIOS (regional) ===\n")
print(embedded_ratios)


# ===========================================================================
# 12. FIGURE 9: World Hasse with embedded scales
# ===========================================================================
par(mfrow = c(1, 1))
df_world_emb <- load_region_data(add_benchmarks = TRUE)
palette(brewer.pal(n = length(unique(df_world_emb$DevRegion)), name = "Set3"))

prep_world_emb <- prepare_poset_data(df_world_emb)
Z_world_emb <- get_poset(prep_world_emb$df_num, prep_world_emb$X)

par(mar = c(0, 0, 0, 0))
plot(Z_world_emb, col = as.factor(prep_world_emb$df$DevRegion), pch = 16, shape = "equispaced")
legend('bottomright', legend = levels(as.factor(prep_world_emb$df$DevRegion)), 
       col = 1:8, pch = 16, cex = 0.9)

# Ratios for world embedded scales poset
ratios_world_emb <- compute_poset_ratios(Z_world_emb)
ratios_world_emb$Region <- "World"
ratios_world_emb$Type <- "Embedded"
embedded_ratios <- rbind(embedded_ratios, ratios_world_emb)

cat("\n=== ALL EMBEDDED SCALES RATIOS ===\n")
print(embedded_ratios)

# Add to complete ratios table
all_ratios <- rbind(all_ratios, embedded_ratios)
cat("\n=== COMPLETE RATIOS TABLE (all types) ===\n")
print(all_ratios)

# ===========================================================================
# 13. FIGURE 5 (with embedded scales): Average heights by region
# ===========================================================================

png(file = "../Geography of HDI/4.1-POSET_HDI23_Embedded_AverageHeights.png", res = 240, height = 2600, width = 2000)
par(mfrow = c(3, 2))
par(mar = c(1, 2, 1, 0))

for (i in 1:length(regions)) {
  df_reg <- load_region_data(regions[i], add_benchmarks = TRUE)
  palette(brewer.pal(n = length(unique(df_reg$DevRegion)), name = "Set3"))
  
  prep_reg <- prepare_poset_data(df_reg)
  Z_reg <- get_poset(prep_reg$df_num, prep_reg$X)
  
  plot_avg_heights(Z_reg, prep_reg$df, title = regions[i], show_legend = FALSE)
}

# Legend panel
df_leg <- load_region_data(add_benchmarks = TRUE)
plot.new()
legend("center", legend = levels(df_leg$DevRegion), col = 1:8, pch = 16, cex = 1.4)

dev.off()

# ===========================================================================
# 14. FIGURE 6: Average heights global (with embedded scales)
# ===========================================================================

df_world_emb <- load_region_data(add_benchmarks = TRUE)
palette(brewer.pal(n = length(unique(df_world_emb$DevRegion)), name = "RdYlBu"))

prep_world_emb <- prepare_poset_data(df_world_emb)
Z_world_emb <- get_poset(prep_world_emb$df_num, prep_world_emb$X)

par(mfrow = c(1, 1))
par(mar = c(0, 2, 0, 0))
avg_h_world <- plot_avg_heights(Z_world_emb, prep_world_emb$df, show_legend = TRUE)

# ===========================================================================
# 15. MISCLASSIFICATION TABLE (Table 2)
# ===========================================================================
par(mfrow = c(1, 1))
df_world_misc <- load_region_data(add_benchmarks = TRUE)
prep_misc <- prepare_poset_data(df_world_misc)
Z_misc <- get_poset(prep_misc$df_num, prep_misc$X)

M_misc <- MRP(Z_misc, method = "approx")
avg_h_misc <- colSums(M_misc)

world_comparison <- cbind(prep_misc$df, avg_heights = avg_h_misc)

# Classify based on position relative to benchmark profiles
# Q1 = top quartile (best performers), Q4 = bottom quartile
world_comparison <- world_comparison %>%
  mutate(misclassification = ifelse(
    avg_heights > avg_h_misc["Q3"], "Q1",
    ifelse(avg_heights > avg_h_misc["Q2"], "Q2",
           ifelse(avg_heights > avg_h_misc["Q1"], "Q3", "Q4"))))

cat("\n=== MISCLASSIFICATION TABLE ===\n")
print(table(world_comparison$misclassification, world_comparison$Level))

# ===========================================================================
# 16. EXPLORATION: Cluster stability (maintained for reference)
# ===========================================================================

# Manual exploration of cluster stability for a single region
# Change regions[5] and centers as needed

centers_explore <- 4
iteracions <- 9

df_explore <- load_region_data(regions[5])
prep_explore <- prepare_poset_data(df_explore)

resumen <- list(
  perc_var = numeric(),
  cluster_comp = matrix(nrow = iteracions, ncol = centers_explore),
  cluster_centers = list()
)

par(mfrow = c(3, 3))
for (j in 1:iteracions) {
  km_tmp <- kmeans(x = prep_explore$df_num, centers = centers_explore, nstart = 100)
  resumen$perc_var[j] <- km_tmp$betweenss / km_tmp$totss
  resumen$cluster_comp[j, ] <- km_tmp$size %>% sort()
  resumen$cluster_centers[[j]] <- km_tmp$centers
  
  Z_tmp <- get_poset(km_tmp$centers, 1:nrow(km_tmp$centers))
  plot(Z_tmp)
}

cat("\n=== EXPLORATION: Variance explained ===\n")
print(resumen$perc_var)
cat("Cluster composition stability (sd):\n")
print(apply(resumen$cluster_comp, 2, sd))
