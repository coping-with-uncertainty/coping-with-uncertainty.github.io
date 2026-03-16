# Script: Permutation_Test.R
# Description: This script performs a permutation test on rental prices from two datasets.

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to save plots

# Load datasets (Ensure the RData files are in the working directory)
daten1 <- readRDS("data/Hartz4_2020.RDS")
y <- daten1$nm / daten1$wfl  # Rent per square meter

daten2 <- readRDS("data/Hartz4_2022.RDS")
z <- daten2$nm / daten2$wfl
z <- z[z < 35]  # Filter extreme values

# Plot 1: Empirical distribution functions of original data
if (generate_pdf) pdf("plot-permute-1.pdf", width = 5, height = 3.5)
plot(ecdf(y), main = "", xlab = "Rent per square meter [EURO/sqm]", 
     ylab = "Distribution function", col = "darkgreen", lwd = 3)
lines(ecdf(z), col = "darkblue", lwd = 3)
if (generate_pdf) dev.off()

# Adjust y-values for inflation
y <- y * 111.5 / 105.2
n <- length(y)
m <- length(z)

# Plot 2: Adjusted empirical distribution functions
if (generate_pdf) pdf("plot-permute-2.pdf", width = 5, height = 3.5)
plot(ecdf(y), main = "", xlab = "Rent per square meter [EURO/sqm]", 
     ylab = "Distribution function", col = "darkgreen", lwd = 3)
lines(ecdf(z), col = "darkblue", lwd = 3)
if (generate_pdf) dev.off()

# Compute Kolmogorov-Smirnov statistic
D <- ks.test(y, z)$statistic

# Permutation test setup
x <- c(y, z)
B <- 1000
DS <- numeric(B)

# Permutation test loop
for (i in 1:B) {
  xs <- sample(x)
  DS[i] <- ks.test(xs[1:n], xs[(n + 1):(n + m)])$statistic
}

# Plot 3: Histogram of simulated KS statistics
if (generate_pdf) pdf("plot-permute-3.pdf", width = 5, height = 3.5)
hist(DS, breaks = 20, main = "", freq = FALSE, 
     xlab = "Simulated D (permutation)", ylab = "Density")
abline(v = D, lwd = 3)
if (generate_pdf) dev.off()
