# Script: Censored_Regression_Example.R
# Description: This script demonstrates censored regression using the 
# censReg package. It visualizes censored data and performs a simple 
# censored regression analysis.

# Load the required library
library(censReg)

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to generate PDF files

# Set parameters and seed
n <- 100  # Number of observations
set.seed(5)  # Set seed for reproducibility

# Generate data
y <- exp(rnorm(n, mean = -1))  # Uncensored data
yc <- y  # Censored version of the data
yc[y <= 0.5] <- 0.5  # Apply left censoring at 0.5
logyc <- log(yc)  # Log-transformed censored data

# Plot the censored data
if (generate_pdf) pdf("plots/plot-cens.pdf", width = 5, height = 5)
plot(yc, ylim = c(0, 4), ylab = "y", xlab = "Observation", las = 1, bty = "l")
abline(h = 0.5, lty = 2)  # Horizontal line indicating censoring threshold
points((1:n)[y > 0.5], y[y > 0.5], las = 1, bty = "l")  # Points for uncensored observations
points((1:n)[y <= 0.5], y[y <= 0.5], col = "grey", las = 1, bty = "l")  # Points for censored observations
if (generate_pdf) dev.off()

# Summary statistics for log-transformed data
cat("Mean of log(y):", mean(log(y)), "\n")
cat("Variance of log(y):", var(log(y)), "\n")
cat("Mean of log(yc):", mean(log(yc)), "\n")
cat("Variance of log(yc):", var(log(yc)), "\n")

# Perform censored regression
cens_reg_result <- censReg(logyc ~ 1, left = log(0.5))  # Left-censored regression
print(summary(cens_reg_result))
