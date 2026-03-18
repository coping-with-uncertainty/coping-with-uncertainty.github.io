# Script: Kolmogorov-Smirnov_Test.R
# Description: This script performs a Kolmogorov-Smirnov test by comparing 
# an empirical distribution function (ECDF) with the theoretical normal CDF.

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to generate PDF files

# Set seed for reproducibility
set.seed(1)

# Number of observations
N <- 25

# Generate and sort random normal data
Y <- rnorm(N)
Y <- sort(Y)

if (generate_pdf) pdf("plots/KS.pdf", height = 6, width = 6)

# Set up two-panel plot layout
par(mfrow = c(2, 1))

# Compute empirical cumulative distribution function (ECDF)
Fn <- ecdf(Y)

# Plot 1: ECDF vs. Normal CDF
plot(Fn, verticals = TRUE, do.points = FALSE, 
     ylab = "distribution function", xlab = "y", xlim = c(-2.5, 2.5),
     main = "", lwd = 1, las = 1, bty = "l")

# Theoretical normal CDF
x <- seq(-4, 4, by = 0.01)
lines(x, pnorm(x), lwd = 1, las = 1, bty = "l")

# Compute Kolmogorov-Smirnov (KS) distance
KS <- cbind((1:N) / N - pnorm(Y))
index <- (1:N)[abs(KS) == max(abs(KS))]

KSS <- Fn(x) - pnorm(x)
Index <- (1:length(x))[abs(KSS) == max(abs(KSS))]

# Draw KS statistic as vertical line
lines(c(x[Index], x[Index]), c(Fn(x[Index]), pnorm(x[Index])), lwd = 3, las = 1, bty = "l")
abline(v = x[Index], lty = 2)

# Plot 2: Kolmogorov-Smirnov distance function
plot(Y, KS, xlim = c(-2.5, 2.5), ylim = c(-0.2, 0.2), type = "n", 
     xlab = "y", ylab = "D_n", las = 1, bty = "l")

title(main = "Kolmogorov-Smirnov-Distance")

# Difference between ECDF and normal CDF
lines(x, Fn(x) - pnorm(x), las = 1, bty = "l")
abline(h = 0)

# Highlight maximum deviation
lines(c(x[Index], x[Index]), c(0, KSS[Index]), lwd = 3, las = 1, bty = "l")
abline(v = x[Index], lty = 2)

if (generate_pdf) dev.off()
