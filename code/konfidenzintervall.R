# Script: Confidence_Interval_Visualization.R
# Description: This script visualizes confidence intervals and related concepts using 
# normal and t-distributions.

# Load required libraries
library(latex2exp)
library(tikzDevice)

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to generate PDF files

# Define x values for the plots
x <- seq(-3, 3, by = 0.001)
x0 <- seq(-3, 1.96, by = 0.001)
y <- dnorm(x)
y0 <- dnorm(x0)

# Plot 1: Standard normal distribution with shaded area
if (generate_pdf) pdf("t-verteilung-1.pdf", height = 6, width = 8)
plot(x, y, type = "l", xlab = "y", ylab = "density")
polygon(c(x0, 1.96, -3), c(y0, 0, 0), col = "grey")
lines(c(1.5, 1.5, 2.5, 2.5, 1.5), c(0, 0.15, 0.15, 0, 0), col = 1, lwd = 3)
if (generate_pdf) dev.off()

# Plot 2: Zoomed-in version of the standard normal distribution
if (generate_pdf) pdf("t-verteilung-2.pdf", height = 6, width = 8)
plot(x, y, type = "l", xlim = c(1.5, 2.5), ylim = c(0, 0.15), xlab = "y", ylab = "density")
polygon(c(x0, 1.96, -3), c(y0, 0, 0), col = "grey")
abline(v = 1.96, col = 1, lwd = 3)
if (generate_pdf) dev.off()

# Plot 3: Visualization of small changes in the critical value
if (generate_pdf) pdf("t-verteilung-3.pdf", height = 6, width = 8)
plot(x, y, type = "l", xlim = c(1.5, 2.5), ylim = c(0, 0.15), xlab = "y", ylab = "density")
abline(v = 1.96, col = 1, lwd = 3)
delta <- 0.2
index <- (x0 >= 1.96 - delta) & (x0 <= 1.96)
polygon(c(x0[index], 1.96, 1.96 - delta), c(y0[index], 0, 0), col = "light green")
abline(v = 1.96 - delta, col = 3, lwd = 3)
index <- (x <= 1.96 + delta) & (x >= 1.96)
polygon(c(x[index], 1.96 + delta, 1.96), c(y[index], 0, 0), col = "light blue")
abline(v = 1.96 + delta, col = 4, lwd = 3)
arrows(1.96, 0.15, x1 = 1.96 - delta, length = delta, col = 3, lwd = 3)
arrows(1.96, 0.15, x1 = 1.96 + delta, length = delta, col = 4, lwd = 3)
f1 <- -abs(round(pnorm(1.96 - delta) - pnorm(1.96), digits = 4))
f2 <- abs(round(pnorm(1.96 + delta) - pnorm(1.96), digits = 4))
text(1.85, 0.03, f1, srt = -90)
text(2.05, 0.03, f2, srt = -90)
if (generate_pdf) dev.off()

# Additional confidence interval plots (unchanged)
# (Kept exactly as in the original file, just added `generate_pdf` flag.)

if (generate_pdf) pdf("t-verteilung-4.pdf", height = 6, width = 8)
yy <- pnorm(x)
yy0 <- pnorm(x0)
plot(x, yy, type = "l", xlim = c(1.5, 2.5), ylim = c(0.9, 1), xlab = "y", ylab = "density")
lines(c(1.96, 1.96, 0), c(0, 0.975, 0.975), col = 1, lwd = 3)
if (generate_pdf) dev.off()

if (generate_pdf) pdf("t-verteilung-6.pdf", height = 6, width = 8)
plot(x, y, type = "l", ylab = "density", xlab = "y", lwd = 2)
lines(x, dt(x, df = 1), col = 2, lwd = 2, lty = 2)
lines(x, dt(x, df = 10), col = 3, lwd = 2, lty = 3)
legend(-3, 0.3, c("normal", "t(1)", "t(10)"), lty = c(1, 2, 3), col = c(1, 2, 3))
if (generate_pdf) dev.off()

# Pivot-based confidence intervals
if (generate_pdf) pdf("KI-pivot-1.pdf", height = 4, width = 6)
xx <- seq(-2.5, 2.5, by = 0.001)
sd <- 0.1
m <- 0.45
plot(xx, dnorm(xx, mean = m, sd = sd), type = "l", 
     ylab = "density", xlab = TeX("\\hat{\\theta}"))
if (generate_pdf) dev.off()

if (generate_pdf) pdf("KI-pivot-3.pdf", height = 4, width = 6)
xx <- seq(-2.5, 2.5, by = 0.001)
plot(xx, dnorm(xx, mean = 0, sd = 1), type = "l", 
     ylab = "density", xlab = TeX("( \\hat{\\theta}- \\theta ) / \\sqrt{Var(\\hat{\\theta})}"))
index <- (xx > -1.96) & (xx < 1.96)
polygon(c(xx[index], 1.96, -1.96), c(dnorm(xx[index]), 0, 0), col = "grey")
if (generate_pdf) dev.off()



