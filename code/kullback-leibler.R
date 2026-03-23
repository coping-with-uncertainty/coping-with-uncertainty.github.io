# Script: Kullback_Leibler_Divergence_Example.R
# Description: This script calculates and visualizes the Kullback-Leibler divergence between two probability distributions.
# It generates multiple plots, which can optionally be saved as PDF files.

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to FALSE if you do not want to generate PDF files

# Define the sequence of x values
x <- seq(-4, 4, by = 0.001)

# Define the first probability density function (PDF) f1
f1 <- function(x) {
  return(0.4 * dnorm(x, mean = -0.6, sd = 0.75) +
           0.6 * dnorm(x, mean = 0.4, sd = 0.75))
}

# Define the second probability density function (PDF) f2 (standard normal distribution)
f2 <- function(x) {
  return(dnorm(x))
}

# Define the cumulative distribution function (CDF) F1 corresponding to f1
F1 <- function(x) {
  return(0.4 * pnorm(x, mean = -0.6, sd = 0.75) +
           0.6 * pnorm(x, mean = 0.4, sd = 0.75))
}

# Define the cumulative distribution function (CDF) F2 corresponding to f2
F2 <- function(x) {
  return(pnorm(x))
}

# Define the Kullback-Leibler divergence function
KL <- function(x) {
  log(f2(x) / f1(x)) * f2(x)
}

# Generate plot comparing f1 and f2
if (generate_pdf) {pdf("plot.KL.1.pdf", height = 4, width = 8)}
plot(x, f1(x), type = "l", lwd = 2, lty = 2, ylab = "density", xlab = "y")
lines(x, f2(x), lty = 1, lwd = 2)
legend(-4, 0.4, lty = c(1, 2), lwd = 2, c("f(y)", "g(y)"))
if (generate_pdf) {dev.off()}

# Generate plot comparing F1 and F2
if (generate_pdf) {pdf("plot.KL.KS.pdf", height = 4, width = 8)}
plot(x, F1(x), type = "l", lwd = 2, lty = 2, ylab = "density", xlab = "y")
lines(x, F2(x), lty = 1, lwd = 2)
legend(-4, 0.4, lty = c(1, 2), lwd = 2, c("f(y)", "g(y)"))
if (generate_pdf) {dev.off()}

# Generate plot for log(f2(x)/f1(x))
if (generate_pdf) {pdf("plot.KL.2.pdf", height = 4, width = 8)}
plot(x, log(f2(x) / f1(x)), ylab = "log(g(y)/f(y))", xlab = "y", type = "l", lwd = 2)
if (generate_pdf) {dev.off()}

# Generate plot for log(f2(x)/f1(x)) * f2(x)
if (generate_pdf) {pdf("plot.KL.3.pdf", height = 4, width = 8)}
plot(x, log(f2(x) / f1(x)) * f2(x), ylab = "log(g(y)/f(y))*g(y)", xlab = "y", type = "l", lwd = 2)
if (generate_pdf) {dev.off()}

# Generate plot from above with a shaded area between the curve and the x-axis
if (generate_pdf) {pdf("plot.KL.4.pdf", height = 4, width = 8)}
curve(KL, from = min(x), to = max(x), ylab = "log(g(y)/f(y))*g(y)", xlab = "y", type = "l", lwd = 2)
x1 <- c(x, min(x))
polygon(x1, y = KL(x1), col = "grey")
if (generate_pdf) {dev.off()}

