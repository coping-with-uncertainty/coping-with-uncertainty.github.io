# Script: Bayesian_Beta_Distribution.R
# Description: This script visualizes different Beta distributions with varying parameters.

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to save plots

# Define probability sequence
p <- seq(0, 1, by = 0.001)

# Function to plot Beta distributions
plot_beta <- function(a, b, filename, ylim, quantiles = c(0.05, 1)) {
  if (generate_pdf) pdf(paste0("plots/", filename), height = 4, width = 5)
  plot(p, dbeta(p, a, b), type = "l", xlab = expression(pi),
       ylab = "density", lwd = 3, ylim = ylim, las = 1, bty = "l")
  x0 <- seq(qbeta(quantiles[1], a, b), qbeta(quantiles[2], a, b), by = 0.001)
  polygon(x = c(x0, max(x0), min(x0)), y = c(dbeta(x0, a, b), 0, 0), 
          col = "lightblue")
  if (generate_pdf) dev.off()
}

# Plot 1: Uniform Beta(1,1)
plot_beta(a = 1, b = 1, filename = "plot.beta-1.pdf", ylim = c(0, 2), quantiles = c(0, 1))

# Plot 2: Beta(2,1)
plot_beta(a = 2, b = 1, filename = "plot.beta-2.pdf", ylim = c(0, 2))

# Plot 3: Beta(11,1)
plot_beta(a = 11, b = 1, filename = "plot.beta-3.pdf", ylim = c(0, 10))

# Plot 4: Beta(91,11) with 95% credible interval
plot_beta(a = 91, b = 11, filename = "plot.beta-4.pdf", ylim = c(0, 13), quantiles = c(0.025, 0.975))
