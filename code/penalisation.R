# Script: Penalisation_Optimization.R
# Description: This script demonstrates penalized likelihood estimation.

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to save plots

# Define parameter range
theta <- seq(-4, 4, by = 0.01)

# Function to compute likelihood and penalized likelihood
plot_penalized_likelihood <- function(y, filename) {
  if (generate_pdf) pdf(filename, height = 5, width = 6)
  
  par(mfrow = c(2, 1))
  sd <- 5
  
  # Compute likelihood
  lik <- apply(sapply(y, function(obs) dnorm(obs, mean = theta, sd = sd)), 1, prod)
  plot(theta, log(lik) - max(log(lik)), ylim = c(-8, 0), 
       ylab = "log likelihood", type = "l")
  
  # Compute penalized likelihood (posterior)
  lik.pen <- lik * dnorm(theta, mean = 0, sd = 1)
  plot(theta, log(lik.pen) - max(log(lik.pen)), 
       ylab = "log posterior", type = "l")
  
  if (generate_pdf) dev.off()
}

# Plot for single observation y = 0
plot_penalized_likelihood(y = c(0), filename = "penopt-1.pdf")

# Plot for observations y = 1, -1
plot_penalized_likelihood(y = c(1, -1), filename = "penopt-2.pdf")
