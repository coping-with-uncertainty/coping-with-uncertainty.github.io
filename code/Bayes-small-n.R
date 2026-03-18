# Script: Bayesian_Small_n.R
# Description: This script demonstrates Bayesian inference for small sample sizes 
# using normal and Poisson likelihoods with priors.

# Load necessary library
library(MCMCpack)

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to save plots

# Define prior distributions
prior.mu <- function(mu) {
  return(dnorm(mu, mean = 0, sd = 10))
}

prior.s2 <- function(s2) {
  return(dinvgamma(s2, shape = 0.01))
}

# Define likelihood function for normal distribution
likel <- function(y, mean, s2) {
  return(prod(dnorm(y, mean = mean, sd = sqrt(s2))))
}

# Define parameter grids
mu <- seq(-5, 5, by = 0.1)
N.mu <- length(mu)
s2 <- seq(0.00001, 4, by = 0.01)
N.s2 <- length(s2)

# Plot 1: Prior distribution for mu
if (generate_pdf) pdf("plots/plot.bayes.small.n-1.pdf", width = 4, height = 4)
plot(mu, prior.mu(mu), type = "l", ylim = c(0, max(prior.mu(mu))), 
     ylab = "Prior", xlab = expression(mu), las = 1, bty = "l")
if (generate_pdf) dev.off()

# Plot 2: Prior distribution for sigma^2
if (generate_pdf) pdf("plots/plot.bayes.small.n-2.pdf", width = 4, height = 4)
plot(s2, prior.s2(s2), type = "l", ylim = c(0, max(prior.s2(s2))), 
     ylab = "Prior", xlab = expression(sigma^2), las = 1, bty = "l")
if (generate_pdf) dev.off()

# Generate synthetic data
set.seed(1)
Y <- rnorm(2)
y <- Y  # Use the full dataset

# Compute likelihood and posterior
Z.l <- matrix(NA, N.mu, N.s2)
Z.lp <- matrix(NA, N.mu, N.s2)

for (i1 in 1:N.mu) {
  for (i2 in 1:N.s2) {
    likelihood <- likel(y, mean = mu[i1], s2 = s2[i2])
    Z.l[i1, i2] <- likelihood
    Z.lp[i1, i2] <- likelihood * prior.mu(mu[i1]) * prior.s2(s2[i2])
  }
}

# Function to generate density plots
plot_density <- function(data, filename) {
  if (generate_pdf) pdf(paste0("plots/", filename), width = 4, height = 4)
  image(mu, s2, data, ylab = expression(sigma^2), xlab = expression(mu))
  if (generate_pdf) dev.off()
}

# Plot 3: Likelihood function
plot_density(Z.l, "plot.bayes.small.n-3.pdf")

# Plot 4: Posterior distribution
plot_density(Z.lp, "plot.bayes.small.n-4.pdf")

# Second example: Poisson likelihood
n <- 1
y <- 0
lambda <- seq(0, 10, by = 0.1)

# Define Poisson likelihood function
likel_pois <- function(y, n, lambda) {
  return(dpois(y, lambda = n * lambda))
}

# Define prior for lambda
prior.lambda <- function(lambda) {
  return(dgamma(lambda, shape = 2, rate = 1))
}

# Compute likelihood and posterior
z.l <- sapply(lambda, function(l) likel_pois(y, n, l))
z.lp <- z.l * prior.lambda(lambda)

# Plot 5: Prior for lambda
if (generate_pdf) pdf("plots/plot.bayes.small.n-5.pdf", width = 4, height = 4)
plot(lambda, prior.lambda(lambda), type = "l", xlab = expression(lambda), ylab = "Prior", las = 1, bty = "l")
if (generate_pdf) dev.off()

# Plot 6: Poisson likelihood
if (generate_pdf) pdf("plots/plot.bayes.small.n-6.pdf", width = 4, height = 4)
plot(lambda, z.l, type = "l", xlab = expression(lambda), ylab = "Likelihood", las = 1, bty = "l")
if (generate_pdf) dev.off()

# Plot 7: Penalized likelihood (Posterior)
if (generate_pdf) pdf("plots/plot.bayes.small.n-7.pdf", width = 4, height = 4)
plot(lambda, z.lp, type = "l", xlab = expression(lambda), ylab = "Penalized Likelihood", las = 1, bty = "l")
if (generate_pdf) dev.off()
