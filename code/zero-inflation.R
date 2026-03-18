# Script: Zero_Inflation_Model.R
# Description: This script simulates a zero-inflated Poisson model and performs Bayesian inference.

# Load necessary libraries
library(MASS)
library(mvtnorm)
library(mnormt)
library(pscl)

# Logit and inverse logit functions
logit <- function(p) log(p / (1 - p))
logit.inv <- function(eta) exp(eta) / (1 + exp(eta))

# Likelihood function for zero-inflated Poisson model
likel <- function(y, l, p) (y == 0) * p + (1 - p) * dpois(y, lambda = l)

# Proposal distributions
p.eta <- function(eta) logit.inv(eta)
p.delta <- function(delta, gamma) {
  lam <- exp(delta)
  lam * gamma * exp(-gamma * lam)
}

# Compute Metropolis-Hastings acceptance probability
calc.alpha <- function(Y, delta.s, eta.s, delta, eta, gamma = 1) {
  lambda <- exp(delta)
  lambda.s <- exp(delta.s)
  p <- logit.inv(eta)
  p.s <- logit.inv(eta.s)
  lr <- exp(sum(log(likel(Y, lambda.s, p.s)) - log(likel(Y, lambda, p))))
  alpha <- min(1, lr * (p.delta(delta.s, gamma) / p.delta(delta, gamma)) * 
                 (p.eta(eta.s) / p.eta(eta)))
  return(alpha)
}

# Set seed for reproducibility
set.seed(1)
n <- 10000

# Generate zero-inflated Poisson data
Z <- rbinom(n, size = 1, prob = 0.3)
Y <- Z * 0 + (1 - Z) * rpois(n, lambda = 5)

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to save plots

# Plot 1: Histogram of observed data
if (generate_pdf) pdf("plots/plot.zi.1.pdf", height = 5, width = 6)
plot(table(Y), ylab = "count", las = 1, bty = "l")
if (generate_pdf) dev.off()

# Initialize parameters
p <- 0.01
lambda <- 3
delta <- log(lambda)
eta <- logit(p)
sig.l <- 1
sig.p <- 1
gamma <- 0.01
ETA <- c()
DELTA <- c()
i <- 0

# Metropolis-Hastings sampling
while (i < 30000 & length(ETA) < 5000) {
  i <- i + 1
  eta.s <- eta + rnorm(1, sd = sig.p)
  delta.s <- delta + rnorm(1, sd = sig.l)
  alpha <- calc.alpha(Y, delta.s, eta.s, delta, eta, gamma)
  if ((alpha == 1) | (runif(1) < alpha)) {
    ETA <- c(ETA, eta.s)
    DELTA <- c(DELTA, delta.s)
    eta <- eta.s
    delta <- delta.s
  }
}

# Plot 2: Trajectory of sampled values
if (generate_pdf) pdf("plots/plot.zi.2.pdf", height = 5, width = 6)
plot(logit.inv(ETA)[1:100], exp(DELTA)[1:100], type = "l",
     ylab = expression(lambda), xlab = expression(pi), las = 1, bty = "l")
if (generate_pdf) dev.off()

# Plot 3: Trace plot for eta
if (generate_pdf) pdf("plots/plot.zi.3.pdf", height = 5, width = 6)
plot(logit.inv(ETA), type = "l", ylab = expression(pi), las = 1, bty = "l")
if (generate_pdf) dev.off()

# Plot 4: Trace plot for delta
if (generate_pdf) pdf("plots/plot.zi.4.pdf", height = 5, width = 6)
plot(exp(DELTA), type = "l", ylab = expression(lambda), las = 1, bty = "l")
if (generate_pdf) dev.off()

# Remove burn-in
DELTA <- DELTA[1001:length(DELTA)]
ETA <- ETA[1001:length(ETA)]

# Convert to valid numeric values
logit_eta <- logit.inv(ETA)
exp_delta <- exp(DELTA)

# Remove missing or infinite values
valid_idx <- is.finite(logit_eta) & is.finite(exp_delta)
logit_eta <- logit_eta[valid_idx]
exp_delta <- exp_delta[valid_idx]

# Ensure we have enough valid points
if (length(logit_eta) > 10 & length(exp_delta) > 10) {
  # Compute density only if we have enough valid values
  density_data <- kde2d(logit_eta, exp_delta, n = 50)
  
  # Plot 5: Joint posterior distribution
  if (generate_pdf) pdf("plots/plot.zi.5.pdf", height = 5, width = 6)
  image(density_data, xlab = expression(pi), ylab = expression(lambda), 
        ylim = c(3, 9), xlim = c(0, 0.8))
  abline(v = 0.3)
  abline(v = mean(logit_eta), lty = 2)
  abline(5, 0)
  abline(mean(exp_delta), 0, lty = 2)
  title(main = paste("n =", n))
  if (generate_pdf) dev.off()
} else {
  warning("Not enough valid data points for kde2d density estimation.")
}


# Fit zero-inflated model
summary(zeroinfl(Y ~ 1))
