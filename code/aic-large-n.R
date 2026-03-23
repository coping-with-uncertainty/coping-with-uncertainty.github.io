# Script: AIC_Large_Sample_Selection.R
# Description: This script performs model selection using AIC for large sample sizes.

# Load necessary library
library(gamlr)

# Set seed for reproducibility
set.seed(1)

# Define number of observations and predictors
n <- 100
K <- 100

# Generate data matrix and true coefficients
X <- matrix(rnorm(n * K), nrow = n)
theta <- matrix(c(rnorm(K / 2), rep(0, K / 2)))

# Generate response variable
Y <- X %*% theta + rnorm(n, sd = 1)

# Fit initial model with intercept only
mod <- lm(Y ~ 1)
aic <- AICc(mod)  # Corrected AIC

# Maximum model dimension
Kmax <- min((n - 1), (K - 2))
models <- matrix(0, Kmax, K)
models[1, ] <- 0

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to save plots

# Forward selection using AIC
for (k in 2:Kmax) {
  print(k)
  index <- 1:K
  index.in <- index[models[k - 1, ] == 1]
  index.out <- index[models[k - 1, ] == 0]
  aic.loop <- c()
  
  # Compute AIC for candidate models
  for (j in seq_along(index.out)) {
    index.mod <- c(index.in, index.out[j])
    aic.loop <- c(aic.loop, AICc(lm(Y ~ X[, index.mod])))
  }
  
  # Select the best predictor
  index.select <- index.out[aic.loop == min(aic.loop)]
  models[k:Kmax, index.select] <- 1
  aic <- c(aic, min(aic.loop))
}

# Function to generate AIC plots
plot_aic <- function(filename) {
  if (generate_pdf) pdf(filename, width = 5, height = 5)
  plot((0:(length(aic) - 1)), log(aic - min(aic) + 0.01),
       xlab = "dimension of model", ylab = "log(AIC)",
       type = "l", lwd = 3)
  abline(v = 50)
  if (generate_pdf) dev.off()
}

# Function to plot estimated parameters
plot_estimates <- function(filename) {
  if (generate_pdf) pdf(filename, width = 5, height = 3)
  plot(theta, xlab = "index of parameter")
  index.best <- models[aic == min(aic), ]
  index.best <- (1:K)[index.best == 1]
  points(index.best, theta[index.best], pch = 16, cex = 1.5)
  if (generate_pdf) dev.off()
}

# Generate AIC and parameter estimate plots
plot_aic("plot-aic-small-n.pdf")
plot_estimates("plot-aic-est-small-n.pdf")
plot_aic("plot-aic-c-small-n.pdf")
plot_estimates("plot-aic-c-est-small-n.pdf")
