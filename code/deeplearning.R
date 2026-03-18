# Script: Deep_Learning_Regularization.R
# Description: This script demonstrates deep learning regularization techniques using ridge regression.

# Load necessary libraries
library(gamlr)
library(mgcv)

# Set seed for reproducibility
set.seed(4)

# Define sample sizes
n <- 100
K <- 200

# Generate predictor matrix and coefficients
X <- matrix(rnorm(n * K), nrow = n)
theta <- matrix(c(rnorm(K)))

# Order theta by absolute magnitude (for feature selection)
oi <- order(abs(theta), decreasing = TRUE)
theta <- theta[oi]

# Generate response variable
Y <- X %*% theta + rnorm(n, sd = 1)

# Generate new dataset for evaluation
n.new <- 10000
X.new <- matrix(rnorm(n.new * K), nrow = n.new)
Y.new <- X.new %*% theta + rnorm(n.new, sd = 1)

# Initialize baseline model with intercept only
mod <- lm(Y ~ 1)

# Regularization parameter for ridge regression
gamma <- 0.5
KL <- c()

# Forward selection with ridge penalty
for (k in 2:K) {
  print(k)
  index.in <- 1:k
  X.mod <- X[, index.in]
  theta.est <- solve(t(X.mod) %*% X.mod + gamma * diag(k)) %*% t(X.mod) %*% Y
  KL <- c(KL, t(Y.new - X.new[, index.in] %*% theta.est) %*% 
            (Y.new - X.new[, index.in] %*% theta.est))
}

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to save plots

# Plot 1: Dimension of Theta vs Log KL divergence
if (generate_pdf) pdf("plots/deepl.pdf", width = 5, height = 5)
plot(2:K, log(KL), col = "grey", xlab = expression(paste("Dimension ", "of ", Theta)), las = 1, bty = "l")
xx <- 2:K
yy <- log(KL)
mm <- gam(yy ~ s(xx))
lines(2:K, fitted(mm), lwd = 3, las = 1, bty = "l")
if (generate_pdf) dev.off()

# Data Augmentation
X.aug <- kronecker(matrix(1, 1000, 1), X)
NN <- dim(X.aug)
X.aug <- matrix(rnorm(NN[1] * NN[2], sd = 0.1), nrow = NN[1]) + X.aug
Y.aug <- kronecker(matrix(1, 1000, 1), Y)

KL <- c()
KK <- seq(2, 200, by = 20)

# Regularization for augmented data
for (k in KK) {
  print(k)
  index.in <- 1:k
  X.mod <- X.aug[, index.in]
  theta.est <- solve(t(X.mod) %*% X.mod) %*% t(X.mod) %*% Y.aug
  KL <- c(KL, mean(t(Y.new - X.new[, index.in] %*% theta.est) %*% 
                     (Y.new - X.new[, index.in] %*% theta.est)))
}

# Plot 2: KL divergence vs Model Complexity (Augmented Data)
if (generate_pdf) pdf("plots/deepl-aug.pdf", width = 5, height = 5)
plot(KK, KL, type = "b", pch = 16, xlab = expression(paste("Dimension ", "of ", Theta)), 
     ylab = "Mean KL Divergence", col = "blue", las = 1, bty = "l")
if (generate_pdf) dev.off()

# Final Ridge Regression Estimation
theta <- solve(t(X.aug) %*% X.aug) %*% (t(X.aug) %*% Y.aug)
KL <- c(KL, t(Y.new - X.new[, 1:length(theta)] %*% theta) %*% 
          (Y.new - X.new[, 1:length(theta)] %*% theta))
