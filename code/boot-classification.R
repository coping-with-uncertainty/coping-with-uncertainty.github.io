# Script: Bootstrap_Classification.R
# Description: This script evaluates classification accuracy using bootstrapping.

# Load necessary library
library(caret)

# Set seed for reproducibility
set.seed(40)

# Number of observations
n <- 1000

# Generate categorical data using multinomial distribution
category <- t(rmultinom(n, size = 1, prob = c(4, 2, 3, 3))) %*% matrix(c(1, 2, 3, 4))

# Generate continuous variables with noise
y <- rnorm(n)
y2 <- y + rnorm(n, sd = 0.5)
y3 <- y + rnorm(n, sd = 0.5)

# Binary classification based on threshold (-0.5)
yc <- ifelse(y > -0.5, "2", "1")
yc2 <- ifelse(y2 > -0.5, "2", "1")
yc3 <- ifelse(y3 > -0.5, "2", "1")

# Compute classification accuracy
ac2 <- confusionMatrix(table(yc, yc2))[[3]][[1]]
ac3 <- confusionMatrix(table(yc, yc3))[[3]][[1]]

# Print accuracy values
print(c(ac2, ac3))

# Compute absolute difference
delta <- abs(ac2 - ac3)
print(delta)

# Number of bootstrap samples
B <- 1000
delta.acc <- numeric(B)

# Bootstrapping loop
for (i in 1:B) {
  index <- sample(n, n, replace = TRUE)
  v <- rbinom(n, size = 1, prob = 0.5)
  
  yb <- y[index]
  y2b <- y2[index] * v + y3[index] * (1 - v)
  y3b <- y3[index] * v + y2[index] * (1 - v)
  
  ycb <- ifelse(yb > -0.5, "2", "1")
  yc2b <- ifelse(y2b > -0.5, "2", "1")
  yc3b <- ifelse(y3b > -0.5, "2", "1")
  
  ac2b <- confusionMatrix(table(ycb, yc2b))[[3]][[1]]
  ac3b <- confusionMatrix(table(ycb, yc3b))[[3]][[1]]
  
  delta.acc[i] <- abs(ac2b - ac3b)
}

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to save the plot

# Plot bootstrapped accuracy differences
if (generate_pdf) pdf("plot.boot.class.pdf", width = 6, height = 4)
plot(ecdf(delta.acc), main = "Bootstrapped values",
     xlab = expression(D^"*"), ylab = expression(F[n]~"( )"),
     verticals = TRUE, do.points = FALSE, lwd = 3)
abline(v = delta, col = "darkblue", lwd = 3)
if (generate_pdf) dev.off()
