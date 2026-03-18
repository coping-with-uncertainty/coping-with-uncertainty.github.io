# Script: Likelihood_Function_Visualization.R
# Description: This script defines and visualizes likelihood functions 
# for normal and binomial distributions, highlighting the effects of 
# sample size and parameter changes.

# Load required library
library(ggplot2)

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to generate PDF files

# Define the log-likelihood function for a normal distribution
log.lik.norm <- function(x, mu) {
  log.lik.return <- c()
  for (i in 1:length(mu)) {
    log.lik.return <- c(log.lik.return, sum(log(dnorm(x, mean = mu[i]))))
  }
  return(log.lik.return)
}

# Define the log-likelihood function for a binomial distribution
log.lik <- function(p, x, n) {
  return(log(dbinom(x, size = n, prob = p)))
}

# Define an approximate log-likelihood function for a binomial distribution
log.lik.approx <- function(p, x, n) {
  p.ml <- x / n
  return(log(dbinom(x, size = n, prob = p.ml)) - 
           0.5 * n * (p - p.ml)^2 / (p.ml * (1 - p.ml)))
}

# Example 0: Consistency for normal distribution 

x_5 <- rowMeans(matrix(rbinom(5*2000,size = 1, prob = 0.3), nrow = 2000))
# x_10 <- rowMeans(matrix(rbinom(10*2000,size = 1, prob = 0.3), nrow = 2000))
# x_15 <- rowMeans(matrix(rbinom(15*2000,size = 1, prob = 0.3), nrow = 2000))
# x_20 <- rowMeans(matrix(rbinom(20*2000,size = 1, prob = 0.3), nrow = 2000))
x_25 <- rowMeans(matrix(rbinom(25*2000,size = 1, prob = 0.3), nrow = 2000))
# x_30 <- rowMeans(matrix(rbinom(30*2000,size = 1, prob = 0.3), nrow = 2000))
# x_35 <- rowMeans(matrix(rbinom(35*2000,size = 1, prob = 0.3), nrow = 2000))
# x_40 <- rowMeans(matrix(rbinom(40*2000,size = 1, prob = 0.3), nrow = 2000))
# x_45 <- rowMeans(matrix(rbinom(45*2000,size = 1, prob = 0.3), nrow = 2000))
x_50 <- rowMeans(matrix(rbinom(n = 50*2000,size = 1, prob = 0.3), nrow = 2000))
x_75 <- rowMeans(matrix(rbinom(75*2000,size = 1, prob = 0.3), nrow = 2000))
x_100 <- rowMeans(matrix(rbinom(100*2000,size = 1, prob = 0.3), nrow = 2000))
x_150 <- rowMeans(matrix(rbinom(150*2000,size = 1, prob = 0.3), nrow = 2000))
data_list <- list(
  "5"  = x_5, 
  # "10" = x_10, "15" = x_15, "20" = x_20, 
  "25" = x_25,
  # "30" = x_30, "35" = x_35, "40" = x_40, "45" = x_45, 
  "50" = x_50, "75" = x_75, "100" = x_100, "150" = x_150
)

# Calculate the error relative to the true parameter p = 0.3
error_list <- lapply(data_list, function(x) abs(x - 0.3))

pdf("plots/consist.pdf", width = 5, height = 5)
par(mar = c(5, 5, 2, 2)) 

boxplot(error_list, 
        axes = FALSE,      # Suppress default axes and box
        xlab = "Sample Size (n)",
        ylab = expression(abs(hat(theta) - theta[0])),
        pch = 20)
sample_sizes <- c(5, 25, 50, 75, 100, 150)
axis(1, at = 1:length(error_list), labels = sample_sizes)
axis(2, las = 1)         
box(bty = "l")
dev.off()
# Example 1: Log-likelihood for normal distribution
set.seed(1)
x <- rnorm(5)  # Small sample size
mu <- seq(-3, 3, by = 0.01)  # Range of mean values


if (generate_pdf) pdf("plots/logik.pdf", width = 5, height = 5)
par(mar = c(5, 5, 4, 2) + 0.1, mgp = c(4, 1, 0))
plot(mu, log.lik.norm(x, mu) - max(log.lik.norm(x, mu)), las = 1, bty = "l",
     type = "l", lwd = 3, ylab = expression("l("*mu*")"), xlab = expression(mu), 
     ylim = c(-25, 0))
x <- rnorm(50)  # Medium sample size
lines(mu, log.lik.norm(x, mu) - max(log.lik.norm(x, mu)), 
      lwd = 2, col = 4, las = 1, bty = "l")
x <- rnorm(500)  # Large sample size
lines(mu, log.lik.norm(x, mu) - max(log.lik.norm(x, mu)), 
      lwd = 2, col = 6, las = 1, bty = "l")
if (generate_pdf) dev.off()

# Example 2: Log-likelihood for binomial distribution
p0 <- 0.3  # True probability
p <- seq(0, 1, by = 0.001)  # Range of probabilities
n <- 10  # Number of trials
x <- rbinom(1, size = n, prob = p0)  # Simulated observation

# Likelihood visualization with varying x
if (generate_pdf) pdf("plots/plot.lik.4.pdf", height = 7, width = 7)
plot(p, log.lik(p, x, n), xlab = expression(pi), type = "n", 
     ylab = expression("l("*mu*")"), ylim = c(-40, 0), las = 1, bty = "l")

PP <- c()
for (i in 0:n) {
  x <- i
  pp <- dbinom(x, size = n, prob = p0)
  PP <- c(PP, pp)
  lines(p, log.lik(p, x, n) - max(log.lik(p, x, n)), 
        type = "l", lwd = pp * 50, col = "blue", las = 1, bty = "l")
}
abline(v = p0)
if (generate_pdf) dev.off()

# Visualizing likelihood weights
if (generate_pdf) pdf("plots/plot.lik.5.pdf", height = 3, width = 7)
barplot(PP * length(PP), ylim = c(0, 8), las = 1, bty = "l")
if (generate_pdf) dev.off()



