---
layout: default
title: "Likelihood"
---

``` r
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

# Example 1: Log-likelihood for normal distribution
set.seed(1)
x <- rnorm(5)  # Small sample size
mu <- seq(-3, 3, by = 0.01)  # Range of mean values

if (generate_pdf) pdf("logik.pdf", width = 5, height = 5)
plot(mu, log.lik.norm(x, mu) - max(log.lik.norm(x, mu)), 
     type = "l", lwd = 3, ylab = "log likelihood", ylim = c(-25, 0))
x <- rnorm(50)  # Medium sample size
lines(mu, log.lik.norm(x, mu) - max(log.lik.norm(x, mu)), 
      lwd = 2, col = 4)
x <- rnorm(500)  # Large sample size
lines(mu, log.lik.norm(x, mu) - max(log.lik.norm(x, mu)), 
      lwd = 2, col = 6)
if (generate_pdf) dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# Example 2: Log-likelihood for binomial distribution
p0 <- 0.3  # True probability
p <- seq(0, 1, by = 0.001)  # Range of probabilities
n <- 10  # Number of trials
x <- rbinom(1, size = n, prob = p0)  # Simulated observation

# Likelihood visualization with varying x
if (generate_pdf) pdf("plot.lik.4.pdf", height = 7, width = 7)
plot(p, log.lik(p, x, n), xlab = "pi", type = "n", 
     ylab = "log likelihood", ylim = c(-40, 0))
PP <- c()
for (i in 0:n) {
  x <- i
  pp <- dbinom(x, size = n, prob = p0)
  PP <- c(PP, pp)
  lines(p, log.lik(p, x, n) - max(log.lik(p, x, n)), 
        type = "l", lwd = pp * 50, col = "blue")
}
abline(v = p0)
if (generate_pdf) dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# Visualizing likelihood weights
if (generate_pdf) pdf("plot.lik.5.pdf", height = 3, width = 7)
barplot(PP * length(PP), ylim = c(0, 8))
if (generate_pdf) dev.off()
```

    ## quartz_off_screen 
    ##                 2
