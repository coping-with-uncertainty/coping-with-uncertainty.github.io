# Script: Random_Number_Generation.R
# Description: This script generates random numbers using a linear congruential generator (LCG),
# visualizes empirical distributions, and implements bootstrapping.

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to save plots

# Linear Congruential Generator (LCG) parameters
x0 <- 5
a <- 7^5
m <- 2^31 - 1
xn <- x0
xnt <- 1
M <- 5000

# Generate random numbers using LCG
for (i in 2:M) {
  xn <- c(xn, (a * xn[i - 1]) %% m)
  xnt <- c(xnt, (a * xnt[i - 1]) %% m)
}

Xn <- xn / m
Xnt <- xnt / m

# Plot 1: LCG generated numbers
if (generate_pdf) pdf("plot-rn-1.pdf", width = 6, height = 4)
plot(Xn, ylab = expression(x[n] / m))
if (generate_pdf) dev.off()

# Plot 2: Empirical CDF of generated numbers
if (generate_pdf) pdf("plot-rn-2.pdf", width = 6, height = 4)
plot(ecdf(Xn), ylab = expression(F[n] ~ (x)), main = "")
if (generate_pdf) dev.off()

# Plot 3: Scatter plot of consecutive random numbers
if (generate_pdf) pdf("plot-rn-3.pdf", width = 6, height = 4)
plot(Xn, Xnt, xlab = expression(x["n,1"] / m), ylab = expression(x["n,2"] / m))
if (generate_pdf) dev.off()

# Function for importance sampling
gt <- function(u, mu) {
  y <- log(u / (1 - u))
  return(exp(-0.5 * (y - mu)^2) * (1 / (u * (1 - u))))
}

# Estimate function for different values of mu
set.seed(1)
N <- 1000
U <- runif(N)
est1 <- gt(U, 0)

# Plot 4: First estimation function
if (generate_pdf) pdf("plot-rn-4.pdf", width = 6, height = 4)
plot(est1, ylab = expression(tilde(g) ~ "(" ~ "U" ~ ")"))
if (generate_pdf) dev.off()

# Confidence interval for estimation
ci_lower <- mean(est1) - 1.96 * sqrt(var(est1) / N)
ci_upper <- mean(est1) + 1.96 * sqrt(var(est1) / N)

# Estimate with different mean
est2 <- gt(U, 4)

# Plot 5: Second estimation function
if (generate_pdf) pdf("plot-rn-5.pdf", width = 6, height = 4)
plot(est2, ylab = expression(tilde(g) ~ "(" ~ "U" ~ ")"))
if (generate_pdf) dev.off()

# Plot 6: Normal CDF visualization
if (generate_pdf) pdf("plot-rn-6.pdf", width = 6, height = 4)
y <- seq(-3, 3, by = 0.01)
plot(y, pnorm(y), type = "l", ylab = "F(y)", lwd = 3)
abline(v = 0, lwd = 3)
U <- 0.78
lines(c(0, qnorm(U)), c(U, U), col = "blue", lwd = 2)
lines(c(qnorm(U), qnorm(U)), c(0, U), col = "blue", lwd = 2)
if (generate_pdf) dev.off()

# Bootstrapping section
set.seed(9)
n <- 100
y <- rnorm(n)

# Plot 7: Original sample
if (generate_pdf) pdf("plot-boot-1.pdf", width = 5, height = 3.5)
plot(y)
if (generate_pdf) dev.off()

# Bootstrapping samples
y1 <- sample(y, replace = TRUE)
y2 <- sample(y, replace = TRUE)
y3 <- sample(y, replace = TRUE)

# Plot 8: Bootstrapped samples
if (generate_pdf) pdf("plot-boot-2.pdf", width = 5, height = 3.5)
plot(y, col = "grey")
points(y1, col = "darkblue", pch = 2)
points(y2, col = "darkgreen", pch = 3)
points(y3, col = "darkred", pch = 4)
if (generate_pdf) dev.off()

# Compute bootstrap variance
B <- 400
sig <- c()
for (i in 1:B) {
  sig <- c(sig, var(sample(y, replace = TRUE)))
}

# Plot 9: Histogram of bootstrapped variance
if (generate_pdf) pdf("plot-boot-3.pdf", width = 5, height = 3.5)
hist(sig, breaks = 20, freq = FALSE, main = "", xlab = expression("bootstrapped" ~ sigma^2))
abline(v = var(y), lwd = 3)
if (generate_pdf) dev.off()

# Plot 10: Bootstrap ECDF with confidence intervals
if (generate_pdf) pdf("plot-boot-4.pdf", width = 5, height = 3.5)
e <- ecdf(sig)
plot(e, verticals = TRUE, do.points = FALSE, ylab = "bootstrap distribution",
     xlab = expression("bootstrapped" ~ sigma^2))
qu <- get("x", environment(e))
U <- 0.05
quu <- qu[floor(U * B) + 1]
lines(c(0, quu), c(U, U), col = "blue", lwd = 2)
lines(c(quu, quu), c(0, U), col = "blue", lwd = 2)
U <- 0.95
quo <- qu[floor(U * B) + 1]
lines(c(0, quo), c(U, U), col = "blue", lwd = 2)
lines(c(quo, quo), c(0, U), col = "blue", lwd = 2)
if (generate_pdf) dev.off()
