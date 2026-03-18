# Script: AIC_vs_BIC_Tests.R
# Description: This script compares AIC and BIC penalization in hypothesis testing.

# Set sample sizes
n1 <- 100
n2 <- 100

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to save plots

# Initialize result storage
PP <- c()
penal <- seq(2, 4, by = 0.1)

# Simulation loop to compare AIC vs BIC
for (j in 1:length(penal)) {
  B <- 50000  # Number of simulations
  pp <- 0
  for (i in 1:B) {
    y1 <- rnorm(n1)
    y2 <- rnorm(n2)
    mu1 <- mean(y1)
    mu2 <- mean(y2)
    y <- c(y1, y2)
    mu <- mean(y)
    
    AIC1 <- -2 * (sum(log(dnorm(y1, mean = mu1))) + 
                    sum(log(dnorm(y2, mean = mu2)))) + penal[j]
    AIC0 <- -2 * sum(log(dnorm(y, mean = mu)))
    
    pp <- pp + (AIC0 < AIC1)
  }
  PP <- c(PP, 1 - pp / B)
}  

# Plot 1: AIC and BIC Penalization
if (generate_pdf) pdf("plots/AIC-Test.pdf", width = 6, height = 4)
p <- seq(1, 15, by = 1)
plot(p, qchisq(0.95, df = p), type = "l",
     ylim = c(1, 30), ylab = "Penalization", xlab = "p", las = 1, bty = "l")
lines(p, 2 * p, col = "darkblue", las = 1, bty = "l")
lines(p, log(100) * p, col = "darkgreen", las = 1, bty = "l")
text(4, 25, "BIC", col = "darkgreen")
text(11, 25, "AIC", col = "darkblue")
text(12, 18, "Test")
if (generate_pdf) dev.off()

# Plot 2: Probability of rejecting H0 under AIC and BIC
if (generate_pdf) pdf("plots/AIC-Test-2.pdf", width = 6, height = 4)
plot(p, 1 - pchisq(2 * p, df = p), type = "l",
     ylim = c(0, 0.15), 
     ylab = expression(P( ~ H[1] ~ "|" ~ H[0] )), 
     xlab = "p", col = "darkblue", las = 1, bty = "l")
abline(h = 0.05)
lines(p, 1 - pchisq(log(100) * p, df = p), col = "darkgreen", las = 1, bty = "l")
text(4, 0.01, "BIC", col = "darkgreen")
text(4, 0.11, "AIC", col = "darkblue")
text(12, 0.06, "Test")
if (generate_pdf) dev.off()

# Plot 3: Probability comparison for large p
if (generate_pdf) pdf("plots/AIC-Test-3.pdf", width = 6, height = 4)
p <- seq(20, 100, by = 1)
plot(p, 1 - pchisq(2 * p, df = p), type = "l",
     ylim = c(0, 0.001), 
     ylab = expression(P(H[1] ~ "|" ~ H[0] )), 
     xlab = "p", col = "darkblue", las = 1, bty = "l")
lines(p, 0.05 / p, las = 1, bty = "l")
lines(p, 1 - pchisq(log(100) * p, df = p), col = "darkgreen", las = 1, bty = "l")
text(4, 0.01, "BIC", col = "darkgreen")
text(4, 0.11, "AIC", col = "darkblue")
text(12, 0.06, "Test")
if (generate_pdf) dev.off()
