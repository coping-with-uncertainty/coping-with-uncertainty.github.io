# Script: Power_of_Test.R
# Description: This script visualizes the power function of a hypothesis test.

# Set the values for mu and sample size
mu <- seq(-1, 1, by = 0.01)
n <- 30

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to generate PDF files

# Plot 1: Power function for different sample sizes
if (generate_pdf) pdf("plots/power-11.pdf", width = 6, height = 4)
plot(mu, 1 - pnorm(1.64 - mu * sqrt(n)), 
     type = "l", lwd = 3,
     xlim = c(-1, 1), ylim = c(0, 1),
     ylab = expression(P(H[1] ~ "|" ~ mu )),
     xlab = expression(mu), las = 1, bty = "l")
polygon(c(-1, 0, 0, -1), c(0, 0, 0.05, 0.05), col = "lightblue")
lines(mu, 1 - pnorm(1.64 - mu * sqrt(2 * n)), lty = 2, lwd = 3, las = 1, bty = "l")
lines(mu, 1 - pnorm(1.64 - mu * sqrt(n)), lty = 1, lwd = 3, las = 1, bty = "l")
abline(h = 0.05, lty = 3)
abline(v = 0, lty = 3)
text(0.15, 0.6, "n=60")
text(0.5, 0.6, "n=30")
text(-0.5, 0.1, expression(alpha ~ "level"))
text(-0.2, 0.5, expression(H[0] ~ ":" ~ mu <= mu[0]))
if (generate_pdf) dev.off()

# Plot 2: Power function with different mu0 values
mu0 <- seq(0, 1, by = 0.01)
if (generate_pdf) pdf("plots/power-21.pdf", width = 6, height = 4)
plot(mu0, 1 - pnorm(1.64 - mu0 * sqrt(n)), 
     type = "l", lwd = 3, 
     xlim = c(-1, 1), ylim = c(0, 1), 
     ylab = expression(P( H[1] ~ "|" ~ mu )),
     xlab = expression(mu), las = 1, bty = "l")
lines(mu0, 1 - pnorm(1.64 - mu0 * sqrt(2 * n)), lty = 2, lwd = 3, las = 1, bty = "l")
polygon(c(-1, 0, 0, -1), c(0, 0, 0.05, 0.05), col = "lightblue")
abline(h = 0.05, lty = 3)
abline(v = 0, lwd = 2)
text(0.15, 0.6, "n=60")
text(0.5, 0.6, "n=30")
text(-0.5, 0.1, expression(alpha ~ "level"))
text(-0.2, 0.5, expression(H[0] ~ ":" ~ mu ~ "=" ~ mu[0]))
if (generate_pdf) dev.off()

# Plot 3: Power function with delta shift
mu0 <- seq(0.245, 1, by = 0.01)
if (generate_pdf) pdf("plots/power-31.pdf", width = 6, height = 4)
plot(mu0, 1 - pnorm(1.64 - mu0 * sqrt(n)), 
     type = "l", lwd = 3, 
     xlim = c(-1, 1), ylim = c(0, 1), 
     ylab = expression(P(H[1]  ~ "|" ~ mu )),
     xlab = expression(mu), las = 1, bty = "l")
polygon(c(0.25, 1, 1, 0.25), c(0.05, 0.05, 1, 1), col = "lightblue")
lines(mu0, 1 - pnorm(1.64 - mu0 * sqrt(2 * n)), lty = 2, lwd = 3, las = 1, bty = "l")
lines(mu0, 1 - pnorm(1.64 - mu0 * sqrt(n)), lty = 1, lwd = 3, las = 1, bty = "l")
abline(v = 0.25, lwd = 2)
polygon(c(-1, 0, 0, -1), c(0, 0, 0.05, 0.05), col = "lightblue")
abline(h = 0.05, lty = 3)
abline(v = 0, lwd = 2)
text(0.15, 0.6, "n=60")
text(0.5, 0.6, "n=30")
text(-0.5, 0.1, expression(alpha ~ "level"))
text(-0.2, 0.5, expression(H[0] ~ ":" ~ mu ~ "=" ~ mu[0]))
text(0.5, 0.25, expression(H[1] ~ ":" ~ mu ~ "=" ~ mu[0] + delta))
if (generate_pdf) dev.off()

# Plot 4: Power function for increasing sample sizes
mu0 <- seq(0.245, 1, by = 0.01)
if (generate_pdf) pdf("plots/power-41.pdf", width = 6, height = 4)
plot(mu0, 1 - pnorm(1.64 - mu0 * sqrt(n)), 
     type = "n", lwd = 3, 
     xlim = c(-0.1, 0.35), ylim = c(0, 1), 
     ylab = expression(P(H[1] ~ "|" ~ mu ~ "=" ~ mu[0] )),
     xlab = expression(mu), las = 1, bty = "l")
polygon(c(0.25, 1, 1, 0.25), c(0.8, 0.8, 1, 1), col = "lightblue")
abline(h = 0.8, lty = 1, lwd = 3)
lines(mu0, 1 - pnorm(1.64 - mu0 * sqrt(2 * n)), lty = 1, lwd = 3, las = 1, bty = "l")
lines(mu0, 1 - pnorm(1.64 - mu0 * sqrt(3 * n)), lty = 2, lwd = 3, las = 1, bty = "l")
lines(mu0, 1 - pnorm(1.64 - mu0 * sqrt(4 * n)), lty = 3, lwd = 3, las = 1, bty = "l")
abline(v = 0.25, lwd = 2)
polygon(c(-1, 0, 0, -1), c(0, 0, 0.05, 0.05), col = "lightblue")
abline(h = 0.05, lty = 3)
abline(v = 0, lwd = 2)
text(0.2, 0.3, "n=60")
text(0.2, 0.5, "n=90")
text(0.2, 0.7, "n=120")
if (generate_pdf) dev.off()
