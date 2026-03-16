# Script: Credibility_Interval_Example.R
# Description: This script visualizes beta distributions and highlights 
# credibility intervals using shading and annotations.

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to generate PDF files

# Define the parameters and sequence
xx <- seq(0, 1, by = 0.001)  # Range of values for the beta distribution
alpha <- 5  # Shape parameter 1
beta <- 10  # Shape parameter 2
y <- dbeta(xx, shape1 = alpha, shape2 = beta)  # Beta distribution density

# Plot 1: Credibility Interval with 90% coverage
if (generate_pdf) pdf("credibility-1.pdf", height = 4, width = 6)
plot(xx, y, type = "l", lwd = 3, xlab = "y", ylab = "Density",
     main = "90% Credibility Interval")
index <- (xx > qbeta(0.05, shape1 = alpha, shape2 = beta)) & 
  (xx < qbeta(0.95, shape1 = alpha, shape2 = beta))
polygon(c(xx[index], max(xx[index]), min(xx[index])), 
        c(y[index], 0, 0), col = "light blue")
text(0.2, 1.0, "0.90", pos = 4)  # Label for the interval
text(0, 1, "0.05", pos = 4, srt = -90)  # Left tail
text(0.6, 1, "0.05", pos = 4, srt = -90)  # Right tail
if (generate_pdf) dev.off()

# Plot 2: Credibility Interval with y = 2
if (generate_pdf) pdf("credibility-2.pdf", height = 4, width = 6)
plot(xx, y, type = "l", lwd = 3, xlab = "y", ylab = "Density",
     main = "Credibility Interval with y = 2")
arrows(0.8, 2, x1 = 0.8, y1 = 1, col = 4, lwd = 3)  # Arrow annotation
nn <- length(xx)
ywert <- 2  # Target y value
ydiff <- abs(round(y - ywert, digits = 5))
index1 <- (1:300)[ydiff[1:300] == min(ydiff[1:300])]
index2 <- (300:nn)[ydiff[300:nn] == min(ydiff[300:nn])]
index <- seq(index1, index2, by = 1)
polygon(c(xx[index], max(xx[index]), min(xx[index])), 
        c(y[index], 0, 0), col = "light blue")  # Highlighted interval
abline(h = 2, col = 4, lwd = 3)  # Reference line for y = 2
if (generate_pdf) dev.off()

# Calculate coverage probability
covprob <- pbeta(xx[index2], shape1 = alpha, shape2 = beta) - 
  pbeta(xx[index1], shape1 = alpha, shape2 = beta)
cat("Coverage Probability:", covprob, "\n")

# Plot 3: Credibility Interval with y = 0.57
if (generate_pdf) pdf("credibility-3.pdf", height = 4, width = 6)
plot(xx, y, type = "l", lwd = 3, xlab = "y", ylab = "Density",
     main = "Credibility Interval with y = 0.57")
ywert <- 0.57  # Target y value
ydiff <- abs(round(y - ywert, digits = 5))
index1 <- (1:300)[ydiff[1:300] == min(ydiff[1:300])]
index2 <- (300:nn)[ydiff[300:nn] == min(ydiff[300:nn])]
index <- seq(index1, index2, by = 1)
polygon(c(xx[index], max(xx[index]), min(xx[index])), 
        c(y[index], 0, 0), col = "light blue")  # Highlighted interval
text(0.2, 1.0, "0.90", pos = 4)  # Interval label
if (generate_pdf) dev.off()
