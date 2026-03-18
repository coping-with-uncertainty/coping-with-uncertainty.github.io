# Script: Gaussian_Markov_Random_Field.R
# Description: This script simulates a Gaussian Markov Random Field (GMRF)
# and visualizes its evolution.

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to save plots

# Define function for transformation
ftrans <- function(x) {
  return((x <= 0.5) * 0.5 * ((2 * x)**(1/2)) +
           (x > 0.5) * (1 - 0.5 * (2 * (1 - x))**(1/2)))
}

# Grid size and parameters
Nt <- 500
N <- 100

# Generate transformed grid
Y <- seq(0, 1, by = 1 / (Nt - 1))
Y1 <- kronecker(Y, matrix(1, Nt, 1))
Y2 <- kronecker(matrix(1, Nt, 1), Y)
Xt <- matrix(ftrans((exp(-1/2 * (Y1 - 0.5)^2 * 1) * 
                       exp(-((Y1 - 0.5)^2) * ((Y2 - 0.5)^2) * 100))**(1/4)), nrow = Nt)

# Generate smaller transformed grid
Y <- seq(0, 1, by = 1 / (N - 1))
Y1 <- kronecker(Y, matrix(1, N, 1))
Y2 <- kronecker(matrix(1, N, 1), Y)
X <- matrix(ftrans(exp(-1/2 * (Y1 - 0.5)^2 * 1) * 
                     exp(-((Y1 - 0.5)^2) * ((Y2 - 0.5)^2) * 100)), nrow = N)

# Plot 1: Initial transformed grid
if (generate_pdf) pdf("plots/plot-gmrf-1.pdf", height = 5, width = 5)
image(Xt, las = 1)
if (generate_pdf) dev.off()

# Plot 2: Contour of transformed grid
if (generate_pdf) pdf("plots/plot-gmrf-2.pdf", height = 5, width = 5)
contour(Xt)
if (generate_pdf) dev.off()

# Define neighborhood points
ns <- 5
Neig.fix <- cbind(N * kronecker(matrix(seq(1 / (2 * ns), (1 - 1 / (2 * ns)), by = 1 / ns)), 
                                matrix(1, ns, 1)),
                  N * kronecker(matrix(1, ns, 1),
                                seq(1 / (2 * ns), (1 - 1 / (2 * ns)), by = 1 / ns)))
Neig.fix <- round(Neig.fix, digits = 0)

# Plot 3: Neighborhood points
if (generate_pdf) pdf("plots/plot-gmrf-3.pdf", height = 5, width = 5)
image(Xt, las = 1)
points(Neig.fix[,1] / N, Neig.fix[,2] / N, pch = 16, cex = 2, las = 1, bty = "l")
if (generate_pdf) dev.off()

# Function to filter valid neighbors
all.pos <- function(Neig, N) {
  for (i in length(Neig):1) {
    if ((min(Neig[[i]]) == 0) | (max(Neig[[i]]) == (N + 1))) {
      Neig <- Neig[-i]
    }
  }
  return(Neig)
}

# Simulation parameters
T <- 1000
Xsim <- array(NA, c(N, N, T))

# GMRF simulation loop
for (t in 1:T) {
  for (i in 1:N) {
    for (j in 1:N) {
      if (!any((Neig.fix[,1] == i) & (Neig.fix[,2] == j))) {
        Neig <- list(c(i, j - 1), c(i + 1, j - 1), c(i + 1, j), c(i + 1, j + 1),
                     c(i, j + 1), c(i - 1, j + 1), c(i - 1, j), c(i - 1, j - 1))
        Neig <- all.pos(Neig, N)
        x <- sapply(Neig, function(n) X[n[1], n[2]])
        X[i, j] <- if (t == 1) 0.75 + rnorm(1, sd = 0.2) else mean(x) + rnorm(1, sd = 0.1 / log(t))
      }
    }
  }
  Xsim[,,t] <- X
  if (t %% 10 == 0) {
    print(t)
    image(X, zlim = c(-0.2, 1.2), las = 1)
  }
}

# Plot 4: First simulation frame
if (generate_pdf) pdf("plots/plot-gmrf-4.pdf", height = 5, width = 5)
image(Xsim[,,1], las = 1)
if (generate_pdf) dev.off()

# Plot 5: Last simulation frame
if (generate_pdf) pdf("plots/plot-gmrf-5.pdf", height = 5, width = 5)
image(Xsim[,,T], las = 1)
if (generate_pdf) dev.off()

# Plot 6: Maximum over last 100 frames
if (generate_pdf) pdf("plots/plot-gmrf-6.pdf", height = 5, width = 5)
Xmax <- apply(Xsim[,,(T - 100):T], MARGIN = c(1,2), FUN = max)
image(Xmax, las = 1)
if (generate_pdf) dev.off()

# Plot 7: Mean over last 100 frames
if (generate_pdf) pdf("plots/plot-gmrf-7.pdf", height = 5, width = 5)
Xmean <- apply(Xsim[,,(T - 100):T], MARGIN = c(1,2), FUN = mean)
image(Xmean, las = 1)
if (generate_pdf) dev.off()

# Plot 8: Standard deviation over last 100 frames
if (generate_pdf) pdf("plots/plot-gmrf-8.pdf", height = 5, width = 5)
Xvar <- apply(Xsim[,,(T - 100):T], MARGIN = c(1,2), FUN = var)
image(sqrt(Xvar), zlim = c(0, max(sqrt(Xvar))), las = 1)
if (generate_pdf) dev.off()