# Script: Extreme_Value_Analysis.R
# Description: This script analyzes extreme temperatures using Generalized Extreme Value (GEV) distributions.

# Load necessary libraries
library(evir)
library(evd)

# Flag to control PDF generation
generate_pdf <- FALSE  # Set to TRUE to save plots

# Load dataset
data <- read.table("data/temp.dat", header = TRUE)

# Convert date column
data$date <- strptime(data[,4], "%Y%m%d")

# Plot 1: Daily maximum temperature over time
if (generate_pdf) pdf("plot.temp-1.pdf", width = 8, height = 4)
plot(data$date, data$Luft, type = "l", xlab = "Year", 
     ylab = "Maximum Daily Temperature [Celsius]")
if (generate_pdf) dev.off()

# Extract unique years
YEAR <- unique(data$date$year)
YEAR <- YEAR[-length(YEAR)]
DATA <- data.frame(year = YEAR + 1900, max.temp = NA, min.temp = NA)

# Compute yearly max and min temperatures
for (i in 1:length(YEAR)) {
  index <- data$date$year == YEAR[i]
  DATA$max.temp[i] <- max(data$Luftemperatur_Max[index])
  DATA$min.temp[i] <- min(data$Luftemperatur_Max[index])
}

# Plot 2: Yearly maximum temperature
if (generate_pdf) pdf("plot.temp-2.pdf", width = 8, height = 4)
plot(DATA$year, DATA$max.temp, xlab = "Year", 
     ylab = "Yearly Maximum Temperature [Celsius]")
if (generate_pdf) dev.off()

# Plot 3: Yearly lowest daily max temperature
if (generate_pdf) pdf("plot.temp-3.pdf", width = 8, height = 4)
plot(DATA$year, DATA$min.temp, xlab = "Year", 
     ylab = "Lowest Daily Max. Temp. [Celsius]")
abline(h = 0, col = "red")
if (generate_pdf) dev.off()

# Fit GEV model for max temperature
mod <- gev(DATA$max.temp)
par <- mod$par.ests

# Plot 4: Empirical CDF with GEV fit for max temperature
if (generate_pdf) pdf("plot.extreme-2.pdf", height = 4, width = 5)
plot(ecdf(DATA$max.temp), ylab = expression("H(y) and" ~ F[n] ~ "(y)"), 
     main = "", xlab = "Maximum Temperature [Celsius]")
xx <- seq(min(DATA$max.temp), max(DATA$max.temp), by = 0.01)
lines(xx, pgev(xx, loc = par[3], scale = par[2], shape = par[1]), 
      lwd = 3, col = "blue")
if (generate_pdf) dev.off()

# Fit GEV model for min temperature
mod <- gev(DATA$min.temp)
par <- mod$par.ests

# Plot 5: Empirical CDF with GEV fit for min temperature
if (generate_pdf) pdf("plot.extreme-3.pdf", height = 4, width = 5)
plot(ecdf(DATA$min.temp), ylab = expression("H(y) and" ~ F[n] ~ "(y)"), 
     main = "", xlab = "Lowest Daily Max. Temp. [Celsius]")
xx <- seq(min(DATA$min.temp), max(DATA$min.temp), by = 0.01)
lines(xx, pgev(xx, loc = par[3], scale = par[2], shape = par[1]), 
      lwd = 3, col = "blue")
if (generate_pdf) dev.off()

# Split data before and after 1985
DATA1 <- DATA[DATA$year <= 1985, ]
DATA2 <- DATA[DATA$year > 1985, ]

# Fit GEV models for both periods
mod1 <- gev(DATA1$max.temp)
mod2 <- gev(DATA2$max.temp)

# Plot 6: Comparison of max temperature distributions before and after 1985
if (generate_pdf) pdf("plot.extreme-4.pdf", height = 4, width = 5)
plot(ecdf(DATA1$max.temp), ylab = expression("H(y) and" ~ F[n] ~ "(y)"), 
     main = "", xlab = "Maximum Temperature [Celsius]", col = "blue")
lines(ecdf(DATA2$max.temp), col = "red")

xx <- seq(min(DATA$max.temp), max(DATA$max.temp), by = 0.01)
par1 <- mod1$par.ests
par2 <- mod2$par.ests

lines(xx, pgev(xx, loc = par1[3], scale = par1[2], shape = par1[1]), 
      lwd = 3, col = "blue")
lines(xx, pgev(xx, loc = par2[3], scale = par2[2], shape = par2[1]), 
      lwd = 3, col = "red")

text(22, 0.6, "Before 1985", col = "blue")
text(26, 0.2, "After 1985", col = "red")
if (generate_pdf) dev.off()

# Simulating extreme values
Y <- rnorm(1)
for (i in 1:8) {
  Y <- c(Y, max(rnorm(10^i)))
}

# Plot 7: Simulated extreme values for increasing sample size
if (generate_pdf) pdf("plot.extreme-1.pdf", height = 4, width = 5)
plot(0:8, Y, xlab = expression("Sample size n [" ~ 10^k ~ "]"), 
     ylab = expression(Y["max"]))
if (generate_pdf) dev.off()

# Extreme values in sprint performance
Jahresbestleistung <- c(10, 10.1, 10.2, 10.2, 10.28, 10.37, 10.32,
                        10.42, 10.36, 10.34, 10.25, 10.23, 10.27,
                        10.16, 10.20, 10.20, 10.24, 10.25, 10.37,
                        10.28, 10.33, 10.24, 10.30, 10.28, 10.20,
                        10.29, 10.19, 10.30, 10.30, 10.28, 10.19,
                        10.21, 10.23, 10.14, 10.22, 10.16, 10.24,
                        10.24, 10.17, 10.18, 10.14, 10.24)

n <- length(Jahresbestleistung)
model <- fgev(Jahresbestleistung)

# Plot 8: Extreme value modeling for sprint performance
if (generate_pdf) pdf("plot.extreme-5.pdf", height = 4, width = 5)
plot(1:n/n, pgev(sort(Jahresbestleistung), loc = model$par[1], 
                 scale = model$par[2], shape = model$par[3]))
abline(0, 1)
if (generate_pdf) dev.off()

# Extreme quantile estimation
qgev(0.001, loc = model$par[1], scale = model$par[2], shape = model$par[3])
