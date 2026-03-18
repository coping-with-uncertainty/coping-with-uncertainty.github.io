# Script: P_Value_Analysis.R
# Description: This script analyzes and visualizes p-values derived from 
# proportions of newborn boys and girls over multiple years.

# Flag to control PDF generation
generate_pdf <- TRUE  # Set to TRUE to generate PDF files

# Input data
male <- c(408464, 397380, 399287, 404041, 402510)  # Number of male newborns
female <- c(387014, 375752, 378792, 383467, 382374)  # Number of female newborns
n <- male + female  # Total number of newborns
malea <- sum(male)  # Total male newborns across all years
femalea <- sum(female)  # Total female newborns across all years
diverse <- c(14, 12, 11, 15, 17)  # Number of diverse newborns
year <- c(2021, 2020, 2019, 2018, 2017)  # Years

# Plot 1: Male and female newborns over the years
if (generate_pdf) pdf("plots/plot-newborns-1.pdf", height = 4, width = 4)
plot(year, male, type = "l", ylim = c(min(female), max(male)), 
     lwd = 2, ylab = "newborns", las = 1, bty = "l")
lines(year, female, lwd = 2, lty = 2, las = 1, bty = "l")
if (generate_pdf) dev.off()

# Plot 2: Proportion of boys over the years
if (generate_pdf) pdf("plots/plot-newborns-2.pdf", height = 4, width = 4)
plot(year, male /female, type = "l", lwd = 2, ylab = "ratio of boys to girls", las = 1, bty = "l")
abline(h = 1, lty = 2, lwd = 2)  # ratio of 1 line
if (generate_pdf) dev.off()

# Pairwise comparisons of proportions
ybar <- male / n  # Proportions of boys
varbar <- ybar * (1 - ybar) / n  # Variance of proportions
werte <- c()
pwerte <- c()

for (i in 1:4) {
  for (j in (i + 1):5) {
    ss <- (ybar[i] - ybar[j]) / sqrt(varbar[i] + varbar[j])  # Test statistic
    werte <- c(werte, ss)
    pwerte <- c(pwerte, 2 * (1 - pnorm(abs(ss))))  # Two-tailed p-value
  }
}

# Plot 3: Test statistic distribution
if (generate_pdf) pdf("plots/plot-newborns-3.pdf", height = 4, width = 4)
xx <- seq(-3, 3, by = 0.001)
plot(xx, dnorm(xx), type = "l", xlab = "z", ylab = "density", lwd = 3, las = 1, bty = "l")
for (i in 1:length(werte)) {
  abline(v = werte[i], lty = 2)
}
if (generate_pdf) dev.off()

# Plot 4: P-values for pairwise comparisons
if (generate_pdf) pdf("plots/plot-newborns-4.pdf", height = 4, width = 4)
plot(pwerte, ylim = c(0, 1), xlab = "pairwise comparisons", ylab = "p value", las = 1, bty = "l")
if (generate_pdf) dev.off()

# Plot 5: Distribution of p-values for varying shifts
if (generate_pdf) pdf("plots/plot-p-value.pdf", height = 6, width = 6)
pp <- seq(0, 1, by = 0.001)
plot(pp, pp, type = "l", ylab = "P( p value < p )", xlab = "p", lwd = 3, las = 1, bty = "l")
c <- 0.5
lines(pp, 1 - pnorm(qnorm(1 - pp) - c), lty = 2, col = "blue", lwd = 3, las = 1, bty = "l")
c <- 1
lines(pp, 1 - pnorm(qnorm(1 - pp) - c), lty = 3, col = "dark green", lwd = 3, las = 1, bty = "l")
legend(0.7, 0.35, lty = c(1, 2, 3), 
       c("c=0", "c=0.5", "c=1"), 
       col = c(1, "blue", "dark green"))
if (generate_pdf) dev.off()

# Plot 6: P-value curve for varying boy counts
if (generate_pdf) pdf("plots/plot-newborns-5.pdf", height = 4, width = 4)
boys <- seq(795478 / 2, 408490, by = 100)
plot(boys, 1 - pnorm((boys - 795478 / 2) / (sqrt(795478 * 0.25))), 
     type = "l", ylab = "p value", las = 1, bty = "l")
if (generate_pdf) dev.off()
