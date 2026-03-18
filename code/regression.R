# Script: Regression_Analysis.R
# Description: This script performs regression analysis on student data,
# estimating weight based on height, age, and gender.

# Load necessary library
library(ggplot2)

# Flag to control PDF and image generation
generate_pdf <- TRUE  # Set to TRUE to save plots

# Read dataset
students <- read.table("../data/students.txt", header = TRUE)

# Regression Model 1: Weight ~ Height
mod1 <- lm(data = students, weight ~ height)
summary(mod1)

# Regression Model 2: Weight ~ Age + Gender + Height
mod2 <- lm(data = students, weight ~ age + gender + height)
summary(mod2)

# Create regression plot
plot_regression <- ggplot(students, aes(height, weight)) + 
  theme_classic(base_size = 25) +
  geom_smooth(method = "lm", se = FALSE, col = "royalblue3", size = 1.5) +
  geom_point(size = 4) +
  xlab("Height [cm]") +
  ylab("Weight [Kg]")

# Save plot if enabled
if (generate_pdf) {
  ggsave("plots/regression_plot.png", plot = plot_regression, width = 8, height = 8)
}

# Print summaries
print(summary(mod1))
print(summary(mod2))
