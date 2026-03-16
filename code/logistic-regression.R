# Script: Logistic_Regression_Visualization.R
# Description: This script plots the logistic function used in logistic regression.

# Load necessary library
library(ggplot2)

# Flag to control image generation
generate_pdf <- FALSE  # Set to TRUE to save the plot

# Create logistic function plot
logistic_plot <- ggplot() + 
  theme_classic(base_size = 25) +
  xlim(-10, 10) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", lwd = 1.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray70", lwd = 1.5) +
  geom_function(fun = function(x) exp(x) / (1 + exp(x)), lwd = 2) +
  xlab(expression(eta[i])) +
  ylab(expression(pi[i])) + 
  theme(axis.title.x = element_text(size = 35), 
        axis.title.y = element_text(size = 35))

# Save plot if enabled
if (generate_pdf) {
  ggsave("logistic_plot.png", plot = logistic_plot, width = 8, height = 8)
}

# Display the plot
logistic_plot
