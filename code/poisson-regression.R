# Script: Poisson_Regression_Analysis.R
# Description: This script generates synthetic sales data and fits a Poisson regression model.

# Load necessary library
library(tidyverse)
library(mgcv)

# Set seed for reproducibility
set.seed(99)

# Flag to control image generation
generate_pdf <- TRUE  # Set to TRUE to save the plot

# Number of observations
n_obs <- 200

# Generate synthetic data
new_data <- tibble(
  review_score = round(runif(n_obs, min = 3, max = 5), 1),  # Random review scores between 3 and 5
  price = rnorm(n_obs, 70, 10),  # Random price
  promotion = sample(c("Yes", "No"), size = n_obs, replace = TRUE),  # Random promotion status
  purchases = rpois(n_obs, lambda = 7 * (review_score - 2) + ifelse(promotion == "Yes", 2, 0))  # Poisson-distributed purchases
)

# Display the first few rows of the generated data
print(head(new_data))

# Plot histogram of purchases
poisson_plot <- ggplot(data = new_data, aes(x = purchases)) +
  theme_classic(base_size = 25) +
  xlim(0, 38) +
  geom_bar() +
  xlab("Sales") +
  ylab("Frequency")

# Save plot if enabled
if (generate_pdf) {
  ggsave("plots/poisson_plot.png", plot = poisson_plot, width = 8, height = 8)
}

# Display the plot
poisson_plot

# Fit Poisson regression model
mod <- gam(data = new_data, family = poisson(link = "log"), 
           formula = purchases ~ review_score + promotion + price)

# Print model summary
print(summary(mod))
