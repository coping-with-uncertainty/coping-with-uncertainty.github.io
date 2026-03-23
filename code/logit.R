# Script: Logistic_Regression_Analysis.R
# Description: This script performs logistic regression to model disease probability 
# based on age and gender.

# Load necessary library
library(ggplot2)

# Flag to control image generation
generate_pdf <- FALSE  # Set to TRUE to save the plot

# Generate dataset
set.seed(999)
gender <- c(rep("M", 26), rep("F", 21))
age <- c(18, 24, 28, 32, 34, 38, 38, 43, 44, 45, 47, 49, 52, 55, 55, 59, 63, 69, 72, 
         74, 76, 84, 85, 88, 89, 91, 19, 26, 29, 31, 34, 38, 43, 45, 47, 52, 56, 57, 
         63, 63, 69, 72, 76, 84, 85, 89, 91)
disease <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 
             0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1)
patients <- data.frame(age, gender, disease)

# Logistic regression model: Disease ~ Age + Gender
mod1 <- glm(data = patients, disease ~ age + gender, family = binomial)
summary(mod1)

# Prepare new data with predicted probabilities
patients$Gender <- as.factor(patients$gender)
new_data <- patients
new_data$vs <- predict(mod1, newdata = patients, type = "response")

# Plot logistic regression results
logistic_plot <- ggplot(data = new_data, 
                        aes(x = age, y = vs, color = Gender)) +
  theme_classic(base_size = 25) +
  geom_line(data = new_data, size = 2) +
  geom_point(size = 4) +
  xlab("Age") +
  ylab("Prob. of disease")

# Save plot if enabled
if (generate_pdf) {
  ggsave("logistic_plot.png", plot = logistic_plot, width = 8, height = 8)
}

# Display 
logistic_plot

# Print model summary
print(summary(mod1))
