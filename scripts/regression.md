---
layout: default
title: "Regression"
---

``` r
# Script: Regression_Analysis.R
# Description: This script performs regression analysis on student data,
# estimating weight based on height, age, and gender.

# Load necessary library
library(ggplot2)

# Flag to control PDF and image generation
generate_pdf <- FALSE  # Set to TRUE to save plots

# Read dataset
students <- read.table("data/students.txt", header = TRUE)

# Regression Model 1: Weight ~ Height
mod1 <- lm(data = students, weight ~ height)
summary(mod1)
```

    ## 
    ## Call:
    ## lm(formula = weight ~ height, data = students)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -17.755  -5.639   1.341   5.137  17.245 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -81.9244    26.6600  -3.073  0.00469 ** 
    ## height        0.8870     0.1557   5.697 4.15e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.943 on 28 degrees of freedom
    ## Multiple R-squared:  0.5368, Adjusted R-squared:  0.5203 
    ## F-statistic: 32.45 on 1 and 28 DF,  p-value: 4.151e-06

``` r
# Regression Model 2: Weight ~ Age + Gender + Height
mod2 <- lm(data = students, weight ~ age + gender + height)
summary(mod2)
```

    ## 
    ## Call:
    ## lm(formula = weight ~ age + gender + height, data = students)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.033  -5.094   1.313   6.135  13.837 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -7.04619   32.97149  -0.214  0.83244   
    ## age         -0.06289    0.45434  -0.138  0.89098   
    ## genderM     13.69275    4.06477   3.369  0.00236 **
    ## height       0.41986    0.19286   2.177  0.03875 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.725 on 26 degrees of freedom
    ## Multiple R-squared:  0.6791, Adjusted R-squared:  0.642 
    ## F-statistic: 18.34 on 3 and 26 DF,  p-value: 1.343e-06

``` r
# Create regression plot
plot_regression <- ggplot(students, aes(height, weight)) + 
  theme_classic(base_size = 25) +
  geom_smooth(method = "lm", se = FALSE, col = "royalblue3", size = 1.5) +
  geom_point(size = 4) +
  xlab("Height [cm]") +
  ylab("Weight [Kg]")

# Save plot if enabled
if (generate_pdf) {
  ggsave("regression_plot.png", plot = plot_regression, width = 8, height = 8)
}

# Print summaries
print(summary(mod1))
```

    ## 
    ## Call:
    ## lm(formula = weight ~ height, data = students)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -17.755  -5.639   1.341   5.137  17.245 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -81.9244    26.6600  -3.073  0.00469 ** 
    ## height        0.8870     0.1557   5.697 4.15e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.943 on 28 degrees of freedom
    ## Multiple R-squared:  0.5368, Adjusted R-squared:  0.5203 
    ## F-statistic: 32.45 on 1 and 28 DF,  p-value: 4.151e-06

``` r
print(summary(mod2))
```

    ## 
    ## Call:
    ## lm(formula = weight ~ age + gender + height, data = students)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.033  -5.094   1.313   6.135  13.837 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -7.04619   32.97149  -0.214  0.83244   
    ## age         -0.06289    0.45434  -0.138  0.89098   
    ## genderM     13.69275    4.06477   3.369  0.00236 **
    ## height       0.41986    0.19286   2.177  0.03875 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.725 on 26 degrees of freedom
    ## Multiple R-squared:  0.6791, Adjusted R-squared:  0.642 
    ## F-statistic: 18.34 on 3 and 26 DF,  p-value: 1.343e-06
