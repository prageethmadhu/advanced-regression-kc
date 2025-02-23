packages <- c("ggplot2", "mgcv", "MASS", "caret", "knitr")
install_if_missing <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(install_if_missing)) install.packages(install_if_missing)


library(ggplot2)
library(mgcv)    # Generalized Additive Models (GAM)
library(MASS)    # Stepwise regression
library(caret)   # Train-test split, cross-validation
library(knitr)   # For mark

house_df <- read.csv("C:\\Users\\Prageeth\\Source\\MSC\\seminar-course\\advanced-regression-kc\\kc_house_data.csv", stringsAsFactors = TRUE)
features <- c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", 
              "condition", "grade", "sqft_above", "sqft_basement", "yr_built")
target <- "price"

simple_lm <- lm ( price ~ sqft_living ,
                  data = house_df )

# Create the plot
ggplot(df, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Regression line
  labs(title = "Price vs. Size Fit",
       x = "Size (sqft_living)",
       y = "Price") +
  theme_minimal()


# R (p. 152)
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade, data = house)
# Coefs: SqFtTotLiving = 228.831, Bedrooms = -47769.955