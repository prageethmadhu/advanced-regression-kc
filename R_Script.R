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
ggplot(house_df, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Regression line
  labs(title = "Price vs. Size Fit",
       x = "Size (sqft_living)",
       y = "Price") +
  theme_minimal()

lm ( price ~ sqft_living ,
     data = house_df )

lm(price ~ sqft_living + sqft_lot + bathrooms +
                 bathrooms + grade, data = house_df,na.action = na.omit)

# Define Multiple Linear Regression Model
house_lm <- lm(price ~ sqft_living + sqft_lot + bathrooms + grade, 
               data = house_df, na.action = na.omit)

# Model Summary (Extracts Coefficients, p-values, R²)
summary(house_lm)

# Extract RMSE
predictions <- predict(house_lm, newdata = house_df)
residuals <- house_df$price - predictions
RMSE <- sqrt(mean(residuals^2))

# Extract R-squared
R_squared <- summary(house_lm)$r.squared

# Extract P-values
p_values <- summary(house_lm)$coefficients[, 4]

# Print Results
cat("RMSE:", RMSE, "\n")
cat("R²:", R_squared, "\n")
cat("P-values:\n")
print(p_values)


##########################

prop_type_dummies <- model.matrix(~ PropertyType - 1, data = house_df)
# Output: 1 for each type present


names(house_df)


house_df["grade"]


##########

house_lm_2 <- lm(price ~ sqft_living + sqft_lot15 + bathrooms +
                 bedrooms + grade, data=house_df, na.action=na.omit)
house_lm_2


house_poly <- lm(price ~ poly(sqft_living, 2) + sqft_lot15 + bathrooms +
                   bedrooms + grade, data=house_df)
house_poly

#######

library(MASS)
house_full <- lm(price ~ sqft_living + sqft_lot15 + bathrooms +
                   bedrooms + grade , data = house_df)
step <- stepAIC(house_full, direction = "both")

###############last part

house_98105 <- house_df[house_df$zipcode == 98105,]
lm_98105 <- lm(price ~ sqft_living + sqft_lot + bathrooms +
                 bedrooms + grade, data=house_98105)

house_98105 ## 182 rows

std_resid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105)
hat_values <- hatvalues(lm_98105)

plot(subset(hat_values, cooks_D > 0.08), subset(std_resid, cooks_D > 0.08),
     xlab='hat_values', ylab='std_resid',
     cex=10*sqrt(subset(cooks_D, cooks_D > 0.08)), pch=16, col='lightgrey')

points(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)


df <- data.frame(resid = residuals(lm_98105), pred = predict(lm_98105))
ggplot(df, aes(pred, abs(resid))) + geom_point() + geom_smooth()


hist(std_resid, breaks=50, col="lightblue", main="Histogram of Standardized Residuals")


library(lmtest)
dwtest(lm_98105)

ggplot(df, aes(pred, abs(resid))) + geom_point() + geom_smooth()