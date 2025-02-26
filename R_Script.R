packages <- c("ggplot2", "mgcv", "MASS", "caret", "knitr","visreg")
install_if_missing <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(install_if_missing)) install.packages(install_if_missing)


library(ggplot2)
library(mgcv)    # Generalized Additive Models (GAM)
library(MASS)    # Stepwise regression
library(caret)   # Train-test split, cross-validation
library(knitr)   # For mark
library(visreg)

house_df <- read.csv("C:\\Users\\Prageeth\\Source\\MSC\\seminar-course\\advanced-regression-kc\\kc_house_data.csv", stringsAsFactors = TRUE)
features <- c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", 
              "condition", "grade", "sqft_above", "sqft_basement", "yr_built")
target <- "price"

simple_lm <- lm ( price ~ sqft_living ,
                  data = house_df )
simple_lm
# Create the plot
ggplot(house_df, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Regression line
  labs(title = "Price vs. Size Fit",
       x = "Size (sqft_living)",
       y = "Price") +
  theme_minimal()




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

#prop_type_dummies <- model.matrix(~ PropertyType - 1, data = house_df)
# Output: 1 for each type present


names(house_df)


max(house_df["condition"])

##### FACTOR# Load necessary library
library(dplyr)
library(car)  # For checking multicollinearity

# MY dataset doesn't have PropertyType column so I create factor varable (one hot encoding with sample)
set.seed(123)  # For reproducibility
house <- data.frame(
  HouseID = 1:12,
  SqFtTotLiving = c(1500, 1800, 1600, 1700, 1900, 1750, 2000, 1400, 2100, 2200, 1850, 1950),
  SqFtLot = c(5000, 6000, 5500, 5200, 5800, 5700, 6200, 4900, 7000, 6800, 6400, 6600),
  Bathrooms = c(2, 3, 2, 2, 4, 3, 3, 2, 4, 3, 3, 2),
  Bedrooms = c(3, 4, 3, 3, 5, 4, 4, 3, 5, 4, 4, 3),  # Ensured no perfect correlation
  BldgGrade = c(7, 8, 7, 6, 9, 8, 7, 6, 10, 9, 8, 7),
  PropertyType = factor(c("Multiplex", "Single Family", "Single Family", "Single Family",
                          "Single Family", "Townhouse", "Multiplex", "Townhouse",
                          "Single Family", "Multiplex", "Townhouse", "Single Family")),
  AdjSalePrice = c(300000, 320000, 310000, 330000, 350000, 340000, 355000, 290000,
                   370000, 360000, 345000, 325000)
)
# refere code for this dataset
# Ensure PropertyType has a valid reference level
house$PropertyType <- relevel(house$PropertyType, ref = "Multiplex")

# Fit the regression model with the refined dataset
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + BldgGrade + PropertyType, data = house)

# Display model summary
summary(house_lm)

##########

house_lm_2 <- lm(price ~ sqft_living + sqft_living + bathrooms +
                 bedrooms + grade, data=house_df, na.action=na.omit)

# Load data (replace 'house_98105' with your dataset)
model_poly <- lm(price ~ poly(sqft_living, 2) + sqft_lot +
                    grade + bathrooms + bedrooms, data=house_df)

# Generate partial residual plot
visreg(model_poly, "sqft_living", gg=TRUE) +
  geom_point(color = "black", shape = 1) + 
  geom_smooth(method="loess", color="blue", size=1.2, linetype="dashed") + 
  theme_minimal() +
  labs(title="Partial Residual Plot", x="SqFtTotLiving", y="Partial Residuals")


# Generate partial residual plot without polynomial
visreg(house_lm_2, "sqft_living", gg=TRUE) +
  geom_point(color = "black", shape = 1) + 
  geom_smooth(method="loess", color="blue", size=1.2, linetype="dashed") + 
  theme_minimal() +
  labs(title="Partial Residual Plot", x="SqFtTotLiving", y="Partial Residuals")


library(splines)

# Fit spline model (Cubic B-Spline)
model_spline <- lm(price ~ bs(sqft_living, degree=3, df=6) + sqft_lot +
                     grade + bathrooms + bedrooms, data=house_df)

# Generate partial residual plot for spline regression
visreg(model_spline, "sqft_living", gg=TRUE) +
  geom_point(color = "black", shape = 1) + 
  geom_smooth(method="loess", color="blue", size=1.2, linetype="dashed") + 
  theme_minimal() +
  labs(title="Partial Residual Plot with Splines", x="SqFtTotLiving", y="Partial Residuals")



#######


library(MASS)
house_full <- lm(price ~ sqft_living + sqft_lot15 + bathrooms +
                   bedrooms + grade+yr_renovated , data = house_df)
step <- stepAIC(house_full, direction = "both")

summary(step)

###### Weights regression
library(lubridate)

min(house_df$Year)

# Convert 'date' column to Date format
house_df$Year <- year(ymd(substr(house_df$date, 1, 8)))  # Extracts YYYYMMDD and converts to year

# Compute weight (Years since 2005)
house_df$Weight <- house_df$Year - 2005

# Fit unweighted linear regression
house_lm <- lm(price ~ sqft_living + sqft_lot + bathrooms +
                 bedrooms + grade, data=house_df)

# Fit weighted linear regression
house_wt <- lm(price ~ sqft_living + sqft_lot + bathrooms +
                 bedrooms + grade, data=house_df, weight=Weight)

# Compare coefficients of both models
round(cbind(house_lm=house_lm$coefficients,
            house_wt=house_wt$coefficients), digits=3)

###############last part

house_98105 <- house_df[house_df$zipcode == 98105,]
lm_98105 <- lm(price ~ sqft_living + sqft_lot + bathrooms +
                 bedrooms + grade, data=house_98105)

#house_98105 ## 182 rows

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