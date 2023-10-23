
housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

# Fit a first-order regression model with predictor variables
model1 <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data = housing)
# Summary of the regression model
summary(model1)

#Print out the regression model values and equation
beta <-coef(model1)
cat("Prediction Equation: ")
cat("Price = ",
    beta[1],
    " + ",
    beta[2],
    " * sqft_living + ",
    beta[3],
    " * sqft_above + ",
    beta[4],
    " * age + ",
    beta[5],
    " * bathrooms + ",
    beta[6],
    " * view\n"
   ) 

# Obtain residuals and fitted values
residuals1 <- residuals(model1)
fitted1 <- fitted(model1)

# Print the first few rows of residuals and fitted values
cat("1st 6 Model 1 Residual Values: \n")
head(residuals1)
cat("1st 6 Model 1 Fitted Values: \n")
head(fitted1)

# Convert the 'view' variable to numeric
housing$view <- as.numeric(housing$view)

# Create scatterplots and calculate correlation coefficients
correlation_sqft_living <- cor(housing$sqft_living, housing$price)
correlation_age <- cor(housing$age, housing$price)
correlation_sqft_above <- cor(housing$sqft_above, housing$price)
correlation_bathrooms <- cor(housing$bathrooms, housing$price)
correlation_view <- cor(housing$view, housing$price)



# Print correlation coefficients
cat("Sqft Living Correlation: ")
cat(correlation_sqft_living)
cat("\nAge Correlation: ")
cat(correlation_age)
cat("\nSqft Above Correlation: ")
cat(correlation_sqft_above)
cat("\nBathrooms Correlation: ")
cat(correlation_bathrooms)
cat("\nView Correlation: ")
cat(correlation_view)

correlation_sqft_living <- cor(housing$sqft_living, housing$price)
correlation_age <- cor(housing$age, housing$price)
correlation_sqft_above <- cor(housing$sqft_above, housing$price)
correlation_bathrooms <- cor(housing$bathrooms, housing$price)
correlation_view <- cor(housing$view, housing$price)

# Create scatterplots for quantitative variables
plot(housing$sqft_living, 
     housing$price, 
     xlab = "Sqft Living", 
     ylab = "Price", 
     main = "Sqft Living vs. Price"
    )

plot(housing$age, 
     housing$price, 
     xlab = "Age", 
     ylab = "Price", 
     main = "Age vs. Price"
    )

plot(housing$sqft_above, 
     housing$price, 
     xlab = "Sqft Above", 
     ylab = "Price", 
     main = "Sqft Above vs. Price"
    )

plot(housing$bathrooms, 
     housing$price, 
     xlab = "Bathrooms", 
     ylab = "Price", 
     main = "Bathrooms vs. Price"
    )

plot(housing$view, 
     housing$price, 
     xlab = "View", 
     ylab = "Price", 
     main = "View vs. Price"
    )

# Overall F-test
cat("Overall F-test: \n")
anova(model1)


# Fit a first-order regression model with qualitative and quantitative predictor variables
model1 <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data = housing)

# Summary of the regression model
summary(model1)

# Obtain residuals and fitted values
residuals1 <- residuals(model1)
fitted1 <- fitted(model1)

# Print the first few rows of residuals and fitted values
cat("1st 6 Model 1 Residual Values: \n")
head(residuals1)
cat("1st 6 Model 1 Fitted Values: \n")
head(fitted1)





# Create residuals vs. fitted values plot
plot(fitted1, residuals1, 
     xlab = "Fitted Values", 
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values Plot for Model #1"
)
abline(h = 0, col = "red")  # Add a horizontal line at y = 0


# Create normal Q-Q plot
qqnorm(residuals1, 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles",
       main = "Normal Q-Q Plot for Model #1"
)
qqline(residuals1, col = "red")  # Add a reference line


#While the rubric and instructions did not explicitly say that we needed to do a reduced model
#for model 1, I am including it to be sure I meet the rubric requirements.

#Reduced model 1
null_model <- lm(price ~ 1, data = housing)
# Perform an F-test to compare the two models
anova_result <- anova(null_model, model1)

# Print the F-test result
print(anova_result)

# Fit the full Model 1
model1_full <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data = housing)

# Fit the reduced model (null model) for Model 1
model1_reduced <- lm(price ~ 1, data = housing)

# Calculate RSS for both models
rss_model1_full <- sum(model1_full$residuals^2)
rss_model1_reduced <- sum(model1_reduced$residuals^2)

# Calculate R-squared for both models
rsquared_model1_full <- 1 - (rss_model1_full / sum((housing$price - mean(housing$price))^2))
rsquared_model1_reduced <- 1 - (rss_model1_reduced / sum((housing$price - mean(housing$price))^2))

# Create a data frame for plotting
model_names <- c("Full Model 1", "Reduced Model 1")
rsquared_values <- c(rsquared_model1_full, rsquared_model1_reduced)
comparison_data <- data.frame(Model = model_names, R_squared = rsquared_values)

# Create a bar plot
barplot(comparison_data$R_squared, 
        names.arg = comparison_data$Model, 
        main = "Comparison of R-squared Values: Full Model 1 vs. Reduced Model 1",
        xlab = "Model",
        ylab = "R-squared Value",
        col = c("blue", "red"),
        ylim = c(0, 1)  # Set the y-axis range from 0 to 1
)

# Add text labels above the bars
text(x = 1:2, 
     y = comparison_data$R_squared, 
     labels = round(comparison_data$R_squared, 2), 
     pos = 3, 
     cex = 0.8)


# Predicting the price for the first home
new_home1 <- data.frame(sqft_living = 2150,
                        sqft_above = 1050,
                        age = 15,
                        bathrooms = 3,
                        view = 0)   #Road view

# Predict the price
predicted_price1 <- predict(model1_full, 
                            newdata = new_home1, 
                            interval = "prediction", 
                            level = 0.90)

# Display the prediction and prediction interval
cat("Predicted Price for Home 1: $", 
    predicted_price1[1], 
    "\n")
cat("90% Prediction Interval for Home 1: [$", 
    predicted_price1[2], 
    " - $", 
    predicted_price1[3], 
    "]\n")

# Predicting the price for the second home
new_home2 <- data.frame(sqft_living = 4250,
                        sqft_above = 2100,
                        age = 5,
                        bathrooms = 5,
                        view = 1)  #waterfront view
# Predict the price
predicted_price2 <- predict(model1_full, 
                            newdata = new_home2, 
                            interval = "prediction", 
                            level = 0.90)

# Display the prediction and prediction interval
cat("Predicted Price for Home 2: $", 
    predicted_price2[1], 
    "\n")
cat("90% Prediction Interval for Home 2: [$", 
    predicted_price2[2], 
    " - $", 
    predicted_price2[3], 
    "]\n")


#Full Model 1 is the best fit between the two compared above, due it accounting for approximately 60%
# of the variability in the housing prices can be accounted for by the combination of predictor 
#variables (sqft_living, sqft_above, age, bathrooms, and view) included in this model.
#While Reduced model 1 only includes an intercept term (no predictor variables), 
#so it cannot explain any of the variability in housing prices based on the given data.



# Fit a complete second-order regression model with school_rating and crime as predictor variables
model2 <- lm(price ~ school_rating + crime + I(school_rating^2) + I(crime^2) + school_rating:crime, data = housing)

# Summary of the regression model
summary(model2)


# Obtain residuals and fitted values
residuals2 <- residuals(model2)
fitted2 <- fitted(model2)

# Print the first few rows of residuals and fitted values
cat("1st 6 Model 2 Residual Values: \n")
head(residuals2)
cat("1st 6 Model 2 Fitted Values: \n")
head(fitted2)

# Create scatterplots and calculate correlation coefficients
correlation_crime <- cor(housing$crime, housing$price)
correlation_sr <- cor(housing$school_rating, housing$price)



# Print correlation coefficients
cat("Crime Correlation: ")
cat(correlation_crime)
cat("\nSchool Rating Correlation: ")
cat(correlation_sr)


#Prediction
school_rating_1 <- 9.80
crime_1 <- 81.02

# Calculate the predicted price
predicted_price_1 <- 733910.73 - 73748.17 * school_rating_1 - 3154.77 * crime_1 +
                      11646.80 * (school_rating_1^2) + 6.38 * (crime_1^2) - 
                      52.27 * school_rating_1 * crime_1

# Print the predicted price
cat("Predicted Price 1: ", predicted_price_1)


# Calculate 90% prediction interval
pred_interval_1 <- predict(model2, newdata = data.frame(school_rating = school_rating_1, crime = crime_1), interval = "prediction", level = 0.90)

# Calculate 90% confidence interval
conf_interval_1 <- predict(model2, newdata = data.frame(school_rating = school_rating_1, crime = crime_1), interval = "confidence", level = 0.90)

# Print the prediction and confidence intervals
pred_interval_1
cat("\nPrediction Interval 1: ", pred_interval_1)
conf_interval_1
cat("\nConfidence Interval 1: ", conf_interval_1)



# Define the values for the second scenario
school_rating_2 <- 4.28
crime_2 <- 215.50

# Calculate the predicted price for the second scenario
predicted_price_2 <- 733910.73 - 73748.17 * school_rating_2 - 3154.77 * crime_2 +
                      11646.80 * (school_rating_2^2) + 6.38 * (crime_2^2) - 
                      52.27 * school_rating_2 * crime_2

# Print the predicted price for the second scenario
cat("Predicted Price 2: ", predicted_price_2)

# Calculate the 90% prediction interval for the second scenario
pred_interval_2 <- predict(model2, newdata = data.frame(school_rating = school_rating_2, crime = crime_2), interval = "prediction", level = 0.90)

# Calculate the 90% confidence interval for the second scenario
conf_interval_2 <- predict(model2, newdata = data.frame(school_rating = school_rating_2, crime = crime_2), interval = "confidence", level = 0.90)

# Print the prediction interval for the second scenario
pred_interval_2
cat("\nPrediction Interval 2: ", pred_interval_2)
conf_interval_2
# Print the confidence interval for the second scenario
cat("\nConfidence Interval 2: ",conf_interval_2)


# Create the complete second-order model
model_second_order <- lm(price ~ school_rating + crime + I(school_rating^2) + I(crime^2) + school_rating:crime, data = housing)

# Obtain the summary of the model
summary(model_second_order)


#Now fit the second order model
coefs <- coef(model_second_order)

print(coefs)

#define coefficients
b0 <- coefs[1]
b1 <- coefs[2]
b2 <- coefs[3]
ba <- coefs[4]
bb <- coefs[5]
bc <- coefs[6]

#Write prediction model equation
pred_equation <- paste(
    "Price = ",
    b0,
    " + ",
    b1,
    " * school_rating + ",
    b2,
    " * crime + ",
    ba,
    " * (school_rating^2) + ",
    bb,
    " * (crime^2) + ",
    bc,
    " * school_rating * crime + epsilon"
)

#Print the prediction equation
cat("\nPrediction Model Equation: \n", pred_equation, "\n")

# Obtain residuals and fitted values
residuals_second_order <- residuals(model_second_order)
fitted_values_second_order <- fitted(model_second_order)

# Create a scatterplot of residuals vs. fitted values
plot(fitted_values_second_order, residuals_second_order, xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")

# Create a Normal Q-Q plot
qqnorm(residuals_second_order, main = "Normal Q-Q Plot")
qqline(residuals_second_order)


#Create diagnostic plots to assess model assumptions

#residuals vs fitted  (checking for linearity and homoscedasticity)
plot(model_second_order, which = 1)

#Normal Q-Q plot (check normality of residuals)
plot(model_second_order, which = 2)

#scale-location plot (check for homoscedasticity)
plot(model_second_order, which = 3)

#residuals vs leverage plot (check for influential points)
plot(model_second_order, which = 5)

#scatterplot of school_rating vs price
plot(housing$school_rating, 
     housing$price,
     xlab = "School Rating",
     ylab = "Price",
     main = "School Rating vs Price")

#Create scatterplot for crime vs price
plot(housing$crime,
     housing$price,
     xlab = "Crime",
     ylab = "Price",
     main = "Crime vs Price")



#Overall F-test
null_model2 <- lm(price~ 1, data = housing)
mod2Ftest <- anova(null_model2, model2)

#Print results
print(mod2Ftest)

#create reduced model for model 2
r_model2 <- lm(price~ school_rating + crime, data = housing)

#Compare model 2 to reduced model 2
anova(model_second_order, r_model2)

#perform the F-test to compare full second order model2 vs redudeced model2
a_result <- anova(r_model2, model_second_order)

#Print the a_result
print(a_result)

#First order full model 2
full_model <- lm(price ~ school_rating + crime + school_rating:crime, data = housing)
#reduced model 2
reduced_model <- lm(price ~ school_rating + crime, data = housing)
#Compare the two models
anova_result <- anova(reduced_model, full_model)

#print the results
print(anova_result)


# Perform ANOVA to compare models
anova_result <- anova(model1, model_second_order)

# Print the ANOVA table
print(anova_result)

f_test <- anova(model1, model_second_order)

#Extract p-value from f-test
p_val <- f_test$`Pr(>F)`[2]

#print p-value
print(p_val)


# Fit a first-order regression model with interaction
model_first_order <- lm(price ~ school_rating * crime, data = housing)

# Summary of the regression model
summary(model_first_order)

# Get the coefficients
coefs <- coef(model_first_order)

# Define coefficients
b0 <- coefs[1]
b1 <- coefs[2]
b2 <- coefs[3]
b3 <- coefs[4]

# Write prediction model equation
pred_equation <- paste(
  "Price = ",
  b0,
  " + ",
  b1,
  " * school_rating + ",
  b2,
  " * crime + ",
  b3,
  " * (school_rating * crime) + epsilon"
)

# Print the prediction equation
cat("\nPrediction Model Equation (First Order with Interaction): \n", pred_equation, "\n")


# Create a residuals plot
par(mfrow=c(1,2)) # Set up a 1x2 grid for two plots side by side
plot(model1$residuals, 
     main="Residuals for Model 1", 
     ylab="Residuals", 
     xlab="Observation Number")
plot(model_second_order$residuals, 
     main="Residuals for Model 2", 
     ylab="Residuals", 
     xlab="Observation Number")

