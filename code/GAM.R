# Generalised Additive Model - Regression on time
getwd()

df_clean <- read.csv("../data/cleaned_data.csv")

#----------------------------------------------------------------------
library(mgcv)
# 1st attempting of GAM to check p values of each variables 
gam.model.test1 <- gam(delivery_time_min ~ courier_age_years + temperature_celsius + humidity_percent + 
      precipitation_mm + distance_km + traffic_level_factor + 
        vehicle_type_factor + weather_category,
          data = df_clean)
summary(gam.model.test1)


# 2nd attempting of GAM: selected only, with smoother s() function
gam.model.test2 <- gam(delivery_time_min ~ s(temperature_celsius) + s(humidity_percent) + 
                           s(distance_km) + traffic_level_factor + 
                         weather_category,
                       data = df_clean)
summary(gam.model.test2) 

# 3rd attempting of GAM, fine turning to reduce the edf(Effective Degrees of Freedom). 
# edf=9 is much too overfitted, using bs = "cs" (cyclic spline with shrinkage)
# k sets the maximum allowable complexity for each smooth term.:
gam.model.test3 <- gam(delivery_time_min ~ s(temperature_celsius, bs = "cs", k=5) + 
                         s(humidity_percent, bs = "cs", k=5) + 
                         s(distance_km, bs = "cs", k=5) + 
                         traffic_level_factor + 
                         weather_category,
                       data = df_clean) 
summary(gam.model.test3) 
#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
plot(gam.model.test3, pages = 1, se = TRUE, rug = TRUE)

# 4th attempting using I() function without smooth s(), 
# with full second-order interaction analysis using the ^2 formula expansion
gam.two.fold <- gam(delivery_time_min ~ (temperature_celsius + humidity_percent + 
                        distance_km + traffic_level_factor + 
                        weather_category)^2,
                      data = df_clean)
summary(gam.two.fold)
#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
# no this model doesn't content a smooth


# 5th attempting
# Check and convert to factor
df_clean$traffic_level_factor <- as.factor(df_clean$traffic_level_factor)


gam.model.test5 <- gam(delivery_time_min ~ s(temperature_celsius, by = traffic_level_factor) +  
    s(humidity_percent) + s(distance_km) + traffic_level_factor + weather_category, 
    data = df_clean)
summary(gam.model.test5)
#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
plot(gam.model.test5, pages = 1, se = TRUE, rug = TRUE)

# 6th fitting, include recommended interations to the GAM with shrinkage of cyclic spline
gam.model.test6 <- gam(delivery_time_min ~       s(temperature_celsius, bs = "cs", k=5) + 
                                                 s(humidity_percent, bs = "cs", k=5) + 
                                                 s(distance_km, bs = "cs", k=5) + 
                                                 traffic_level_factor + 
                                                 weather_category +
                                                 temperature_celsius:humidity_percent +
                                                 distance_km:traffic_level_factor,
                                                 data = df_clean)
summary(gam.model.test6)
#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
plot(gam.model.test6, pages = 1, se = TRUE, rug = TRUE)

# 7th fitting without shrinkage of cyclic spline
gam.model.test7 <- gam(delivery_time_min ~       s(temperature_celsius) + 
                         s(humidity_percent) + 
                         s(distance_km) + 
                         traffic_level_factor + 
                         weather_category +
                         temperature_celsius:humidity_percent +
                         distance_km:traffic_level_factor,
                       data = df_clean)
summary(gam.model.test7)
#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
plot(gam.model.test7, pages = 1, se = TRUE, rug = TRUE)

# 8th continue after 6th, remove insignificant variables
gam.model.test8 <- gam(delivery_time_min ~       s(temperature_celsius, bs = "cs", k=5) + 
                         s(humidity_percent, bs = "cs", k=5) + 
                         s(distance_km, bs = "cs", k=5) + 
                         traffic_level_factor + 
                         weather_category +
                         temperature_celsius:humidity_percent,
                       data = df_clean)
summary(gam.model.test8)
#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
plot(gam.model.test8, pages = 1, se = TRUE, rug = TRUE, main="Smooth Terms of GAM Model Test 8")

#----------------------------------------------------------------------------


#comparision between two models
anova(gam.model.test3,gam.model.test6)
anova(gam.model.test3,gam.model.test8)
anova(gam.model.test6,gam.model.test8)

# Extract and print parametric coefficients
coef(gam.model.test6)
# Extract parametric coefficients
coefs <- coef(gam.model.test6)

# Print formula-like expression (only for parametric terms)
cat("delivery_time_min = ", round(coefs[1], 3), " + ")

# Add other parametric terms (e.g., traffic_level_factor)
for (i in 2:length(coefs)) {
  cat(round(coefs[i], 3), "*", names(coefs)[i], " + ")
}
cat("smooth functions of temperature_celsius, humidity_percent, and distance_km\n")


#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
plot(gam.model.test6, pages = 1, se = TRUE, rug = TRUE)

# Check for  Cross-Validation -- gam.model.test6

library(mgcv)
library(caret)

set.seed(123)
train_index <- createDataPartition(df_clean$delivery_time_min, p = 0.8, list = FALSE)
train_data <- df_clean[train_index, ]
test_data <- df_clean[-train_index, ]

# Fit GAM on training set using gam.model.test6
gam.model.train <- gam( delivery_time_min ~
  s(temperature_celsius, bs = "cs", k=5) + 
    s(humidity_percent, bs = "cs", k=5) + 
    s(distance_km, bs = "cs", k=5) + 
    traffic_level_factor + 
    weather_category +
    temperature_celsius:humidity_percent +
    distance_km:traffic_level_factor,
  data = train_data,
  method = "REML"
)

# Predict on test set
pred <- predict(gam.model.train, newdata = test_data)

# Evaluate performance
RMSE <- sqrt(mean((pred - test_data$delivery_time_min)^2))
MAE <- mean(abs(pred - test_data$delivery_time_min))

RMSE
MAE

# Check for  Cross-Validation -- gam.model.test8

library(mgcv)
library(caret)

set.seed(100)
train_index <- createDataPartition(df_clean$delivery_time_min, p = 0.8, list = FALSE)
train_data <- df_clean[train_index, ]
test_data <- df_clean[-train_index, ]

# Fit GAM on training set using gam.model.test8
gam.model.traintest8 <- gam( delivery_time_min ~
                          s(temperature_celsius, bs = "cs", k=5) + 
                          s(humidity_percent, bs = "cs", k=5) + 
                          s(distance_km, bs = "cs", k=5) + 
                          traffic_level_factor + 
                          weather_category +
                          temperature_celsius:humidity_percent,
                        data = train_data,
                        method = "REML"
)

# Predict on test set
pred <- predict(gam.model.traintest8, newdata = test_data)

# Evaluate performance
RMSE <- sqrt(mean((pred - test_data$delivery_time_min)^2))
MAE <- mean(abs(pred - test_data$delivery_time_min))

RMSE
MAE

#-----------------------------------------------------------------
# comparison between a linear model
linear.model <- gam(
  delivery_time_min ~ temperature_celsius + humidity_percent + distance_km + 
    traffic_level_factor + weather_category,
  data = df_clean,
  method = "REML"
)

# Compare summary statistics
summary(gam.model.test8)      # Your existing smooth GAM
summary(linear.model)         # New linear-only model

# Compare using AIC
# Lower AIC = better fit with a penalty for complexity.
AIC(gam.model.test8, linear.model)

# Visualize residuals

par(mfrow = c(1, 2))
plot(gam.model.test6$residuals, main = "GAM Residuals")
plot(linear.model$residuals, main = "Linear Model Residuals")

# Plot predicted vs. actual delivery_time_min
# Predicted values from both models
df_clean$pred_gam <- predict(gam.model.test6, type = "response")
df_clean$pred_lm  <- predict(linear.model, type = "response")

# Plot predicted vs. actual (base R version)
par(mfrow = c(1, 2))

# GAM
plot(df_clean$delivery_time_min, df_clean$pred_gam,
     main = "GAM: Predicted vs Actual",
     xlab = "Actual delivery_time_min", ylab = "Predicted delivery_time_min",
     pch = 19, col = rgb(0, 0, 1, 0.3))
abline(0, 1, col = "red", lwd = 2)

# Linear Model
plot(df_clean$delivery_time_min, df_clean$pred_lm,
     main = "Linear Model: Predicted vs Actual",
     xlab = "Actual delivery_time_min", ylab = "Predicted delivery_time_min",
     pch = 19, col = rgb(0, 0.6, 0, 0.3))
abline(0, 1, col = "red", lwd = 2)

# Visualize residuals -- 2

par(mfrow = c(1, 2))
plot(gam.model.test3$residuals, main = "GAM Residuals")
plot(linear.model$residuals, main = "Linear Model Residuals")

# Plot predicted vs. actual delivery_time_min
# Predicted values from both models
df_clean$pred_gam <- predict(gam.model.test3, type = "response")
df_clean$pred_lm  <- predict(linear.model, type = "response")

# Plot predicted vs. actual (base R version)
par(mfrow = c(1, 2))

# GAM
plot(df_clean$delivery_time_min, df_clean$pred_gam,
     main = "GAM: Predicted vs Actual",
     xlab = "Actual delivery_time_min", ylab = "Predicted delivery_time_min",
     pch = 19, col = rgb(0, 0, 1, 0.3))
abline(0, 1, col = "red", lwd = 2)

# Linear Model
plot(df_clean$delivery_time_min, df_clean$pred_lm,
     main = "Linear Model: Predicted vs Actual",
     xlab = "Actual delivery_time_min", ylab = "Predicted delivery_time_min",
     pch = 19, col = rgb(0, 0.6, 0, 0.3))
abline(0, 1, col = "red", lwd = 2)

#--------------------------------------------------------------
# Adding log transformed delivery time to d.food_time
library(tidyverse)
df_clean <- df_clean %>%
  mutate(log_delivery_time = log(delivery_time_min))

# 1st attempting of GAM to check p values of each variables 
gam.model.logtest1 <- gam(log_delivery_time ~ courier_age_years + temperature_celsius + humidity_percent + 
                         precipitation_mm + distance_km + traffic_level_factor + 
                         vehicle_type_factor + weather_category,
                       data = df_clean)
summary(gam.model.logtest1)

# 3rd attempting of GAM, fine turning to reduce the edf(Effective Degrees of Freedom). 
# edf=9 is much too overfitted, using bs = "cs" (cyclic spline with shrinkage)
# k sets the maximum allowable complexity for each smooth term.:
gam.model.logtest3 <- gam(log_delivery_time ~ s(temperature_celsius, bs = "cs", k=5) + 
                         s(humidity_percent, bs = "cs", k=5) + 
                         s(distance_km, bs = "cs", k=5) + 
                         traffic_level_factor + 
                         weather_category,
                       data = df_clean) 
summary(gam.model.logtest3) 
#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
plot(gam.model.logtest3, pages = 1, se = TRUE, rug = TRUE)

# 6th fitting, include recommended interations to the GAM with shrinkage of cyclic spline
gam.model.logtest6 <- gam(log_delivery_time ~       s(temperature_celsius, bs = "cs", k=5) + 
                         s(humidity_percent, bs = "cs", k=5) + 
                         s(distance_km, bs = "cs", k=5) + 
                         traffic_level_factor + 
                         weather_category +
                         temperature_celsius:humidity_percent +
                         distance_km:traffic_level_factor,
                       data = df_clean)
summary(gam.model.logtest6)

# 8th continue after 6th, remove insignificant variables
gam.model.logtest8 <- gam(log_delivery_time ~       s(temperature_celsius, bs = "cs", k=5) + 
                         s(humidity_percent, bs = "cs", k=5) + 
                         s(distance_km, bs = "cs", k=5) + 
                         traffic_level_factor + 
                         weather_category +
                         temperature_celsius:humidity_percent,
                       data = df_clean)
summary(gam.model.test8)
#  visualize the smooth terms, Plot all smooth terms with confidence intervals 
plot(gam.model.test8, pages = 1, se = TRUE, rug = TRUE, main="Smooth Terms of GAM Model Test 8")

# model comparing test8 and logtest8

AIC(gam.model.test8, gam.model.logtest8)

# Check for  Cross-Validation -- gam.model.logtest8

library(mgcv)
library(caret)

set.seed(101)
train_index <- createDataPartition(df_clean$log_delivery_time, p = 0.8, list = FALSE)
train_data <- df_clean[train_index, ]
test_data <- df_clean[-train_index, ]

# Fit GAM on training set using gam.model.test8
gam.model.trainlogtest8 <- gam(log_delivery_time ~       s(temperature_celsius, bs = "cs", k=5) + 
                                 s(humidity_percent, bs = "cs", k=5) + 
                                 s(distance_km, bs = "cs", k=5) + 
                                 traffic_level_factor + 
                                 weather_category +
                                 temperature_celsius:humidity_percent,
                               data = df_clean,
                             method = "REML"
)

pred <- predict(gam.model.trainlogtest8, newdata = test_data)
pred_back <- exp(pred)

RMSE <- sqrt(mean((pred_back - test_data$delivery_time_min)^2))
MAE <- mean(abs(pred_back - test_data$delivery_time_min))

RMSE
MAE

# comparison between a linear model
linear.model <- gam(
  delivery_time_min ~ temperature_celsius + humidity_percent + distance_km + 
    traffic_level_factor + weather_category,
  data = df_clean,
  method = "REML"
)
