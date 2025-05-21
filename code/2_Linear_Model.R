##************** Group work ML1 - Linear regression*************

"""
Author: Curdin Caderas
Start: 10.05.2025
"""

# Libraries
install.packages("GGally")
library(ggplot2)
library(dplyr)
library(caret)
library(patchwork)
library(GGally)
library(broom)
library(knitr)

"""
PLACEHOLDER
"""

####************** PREPARATORY TASK 1: Loading Dataset and checking the dataset ####

# Loading cleaned data

d.food_time <- read.csv("../data/cleaned_data.csv", header = TRUE)

# Checking data (structure / head / summary / NA)

str(d.food_time)
head(d.food_time)
summary(d.food_time)
colSums(is.na(d.food_time))
colnames(d.food_time)

View(d.food_time)

####************** PREPARATORY TASK 2: Investigate the distributions of target variable and predictors before starting build the model ####

##Checking distribution of target variable
hist(d.food_time$delivery_time_min, main = "Distribution of delivery time [min]")
boxplot(d.food_time$delivery_time_min, main = "Boxplot of delivery time [min]")
summary(d.food_time$delivery_time_min)

par(mfrow = c(2,1))

qqnorm(d.food_time$delivery_time_min)
qqline(d.food_time$delivery_time_min, col = "red")

"""
Comments on distribution of target variable:
- From looking at the histogram, we see that the distribution is right-skewed
- This is furhtermore confirmed by the boxplot which shows outliers
- Furthermore from looking at summary of delivery_time_min we see that Mean is > Median
- And Max Value is 4x the size of median

Due to this, we will log transform our target variable.

"""

#Log transforming target variable and investigating it's distribution
hist(log(d.food_time$delivery_time_min), main = "Log-transformed distribution of delivery time")
boxplot(log(d.food_time$delivery_time_min), main = "Boxplot of log-transformed delivery time [min]")
summary(log(d.food_time$delivery_time_min))

qqnorm(d.food_time$log_delivery_time)
qqline(d.food_time$log_delivery_time, col = "red")

"""
After log transforming the distribution is more symmetrical. we therefore add a column to our df with the log
transformed delivery_time_min.

Remark: Mean and median now closer to each other
"""

#Adding log transformed delivery time to d.food_time

d.food_time <- d.food_time %>%
  mutate(log_delivery_time = log(delivery_time_min))



##Checking distributions of numeric predictors

numeric_vars <- d.food_time %>%
  select(distance_km, courier_age_years, courier_rating_1_to_5,
         temperature_celsius, humidity_percent, precipitation_mm, average_speed_kmph)

par(mfrow = c(3, 3))  
for (var in names(numeric_vars)) {
  hist(numeric_vars[[var]], main = var, xlab = var, col = "lightgray", breaks = 30)
}

"""
Important remark:
- We must not use average_speed_kmph as it has been caluclated from the delivery time (which is our target variable)
- Also we will disregard precipitation because there seems to be missing data. At least the low amount of percipitation seems questionable.
- Percipitation would not add additional information (low variance) to the model
"""


####************** PREPARATORY TASK 3: Investigate numeric predictors (Scatterplots / cor) ####

"""
In a next step we will visually inspect the relationship between our target variable and numeric predictors.
Numeric predictors: distance_km, courier_age_years, courier_rating_1_to_5, temperature_celsius, humidity_percent
"""


par(mfrow = c(3, 3))


# Variable for target variable
target <- "log_delivery_time"

# Predictors, just numeric.
numeric_predictors <- c("distance_km", "courier_age_years", "courier_rating_1_to_5",
                        "temperature_celsius", "humidity_percent")


# Loop to plot all the scatterplot of numeric variables
for (var in numeric_predictors) {
  plot(d.food_time[[var]], d.food_time[[target]],
       main = paste(var, "vs", target),
       xlab = var, ylab = target,
       pch = 20, col = rgb(0, 0, 0, 0.3))
  
  abline(lm(d.food_time[[target]] ~ d.food_time[[var]]),
         col = "blue", lwd = 2)
  
}

"""
From just looking at the plots and the regression line we can see that between:

- log_delivery_time and distance_km there seems to be a strong positive relationship
- log_delivery_time and courier_rating_1_to_5 there seems to be a rather weak negative relationship
- log_delivery_time and temperature_celsius there seems to be a weak positive relationship

For humidity_percent and courier_age there seems to be little to no relationship.

After visually inspecting a possible relationship we will calculate the correlation between those relationships

****NINA: COR ZU PLOT HINZUFÜGEN****

"""

cor(d.food_time[, c(numeric_predictors, target)]) 

"""
The correlation matrix supports our observation from above.

- distance_km has the highest positive correlation with 0.8432
- courier_rating_1_to_5 has a weak negative correlation with -0.099 --> small correlation but it seems plausible (higher ratings, lower delivery times)
"""

####************** PREPARATORY TASK 4: Investigate categorial predictors (Boxplots) ####

"""
After investigating the numeric variables, we will move on to categorical variables and investigate them by using boxplots.

We will investigate the following categorical variables:
- order_type_factor
- vehicle_type_factor
- traffic_level_factor
- weather_category


"""
# Releveling factor traffic_level & set to meaningful sequence (increasing)
d.food_time$traffic_level_factor <- factor(
  d.food_time$traffic_level_factor,
  levels = c("very low", "low", "moderate", "high", "very high")
)

par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))  

# Boxplot for order type
boxplot(log_delivery_time ~ order_type_factor, data = d.food_time,
        main = "Log Delivery Time by Order Type",
        xlab = "Order Type", ylab = "log(Delivery Time)",
        col = "lightblue", border = "darkblue", cex.axis = 0.8)

# Boxplot for vehicle type
boxplot(log_delivery_time ~ vehicle_type_factor, data = d.food_time,
        main = "Log Delivery Time by Vehicle Type",
        xlab = "Vehicle Type", ylab = "log(Delivery Time)",
        col = "lightgreen", border = "darkblue", cex.axis = 0.8)

# boxplot for traffic level
boxplot(log_delivery_time ~ traffic_level_factor, data = d.food_time,
        main = "Log Delivery Time by Traffic Level",
        xlab = "Traffic Level", ylab = "log(Delivery Time)",
        col = "orange", border = "darkblue", cex.axis = 0.8)

# boxplot for weather category
boxplot(log_delivery_time ~ weather_category, data = d.food_time,
        main = "Log Delivery Time by Weather",
        xlab = "Weather Category", ylab = "log(Delivery Time)",
        col = "lightgray", border = "darkblue", cex.axis = 0.8)

# Checking relation between distance_km and vehicle_type_factor in a boxplot
boxplot(distance_km ~ vehicle_type_factor, data = d.food_time)

selected_cols <- c("courier_age_years",
                   "courier_rating_1_to_5",
                   "temperature_celsius",
                   "humidity_percent",
                   "precipitation_mm",
                   "distance_km",
                   "delivery_time_min",
                   "long_delivery_flag",
                   "average_speed_kmph",
                   "log_delivery_time"
                   )

pairs(d.food_time[, selected_cols])


"""
From looking at the boxplots we can clearly see that traffic level seems to play an 
important role on the delivery time. <very high> has the longest delivery time and <very low>
the shortest delivery time.

The data we have points towards a high impact from traffic level to delivery time which seems plausible.

the other factors we' ve looked at do not seem to have a big impact on delivery time.
When looking at <vehicle type> we can see that bicycle seems to have a small tendency towards 
shorter delivery times. But this could be influenced by e.g. shorter distances.
By creating a boxplot with distance ~ vehicle type we see that this is likely to be the case.

****NINA: TRAFFIC LEVEL BEREITS HIER SORTIEREN!****
"""


"""
Next to looking at boxplots will will further visually inspect two categories

a) traffic level
b) order type

We will start with scatterplots using different colours for the different levels
I.e. will plot distance vs log_delivery_time with the above mentioned categories for which 
we use different colours.
"""

# Using ggplot to create a scatterplot (delivery time ~ distance) with traffic level in different colors
ggplot(d.food_time, aes(x = distance_km, y = log_delivery_time, color = traffic_level_factor)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Distance vs Log Delivery Time by Traffic Level",
    x = "Distance (km)",
    y = "Log Delivery Time",
    color = "Traffic Level"
  ) +
  theme_minimal()

# Out of curiosity [Do not use for report]: same plot but not with log transformed delivery time
ggplot(d.food_time, aes(x = distance_km, y = delivery_time_min, color = traffic_level_factor)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Distance vs Delivery Time by Traffic Level",
    x = "Distance (km)",
    y = "Delivery Time",
    color = "Traffic Level"
  ) +
  theme_minimal()


# Using ggplot to create a scatterplot (delivery time ~ distance) with order type in different colors
ggplot(d.food_time, aes(x = distance_km, y = log_delivery_time, color = order_type_factor)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Distance vs Log Delivery Time by Order Type",
    x = "Distance (km)",
    y = "Log Delivery Time",
    color = "Order Type"
  ) +
  theme_minimal()


"""
*** NINA ORDNEN***
From the two plots we can draw similar conclusions from what we've already seen in the boxplot.
- Traffic level shows a big impact
- Order type shows little impact whereas (drinks seem to have the longest delivery time (but not a strong difference)
- Also for both predictors we see that in certain areas the different categories overlap.
"""



####************** Model fitting - TASK 5 ####

"""
After investigating the numeric and categorial variables, we will begin by fitting a model with all possibly
relevant factors.

To start with we choose the following predictors:
- distance_km - numeric
- courier_rating_1_to_5 - numeric
- traffic_level_factor
- order_type_factor

First we need to do divide into test and training data. We will do stratified sampling because
of the factor traffic_level which does not contain a similar amount of records in each category.

"""

# dividing into train and test data


set.seed(42)
train_indices <- createDataPartition(d.food_time$traffic_level_factor, p = 0.8, list = FALSE)

train_data <- d.food_time[train_indices, ]
test_data  <- d.food_time[-train_indices, ]


# Releveling factor traffic_level & set to meaningful sequence (increasing)
d.food_time$traffic_level_factor <- factor(
  d.food_time$traffic_level_factor,
  levels = c("very low", "low", "moderate", "high", "very high")
)

# Fitting linear model with two numeric and two categorial variables
lm.delivery1 <- lm(log_delivery_time ~ distance_km + courier_rating_1_to_5 + traffic_level_factor + order_type_factor, data = train_data)

# Investigating model coefficients
summary(lm.delivery1)

# Exponentiate coefficients from model
exp(lm.delivery1$coefficients)

# Confidence intervals for coefficients
confint(lm.delivery1)

# diagnostic plots

# prepare data
df_plot <- data.frame(
  fitted = fitted(lm.delivery1),
  residuals = resid(lm.delivery1)
)

# Plot: residuals vs Fitted
ggplot(df_plot, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

qqnorm(resid(lm.delivery1))
qqline(resid(lm.delivery1), col = "red")

"""
***NINA: WEsHALB unterschiedliche skalen auf x und y achsen von qq plot***
*** NINA: WERTE PRÜFEN ***
Interpretation of summary of linear model <lm.delivery1>

General remarks

1) Intercept:
The intercept on the log transformed target variable is at 2.512.
exP(2.5283589) = 12.5329214
Interpretation: the intercept is the delivery_time we expect if all other variables = 0 and
the categorial variables are at reference level.
The time of 12.53 minutes is of no practical relevance here.


2) <distance_km>: 

There is strong evidence that the slope for disctance_km is not flat.

For each increase of 1 unit in distance_km (km) the log transformed response variable will
increase by 0.0133558. The exponentiated delivery_time_min will increase by 1.0134453 which corresponds to 1.34%.


3) <courier_rating_1_to_5

There is no evidence that courier_rating_1_to_5 has a significant effect on the response variable.
This can be seen by looking at the p-value [0.242] which is much higher than 0.05. 
Also it can be seen by the confidence intervals of this coefficient: [-0.01506945  0.003810957] which contains 0.

(in a next step this coefficient will be dropped)


4) <traffic_level_factor>

Before fitting the model we've set the traffic_level_factor> [very low] as reference category.
This means that all the other factors from low - very high show the difference to this reference level.

I.e. with increasing traffic the delivery time increases (strongly).

All factors show very strong evidence against the null hypothesis that there is no effect.
This can be seen by looking at the p-values which are very small.

Example for interpretation: the coefficient for traffic_level_factor [very high] is 1.2187542 (on the log transformed target variable).
To interpret this on the original scale we need to exponentiate:

exp(1.2187542) = 3.3829707 

******************************* NINA KORREKTUR ***************************************
--> this means that (if all other variables are held constant) deliveries under <very high> traffic conditions
take on average 3.38 times as long as under <very low> traffic conditions.
******************************* NINA KORREKTUR ***************************************

5) <order type factor>

For the order_type_factor the reference category is <Buffet>. 
All factors show very strong evidence against the null hypothesis that there is no effect.

Compared to traffic_level_factor the different factors show a much smaller impact.
<Drinks> takes longer than the other categories and <Meal> is delivered faster. 

Exmplae for drinks:
coefficient (for log transformed target variable) is 0.2140612.
exp(0.2140612) ~ 1.2386985 --> which leads to a 23.8% longer delivery time.

One approach to explain this could be that the handling of drinks is a bit more delicate. 
But this is just speculation.

Overall model performance: 

1)R-squared: 0.9202

This value indicates that 92.02% of the total variance in the log transformed target variable is explained.

2) Adjusted R-squared: 0.9201

Adjusted R-squared corrects the normal R-squared for the number of predictors used (i.e. it penalized complexer models).
Here the adjusted R-squared is almost identical to R-squared. This indicates that all predictors contribute to the model.

3) Residual standard error: 0.1332

The residual standard error estimates the standard deviation of the residuals (i.e. difference between observed
and fitted values).

**************************[KORREKTUR NINA]**************************************************
The number from summary() --> 0.1332 is the deviation on the log-scale. To interpret this we need to exponentiate it.
exp(0.1332) ~ 1.142.

Interpretation: the fitted delivery times are on average around 14.2 % above or below the observed delivery times.
**************************[KORREKTUR NINA]**************************************************

Diagnostic plots

We furthermore performed two diagnostic plots on the lm.delivery1

a) Plotting residuals vs fitted values.

We can observe that our data points are more or less centered around 0. 
Also we can see that most of the residuals lie within -0.4 and +0.4.

What we can see however is certain vertical stripes - these jumps are likely to be caused
by the categorical variables we used. 

Also we can see a small effect of heteroscedasticity towards the right side (larger values).
This is a hint that the variance is not completely constant which would be in violation of a basic assumption for linear
modelling: Homoscedasticity.

For now we will not implement further measures and tolerate what we have seen.
We did not see an indication of a serious violation of model assumptions.


In a next step we will use drop1() to investigate the effect of removing a certain variable
from our model. I.e. <what happens if we remove one variable and the rest remains the same>.

"""

# using drop1() on first linear model
drop1(lm.delivery1, test = "F")

"""
From looking at the output (as also visible in summary() above) we can see that
<courier_rating_1_to_5> has a high p-value. Therefore we can not reject the null hypothesis that slope of this
variable is flat. 

Furthermore when looking at RSS and AIC we can see that if we remove this variable the RSS increases only very little
i.e. from 128.16 to 128.19 and that the AIC remains the same.

That means that removing this variable does not worsen the model.

All other variables explain the variance and have very low p-values. Meaning that we should not remove them.
"""

# simplifying the model

lm.delivery2 <- update(lm.delivery1, . ~ . - courier_rating_1_to_5)
summary(lm.delivery2)
summary(lm.delivery1)

"""
From looking at the summary() of both models we can conclude that the quality of the model
did not worsen by removing courier_rating_1_to_5. R-squared and adjusted R-squared remained the same
"""


# Akaike Information Criterion
AIC(lm.delivery1)
AIC(lm.delivery2)

"""
Based on Akaike Information Criterion the two models are basically equally good 
This supports the idea that we could simplify the model and not having an information loss
"""

"""
In a next step we will also check for interaction between traffic_level_factor and distance_km.
Reason: the effect of distance on time could be bigger if on a long distance there is a lot of traffic.

I.e. slow progress on a long distance could lead to very long delivery times
"""

# Including interaction between distance_km and traffic_level_factor
lm.interaction <- lm(log_delivery_time ~ distance_km * traffic_level_factor + order_type_factor, data = train_data)
summary(lm.interaction)
summary(lm.delivery2)
"""
By including interaction between distance_km and traffic_level_factor we allow
the effect of distance_km to differ depening on the traffic_level_factor.

The model contains all main effects as well as all interaction terms between distance and traffic level.

By looking at summary() we can directly see that:

- Residual Standard error is smaller as for lm.delivery2 (i.e.: better model!)
- Multiple R-squared and adjusted R-squared are also a littler higher

***NINA: Was soll gesagt werden mit F-Statistik?***
- F-statistic: tests null hypothesis that all regression coefficients (apart from intercept = 0). There is strong evidence against that
- The number in F-statistic is higher for lm.delivery2 --> this is most likely due to more predictors (12)

--> F statistic from summary only compares against zero model (see above)

In order to compare lm.interaction and lm.delivery2 we will use anova and AIC.
"""

# AIC to compare models
AIC(lm.delivery2)
AIC(lm.interaction)

# anova to compare models
anova(lm.delivery2, lm.interaction)

"""
Both Akaike Information Criterion and ANOVA are in favour of the model with interation lm.interaction.

AIC:    the AIC is lower for the interaction model 385 points
ANOVA:  residual sum of squares is lower by 6.79 with an F value of 100.79 and a very low p-value.

By comparing the two models we choose to go on with the model that includes an interaction term.
lm.interaction explains more variance.

Meaning that the effect ot the distance is dependend on the traffic level.

Conclusion: we decide to use lm.interaction as our preferred model.
"""








####************** Predicting - TASK 6: ####
"""
After choosing lm.delivery as the model to use, we now would like to predict 
from the test data
"""

# Prediction, exponentation of prediction and actual delivery time from test_data
predicted_log <- predict(lm.interaction, newdata = test_data)
predicted_min <- exp(predicted_log)
actual_min <- test_data$delivery_time_min


# Plotting predicted vs. actual delivery time

ggplot(test_data, aes(x = predicted_min, y = actual_min)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Delivery Time",
    x = "Predicted Delivery Time [min]",
    y = "Actual Delivery Time [min]"
  ) +
  theme_minimal()

# Plotting prediction errors on test data

ggplot(test_data, aes(x = predicted_min, y = actual_min)) +
  geom_point(alpha = 0.3) +
  geom_segment(aes(xend = predicted_min, yend = predicted_min), color = "blue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    title = "Prediction Errors on Test Data",
    x = "Predicted Delivery Time [min]",
    y = "Actual Delivery Time [min]"
  ) +
  theme_minimal()

# calculation root mean squared error and r_squared on test data
rmse <- sqrt(mean((predicted_min - actual_min)^2))
r_squared <- 1 - sum((predicted_min - actual_min)^2) / sum((actual_min - mean(actual_min))^2)

rmse
r_squared

# calculating rmse for only short deliveries (i.e. < 40min)
results <- data.frame(predicted_min = predicted_min, actual_min = actual_min)
short_deliveries <- results[results$actual_min < 40, ]
rmse_short <- sqrt(mean((short_deliveries$predicted_min - short_deliveries$actual_min)^2))
rmse_short


"""
To investigate the prediction on the test data, we plot the actual delivery time vs. predicted delivery time.
Furthermore we included error bars (vertical line between actual and predicted values).

- We can see on this plot that for predictions < 40 min the distance between points and the red line is small. I.e. our model works very good for this range.
- With increasing delivery time, the scattering gets much bigger, > 50 minute.
- For very large delivery times there are a few extreme outliers.
- The general structure is linear - all in all this model is stable.

We furthermore calcualted the root mean squared error and r_squared for the testdata.

- RMSE:       5.59 minutes (meaning that the average prediction error is around 5.6 minutes)
- r_squared:  0.883 meaning that the model explains 88.3 of the variance of total delivery times. It is only a little smaller compared
              to r_squared of training data (hint for no overfitting)

Additionally: when we calculate the rmse for deliveries under 40min, we achieve 3.09 minutes.

***NINA on longer distance, more can happen***

"""


lm.test_notlog <- lm(delivery_time_min ~ distance_km * traffic_level_factor + order_type_factor, data = train_data)
summary(lm.test_notlog)

predict_test <- predict(lm.test_notlog, newdata = test_data)
actual_min <- test_data$delivery_time_min

AIC(lm.test_notlog)
AIC(lm.interaction)

# Plotting prediction errors on test data

ggplot(test_data, aes(x = predict_test, y = actual_min)) +
  geom_point(alpha = 0.3) +
  geom_segment(aes(xend = predict_test, yend = predict_test), color = "blue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    title = "Prediction Errors on Test Data - not log transformed",
    x = "Predicted Delivery Time [min]",
    y = "Actual Delivery Time [min]"
  ) +
  theme_minimal()

rmse_test <- sqrt(mean((predict_test - actual_min)^2))
rmse_test


