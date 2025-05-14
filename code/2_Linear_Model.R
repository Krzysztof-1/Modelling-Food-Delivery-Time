##************** Group work ML1 - Linear regression*************

"""
Author: Curdin Caderas
Start: 10.05.2025
"""

# Libraries

library(ggplot2)
library(dplyr)

"""
PLACEHOLDER
"""

####************** PREPARATORY TASK 1: Loading Dataset and checking the dataset ####

# Loading cleaned data

d.food_time <- read.csv("C:/Users/Setup/OneDrive/02_Masterstudium/03_Module/02_Semester_2/03_ML1/git_group_work/Modelling-Food-Delivery-Time/data/cleaned_data.csv", header = TRUE)

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


"""
Comments on distribution of target variable:
- From looking at the histogramm, we see that the distribution is right-skewed
- This is furhtermore confirmed by the boxplot which shows outliers
- Furthermore from looking at summary of delivery_time_min we see that Mean is > Median
- And Max Value is 4x the size of median

Due to this, we will log transform our target variable.

"""

#Log transforming target variable and investigating it's distribution
hist(log(d.food_time$delivery_time_min), main = "Log-transformed distribution of delivery time")
boxplot(log(d.food_time$delivery_time_min), main = "Boxplot of log-transformed delivery time [min]")
summary(log(d.food_time$delivery_time_min))


"""
After log transforming the distribution is more symmetrical. we therefore add a column to our df with the log
transformed delivery_time_min.
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
- Also we will disregard percipitation because there seems to be missing data. At least the low amount of percipitation seems questionable.
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

For humidity_percent and courier_age there seems to be no effect.

After visually inspecting a possible relationship we will calculate the correlation between those relationships
"""

cor(d.food_time[, c(numeric_predictors, target)]) 

"""
The correlation matrix supports our observation from above.

- distance_km has the highest positive correlation with 0.8432
- courier_rating_1_to_5 has a weak negative correlation with -0.099 --> small effect but it seems plausible (higher ratings, lower delivery times)
"""

####************** PREPARATORY TASK 4: Investigate categorial predictors (Boxplots) ####

"""
After investigating the numeric variables, we will move on to categorical variables and investigate them by using boxplots.

We will investigate the following categorical variables:
- order_type_factor
- vehicle_type_factor
- traffic_level_factor
- wheater_category


"""

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

boxplot(distance_km ~ vehicle_type_factor, data = d.food_time)

"""
From looking at the boxplots we can clearly see that traffic level seems to play an 
important role on the delivery time. <very high> has the longest delivery time and <very low>
the shortest delivery time.

The data we have points towards a high impact from traffic level to delivery time which seems plausible.

the other factors we' ve looked do not seem to have a big impact on delivery time.
When looking at <vehicle type> we can see that bicyle seems to have a small tendency towards 
shorter delivery times. But this could be influenced by e.g. shorter distances.
By creating a boxplot with distance ~ vehicle type we see that this is likely to be the cause
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
From the two plots we can draw similar conclusions from what we've already seen in the boxplot.
- Traffic level shows a big impact
- Order type shows little impact whereas (drinks seem to have the longest delivery time (but not a strong difference)
"""



####************** Model fitting - TASK 5 ####
"""
After investigating the numeric and categorial variables, we will begin by fitting a model with all possibly
relevant factors
"""