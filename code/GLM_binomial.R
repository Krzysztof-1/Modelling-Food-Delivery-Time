suppressPackageStartupMessages(library(tidyverse))
library(tidyverse)
library(gridExtra)

getwd()
# Generalised Linear Model Poisson – Regression on average_speed_kmph
df_clean <- read.csv("../data/cleaned_data.csv")
dim(df_clean)
#options(width = 160)
head(df_clean)
summary(df_clean)
# checking traffic_level_factor
table(df_clean$traffic_level_factor, useNA = "always")
levels(df_clean$traffic_level_factor)
summary(df_clean$traffic_level_clean)

# Fix factor ordering in the original dataframe
# df_clean$traffic_level_factor <- factor(
#   df_clean$traffic_level_factor,
#   levels = c("very low", "low", "moderate", "high", "very high"),
#   ordered = TRUE
# )

#  Convert to unordered factor
df_clean$traffic_level_factor <- factor(df_clean$traffic_level_factor, ordered = FALSE)
#  Set the reference level
df_clean$traffic_level_factor <- relevel(df_clean$traffic_level_factor, ref = "very low")

table(df_clean$vehicle_type_factor)
table(df_clean$traffic_level_factor)

# Pre-process vehicle_type_factor
df_clean <- df_clean %>%
  # Remove bicycle as only 10 observations
  filter(vehicle_type_factor != "bicycle") %>%
  mutate(
    # Merge electric scooter and scooter
    vehicle_type_factor = if_else(
      vehicle_type_factor == 'electric_scooter',
      "scooter",
      vehicle_type_factor
    ),
    # Merge very low and low
    traffic_level_factor = if_else(
      traffic_level_factor == 'very low',
      "low",
      traffic_level_factor
    )
  )

# Plot bar charts of observations per factor
p1 <- ggplot(data=df_clean, mapping=aes(x=order_type_factor)) +
  geom_bar(mapping=aes(fill = order_type_factor))
p2 <- ggplot(data=df_clean, mapping=aes(x=vehicle_type_factor)) +
  geom_bar(mapping=aes(fill = vehicle_type_factor))
p3 <- ggplot(data=df_clean, mapping=aes(x=traffic_level_factor)) +
  geom_bar(mapping=aes(fill = traffic_level_factor))
p4 <- ggplot(data=df_clean, mapping=aes(x=weather_category)) +
  geom_bar(mapping=aes(fill = weather_category))

grid.arrange(p1, p2, p3, p4, nrow = 4, ncol=1)

# Boxplots for completeness
p1 <- ggplot(data=df_clean, mapping=aes(x=order_type_factor, y=delivery_time_min)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=40),colour="red")
p2 <- ggplot(data=df_clean, mapping=aes(x=vehicle_type_factor, y=delivery_time_min)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=40),colour="red")
p3 <- ggplot(data=df_clean, mapping=aes(x=traffic_level_factor, y=delivery_time_min)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=40),colour="red")
p4 <- ggplot(data=df_clean, mapping=aes(x=weather_category, y=delivery_time_min)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=40),colour="red")

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol=2)

# Plot continuous variables against delivery time
p1 <- ggplot(data=df_clean, mapping=aes(x=courier_age_years, y=delivery_time_min)) +
  geom_point() +
  geom_smooth(method='lm')
p2 <- ggplot(data=df_clean, mapping=aes(x=courier_rating_1_to_5, y=delivery_time_min)) +
    geom_point() +
  geom_smooth(method='lm')
p3 <- ggplot(data=df_clean, mapping=aes(x=temperature_celsius, y=delivery_time_min)) +
    geom_point() +
  geom_smooth(method='lm')
p4 <- ggplot(data=df_clean, mapping=aes(x=humidity_percent, y=delivery_time_min)) +
    geom_point() +
  geom_smooth(method='lm')
p5 <- ggplot(data=df_clean, mapping=aes(x=precipitation_mm, y=delivery_time_min)) +
    geom_point() +
  geom_smooth(method='lm')
p6 <- ggplot(data=df_clean, mapping=aes(x=distance_km, y=delivery_time_min)) +
    geom_point() +
  geom_smooth(method='lm')

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol=2)

# Plot continuous variables against long delivery flag
p1 <- ggplot(data=df_clean, mapping=aes(x=courier_age_years, y=long_delivery_flag)) +
  geom_point()
p2 <- ggplot(data=df_clean, mapping=aes(x=courier_rating_1_to_5, y=long_delivery_flag)) +
  geom_point()
p3 <- ggplot(data=df_clean, mapping=aes(x=temperature_celsius, y=long_delivery_flag)) +
  geom_point()
p4 <- ggplot(data=df_clean, mapping=aes(x=humidity_percent, y=long_delivery_flag)) +
  geom_point()
p5 <- ggplot(data=df_clean, mapping=aes(x=precipitation_mm, y=long_delivery_flag)) +
  geom_point()
p6 <- ggplot(data=df_clean, mapping=aes(x=distance_km, y=long_delivery_flag)) +
  geom_point()

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol=2)

## Cannot use courier rating as heavily left-skewed
ggplot(data=df_clean, mapping=aes(x=courier_rating_1_to_5)) +
  geom_histogram()

## Cannot use precipitation as observations not diverse enough
ggplot(data=df_clean, mapping=aes(x=precipitation_mm)) +
  geom_histogram() +
  scale_y_log10()  # log-scale on counts

# Conclusion: Amongst continuous variables, only temperature_celsius and 
# distance_km seem appropriate to model long_delivery_flag

# GLM Binomial model (all features)
binomial.model.test <- glm(
  long_delivery_flag ~ courier_age_years + courier_rating_1_to_5 + 
    temperature_celsius + humidity_percent + precipitation_mm +
    distance_km + order_type_factor + vehicle_type_factor +
    traffic_level_factor +  weather_category,
  data = df_clean,
  family = binomial
)
summary(binomial.model.test)
df_clean$y_pred = predict(binomial.model.test,
  df_clean[c('courier_age_years', 'courier_rating_1_to_5',
  'temperature_celsius', 'humidity_percent', 'precipitation_mm',
  'distance_km', 'order_type_factor', 'vehicle_type_factor',
  'traffic_level_factor', 'weather_category')], type="response")


# GLM Binomial model (1st iteration screened features)
binomial.model.test <- glm(
  long_delivery_flag ~ temperature_celsius + distance_km +
    order_type_factor + vehicle_type_factor +
    traffic_level_factor + weather_category,
  data = df_clean,
  family = binomial
)
summary(binomial.model.test)
df_clean$y_pred = predict(binomial.model.test,
    df_clean[c('temperature_celsius', 'distance_km', 'order_type_factor',
     'vehicle_type_factor', 'traffic_level_factor',
     'weather_category')], type="response")

## Vehicle Type and Traffic level have very high p-values
## There is not enough statistical evidence to prove they have a significant effect

# Refine the Binomial regression by removing unnecessary variables
binomial.model.test <- glm(
  long_delivery_flag ~ temperature_celsius + distance_km +
    order_type_factor + weather_category,
  data = df_clean,
  family = binomial
)
summary(binomial.model.test)


## AIC increased from 2704 to 4602 but now all features have a significant effect
## according to p-values.

# Make predictions on old data
df_clean$y_pred = predict(binomial.model.test,
  df_clean[c('temperature_celsius', 'distance_km', 'order_type_factor',
               'weather_category')], type="response")

# Convert to class labels (using 0.5 cutoff)
df_clean <- df_clean %>%
  mutate(y_pred = if_else(y_pred > 0.5, 1, 0))

# Confusion matrix
table(Predicted = df_clean$y_pred, Actual = df_clean$long_delivery_flag)

# Accuracy
mean(df_clean$y_pred == df_clean$long_delivery_flag)


# Print the regression formula with coefficients
cat("Long_delivery_flag = ", paste0(
  round(coef(binomial.model.test)[1], 3), " + ",
  paste0(names(coef(binomial.model.test))[-1], " * ", round(coef(binomial.model.test)[-1], 3), collapse = " + ")
))

coef.distance.10 <- coef(binomial.model.test)["distance_km"] * 10
coef.distance.10
##
print(exp(coef.distance.10), digits = 5)
