getwd()
# Generalised Linear Model Poisson – Regression on average_speed_kmph
df_clean <- read.csv("../data/cleaned_data.csv")
dim(df_clean)
head(df_clean)
summary(df_clean)
# checking traffic_level_factor
table(df_clean$traffic_level_factor, useNA = "always")
levels(df_clean$traffic_level_factor)
summary(df_clean$traffic_level_factor)

# Fix factor ordering in the original dataframe
df_clean$traffic_level_factor <- factor(
  df_clean$traffic_level_factor,
  levels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  ordered = TRUE
)

# regression with glm poisson model
poisson.model.test <- glm(
  average_speed_kmph ~ courier_age_years + temperature_celsius + humidity_percent + precipitation_mm +
    distance_km + traffic_level_factor + vehicle_type_factor +
    weather_category,
  data = df_clean,
  family = poisson(link="log")
)

# check model summary
summary(poisson.model.test)

# Refine the Poisson regression by removing unnecessary variables.
# regression with glm poisson model
poisson.model <- glm(
  average_speed_kmph ~ temperature_celsius + humidity_percent + distance_km + traffic_level_factor +
    weather_category,
  data = df_clean,
  family = poisson(link="log")
)

# check model summary
summary(poisson.model)

# print the regression formula with coefficients
cat("Log(average_speed_kmph) = ", paste0(
  round(coef(poisson.model)[1], 3), " + ",
  paste0(names(coef(poisson.model))[-1], " * ", round(coef(poisson.model)[-1], 3), collapse = " + ")
))


# simulate some data from this model
library(ggplot2)
set.seed(2)
sim.data.average_speed_kmph.Poisson <- simulate(poisson.model)
names(sim.data.average_speed_kmph.Poisson) <- "sim_average_speed_kmph"
NROW(sim.data.average_speed_kmph.Poisson)
head(sim.data.average_speed_kmph.Poisson)
tail(sim.data.average_speed_kmph.Poisson)
# Combine simulated data with traffic level info
df_plot <- data.frame(
  Traffic_Level = df_clean$traffic_level_factor,
  sim_average_speed_kmph = sim.data.average_speed_kmph.Poisson$sim_average_speed_kmph
)

# set order of traffic levels
df_plot$Traffic_Level <- factor(df_plot$Traffic_Level,
    levels = c("Very Low", "Low", "Moderate", "High", "Very High")
    )

# Create the boxplot grouped by traffic level
ggplot(df_plot, aes(x = Traffic_Level, y = sim_average_speed_kmph)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ylab("Simulated delivery average_speed_kmph\n(assuming Poisson model)") +
  xlab("Traffic Level") +
  theme_minimal()

library(effects)

# Compute effects
eff <- allEffects(poisson.model)

# Fix x-axis factor level order
eff[["traffic_level_factor"]]$x$traffic_level_factor <- factor(
  eff[["traffic_level_factor"]]$x$traffic_level_factor,
  levels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  ordered = TRUE
)

# Fix internal effect levels as well
eff[["traffic_level_factor"]]$xlevels$traffic_level_factor <- c("Very Low", "Low", "Moderate", "High", "Very High")

# Now plot
#plot(eff, multiline = TRUE)

# Plot each effect individually
plot(eff[["temperature_celsius"]], main = "Effect of Temperature on Speed")
plot(eff[["humidity_percent"]], main = "Effect of Humidity on Speed")
plot(eff[["distance_km"]], main = "Effect of Distance on Speed")
plot(eff[["traffic_level_factor"]], main = "Effect of Traffic Level on Speed")
plot(eff[["weather_category"]], main = "Effect of Weather on Speed")

library(ggeffects)
plot(ggpredict(poisson.model, terms = "temperature_celsius"))


# Create combined data frame for plotting
df_plot_combined <- bind_rows(
  df_clean %>%
    select(Traffic_Level = traffic_level_factor, speed = average_speed_kmph) %>%
    mutate(Type = "Real"),
  
  data.frame(
    Traffic_Level = df_clean$traffic_level_factor,
    speed = sim.data.average_speed_kmph.Poisson$sim_average_speed_kmph,
    Type = "Simulated"
  )
)

# ensures the plot uses the correct order
df_plot_combined$Traffic_Level <- factor(
  df_plot_combined$Traffic_Level,
  levels = c("Very Low", "Low", "Moderate", "High", "Very High"),
  ordered = TRUE
)

# Plot grouped boxplot
ggplot(df_plot_combined, aes(x = Traffic_Level, y = speed, fill = Type)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Real" = "skyblue", "Simulated" = "lightpink")) +
  labs(
    title = "Real vs. Simulated Delivery Speeds by Traffic Level",
    x = "Traffic Level",
    y = "Average Speed (km/h)",
    fill = "Speed Type"
  ) +
  theme_minimal()

# Create combined data frame for plotting by weather category
df_plot_weather <- bind_rows(
  df_clean %>%
    select(Weather = weather_category, speed = average_speed_kmph) %>%
    mutate(Type = "Real"),
  
  data.frame(
    Weather = df_clean$weather_category,
    speed = sim.data.average_speed_kmph.Poisson$sim_average_speed_kmph,
    Type = "Simulated"
  )
)

# Ensure correct factor level order
df_plot_weather$Weather <- factor(df_plot_weather$Weather,
                                  levels = c("Clear", "Poor Visibility", "Rainy"),
                                  ordered = TRUE)

# Plot grouped boxplot for weather category
ggplot(df_plot_weather, aes(x = Weather, y = speed, fill = Type)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Real" = "skyblue", "Simulated" = "lightpink")) +
  labs(
    title = "Real vs. Simulated Delivery Speeds by Weather Category",
    x = "Weather Category",
    y = "Average Speed (km/h)",
    fill = "Speed Type"
  ) +
  theme_minimal()

# comparision real speed value vs predicted speed value
# 1. Predict average speed (on response scale)
df_clean$predicted_speed_kmph <- predict(poisson.model, type = "response")

# 2. Create a long-format comparison dataset
df_compare <- df_clean %>%
  select(traffic_level_factor, weather_category, 
         average_speed_kmph, predicted_speed_kmph) %>%
  pivot_longer(cols = c(average_speed_kmph, predicted_speed_kmph),
               names_to = "Type", values_to = "Speed")

# 3. Plot by traffic level
library(ggplot2)

ggplot(df_compare, aes(x = traffic_level_factor, y = Speed, fill = Type)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.7) +
  scale_fill_manual(values = c("average_speed_kmph" = "skyblue", 
                               "predicted_speed_kmph" = "lightpink"),
                    labels = c("Observed", "Predicted")) +
  labs(title = "Observed vs. Predicted Speed by Traffic Level",
       x = "Traffic Level", y = "Speed (km/h)", fill = "Speed Type") +
  theme_minimal()

# visualize real speed vs. predicted speed scatter plot
# Add predicted speed to the dataset
df_clean$predicted_speed_kmph <- predict(poisson.model, type = "response")

# Scatter plot: predicted vs. real speed
library(ggplot2)

ggplot(df_clean, aes(x = predicted_speed_kmph, y = average_speed_kmph)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Observed vs. Predicted Delivery Speed",
    x = "Predicted Speed (km/h)",
    y = "Observed Speed (km/h)"
  ) +
  theme_minimal()

# residual analysis
# 1.compute residuals
df_clean$residuals_raw <- residuals(poisson.model, type = "response")   # raw residuals
df_clean$residuals_deviance <- residuals(poisson.model, type = "deviance")  # deviance residuals
df_clean$residuals_pearson <- residuals(poisson.model, type = "pearson")  # for overdispersion check

# 2. Plot: Predicted vs. Residuals
ggplot(df_clean, aes(x = predicted_speed_kmph, y = residuals_deviance)) +
  geom_point(alpha = 0.4, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Deviance Residuals vs. Predicted Speed",
    x = "Predicted Speed (km/h)",
    y = "Deviance Residuals"
  ) +
  theme_minimal()

# 3. Histogram of Residuals
ggplot(df_clean, aes(x = residuals_deviance)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(
    title = "Histogram of Deviance Residuals",
    x = "Deviance Residuals",
    y = "Count"
  ) +
  theme_minimal()

# 4. Check for Overdispersion
overdispersion <- sum(residuals(poisson.model, type = "pearson")^2) / poisson.model$df.residual
overdispersion  # > 1 suggests overdispersion; if >>1, Poisson may not be appropriate

# 5. re-run poisson model, relevel traffic_level_factor
#  Convert to unordered factor
df_clean$traffic_level_factor <- factor(df_clean$traffic_level_factor, ordered = FALSE)

#  Set the reference level
df_clean$traffic_level_factor <- relevel(df_clean$traffic_level_factor, ref = "Very Low")

poisson.model.treat <- glm(
  average_speed_kmph ~ traffic_level_factor + temperature_celsius + humidity_percent 
  + distance_km + weather_category,
  family = poisson(link = "log"),
  data = df_clean
)
summary(poisson.model.treat)
exp(coef(poisson.model.treat))
