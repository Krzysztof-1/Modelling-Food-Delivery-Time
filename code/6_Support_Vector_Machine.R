##************** Group work ML1 - Support Vector Machine*************

"""
Author: Curdin Caderas
Start: 16.05.2025
"""

# Libraries

library(ggplot2)
library(dplyr)
library(caret)
library(e1071)

"""
In this chapter we will use a Support Vector Machine to try to do classifications on

- traffic_level
- vehicle_type_factor
- Order type


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

####************** PREPARATORY TASK 2: Visual analysis of variables  ####

"""
To beginn we will plot the objects we would like to classify by distance and delivery time.
"""

# Plotting traffic level by delivery time / Distance

ggplot(d.food_time, aes(x = distance_km, y = delivery_time_min, color = traffic_level_factor)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Delivery Time vs Distance by Traffic Level",
    x = "Distance [km]",
    y = "Delivery Time [min]",
    color = "Traffic Level"
  ) +
  theme_minimal()

# Plotting traffic level  average_speed / distance

ggplot(d.food_time, aes(x = distance_km, y = average_speed_kmph, color = traffic_level_factor)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Average Speed vs Distance by Traffic Level",
    x = "Distance [km]",
    y = "Average Speed [km/h]",
    color = "Traffic Level"
  ) +
  theme_minimal()


# Plotting vehicle type factor by time / distance

ggplot(d.food_time, aes(x = distance_km, y = delivery_time_min, color = vehicle_type_factor)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Delivery Time vs Distance by Vehicle Type",
    x = "Distance [km]",
    y = "Delivery Time [min]",
    color = "Vehicle Type"
  ) +
  theme_minimal()

# Plotting verhicle type factor by average speed / distance
ggplot(d.food_time, aes(x = distance_km, y = average_speed_kmph, color = vehicle_type_factor)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Average Speed vs Distance by Vehicle Type",
    x = "Distance [km]",
    y = "Average Speed [km / h]",
    color = "Vehicle Type"
  ) +
  theme_minimal()


# Plotting order type by time / distance

ggplot(d.food_time, aes(x = distance_km, y = delivery_time_min, color = order_type_factor)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Delivery Time vs Distance by Order Type",
    x = "Distance [km]",
    y = "Delivery Time [min]",
    color = "Order Type"
  ) +
  theme_minimal()

"""B(
In the first plot we can see, that traffic_level_factor is most likely linearly separable.
However the other two plots show a big overlap which is a hint that it could be difficult with a linear kernel.
"""

####************** PREPARATORY TASK 3: Preparation of relevant data ####

"""
We will start trying to classify the traffic level.

To do this we first perform the following steps:
- rearranging the factors in traffic level (very low - very high) for better readability
- create df: svm_data
- ensure target variable is a factor
- divide into a train and a test part (stratified sampling)


"""

# Rearranging the factors from <very low> to <very high>

d.food_time$traffic_level_factor <- factor(
  d.food_time$traffic_level_factor,
  levels = c("very low", "low", "moderate", "high", "very high"),
  ordered = TRUE
)


# Prepare data for svm

svm_data <- d.food_time %>%
  select(traffic_level_factor, distance_km, average_speed_kmph)

svm_data$traffic_level_factor <- factor(svm_data$traffic_level_factor) # target variable as factor
View(svm_data)

# Divide into a train and a test part

set.seed(123)  
split_index <- createDataPartition(svm_data$traffic_level_factor, p = 0.8, list = FALSE)

train_svm <- svm_data[split_index, ]
test_svm  <- svm_data[-split_index, ]


####************** TASK 4 - Model fit & prediction [Classification] #### 

"""
To fit a first model we will try to work with:
  
- distance_km
- avg_speed

From the lecture note we know, that we need to be careful to not overfit the SVM
To get practise, we will use caret to prepare the data and e1071 for the SVM.

we start with a linear kernel and a cost of 10. Later on we will try to improve the model.

"""


# Training the model using a linear kernel

svm_model1 <- svm(traffic_level_factor ~ distance_km + average_speed_kmph, data = train_svm, method = "linear", scale = TRUE, cost = 10)


# Making predictions

pred_svm <- predict(svm_model1, newdata = test_svm)

# Checking classification results

confusionMatrix(pred_svm, test_svm$traffic_level_factor)

# Plot of SVM decision boundaries for traffic level.


# Create a grid (200 values per axis - 40k dots)
xrange <- seq(min(svm_data$distance_km), max(svm_data$distance_km), length.out = 200)
yrange <- seq(min(svm_data$average_speed_kmph), max(svm_data$average_speed_kmph), length.out = 200)
grid <- expand.grid(distance_km = xrange, average_speed_kmph = yrange)


# apply svm model to points in grid
grid$predicted <- predict(svm_model, newdata = grid)

# apply colors (with rectangles) for decision of model and include points
ggplot() +
  geom_tile(data = grid, aes(x = distance_km, y = average_speed_kmph, fill = predicted), alpha = 0.3) +
  geom_point(data = train_svm, aes(x = distance_km, y = average_speed_kmph, color = traffic_level_factor), alpha = 0.7, size = 1.2) +
  scale_fill_viridis_d(option = "plasma", name = "Predicted Class") +
  scale_color_viridis_d(option = "plasma", name = "True Class") +
  labs(
    title = "SVM Decision Boundaries for Traffic Level",
    x = "Distance [km]",
    y = "Average Speed [km/h]"
  ) +
  theme_minimal()

"""
After predicting we did the following two steps:
- Plotting SVM decision boundaries for traffic level incl. points (with color for true class)
- created a confusion matrix

Overall quality of the model 
- Accuracy of 84.5 % of the test data was correctly classified
- Confidence interval: 82.7 -5 - 86.1 %
- Model as a whole is strongly significant (compared to model which just guesses the most frequent class)
- between traffic level moderate and high the model is less accurate (i.e. 45 moderate wrongly classified as high and 48 high wrongly classified as moderate)

The last point with the higher error between moderate and high is also visible on the SVM Decision Boundaries plot.


In a next step we will try to use a non-linear kernel. We will use <radial>
"""

# Fitting another model with radial kernel

svm_model2 <- svm(traffic_level_factor ~ distance_km + average_speed_kmph, data = train_svm, kernel = "radial", cost = 10, gamma = 0.1, scale = TRUE)

# Prediction with new model
pred_rbf <- predict(svm_model2, newdata = test_svm)

# confusion matrix
confusionMatrix(pred_rbf, test_svm$traffic_level_factor)

"""
By looking at the confusion matrix of the radial model we can see that both models
are in terms of their overall quality practically identifcal.

We will move on by trying to tune the model. We do this by trying to find optimal values for <cost> and <gamma> via an automated grid search [e1071::tune()].

Values to try:
- cost = 0.1,1,10,100
- gamma = 0.1,0.05,0.1,0.5

Procedure: we use cross validation to find the best values & combination of values for cost and gamma.
The funciton tune() from e1071 package performs a hyper paramer tuning. This includes cross validation, to find the combination of parameters
which work best.

"""

set.seed(123)  # reproducibility

# Tuning using cross validation
tuned_model <- tune(
  svm,
  traffic_level_factor ~ distance_km + average_speed_kmph,
  data = train_svm,
  kernel = "radial",
  ranges = list(
    cost = c(0.1, 1, 10, 100),
    gamma = c(0.01, 0.05, 0.1, 0.5)
  )
)

# Extract the best model
best_model <- tuned_model$best.model

# Overview about the results
summary(tuned_model)

# Predict model based on test data
pred_tuned <- predict(best_model, newdata = test_svm)

# Confusion matrix with new, tuned model 
confusionMatrix(pred_tuned, test_svm$traffic_level_factor)

"""
Conclusion: no improvement with model tuning. 
Initial model was just as good.

ErgC$nzen: Kommentar C<ber Rechenpower

"""


