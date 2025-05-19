## ML1 Project
## Modelling-Food-Delivery-Time
## KB CC WX

# **************************** LIBRARIES

library(tidyverse)
library(dplyr)
library(readr) 

# **************************** PART 1: Loading and inspecting the dataset

# Set working directory
getwd()

# Loading data into csv

df.food_time <- read.csv("../data/Food_Time_Data_Set.csv")

# Investigating if loaded correctly

dim(df.food_time)
head(df.food_time)
summary(df.food_time)

# **************************** PART 2: Renaming column names to snake case (incl. more descriptive names)

df.food_time <- df.food_time %>%
  rename(
    order_id = ID,
    courier_id = Delivery_person_ID,
    courier_age_years = Delivery_person_Age,
    courier_rating_1_to_5 = Delivery_person_Ratings,
    restaurant_latitude_deg = Restaurant_latitude,
    restaurant_longitude_deg = Restaurant_longitude,
    customer_latitude_deg = Delivery_location_latitude,
    customer_longitude_deg = Delivery_location_longitude,
    order_type = Type_of_order,
    vehicle_type = Type_of_vehicle,
    temperature_celsius = temperature,
    humidity_percent = humidity,
    precipitation_mm = precipitation,
    weather_description = weather_description,
    traffic_level = Traffic_Level,
    distance_km = Distance..km.,
    delivery_time_min = TARGET
  )

colnames(df.food_time)
# save the rename the raw data with updated colnames
write.csv(df.food_time, "../data/Food_Time_Data_Set.csv", row.names = FALSE)
