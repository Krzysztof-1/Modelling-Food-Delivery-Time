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
setwd("C:/Users/Setup/OneDrive/02_Masterstudium/03_Module/02_Semester_2/03_ML1/group_work/Modelling-Food-Delivery-Time")
list.files()

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

# **************************** PART 3: Cleaning the data


#This part will do the following:
#- Clean numeric values and convert to numeric
#- Clean and convert categorical variables to factors
#- Clean traffic level
#- Clean weather description & create the three categories Clear / Poor Visibility / Rainy
#- Insert a flag for long delivery time (if >= 40 min)
#- Calculate the average speed in km/h
#- Remove rows with NA values
#- Filter out coordinates which are not in India

df_clean <- df.food_time %>%
  select(-X) %>%
  mutate(
    # Clean numeric values and convert to numeric
    distance_km = as.numeric(na_if(gsub("[^0-9\\.]", "", distance_km), "")),  
    delivery_time_min = as.numeric(na_if(gsub("[^0-9\\.]", "", delivery_time_min), "")),
    
    # Clean and convert categorical variables to factors
    order_type_factor = factor(str_trim(order_type)),
    vehicle_type_factor = factor(str_trim(vehicle_type)),
    
    # Clean traffic level (lowercase and trimmed) 
    traffic_level_cleaned = str_trim(tolower(traffic_level)),
    traffic_level_factor = factor(traffic_level_cleaned),
    
    # Clean weather description
    weather_description_cleaned = str_trim(tolower(weather_description)),
    weather_category = factor(case_when(
      weather_description_cleaned %in% c("broken clouds", "clear sky", "few clouds", "overcast clouds", "scattered clouds") ~ "Clear",
      weather_description_cleaned %in% c("fog", "haze", "smoke") ~ "Poor Visibility",
      weather_description_cleaned %in% c("mist", "moderate rain", "light rain") ~ "Rainy",
      TRUE ~ NA_character_
    ), levels = c("Clear", "Poor Visibility", "Rainy"), ordered = TRUE),
    
    # Flag for long delivery time
    long_delivery_flag = if_else(delivery_time_min >= 40, 1, 0),
    
    # Calculate average speed (km/h)
    average_speed_kmph = as.integer(if_else(delivery_time_min > 0, distance_km * 60 / delivery_time_min, NA_real_))
  ) %>%
  drop_na() %>%
  filter(
    customer_longitude_deg >= 60, restaurant_longitude_deg >= 60,
    customer_latitude_deg >= 0, restaurant_latitude_deg >= 0
  )

# Check structure after cleaning
str(df_clean)


# Check for missing values in cleaned data & str
colSums(is.na(df_clean))
str(df_clean)
dim(df_clean)

# View the cleaned dataset
getwd()
head(df_clean)
write.csv(df_clean, "../data/cleaned_data.csv", row.names = FALSE)

dim(df_clean)
str(df_clean)
head(df_clean)
View(df_clean)
