# ML1 Project
# Modelling-Food-Delivery-Time
# KB CC WX

#Load and inspect the dataset.
# Read the CSV file
getwd()
list.files()

library(tidyverse)
library(dplyr)
library(readr) 
df <- read.csv("../data/Food_Time_Data_Set.csv")

dim(df)
head(df)
summary(df)

# data cleaning
# Count NA values in each column
colSums(is.na(df))

# Clean data: Remove NA values and filter valid coordinates
# Data Cleaning
df_clean <- df %>%
  select(-X) %>%  
  mutate(
    Distance..km. = as.numeric(na_if(gsub("[^0-9\\.]", "", Distance..km.), "")),  
    TARGET = as.numeric(na_if(gsub("[^0-9\\.]", "", TARGET), "")),
    # Convert categorical variables to factors
    type_of_order_factor = factor(`Type_of_order`),
    type_of_vehicle_factor = factor(`Type_of_vehicle`),
    
    Traffic_Level_cleaned = str_trim(tolower(`Traffic_Level`)),
    Traffic_Level_factor = factor(`Traffic_Level`),
    
    weather_description_cleaned = str_trim(tolower(`weather_description`)),  
    weather_descriptions_factor = factor(case_when(
      weather_description_cleaned %in% c("broken clouds", "clear sky", "few clouds", "overcast clouds", "scattered clouds") ~ "Clear",
      weather_description_cleaned %in% c("fog", "haze", "smoke") ~ "Mild Weather",
      weather_description_cleaned %in% c("mist", "moderate rain", "light rain") ~ "Rainy",
      TRUE ~ NA_character_
    ), levels = c("Clear", "Poor Visibility", "Rainy"), ordered = TRUE),
    Long_Delivery_Flag= if_else(TARGET >= 40, 1, 0),
    speed = if_else(TARGET > 0, as.integer(Distance..km. *60/ TARGET), NA_real_)  
  ) %>%
  
  drop_na() %>%
  filter(
    Delivery_location_longitude >= 60, Restaurant_longitude >= 60,  
    Delivery_location_latitude >= 0, Restaurant_latitude >= 0       
  )

# Check structure after cleaning
str(df_clean)

# View summary to check for remaining NA values
summary(df_clean)
dim(df_clean)

# View the cleaned dataset
head(df_clean)
write.csv(df_clean, "../data/cleaned_data.csv", row.names = FALSE)

