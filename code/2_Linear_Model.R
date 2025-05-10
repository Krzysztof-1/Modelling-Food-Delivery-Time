##************** Group work ML1 - Linear regression*************

"""
Author: Curdin Caderas
Start: 10.05.2025
"""

# Libraries
"""
PLACEHOLDER
"""

#*************** Preparatory tasks: Loading Dataset and checking the dataset ***********************************

# Loading cleaned data

d.food_time <- read.csv("C:/Users/Setup/OneDrive/02_Masterstudium/03_Module/02_Semester_2/03_ML1/group_work/Modelling-Food-Delivery-Time/data/cleaned_data.csv", header = TRUE)

# Checking data (structure / head / summary / NA)

str(d.food_time)
head(d.food_time)
summary(d.food_time)
colSums(is.na(d.food_time))


#************** Investigate the variables 


# EDA before starting with linear model

hist(d.food_time$TARGET)
