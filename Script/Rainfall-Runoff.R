# Tutorial: How to make a Rainfall-Runoff Model
# Written by Holly Bacon 2024

# ---- Library ----

install.packages("nasapower")
library(nasapower)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# ---- Load data ----

Discharge <- read.csv("data/Daily_Flow.csv")
Precipitation <- read.csv("data/Rainfall_Data.csv")

# Download evapotranspiration data from NASA!

# Set coordinates for the Tweed catchment

latitude <- 55.647
longitude <- -3.179

# Get evapotranspiration data

Evapotranspiration <- get_power(
  community = "AG",
  pars = "EVPTRNS",  # Evapotranspiration
  lonlat = c(longitude, latitude),
  temporal_api = "daily",
  dates = c("2015-01-01", "2015-12-31")
)

# ---- Data preparation ----

# Remove first 19 rows of metadata

Discharge_clean <- Discharge[20:nrow(Discharge), ]

Precipitation_clean <- Precipitation[20:nrow(Precipitation), ]

# Name them

colnames(Discharge_clean) <- c("Date", "Discharge_m3/s")

colnames(Precipitation_clean) <- c("Date", "Precipitation_mm")

# Select necessary columns

Discharge_clean <- Discharge_clean %>%
  select(Date, Discharge_m3/s )

Precipitation_clean <- Precipitation_clean %>%
  select(Date, `Precipitation_mm )

Evapotranspiration_clean <- Evapotranspiration %>%
  select(YYYYMMDD, EVPTRNS)

colnames(Evapotranspiration_clean) <- c("Date", "Evapotranspiration_mm")

# ---- Changing each variable into the same unit ----

# Okay now we need to make sure the units are correct. 

# First, we need Precipitation to be in metres
# mm ----> m
# To do this, we just need to divide by 1000. 

# Convert "Rainfall" from mm to meters (creating a new column)
Precipitation_clean$Precipitation_m <- Precipitation_clean$Precipitation_mm / 1000



# Merge the 3 datasets!# Merge the 3 datasets!`Precipitation (mm)`

merged_data <- Flow_clean %>%
  left_join(Rainfall_clean, by = "Date")

final_merged_data <- merged_data %>%
  left_join(Evapotranspiration_clean, by = "Date")

# Check the format of the Date column in both datasets
str(merged_data$Date)
str(Evapotranspiration_clean$Date)

# Change Evapotranspiration_clean to a character to match with other data sets
Evapotranspiration_clean$Date <- as.character(Evapotranspiration_clean$Date)

# Try again
final_merged_data <- merged_data %>%
  left_join(Evapotranspiration_clean, by = "Date")

# Filter to just 2015

# Filter for dates within the range
filtered_data <- final_merged_data %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2015-12-31"))

# Change to long format and have all values together so we can plot
combined_long <- filtered_data %>%
  pivot_longer(cols = c("Rainfall", "Flow", "Evapotranspiration"), 
               names_to = "Variable", 
               values_to = "Value")
# Error, lets check they're all the same data type:
str(filtered_data)
# Flow and Rainfall are chr, need to be num like evpt

filtered_data$Rainfall <- as.numeric(filtered_data$Rainfall)
filtered_data$Flow <- as.numeric(filtered_data$Flow)

# Try again
combined_long <- filtered_data %>%
  pivot_longer(cols = c("Rainfall", "Flow", "Evapotranspiration"), 
               names_to = "Variable", 
               values_to = "Value")

# ---- Data visualisation ----

ggplot(combined_long, aes(x = Date, y = Value, color = Variable, group = Variable)) + # When plotting multiple variables with geom_line(), must specify group aesthetic to ensure lines are drawn for EACH variable. 
  geom_line() +
  labs(title = "Rainfall, Flow, and Evapotranspiration Over Time", 
       x = "Date", 
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green"))  # Customize colors

# Date x axis is just a black line becuase too many words/dates.
# Lets change data frame to months instead.

ggplot(combined_long, aes(x = Date, y = Value, color = Variable, group = Variable)) + # When plotting multiple variables with geom_line(), must specify group aesthetic to ensure lines are drawn for EACH variable. 
  geom_line() +
  labs(title = "Rainfall, Flow, and Evapotranspiration Over Time", 
       x = "Date", 
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green"))  # Customize colors
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

# Not working, check Date data type is date  
str(combined_long$Date) # Its a character
combined_long$Date <- as.Date(combined_long$Date) # Changed to date

# Third time lucky
ggplot(combined_long, aes(x = Date, y = Value, color = Variable, group = Variable)) + # When plotting multiple variables with geom_line(), must specify group aesthetic to ensure lines are drawn for EACH variable. 
  geom_line() +
  labs(title = "Rainfall, Flow, and Evapotranspiration Over Time", 
       x = "Date", 
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) +  # Customize colors
  scale_x_date(date_labels = "%b", date_breaks = "1 month") # %b displays "Jan", "Feb"

# Got it!! 


