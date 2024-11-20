# Tutorial: How to make a Rainfall-Runoff Model
# Written by Holly Bacon 2024

# ---- Library ----

library(nasapower)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)

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

colnames(Discharge_clean) <- c("Date", "Discharge_m3pers")

colnames(Precipitation_clean) <- c("Date", "Precipitation_mm")

# Select necessary columns

Discharge_clean <- Discharge_clean %>%
  select(Date, Discharge_m3pers )

Precipitation_clean <- Precipitation_clean %>%
  select(Date, Precipitation_mm )

Evapotranspiration_clean <- Evapotranspiration %>%
  select(YYYYMMDD, EVPTRNS)

colnames(Evapotranspiration_clean) <- c("Date", "Evapotranspiration_mm")

# ---- Fixing units ----

# Precipitation conversion ----

# First, we need Precipitation to be in metres
# mm ----> m
# To do this, we just need to divide by 1000. 

# Convert "Rainfall" from mm to meters (creating a new column)
Precipitation_clean$Precipitation_m <- Precipitation_clean$Precipitation_mm / 1000
# Not working, check type

str(Precipitation_clean$Precipitation_m) # Character

Precipitation_clean$Precipitation_mm <- as.numeric(Precipitation_clean$Precipitation_mm) # Change to numeric

# Try again
Precipitation_clean$Precipitation_m <- Precipitation_clean$Precipitation_mm / 1000

Precipitation_clean$Month <- format(Precipitation_clean$Date, "%Y-%m") # No working.

# Discharge luckily came in the correct units = m3/s (cubic metres per second)

# Dont worry about evapotranspiration right now!

# ---- Merge 3 datasets ----

merged_data <- Discharge_clean %>%
  left_join(Precipitation_clean, by = "Date")

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

# ---- Filter to just 2015 ----

# Filter for dates within the range
filtered_data <- final_merged_data %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2015-12-31"))

# Didnt work because of NA

# Remove columns with missing or empty names
final_merged_data <- final_merged_data[, !is.na(colnames(final_merged_data)) & colnames(final_merged_data) != ""]

# Again!
filtered_data <- final_merged_data %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2015-12-31"))

Data <- filtered_data

# ---- Create a new column "Month" ----

# Now lets create a new column called month which makes it easier to assign parameters monthly/seasonally

# Convert Date column to Date format if it isn't already

Data$Date <- as.Date(Data$Date) # Make sure column date is in right format called date

Data <- Data %>%
  mutate(Month = format(Date, "%b"))  # Extracts abbreviated month names (e.g., "Jan", "Feb", etc.)

# Move new month column next to date column
Data <- Data %>%
  select(Date, Month, everything()) 

# ---- Rainfall-Runoff Model ----

# The easiest way to understand this is to go through one month first, and then the rest of months will follow the same idea. 

# ---- INITIAL SURFACE STORAGE (CU/M)

# This is the amount of water held in the surface storage from previous months. For Jan, lets just start it on 0. 

Rainfall_Runoff_Model <- Data %>%
  mutate(Initial_surface_storage = ifelse(format(Date, "%m") == "01", 0, NA)))

# ---- RAINFALL

Precipitation_m <- Precipitation_clean$Precipitation_m # We already have this! 

# ---- AREA 

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  mutate(Area = 694) #km2

# ---- TOTAL WATER INPUT

# This is the total water across the whole area.

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  mutate(Total_water_input = Precipitation_m * Area * 1000000) # 1million because km2 --> m2 

# ---- SURFACE STORAGE 

# Now, we need to calculate surface storage, thinking about evapotranspiration and interception. 
# This is the amount after interception and after evapotranspiration. How much rainfall actually hits the ground? 

# For this we can plot out evapotranspiration and see how it varies throughout the year.

ggplot(Rainfall_Runoff_Model, aes(x = Month, y = Evapotranspiration_mm)) + # When plotting multiple variables with geom_line(), must specify group aesthetic to ensure lines are drawn for EACH variable. 
  geom_line() +
  labs(title = "Evapotranspiration Over Time", 
       x = "Date", 
       y = "Evapotranspiration (mm)") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")
# Now we can see how Et varies seasonally and when it peaks. 

# Nov-April low-none Et
# Apr-June and Sept-Nov mid Et
# Summer aka June-Aug VVV high!

# So what does this mean?




