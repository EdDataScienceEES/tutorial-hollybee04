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
  dates = c("2015-01-01", "2017-12-31")
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

Precipitation_clean <- Precipitation_clean %>%
  group_by(Month,Year) %>%
  mutate(sum(Precipitation_m))

# Discharge luckily came in the correct units = m3/s (cubic metres per second)
# But we only want one summed discharge value for the whole month!
# So lets first get each discharge per day instead of per secs then add up all the days to get a sum of the month!

days_in_month <- function(month, year) {
  if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {  # 31 days months
    return(31)
  } else if (month == 2) {  # February (check for leap year)
    if (year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0)) {  # Leap year
      return(29)
    } else {  # Non-leap year
      return(28)
    }
  } else {  # 30 days months
    return(30)
  }
}

Discharge_clean <- Discharge_clean %>%
  mutate(
    Year = as.numeric(format(Date, "%Y")),  # Extract year
    Month = as.numeric(format(Date, "%m"))  # Extract month
  ) %>%
  group_by(Year, Month) %>%
  mutate(
    days_in_this_month = mapply(days_in_month, Month, Year),  # Get the number of days in this month
    daily_discharge_m3 = Discharge_m3pers * 86400) %>%
  ungroup()

Discharge_clean$daily_discharge_m3 <- as.numeric(Discharge_clean$daily_discharge_m3)

str(Discharge_clean$daily_discharge_m3)

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

# ---- Filter to 2015-2017 ----

# Filter for dates within the range
filtered_data <- final_merged_data %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2017-12-31"))

# Didnt work because of NA

# Remove columns with missing or empty names
final_merged_data <- final_merged_data[, !is.na(colnames(final_merged_data)) & colnames(final_merged_data) != ""]

# Again!
filtered_data <- final_merged_data %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2017-12-31"))

Data <- filtered_data

# ---- Create a new column "Month" and "Year" ----

# Now lets create a new column called month which makes it easier to assign parameters monthly/seasonally

# Convert Date column to Date format if it isn't already

Data$Date <- as.Date(Data$Date) # Make sure column date is in right format called date

Data <- Data %>%
  mutate(Month = format(Date, "%b"))  # Extracts abbreviated month names (e.g., "Jan", "Feb", etc.)

Data$Year <- format(as.Date(Data$Date), "%Y") # New Year column to help group. 

# Move new month column next to date column
Data <- Data %>%
  select(Date, Month, Year, everything()) 

# Month is currently a categorical variable in alphabetical order so need to set to a factor. 
Data <- Data %>%
  mutate(Month = factor(format(Date, "%b"), 
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

Data <- Data %>%
  group_by(Month,Year) %>%
  mutate(sum(Precipitation_m))



# ---- CHANGING DISCHARGE UNITS ----

# Discharge luckily came in the correct units = m3/s (cubic metres per second)
# But we only want one summed discharge value for the whole month!
# So lets first get each discharge per day instead of per secs then add up all the days to get a sum of the month!

days_in_month <- function(month, year) {
  if (month %in% c(1, 3, 5, 7, 8, 10, 12)) {  # 31 days months
    return(31)
  } else if (month == 2) {  # February (check for leap year)
    if (year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0)) {  # Leap year
      return(29)
    } else {  # Non-leap year
      return(28)
    }
  } else {  # 30 days months
    return(30)
  }
}

Data <- Data %>%
  mutate(
    Year = as.numeric(format(Date, "%Y")),  # Extract year
    Month = as.numeric(format(Date, "%m"))  # Extract month
  ) %>%
  group_by(Year, Month) %>%
  mutate(
    days_in_this_month = mapply(days_in_month, Month, Year),  # Get the number of days in this month
    daily_discharge_m3 = Discharge_m3pers * 86400,  # Convert m3/s to m3/day (multiply by seconds in a day)
    total_monthly_discharge = sum(daily_discharge_m3) # Multiply by the number of days in the month
  ) %>%
  ungroup()


# ---- OBSERVED VALUES ----

# Lets calculate these, to get a jist of what were aiming for. 

Data$Discharge_m3pers <- as.numeric(as.character(Data$Discharge_m3pers)) # Ensure Discharge is numeric

Observed <- Data %>%
  select(Month, Year, Date, Discharge_m3pers, daily_discharge_m3, total_monthly_discharge) %>%
  group_by(Year, Month) %>%  # Group by both Year and Month
  mutate(OBSERVED_meanflow = total_monthly_discharge / (31*24*60*60)) %>%
  ungroup()


# ---- Rainfall-Runoff Model ----

# PARAMETERS!! ----


# L1 ----

# L1 = Amount of water that reaches the surface AFTER interception, AFTER evapotranspiration!!! 
# 0.2 means 20% of water reaches the surface. 
# 0.6 means 60% of water reaches the surface.

# But hang on, its gonna differ seasonally. 

# Lets look at evapotrasnpiration levels throughout the years 

Rainfall_Runoff_Model <- Data

# Month is currently a categorical variable in alphabetical order so need to set to a factor. 
Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  mutate(Month = factor(format(Date, "%b"), 
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))


# New year column
Rainfall_Runoff_Model$Year <- format(as.Date(Rainfall_Runoff_Model$Date), "%Y") # New Year column to help group. 


ggplot(Rainfall_Runoff_Model, aes(x = Month, y = Evapotranspiration_mm, color = Year, group = Year)) + 
  geom_point() +
  labs(title = "Evapotranspiration Levels by Year", 
       x = "Month", 
       y = "Evapotranspiration (mm)") +
  scale_color_viridis_d() +  # Optional: Use a colorblind-friendly palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nov-March low-none Et
# March, Apr and Oct mid Et
# Summer aka May-Aug VVV high!

# So what does this mean?
# More evapotranspiration = less rainfall reaching the ground. 

# Now, think about interception.

# For this look at vegetation cover over the catchment on CEH. Is it mostly urban? rural? woodland? agriculture? 
# CEH: Mostly grassland and mountain/heath/bog. With a little amount of arable and woodland and very very small section of built-up areas.

# These 2 factors (Et and interception) help us decide a loss parameter for the model called L1. 

# L1 

# Based on the Et graph, we may choose to separate into seasons. 

# So Nov-March = Little Et, little interception by urban/woodland therefore we may expect a large amount of rainfall to reach the surface. 

# 0.8 = 80% of rainfall reaches the surface. 20% is either intercepted or evapotranspirated.

# As for the other months, April-May and Aug-Oct with mid Et, a parameter of 0.5 may make more sense

# And for June to July with v v v high Et, a parameter of 0.2 makes sense. 

# C1 ----

# SURFACE --> CHANNEL 

# Topography, soil type - permeable, impermeable? 

# Low permeability 
# UPLAND = STEEP = FAST RUNOFF
# However, it does have floodplains which can store water in flood events, reducing runoff.
# But generally, we would expect this number to be high. 
# C1 + C2 CANT EQUAL 1 OR MORE THAN 1 = STORAGE IS EMPTY

# 0.6 = 60% of water goes straight to channel.

# C2 ----

# SURFACE --> GROUND

# Low permeability

# 0.3

# C3 ----

# GROUND ---> CHANNEL

# 0.55

# L2 ----

# How much leakage occurs? 

# This is hard, you may have to research for this.

# But lets just say 20% for now. 

# BUILDING THE MODEL ----

# ---- INITIAL SURFACE STORAGE (CU/M)

# This is the amount of water held in the surface storage from previous months. For Jan, lets just start it on 0. 

Rainfall_Runoff_Model <- Data %>%
  mutate(Initial_surface_storage = ifelse(format(Date, "%m") == "01", 0, NA))

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

# Initial surface storage PLUS our parameter L1 and times the total water input.

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  mutate(Surface_storage = Initial_surface_storage + 0.8 * Total_water_input) # Only for Jan. 

# ---- C1, C2 + C3

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  mutate(C1 = 0.6,
         C2 = 0.3,
         C3 = 0.55)

# ---- REMAINS

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  mutate(Surface_to_channel = C1 * Surface_storage, # to channel
         Surface_to_ground = C2 * Surface_storage, # to ground
         Initial_ground_storage = 0, #0 for Jan
         Ground_storage = Initial_surface_storage + 0.2 * Surface_to_ground, # 0.2 = L2 parameter
         Ground_to_channel = C3 * Ground_storage)

# ---- PREDICTED

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  mutate(Pred_channel_input = Surface_to_channel + Ground_to_channel,
         Pred_mean_channel_discharge = Pred_channel_input / (31*24*60*60))

# ---- OBSERVED/REALITY

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  left_join(x = Rainfall_Runoff_Model, y = Observed, by = "Date")

Plot <- Rainfall_Runoff_Model %>%
  select(Date, Year.x, Month.x, Pred_mean_channel_discharge, Observed_meanflow)



# Visualise

# Plotting both predicted and observed discharge over time
ggplot(Rainfall_Runoff_Model, aes(x = Year.x)) +
  # Plot for observed discharge
  geom_point(aes(y = Observed_meanflow, color = "Observed"), size = 1) +
  # Plot for predicted discharge
  geom_line(aes(y = Pred_mean_channel_discharge, color = "Predicted"), size = 1) +
  # Add labels and title
  labs(
    title = "Observed vs Predicted Mean Channel Discharge",
    x = "Year",
    y = "Mean Channel Discharge (m³/s)",
    color = "Legend"
  ) +
  # Customize the theme
  theme_minimal() +
  # Adjust the colors and style
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red"))

