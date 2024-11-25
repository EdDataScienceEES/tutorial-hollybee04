# Tutorial: How to make a Rainfall-Runoff Model
# Written by Holly Bacon 2024

# 1. ---- Library ----

library(nasapower)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)


# 2. ---- Load data ----

Flow <- read.csv("data/Daily_Flow.csv")
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

# 3. ---- Data preparation ----

# Remove first 19 rows of metadata

Flow_clean <- Flow[20:nrow(Flow), ]

Precipitation_clean <- Precipitation[20:nrow(Precipitation), ]

# Name them

colnames(Flow_clean) <- c("Date", "Daily_flow_m3pers") # m3/s

colnames(Precipitation_clean) <- c("Date", "Precipitation_mm")

# Select necessary columns

Flow_clean <- Flow_clean %>%
  select(Date, Daily_flow_m3pers )

Precipitation_clean <- Precipitation_clean %>%
  select(Date, Precipitation_mm )

Evapotranspiration_clean <- Evapotranspiration %>%
  select(YEAR, MM, DD, DOY, YYYYMMDD, EVPTRNS)

colnames(Evapotranspiration_clean) <- c("Year", "MM", "DD", "DOY", "Date", "Et_mm")

Evapotranspiration_clean$MM <- month.abb[Evapotranspiration_clean$MM] # Change to names instead of numeric month values.

# 3.1 Merge 3 datasets ----

Merged_data <- Flow_clean %>%
  left_join(Precipitation_clean, by = "Date")

# Change Evapotranspiration_clean to a character to match with other data sets
Evapotranspiration_clean$Date <- as.character(Evapotranspiration_clean$Date)

Final_merged_data <- Merged_data %>%
  left_join(Evapotranspiration_clean, by = "Date")

# 3.2 Filter to 2015-2017 ----

# Filter for dates within the range
Filtered_data <- Final_merged_data %>% 
  filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2017-12-31"))

# 3.3 Fix units ----

# Precipitation conversion

# First, we need Precipitation to be in metres
# mm ----> m
# To do this, we just need to divide by 1000. 

# Convert "Rainfall" from mm to meters (creating a new column)
Filtered_data$Precipitation_m <- Filtered_data$Precipitation_mm / 1000
# Not working, check type

str(Filtered_data$Precipitation_m) # Character

Filtered_data$Precipitation_mm <- as.numeric(Filtered_data$Precipitation_mm) # Change to numeric

# Try again
Filtered_data$Precipitation_m <- Filtered_data$Precipitation_mm / 1000

# Now we only want one precipitation value each month, so we need to sum each month up:
Filtered_data <- Filtered_data %>%
  group_by(Year, MM) %>%
  mutate(Monthly_precipitation_m = sum(Precipitation_m))


# Daily flow luckily came in the correct units = m3/s (cubic metres per second)
# But we only want one summed discharge value for the whole month!
# So lets first get each discharge per day instead of per secs then add up all the days to get a sum of the month!

# Remember to change Daily flow m3/s to numeric NOT character. 
Filtered_data$Daily_flow_m3pers <- as.numeric(Filtered_data$Daily_flow_m3pers)

# Times each Daily flow by the number of seconds in a day (86400) then by days in the month. 
Filtered_data <- Filtered_data %>%
  group_by(Year, DD) %>%
  mutate(Daily_flow_m3 = Daily_flow_m3pers * 86400) %>%
  ungroup() %>%
  group_by(Year, MM) %>%
  mutate(Monthly_flow_m3 = sum(Daily_flow_m3)) %>%
  ungroup() 

# Set Month and year to a factor with the correct order
Filtered_data <- Filtered_data %>%
  mutate(
    MM = factor(
      MM,
      levels = month.abb  # Levels set to the correct order (Jan, Feb, ..., Dec)
    ),
    Year = as.factor(Year)
  )

Data <- Filtered_data

# 4 ---- OBSERVED VALUES ----

# Lets calculate these, to get a jist of what were aiming for. 

Data <- Data %>%
  group_by(Year, MM) %>%  # Group by both Year and Month
  mutate(Observed_flow = Monthly_flow_m3 / (31*24*60*60)) %>%
  ungroup()

# 5 ---- PARAMETERS ----


# 5.1 Loss term 1 (L1) ----

# L1 = Amount of water that reaches the surface AFTER interception, AFTER evapotranspiration!!! 
# 0.2 means 20% of water reaches the surface. 
# 0.6 means 60% of water reaches the surface.

# But hang on, its gonna differ seasonally. 
# Lets look at Et levels throughout the years 

ggplot(Data, aes(x = MM, y = Et_mm, color = Year, group = Year)) + 
  geom_point() +
  labs(title = "Et Levels 2015-2017", 
       x = "Month", 
       y = "Evapotranspiration (mm)") +
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

# Based on the Et graph, we may choose to separate into seasons. 

# So Nov-March = Little Et, little interception by urban/woodland therefore we may expect a large amount of rainfall to reach the surface. 

# 0.8 = 80% of rainfall reaches the surface. 20% is either intercepted or evapotranspirated.

# As for the other months, April-May and Aug-Oct with mid Et, a parameter of 0.5 may make more sense

# And for June to July with v v v high Et, a parameter of 0.2 makes sense. 

# 5.2 Surface to Channel (C1) ----

# Topography, soil type - permeable, impermeable? 

# Low permeability 
# UPLAND = STEEP = FAST RUNOFF
# However, it does have floodplains which can store water in flood events, reducing runoff.
# But generally, we would expect this number to be high. 
# C1 + C2 CANT EQUAL 1 OR MORE THAN 1 = STORAGE IS EMPTY

# 0.6 = 60% of water goes straight to channel.

# 5.3 Surface to ground (C2) ----

# Low permeability

# 0.3

# 5.4 Ground to channel (C3) ----

# 0.55

# 5.5 Loss term 2 (L2) ----

# How much leakage occurs? 

# This is hard, you may have to research for this.

# But lets just say 20% for now. 

# 6 ---- BUILDING THE MODEL ----

# Aggregate daily data into monthly data so we only have 1 row each month
Monthly_data <- Data %>%
  group_by(Year, MM) %>% # Group by Year and Month
  summarize(
    # Retain the first value for columns that don't vary within the month
    Monthly_precipitation_m = first(Monthly_precipitation_m),
    Observed_flow = first(Observed_flow),
    .groups = "drop" # Ungroup after summarizing
  )

# Create columns of each stage in the model
Rainfall_Runoff_Model <- Monthly_data %>%
  mutate(
    Area = 694,
    Initial_surface_storage = 0,
    Initial_ground_storage = 0,
    C1 = 0.6,
    C2 = 0.3,
    C3 = 0.55,
    L2 = 0.2,
    Surface_storage = NA,
    Ground_storage = NA,
    Surface_to_channel = NA,
    Surface_to_ground = NA,
    Ground_to_channel = NA,
    Pred_channel_input = NA,
    Pred_mean_channel_discharge = NA
  )

# Iterate over months in the data
for (i in 1:nrow(Rainfall_Runoff_Model)) {
  
  # Extract the current row
  current_month <- Rainfall_Runoff_Model[i, ]
  
  # Calculate total water input
  Total_water_input <- current_month$Monthly_precipitation_m * current_month$Area * 1000000
  
  # Calculate surface storage
  Surface_storage <- current_month$Initial_surface_storage + 0.8 * Total_water_input
  
  # Calculate surface to channel and surface to ground
  Surface_to_channel <- current_month$C1 * Surface_storage
  Surface_to_ground <- current_month$C2 * Surface_storage
  
  # Calculate ground storage
  Ground_storage <- current_month$Initial_ground_storage + current_month$L2 + Surface_to_ground
  
  # Calculate ground to channel
  Ground_to_channel <- current_month$C3 * Ground_storage
  
  # Predicted channel input
  Pred_channel_input <- Surface_to_channel + Ground_to_channel
  
  # Predicted mean channel discharge
  Pred_mean_channel_discharge <- Pred_channel_input / (31 * 24 * 60 * 60)  # Assuming all months have 31 days
  
  # Update the values in the dataset
  Rainfall_Runoff_Model$Surface_storage[i] <- Surface_storage
  Rainfall_Runoff_Model$Ground_storage[i] <- Ground_storage
  Rainfall_Runoff_Model$Surface_to_channel[i] <- Surface_to_channel
  Rainfall_Runoff_Model$Surface_to_ground[i] <- Surface_to_ground
  Rainfall_Runoff_Model$Ground_to_channel[i] <- Ground_to_channel
  Rainfall_Runoff_Model$Pred_channel_input[i] <- Pred_channel_input
  Rainfall_Runoff_Model$Pred_mean_channel_discharge[i] <- Pred_mean_channel_discharge
  
  # Update initial values for the next month, if it exists
  if (i < nrow(Rainfall_Runoff_Model)) {
    Rainfall_Runoff_Model$Initial_surface_storage[i + 1] <- Surface_storage * (1 - current_month$C1 - current_month$C2)
    Rainfall_Runoff_Model$Initial_ground_storage[i + 1] <- Ground_storage - Ground_to_channel
  }
}

# 8 ---- Time to compare ----

# Assuming 'Rainfall_Runoff_Model' has columns: Date, Rainfall, Runoff

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  # Create a 'Date' column with first day of each month
  mutate(Date = seq(ymd("2015-01-01"), by = "month", length.out = nrow(Rainfall_Runoff_Model)))

# PLOT!!!!!
ggplot(Rainfall_Runoff_Model, aes(x = Date)) +
  geom_line(aes(y = Pred_mean_channel_discharge, color = "Predicted mean channel discharge"), size = 1) +
  geom_line(aes(y = Observed_flow, color = "Observed flow"), size = 1, linetype = "dashed") +
  labs(
    title = "Rainfall and Runoff Over Time",
    x = "Date",
    y = "Flow (m³)",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Predicted mean channel discharge" = "blue", "Observed flow" = "red")) +
  theme_minimal()

# YAYYY




