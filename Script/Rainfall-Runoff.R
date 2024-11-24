# Tutorial: How to make a Rainfall-Runoff Model
# Written by Holly Bacon 2024

# 1. ---- Library ----

library(nasapower)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(purrr)

# 2. ---- Load data ----

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

# 3. ---- Data preparation ----

# Remove first 19 rows of metadata

Discharge_clean <- Discharge[20:nrow(Discharge), ]

Precipitation_clean <- Precipitation[20:nrow(Precipitation), ]

# Name them

colnames(Discharge_clean) <- c("Date", "Daily_flow_m3pers") # m3/s

colnames(Precipitation_clean) <- c("Date", "Precipitation_mm")

# Select necessary columns

Discharge_clean <- Discharge_clean %>%
  select(Date, Daily_flow_m3pers )

Precipitation_clean <- Precipitation_clean %>%
  select(Date, Precipitation_mm )

Evapotranspiration_clean <- Evapotranspiration %>%
  select(YEAR, MM, DD, DOY, YYYYMMDD, EVPTRNS)

colnames(Evapotranspiration_clean) <- c("Year", "MM", "DD", "DOY", "Date", "Et_mm")

Evapotranspiration_clean$MM <- month.abb[Evapotranspiration_clean$MM] # Change to names instead of numeric month values.

# 3.1 Merge 3 datasets ----

Merged_data <- Discharge_clean %>%
  left_join(Precipitation_clean, by = "Date")

# Change Evapotranspiration_clean to a character to match with other data sets
Evapotranspiration_clean$Date <- as.character(Evapotranspiration_clean$Date)

Final_merged_data <- Merged_data %>%
  left_join(Evapotranspiration_clean, by = "Date")

# 3.2 Filter to 2015-2017 ----

# Filter for dates within the range
Filtered_data <- Final_merged_data %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2017-12-31"))

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

library(dplyr)

Rainfall_Runoff_Model <- Data %>%
  
  # Select relevant columns
  select(Year, MM, DD, Monthly_precipitation_m, Observed_flow) %>%
  
  # Group by Year
  group_by(Year) %>%
  
  # Apply the calculations
  mutate(
    # Area constant
    Area = 694,
    
    # Total water input (m³)
    Total_water_input = Monthly_precipitation_m * Area * 1000000,  # Convert km² to m²
    
    # Constants
    C1 = 0.6,
    C2 = 0.3,
    C3 = 0.55
  ) %>%
  
  # For January, set Initial Surface Storage to 0
  mutate(
    Initial_surface_storage = if_else(MM == "Jan", 0, lag(Surface_storage, default = 0))
  ) %>%
  
  # Sequential calculation for Surface_storage (using lag to carry over values from the previous month)
  mutate(
    Surface_storage = if_else(MM == "Jan", 0, lag(Surface_storage, default = 0) * (1 - (C1 + C2)) + Total_water_input * 0.8),
    
    # Surface to channel
    Surface_to_channel = C1 * Surface_storage,
    
    # Surface to ground
    Surface_to_ground = C2 * Surface_storage
  ) %>%
  
  # Sequential calculation for Ground_storage
  mutate(
    Ground_storage = if_else(MM == "Jan", 0, lag(Ground_storage, default = 0) + Surface_to_ground * 0.2),
    
    # Ground to channel
    Ground_to_channel = C3 * Ground_storage,
    
    # Predicted channel input (sum of surface and ground to channel)
    Pred_channel_input = Surface_to_channel + Ground_to_channel,
    
    # Predicted mean channel discharge (divide by seconds in the month)
    Pred_mean_channel_discharge = Pred_channel_input / (31 * 24 * 60 * 60)  # Seconds in a month
  ) %>%
  
  # Ungroup after calculations
  ungroup()

# View the final output
print(Rainfall_Runoff_Model)



# Start with January first. Because we cant get feb values until we have Jan and so on...



# JANUARY *****

Rainfall_Runoff_Model <- Data %>%
  
  # Select years from Data that we need
  select(Year, MM, DD, Monthly_precipitation_m, Observed_flow) %>%
  
  # 6.1 INITIAL SURFACE STORAGE (CU/M)  
  mutate(Initial_surface_storage = if_else(MM == "Jan", 0, NA_real_),

         # 6.3 AREA (KM2)
         Area = 694,
         
         # 6.4 TOTAL WATER INPUT 
         # This is the total water across the whole area.

         Total_water_input = Monthly_precipitation_m * Area * 1000000, # 1million because km2 --> m2 

         # 6.4 SURFACE STORAGE 
         # Initial surface storage PLUS our parameter L1 and times the total water input.
         
         Surface_storage = Initial_surface_storage + 0.8 * Total_water_input, # Only for Jan. 

         # 6.5 C1, C2 + C3

         C1 = 0.6,
         C2 = 0.3,
         C3 = 0.55,
         
         # 6.5 SURFACE TO CHANNEL
         Surface_to_channel = C1 * Surface_storage,
         
         # 6.6 SURFACE TO GROUND
         Surface_to_ground = C2 * Surface_storage, 
         
         # 6.7 INITIAL GROUND STORAGE
         Initial_ground_storage = 0, #0 for Jan
         
         # 6.8 GROUND STORAGE 
         Ground_storage = if_else(MM == "Jan", 0, lag(Ground_storage, default = 0) + Surface_to_ground * 0.2),
         
         # 6.9 GROUND TO CHANNEL
         Ground_to_channel = C3 * Ground_storage,
         
         # 7 PREDICTED VALUES ----
         # Predicted channel input
         Pred_channel_input = Surface_to_channel + Ground_to_channel,
         
         # Use pred channel input to calculate pred mean channel discharge, our final value! 
         Pred_mean_channel_discharge = Pred_channel_input / (31*24*60*60))

# Perfect now we have January we can use this same template to apply it to the rest of the year. 

# REST OF YEAR *****

Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  
  # 6.1 INITIAL SURFACE STORAGE (CU/M)  
  # This is the amount of water held in the surface storage from previous months. 
  # To calculate this we add C1 AND C2 together to get x 
  # Then 1-x *100 = initial surface storage 
  mutate(
    # Initialize surface storage
    
    # Calculate leftover percentage for months other than January
    Initial_surface_storage = if_else(
      MM != "01",
      lag(Surface_storage) * (1 - (C1 + C2)),  # Use previous month's surface storage
      Initial_surface_storage,  # Keep 0 for January
  
         # 6.3 AREA (KM2)
         Area = 694,
         
         # 6.4 TOTAL WATER INPUT 
         # This is the total water across the whole area.
         
         Total_water_input = Monthly_precipitation_m * Area * 1000000, # 1million because km2 --> m2 
         
         # 6.4 SURFACE STORAGE 
         # Initial surface storage PLUS our parameter L1 and times the total water input.
         
         Surface_storage = if_else(MM == "Jan", 0, lag(Surface_storage, default = 0) * (1 - (C1 + C2)) + Total_water_input * 0.8),
         
         # 6.5 C1, C2 + C3
         
         C1 = 0.6,
         C2 = 0.3,
         C3 = 0.55,
         
         # 6.5 SURFACE TO CHANNEL
         Surface_to_channel = C1 * Surface_storage,
         
         # 6.6 SURFACE TO GROUND
         Surface_to_ground = C2 * Surface_storage, 
         
         # 6.7 INITIAL GROUND STORAGE
         Initial_ground_storage = 0, #0 for Jan
         
         # 6.8 GROUND STORAGE 
         Ground_storage = Initial_surface_storage + 0.2 * Surface_to_ground, # 0.2 = L2 parameter
         
         # 6.9 GROUND TO CHANNEL
         Ground_to_channel = C3 * Ground_storage,
         
         # 7 PREDICTED VALUES ----
         # Predicted channel input
         Pred_channel_input = Surface_to_channel + Ground_to_channel,
         
         # Use pred channel input to calculate pred mean channel discharge, our final value! 
         Pred_mean_channel_discharge = Pred_channel_input / (31*24*60*60))

# Perfect now we have January we can use this same template to apply it to the rest of the year. 



# 8 ---- Time to compare ----

ggplot(Rainfall_Runoff_Model, aes(x = MM)) + 
  geom_line(aes(y = Observed_flow, color = "Observed Flow"), size = 1) +  # Observed
  geom_line(aes(y = Pred_mean_channel_discharge, color = "Predicted Flow"), size = 1, linetype = "dashed") +  # Predicted
  facet_wrap(~ Year, scales = "free_y") +  # Optional: Separate facets for each year
  scale_color_manual(values = c("Observed Flow" = "blue", "Predicted Flow" = "red")) +  # Custom colors
  labs(
    title = "Comparison of Observed and Predicted Flow (2015–2017)",
    x = "Month",
    y = "Flow (m³/s)",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5),  # Center-align title
    legend.position = "top"  # Position legend above the plot
  )




