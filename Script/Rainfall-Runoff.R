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

Flow <- read.csv("data/Daily_Flow.csv")
Rainfall <- read.csv("data/Rainfall_Data.csv")

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

Flow_clean <- Flow[20:nrow(Flow), ]

Rainfall_clean <- Rainfall[20:nrow(Rainfall), ]

# Name them

colnames(Flow_clean) <- c("Date", "Flow")

colnames(Rainfall_clean) <- c("Date", "Rainfall")

# Select necessary columns

Flow_clean <- Flow_clean %>%
  select(Date, Flow)

Rainfall_clean <- Rainfall_clean %>%
  select(Date, Rainfall)

Evapotranspiration_clean <- Evapotranspiration %>%
  select(YYYYMMDD, EVPTRNS)

colnames(Evapotranspiration_clean) <- c("Date", "Evapotranspiration")

# Merge the 3 datasets!

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

