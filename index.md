---
layout: tutorial
title: Understanding and Building a Rainfall-Runoff Model
subtitle: A Step-by-Step Guide
date: 2024-26-11 10:00:00
author: Holly Bacon
tags: modelling      
---

<div class="block">
	<center>
		<img src="{{ site.baseurl }}/Figures/Tut_title.png" alt="Img">
	</center>

 </div>
<div style="text-align: center;">
  <img src="Figures/Tut_title.png" width="1000" height="400">
</div> 

*Created by Holly Bacon* 

## Tutorial Aims:

1. Learn the importance of Rainfall-Runoff models, what they do and why we need them.
2. Understand how we decide model parameters.
3. Become confident in creating a Rainfall-Runoff model that can accurately predict flow values that match up with real values.

## Tutorial Steps:

## <a href="#1"> 1. What is a Rainfall-Runoff Model?</a>

#### <a href="#1.2"> 1a. Why do we want to do this?</a>

## <a href="#2"> 2. Data preparation</a>

#### <a href="#2"> 2a. Install packages and load data</a>

#### <a href="#2"> 2b. Remove metadata, name and select columns</a>

#### <a href="#2"> 2c. Using `left_join()` to merge datasets</a>

#### <a href="#2"> 2d. Filter dataset to 2015-2017</a>

#### <a href="#2"> 2e. Fix up the units</a>


<a name="1"></a>
### 1. What is a Rainfall-Runoff model?

Welcome! Here, you're going to learn the basis of creating a Rainfall-Runoff model. But, you might be wondering... okay but what is a Rainfall-Runoff model? If you're not familiar with this, that's okay. Let's go over it:

Let's zoom right out and look at the Hydrological cycle. 

<div style="text-align: center;">
  <img src="Figures/Hydrologic_cycle.png" width="700" height="400">
</div>

Here, we can see precipitation enters the system and either:
- runs straight to the channel (surface runoff),
- infiltrates into the ground
- or evapotranspires back up.

So.. what if we created a model that could predict the average flow of water through a river each month in any year. 

<a name="1.2"></a>
### 1a. Why do we want to do this?

Runoff models aim to successfully track changes in water availability, floods and droughts over time (Jehanzaib et al, 2022). 

For example, imagine you're a hydrologist studying a Scottish catchment. You have access to precipitation and daily flow data from 1970-now. Based on your understanding of the catchment characteristics - such as, soil type, topography, land cover, evapotranspiration - you can create a model that reflects the observed trends and patterns within this catchment. By adjusting parameters, such as infiltration rates, interception rates and surface runoff coefficients, you can fine-tune the model to mimic how the hydrological cycle behaves in the region.

At the end, the model will provide a predicted flow, which can be plotted against the observed daily flow to see how well your model performs. Do the lines match up? Did it capture seasonal trends? If the lines match up, this means you've successfully created a Rainfall-Runoff model that captures the hydrological dynamics of your catchment. 

Once validated, the model becomes a powerful resource. It can be used to predict future flow rates based on estimated precipitation levels, providing valuable insights for mitigating flood risks and improving water resource management. By simulating different scenarios, such as changes in land use or climate, hydrologists can use runoff models in decision-making and improve resilience to extreme climatic events.

> **_TIP:_**
If you don't have much experience with R, you should check out some of the Coding Club tutorials such as, "Intro to R" (https://ourcodingclub.github.io/tutorials/intro-to-r/) to get a grip of the basics. This tutorial will also incorparate various functions from the `dplyr` package, therefore the "Basic data manipulation" tutorial (https://ourcodingclub.github.io/tutorials/data-manip-intro/) will also be very useful if you've never used the `dplyr` package before.

<a name="2"></a>
### 2. Data preparation  

> **_TIP:_**
All the files you need to complete this tutorial can be downloaded from this <a href="https://github.com/EdDataScienceEES/tutorial-hollybee04.git" target="_blank" markdown="1">repository</a>. Click code, download the URL and paste in a new project in R Studio. 
Open a new script, write a title, your name, the date and load in the packages and data. Throughout this tutorial you can copy the code boxes into your own script. Remember # give extra context and explain what the code is doing! 

The UK Centre for Ecology and Hydrology (https://nrfa.ceh.ac.uk/data/search) collects precipitation and daily flow data across the whole of the UK, as well as detailed catchment info. For this tutorial, we're going to be using the Tweed at Peebles in Scotland. 

<a name="2.1"></a>
### 2a. Install packages and load data
```r
# Tutorial: Understanding and Building a Rainfall-Runoff Model
# Written by ...
# Date 

---- Library ----

library(nasapower) # for downloading evapotranspiration data 
library(dplyr) # for data manipulation
library(ggplot2) # for data visualisation
library(lubridate) # for data handling

---- Load data ----

Flow <- read.csv("data/Daily_flow.csv")
Precipitation <- read.csv("data/Rainfall_Data.csv")

# Download Et data from NASA! (nasapower)

# Set coordinates for the Tweed catchment.

Latitude <- 55.647
Longitude <- -3.179

Evapotranspiration <- get_power(
  community = "AG", # AG = agriculture
  pars = "EVPTRNS",  # Evapotranspiration
  lonlat = c(longitude, latitude),
  temporal_api = "daily",
  dates = c("2015-01-01", "2017-12-31") # Data range. We are choosing to base the model on these 3 years but you could do different years or for a longer time. 
)

```

If you were to pick a different catchment, you would download data specific to that area, including latitude and longitude values. I found these on google maps by matching up roughly where the rain guage was on CEH.

<a name="2.2"></a>
### 2b. Remove metadata, name and select columns

```r
---- Data preparation ----

# Remove first 19 rows of metadata

Flow_clean <- Flow[20:nrow(Flow), ] # This excludes the first 19 rows and only rows 20 and on are kept in the new data frame "Flow_clean".

Precipitation_clean <- Precipitation[20:nrow(Precipitation), ]

# Name the new columns

colnames(Flow_clean) <- c("Date", "Daily_flow_m3pers") # CEH states the daily flow data is recorded in m3/s.

colnames(Precipitation_clean) <- c("Date", "Precipitation_mm")

# Select necessary columns

Flow_clean <- Flow_clean %>%
  select(Date, Daily_flow_m3pers )

Precipitation_clean <- Precipitation_clean %>%
  select(Date, Precipitation_mm )

Evapotranspiration_clean <- Evapotranspiration %>%
  select(YEAR, MM, DD, DOY, YYYYMMDD, EVPTRNS)

# Name Et columns 

colnames(Evapotranspiration_clean) <- c("Year", "MM", "DD", "DOY", "Date", "Et_mm")

Evapotranspiration_clean$MM <- month.abb[Evapotranspiration_clean$MM] # Change to names instead of numeric month values. "month.abb" = 3 letter abreviations for the months.

```

If you're confused what the pipes (`%>%`) are doing, head to the 'Efficient data manipulation' tutorial (https://ourcodingclub.github.io/tutorials/data-manip-efficient/) which introduces pipes. If you just need a quick reminder - pipes chain operations together in a more efficient and readable way! 

<a name="2.3"></a>
### 2c. Using `left_join()` to merge datasets

Great! Now we have all the data we need and it's pretty organised now. But it's all seperate. Lets combine the 3 datasets together using a cool `dplyr` function called `left_join` 

> **_TIP:_**
Why I love `left_join()`: 
It's really easy to use and allows you to combine data sets together, but specifically when you want to keep the rows from the left data frame and add matching values from the right data frame. Where values don't match, an NA will appear. This is useful when you have collected data on multiple different variables and so you have may have separate data frames.

```r
Merge 3 datasets ----

# First merge flow and precipitation data together by matching dates 

Merged_data <- Flow_clean %>%
left_join(Precipitation_clean, by = "Date")
# Merged_data now contains both flow and precipitation. Now lets join Et.

Final_merged_data <- Merged_data %>%
  left_join(Evapotranspiration_clean, by = "Date") # Doesn't work!

```

HANG ON! Notice when you try to join Et on an error appears:

<div style="text-align: center;">
  <img src="Figures/Left_join_error.png" width="500" height="100">
</div>

This is because the date column in Flow and Precipitation (x$Date) is a character but the date column in Et is in 'date' form (use `str(Evapotranspiration_clean$Date)` if you don't believe me!). This means we need to change the date column in Et to a character!

```r
# Change Evapotranspiration_clean to a character to match with other data sets
Evapotranspiration_clean$Date <- as.character(Evapotranspiration_clean$Date)

# Try again!
Final_merged_data <- Merged_data %>%
  left_join(Evapotranspiration_clean, by = "Date")

```

You can also use this when you need to change a column to a factor or numeric: `as.factor`, `as.numeric`. 

OKAY! Now we have one big data set containing all the information we need. What's next?

<a name="2.4"></a>
### 2d. Filter dataset to 2015-2017

Now we need to filter to the years we want to calibrate our model with (2015-2017). We can do this by using the `filter()` function which selects rows based on the conditions we set. 

```r
# Filter to 2015-2017 ----

# Filter for dates within the range
Filtered_data <- Final_merged_data %>% 
  filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2017-12-31"))

```

- Condition 1: "Date >= as.Date("2015-01-01")" keeps rows where the Date column is greater than or equal to January 1st 2015. "as.Date" ensures date is treated as a date and not just text.
- Condition 2: "Date <= as.Date("2017-12-31")" keeps rows where the Date column is less than or equal to December 31st 2017.

<a name="2.5"></a>
### 2e. Fix up the units

Now we have our dataset with all the information we need AND filtered to the right timeline. The next step is to ensure we have the correct units. In hydrological modelling we often use m to maintain consistency with other variables (such as flow). We can leave Et as it is as we are only going to be visualising Et trends to help us decide one of the parameters. 

Remember before we changed the 'date' column to a character to merge two data sets. Well, now we're dealing with numbers and maths, we need to change precipitation from a character to a numeric value! Luckily, we already know how to do this. Once we've converted it, we then need to add up all the daily precipitation values for each month as we only really want one value per month telling us.. okay there was this amount of rain in January or there was this amount of rain in March. The same for flow too!

```r
# Fix units ----

# Precipitation conversion
# mm ----> m
# Divide by 1000. 

str(Filtered_data$Precipitation_m) # Character

# Change to numeric
Filtered_data$Precipitation_mm <- as.numeric(Filtered_data$Precipitation_mm) 

# Create a new column with the precipitation in metres.
Filtered_data$Precipitation_m <- Filtered_data$Precipitation_mm / 1000

# Now we only want one precipitation value each month, so we need to sum each month up:
Filtered_data <- Filtered_data %>%
  group_by(Year, MM) %>% # Group by year and month so it knows to sum up jan of 2015, feb of 2015..., jan of 2016 and so on. 
  mutate(Monthly_precipitation_m = sum(Precipitation_m)) # Create a new column containing monthly precipitation. 

# Daily flow luckily came in the correct units = m3/s (cubic metres per second)
# But, like precipitation, we only want one summed flow value for the whole month!
# So lets first get each discharge per day instead of per secs then add up all the days to get a sum of the month!

# Remember to change Daily flow m3/s to numeric NOT character. 
Filtered_data$Daily_flow_m3pers <- as.numeric(Filtered_data$Daily_flow_m3pers)

# Times each Daily flow by the number of seconds in a day (86400) then by days in the month. 
Filtered_data <- Filtered_data %>%
  group_by(Year, DD) %>% # DD = day, because we're focus on individual days here. 
  mutate(Daily_flow_m3 = Daily_flow_m3pers * 86400) %>%
  ungroup() %>% 
  group_by(Year, MM) %>% # Now we're focusing on months to get the monthly flow. 
  mutate(Monthly_flow_m3 = sum(Daily_flow_m3)) %>%
  ungroup()
```

> **_TIP:_**
ALWAYS remember to `ungroup()` after you're done with that operation to make sure operations later on are not affected by certain columns being grouped together.

PERFECT! Well done. Now, we have successfully prepared our data and we're almost ready to start building our model. Before we jump into creating the model, why not we plot out the observed ("real") flow values that were measured during 2015, 2016 and 2017 to get an idea of the general trends, similarities and differences between the three years. 

<a name="3"></a>
### 3. Visualising the observed flow values

The aim of a Rainfall-Runoff model is to match up the predicted channel discharge values that the model produces with the observed values, measured in real life. This can be difficult because it will be tempting to adjust parameters as much as possible to align with real life, but we must avoid this because the parameters must attempt to reflect the hydrological processes within the catchment and the value we decide for them has to be justified by a hydrological process or characteristic observed in the catchment. Over calibrating and tweaking parameters for no reason, reduces accuracy in the model predicting extreme events if it doesn't align with the correct processes and characteristics. 

Okay, enough of that! Let's check out these observed flows. 

We're going to be using the `lubricate` package to note down the number of days in each month into a column called `Days_in_month`. This saves us having to go through each month and state the number of days. That would take ages! 

```r

# Calculate observed flow (m3/s)
Observed_values <- Filtered_data %>%
  mutate(
    Days_in_month = days_in_month(ymd(paste(Year, MM, "01"))), # Get the number of days in the month
    Observed_flow_m3pers = Monthly_flow_m3 / (Days_in_month * 24 * 60 * 60) # Convert to m³/s
  ) %>%
  group_by(Year, MM) %>%
  summarize(
    Observed_flow_m3/s = first(Observed_flow_m3pers), # It's going to be the same value for every day of the month so let's only keep only one value per group
    .groups = "drop" # Ungroup after summarizing
  )

# Plot time!

# Use ggplot to visualise flow over time
ggplot(Observed_values, aes(x = MM, y = Observed_flow_m3pers, group = Year, color = factor(Year))) + # group ensures data from the same year are on the same line. colour ensures a different colour for each line. factor ensures year is treated as a categorical variable and not numeric. 
  geom_line() +
  labs(
    x = "Month",
    y = "Observed flow (m³/s)",
    color = "Year"
  ) +
  theme_minimal()
```

<center> <img src="{{ site.baseurl }}/Figures/Flow.month.png" alt="Img" style="width: 800px;"/> </center>


# consider a facet

ggplot(Observed_values, aes(x = MM, y = Observed_flow)) +
  geom_line(aes(group = Year, color = factor(Year))) + # Line for each year
  geom_point(aes(color = factor(Year))) +             # Points for clarity
  facet_wrap(~ Year) +                                # Create a facet for each year
  labs(
    x = "Month",
    y = "Observed Flow",
    color = "Year"
  ) +
  theme_minimal()


# 5 ---- PARAMETERS ----


# 5.1 Loss term 1 (L1) ----

# L1 = Amount of water that reaches the surface AFTER interception, AFTER evapotranspiration!!! 
# 0.2 means 20% of water reaches the surface. 
# 0.6 means 60% of water reaches the surface.

# But hang on, its gonna differ seasonally. 
# Lets look at Et levels throughout the years 
# Set Month and year to a factor with the correct order
Filtered_data <- Filtered_data %>%
  mutate(
    MM = factor(
      MM,
      levels = month.abb  # Levels set to the correct order (Jan, Feb, ..., Dec)
    ),
    Year = as.factor(Year)
  )

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
