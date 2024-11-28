---
layout: tutorial
title: Understanding and Building a Rainfall-Runoff Model
subtitle: A Step-by-Step Guide
date: 2024-26-11 10:00:00
author: Holly Bacon
tags: modelling      
---

<div style="text-align: center; width: 100%; overflow: hidden;">
  <img src="Figures/Tut_title.png" style="width: 100%; height: auto;" alt="Tutorial Title Banner">
</div>


## Tutorial Aims:

1. Learn the importance of Rainfall-Runoff models, what they do and why we need them.
2. Understand how we decide model parameters.
3. Become confident in creating a Rainfall-Runoff model that can accurately predict flow values that match up with real values.

## Tutorial Steps:

## <a href="#1"> 1. What is a Rainfall-Runoff Model?</a>

## <a href="#2"> 2. Data preparation</a>

## <a href="#3"> 3. Visualising observed flow values</a>

## <a href="#4"> 4. Parameters</a>

## <a href="#5"> 5. Building the model</a>

## <a href="#6"> 6. Time to compare predicted values VS. observed values</a>

> **_TIP:_**
All the files you need to complete this tutorial can be downloaded from this <a href="https://github.com/EdDataScienceEES/tutorial-hollybee04.git" target="_blank" markdown="1">repository</a>. Click code, download the URL and paste in a new project in R Studio. 
Open a new script, write a title, your name, the date and load in the packages and data. Throughout this tutorial you can copy the code boxes into your own script. Remember # give extra context and explain what the code is doing! 



<a name="1"></a>
## 1. What is a Rainfall-Runoff model?

Welcome! Here, you're going to learn the basis of creating a Rainfall-Runoff model. But, you might be wondering... okay but what is a Rainfall-Runoff model? If you're not familiar with this, that's okay. Let's go over it:

Let's zoom right out and look at the Hydrological cycle. 

<div style="text-align: center;">
  <img src="Figures/Hydrologic_cycle.png" width="700" height="400">
</div>

*Figure 1: Hydrological Model (Kansas Geological Survey, 2024)*

Here, we can see precipitation enters the system and either:
- Flows directly to the channel (surface runoff),
- Infiltrates into the ground
- Or returns to the atmosphere through evapotranspiration.

So.. what if we created a model that could predict the average flow of water through a river each month in any year. 

### 1a. Why do we want to do this?

Runoff models aim to successfully track changes in water availability, floods and droughts over time (Jehanzaib et al, 2022). 

For example, imagine you're a hydrologist studying a Scottish catchment. You have access to precipitation and daily flow data from 1970-now. Based on your understanding of the catchment characteristics - such as, soil type, topography, land cover, evapotranspiration - you can create a model that reflects the observed trends and patterns within this catchment. By adjusting parameters, such as infiltration rates, interception rates and surface runoff coefficients, you can fine-tune the model to mimic how the hydrological cycle behaves in the region.

At the end, the model will provide a predicted flow, which can be plotted against the observed daily flow to see how well your model performs. Do the lines match up? Did it capture seasonal trends? If the lines match up, this means you've successfully created a Rainfall-Runoff model that captures the hydrological dynamics of your catchment. 

Once validated, the model becomes a powerful resource. It can be used to predict future flow rates based on estimated precipitation levels, providing valuable insights for mitigating flood risks and improving water resource management. By simulating different scenarios, such as changes in land use or climate, hydrologists can use runoff models in decision-making and improve resilience to extreme climatic events.

> **_TIP:_**
If you don't have much experience with R, you should check out some of the Coding Club tutorials such as, [Intro to R](https://ourcodingclub.github.io/tutorials/intro-to-r/) to get a grip of the basics. This tutorial will also incorparate various functions from the `dplyr` package, therefore the [Basic data manipulation](https://ourcodingclub.github.io/tutorials/data-manip-intro/) tutorial will also be very useful if you've never used the `dplyr` package before.

## 2. Data preparation  

[The UK Centre for Ecology and Hydrology (CEH)](https://nrfa.ceh.ac.uk/data/search) collects precipitation and daily flow data across the whole of the UK, as well as detailed catchment info. For this tutorial, we're going to be using the Tweed at Peebles in Scotland. 

<a name="2"></a>
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
library(viridis) # colour blind friendly palette

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

If you're confused what the pipes (`%>%`) are doing, head to the [Efficient data manipulation](https://ourcodingclub.github.io/tutorials/data-manip-efficient/) tutorial which introduces pipes. If you just need a quick reminder - pipes chain operations together in a more efficient and readable way! 

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

# One last thing!

# Month and year must be set to a factor with the correct order so they dont appear on figures as "April".."June".."Dec"... the wrong order!

Filtered_data <- Filtered_data %>%
  mutate(
    MM = factor(
      MM,
      levels = month.abb  # Levels set to the correct order (Jan, Feb, ..., Dec)
    ),
    Year = as.factor(Year)
  )
```

> **_TIP:_**
ALWAYS remember to `ungroup()` after you're done with that operation to make sure operations later on are not affected by certain columns being grouped together.

PERFECT! Well done. Now, we have successfully prepared our data and we're almost ready to start building our model. Before we jump into creating the model, why not we plot out the observed ("real") flow values that were measured during 2015, 2016 and 2017 to get an idea of the general trends, similarities and differences between the three years. 

<a name="3"></a>
## 3. Visualising observed flow values

The aim of a Rainfall-Runoff model is to match up the predicted channel discharge values that the model produces with the observed values, measured in real life. This can be difficult because it will be tempting to adjust parameters as much as possible to align with real life, but we must avoid this because the parameters must attempt to reflect the hydrological processes within the catchment and the value we decide for them has to be justified by a hydrological process or characteristic observed in the catchment. Over calibrating and tweaking parameters for no reason, reduces accuracy in the model predicting extreme events if it doesn't align with the correct processes and characteristics. 

Okay, enough of that! Let's check out these observed flows. 

We're going to be using the `lubricate` package to note down the number of days in each month into a column called `Days_in_month`. This saves us having to go through each month and state the number of days. That would take ages! 

```r

# ---- Observed values ----

# Calculate observed flow (m3/s)
Observed_values <- Filtered_data %>%
  mutate(
    Days_in_month = days_in_month(ymd(paste(Year, MM, "01"))), # Get the number of days in the month
    Observed_flow_m3pers = Monthly_flow_m3 / (Days_in_month * 24 * 60 * 60) # Convert to m³/s
  ) %>%
  group_by(Date, Year, MM) %>%
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
  scale_color_viridis(discrete = TRUE) +  # Colorblind-friendly viridis palette
  theme_bw() + # Border around figure
  theme(
     panel.grid = element_blank()) # Get rid of those grey lines

```

<img src="{{ site.baseurl }}/Figures/Flow.month.png" alt="Observed flow against time" width="600"/>
*Figure 2: Observed flow (m³/s) against time.*

Okay. Here, we begin to realise the challenges with hydrological modelling. Our goal is to calibrate a model that works as best as possible for all three years. It might work perfectly for one year, but then if it doesn't for the other two years, then it's not exactly representative of the catchment. Ideally, you could use more years, perhaps over 10 or 20 years to gain a better perspective of changes and typical trends. 

Another way to visualise this data is by using `facet_wrap` which splits it up into three boxes (by year). Try it out!

```r
# Plotting with facet_wrap

ggplot(Observed_values, aes(x = MM, y = Observed_flow_m3pers)) +
  geom_line(aes(group = Year, color = factor(Year))) + # Line for each year
  geom_point(aes(color = factor(Year))) +  # Points for clarity
  facet_wrap(~ Year) +  # Create a facet for each year
  labs(
    x = "Month",
    y = "Observed Flow",
    color = "Year"
  ) +
  scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),   
    axis.text.x = element_text(angle = 60, hjust = 1)) # Tilt month names so they fit in better

```

  <img src="{{ site.baseurl }}/Figures/Flow.year.png" alt="Observed flow in 2015, 2016 and 2017" width="600"/>
*Figure 3: Observed flow (m³/s) each year.*

This enables us to view each year more easily. What can you notice about each one? Any similarities? Big differences?

- I can definitely see a big peek at the end of 2015.
- January flows of 2015 and 2016 are both pretty high.
- Flow in 2017 is a lot lower than the previous 2 years. 

<a name="4"></a>
## 4. Parameters

<a name="4a."></a>
### 4a. Understanding how to decide parameter values

I think the easiest way to understand the parameters we're going to be using, is to get a grasp of the system. Below, shows the different pathways P (precipitation) can take. Take a moment to have a look at what's going on and then I'll explain how the different parameters fit into this. 

  <img src="{{ site.baseurl }}/Figures/Model.jpg" alt="Rainfall-Runoff model" width="600"/>
*Figure 4: Rainfall-Runoff Model (Moore, 2007)*

Look at P and follow the arrows downwards. The first thing we see is E (Evapotranspiration). This will affect how much water is available for other pathways. This leads us to our first parameter:

### 4b. L1

Loss term 1! 
This parameter will account for Et losses AND interception losses. This is the amount of precipitation that is intercepted by trees or plants on its way down. We will decide this by visualising Et data throughout the year, identify patterns and seasonal changes. For interception, we can use CEH to look at vegetation cover over the catchment. Is it mostly urban? rural? woodland? agriculture? 

This catchment is mostly grassland and mountain/heath/bog. With a little amount of arable and woodland and very very small section of built-up areas.

We will come back to figure 4 in a second, but let's decide our L1 parameter first!

```r
# ---- PARAMETERS ----

# Loss term 1 (L1) ----

# L1 = Amount of water that reaches the surface AFTER interception, AFTER evapotranspiration!!! 

ggplot(Filtered_data, aes(x = MM, y = Et_mm, color = Year, group = Year)) + 
  geom_point() +
  labs(
       x = "Month", 
       y = "Evapotranspiration (mm)") +
  scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank())
```
 
 <img src="{{ site.baseurl }}/Figures/Et.png" alt="Et 2015-2017" width="600"/>
*Figure 5: Et from 2015-2017*

From this, we can see:
- Nov-March there is low-no Et
- March, Apr and Oct mid Et
- Summer aka May-Aug VVV high!

So what does this mean? Well.. based on the Et graph, we may choose to separate into seasons.

- __November to March__ = Little Et, little interception by urban/woodland therefore we may expect a large amount of rainfall to reach the surface = __0.8__ (80% of rainfall reaches the surface. 20% is either intercepted or evapotranspires)

- __April to May__ and __August to October__ = Mid Et = __0.5__

- __June to July__ = VERY high Et = __0.2__ 

### 4c. Surface to channel (C1)

Okay now lets think about what happens to precipitation when it hits the surface. It's either going to infiltrate into the ground (__C2__) or run straight to the channel, over the surface as surface runoff (__C1__). 

  <img src="{{ site.baseurl }}/Figures/Model.jpg" alt="Rainfall-Runoff model" width="600"/>
*Figure 4: Rainfall-Runoff Model (Moore, 2007)*

For this, we need to be thinking about:
- __TOPOGRAPHY__: UPLAND = STEEP = FAST RUNOFF
- __SOIL TYPE__: Permeable or impermeable?

Guess what! [CEH](https://nrfa.ceh.ac.uk/data/search) contains everything we need! The catchment has low permeability, upland, but it does have floodplains which can store water in flood events, reducing runoff. In general, we would expect this parameter value to be high due to the low permeability, meaning less water will infiltrate and the upland topography promoting fast runoff. However, it does contain floodplains so we can't make the value too high. 

What do we think?

__C1 = 0.6__ (meaning 60% of water goes straight to the channel).

> **_TIP:_** As we're keeping this model pretty simple, we're going to assume it remains the same throughout the whole year. But, if you were to create a model of your own, you might want to consider changing this value seasonally with changes in groundwater and soil moisture storage, which may differ throughout the year and therefore have an affect on C2 (infiltration) rates, ultimately affecting C1 as well! 

### 4d. Surface to ground (C2)

__C2__ represents the fraction of water in the surface storage that infiltrates into the groundwater storage. Factors that influence this include, soil type, vegetation cover and the intensity of rainfall. 

  <img src="{{ site.baseurl }}/Figures/Geology.png" alt="Catchment geology" width="600"/>
*Figure 5: Catchment geology (UK Centre for Ecology and Hydrology, 2024)*

Figure 5 displays geology at the Tweed Catchment. We can see that 87% of the bedrock is very low permeability, suggesting a low C2 value, also considering the upland nature of the catchment. 

__C2 = 0.3__ 

> **_TIP:_** __C1__ and __C2__ CAN NOT = 1 because this means that 100% of water is either going straight to the channel or leaving the surface storage, leaving the surface storage completely empty for the next month - not realistic! 

### 4e. Ground to channel (C3)

This is baseflow and C2 will represent the fraction of water in the groundwater storage that flows into the channel through the ground. C3 can be difficult to get right as it relies on having further knowledge on the trends in groundwater recharge and soil moisture change throughout the year. This might be slighty beyound this tutorial, but if you were to build your own model, you would research into this and perhaps have more information on this area. But for now, we will give it a parameter of 0.3 due to the low permeability of the soil, making it difficult for water to move through it. 

__C3 = 0.3__

### 4f. L2

This is loss term 2 and it represents the portion of water that is lost through leakage. Again, this is difficult to control for and requires further research on the catchment. For now, we will give it a parameter of 0.2, stating that 20% of groundwater storage leaks out to the wider area. 

__L2 = 0.2__

I hope this gives you a better understanding of each of the parameters and how we decide on the values.

<a name="5."></a>
## 5. Building the model

Yay! Finally, we're reading to start creating the model with our data set and chosen parameter values. Are you ready? 

First, we need to get only one value for each month as right now all the values each day in each month are the same. We only need one! Then, we will create a new dataset called `Rainfall_Runoff_Model` which will store our model. 

```r
# ---- BUILDING THE MODEL ----

# Aggregate daily data into monthly data so we only have 1 row each month
Monthly_data <- Filtered_data %>%
  group_by(Year, MM) %>% # Group by Year and Month
  summarize(
    # Retain the first value for columns that don't vary within the month
    Monthly_precipitation_m = first(Monthly_precipitation_m),
    .groups = "drop" # Ungroup after summarizing
  )

# Create columns of each stage in the model
Rainfall_Runoff_Model <- Monthly_data %>%
  mutate(
    Area = 694, #km2 (the area of the catchment is on the CEH website)
    Initial_surface_storage = 0, # For January we start on 0.
    Initial_ground_storage = 0, # For Jan!
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
```

Now, something to note is within this model, the values of Feb rely on values from Jan, and values from March rely on values from Feb and so on.... therefore, we need to create a sort of loop-like cycle that will capture this process.

To do this, we need to set up a `for loop` which is useful when you need to process rows individually. 

```
  # Iterate over rows (months) in the data
  for (i in 1:nrow(Rainfall_Runoff_Model)) { 

  # Extract the current row
  current_month <- Rainfall_Runoff_Model[i, ]

  # Calculate the number of days in the current month
  Days_in_month <- days_in_month(ymd(paste(current_month$Year, current_month$MM, "01")))

  # Calculate total water input based on precipitation
  Total_water_input <- current_month$Monthly_precipitation_m * current_month$Area * 1000000

  # Calculate surface storage for the current month
  Surface_storage <- current_month$Initial_surface_storage + 0.8 * Total_water_input

  # Calculate surface to channel and surface to ground
  Surface_to_channel <- current_month$C1 * Surface_storage
  Surface_to_ground <- current_month$C2 * Surface_storage

  # Calculate ground storage based on the initial ground storage left from previous month + leakage + amount of water flowing from the surface to the ground
  Ground_storage <- current_month$Initial_ground_storage + current_month$L2 + Surface_to_ground

  # Calculate ground to channel
  Ground_to_channel <- current_month$C3 * Ground_storage

  # Predicted channel input
  Pred_channel_input <- Surface_to_channel + Ground_to_channel

  # Predicted mean channel discharge (m3/s) 
  Pred_mean_channel_discharge <- Pred_channel_input / (Days_in_month * 24 * 60 * 60)  

  # Save all the values back into the dataset
  Rainfall_Runoff_Model$Surface_storage[i] <- Surface_storage
  Rainfall_Runoff_Model$Ground_storage[i] <- Ground_storage
  Rainfall_Runoff_Model$Surface_to_channel[i] <- Surface_to_channel
  Rainfall_Runoff_Model$Surface_to_ground[i] <- Surface_to_ground
  Rainfall_Runoff_Model$Ground_to_channel[i] <- Ground_to_channel
  Rainfall_Runoff_Model$Pred_channel_input[i] <- Pred_channel_input
  Rainfall_Runoff_Model$Pred_mean_channel_discharge[i] <- Pred_mean_channel_discharge

  # Update initial surface and ground storage for the next month, based on how much water is left over.
  if (i < nrow(Rainfall_Runoff_Model)) {
    Rainfall_Runoff_Model$Initial_surface_storage[i + 1] <- Surface_storage * (1 - current_month$C1 - current_month$C2)
    Rainfall_Runoff_Model$Initial_ground_storage[i + 1] <- Ground_storage - Ground_to_channel
  }
}
```

<a name="6."></a>
## 6. Time to compare predicted values VS. observed values

We've nearly reached the end. I hope you've understood so far. Once you've got your model working, the way to test if you've succeeded and managed to represent the hydrological dynamics of your catchment is to plot up the observed values against your fresh predicted 'modelled' values. 

```r
# ---- Time to compare ----

# Create a 'Date' column with first day of each month
Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  mutate(Date = seq(ymd("2015-01-01"), by = "month", length.out = nrow(Rainfall_Runoff_Model)))

# Add observed values to the model using left_join!

Observed_values$Date <- as.Date(Observed_values$Date) # Remember it needs to be in the right format to match with Date in the model dataset.

# Select only the 'Observed_flow_m3pers' column from Observed_values and join it with Rainfall_Runoff_Model
Rainfall_Runoff_Model <- Rainfall_Runoff_Model %>%
  left_join(Observed_values %>% select(Date, Observed_flow_m3pers), by = "Date")

# PLOT!!!!!
ggplot(Rainfall_Runoff_Model, aes(x = Date)) +
  geom_line(aes(y = Pred_mean_channel_discharge, color = "Predicted mean channel discharge"), size = 1) +
  geom_line(aes(y = Observed_flow_m3pers, color = "Observed monthly mean flow"), size = 1, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Flow (m³/s)",
    color = "Legend"
  ) +
  scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
  panel.grid = element_blank())
```

  <img src="{{ site.baseurl }}/Figures/Final.plot.png" alt="Predicted flow against observed flow" width="900"/>
*Figure 6: Predicted flow against observed flow (m3/s)*

