---
layout: tutorial   
title: Understanding and Building a Rainfall-Runoff Model
author: Holly Bacon
subtitle: Deciding parameters, visualising, calibrating and validating.
---

<div style="text-align: center;">
  <img src="Figures/Tut_title.png" width="1000" height="400">
</div> 

*Created by Holly Bacon*

## Tutorial Aims:

1. Learn the importance of Rainfall-Runoff models, what they do and why we need them.
2. Understand how we decide model parameters.
3. Become confident in creating a Rainfall-Runoff model that can accurately predict flow values that match up with real values.

## Tutorial Steps:

#### <a href="#section1"> 1. What is a Rainfall-Runoff Model?</a>

#### <a href="#section2"> Part 2. Data preparationa</a>

#### <a href="#section3"> 2.1 Install packages and load data</a>

#### <a href="#section4"> 2.2 

#### <a href="#section4"> 3. The third section</a>



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

### Why do we want to do this?

Runoff models aim to successfully track changes in water availability, floods and droughts over time (Jehanzaib et al, 2022). 

For example, imagine you're a hydrologist studying a Scottish catchment. You have access to precipitation and daily flow data from 1970-now. Based on your understanding of the catchment characteristics - such as, soil type, topography, land cover, evapotranspiration - you can create a model that reflects the observed trends and patterns within this catchment. By adjusting parameters, such as infiltration rates, interception rates and surface runoff coefficients, you can fine-tune the model to mimic how the hydrological cycle behaves in the region.

At the end, the model will provide a predicted flow, which can be plotted against the observed daily flow to see how well your model performs. Do the lines match up? Did it capture seasonal trends? If the lines match up, this means you've successfully created a Rainfall-Runoff model that captures the hydrological dynamics of your catchment. 

Once validated, the model becomes a powerful resource. It can be used to predict future flow rates based on estimated precipitation levels, providing valuable insights for mitigating flood risks and improving water resource management. By simulating different scenarios, such as changes in land use or climate, hydrologists can use runoff models in decision-making and improve resilience to extreme climatic events.

> **_TIP:_**
If you don't have much experience with R, you should check out some of the Coding Club tutorials such as, "Intro to R" (https://ourcodingclub.github.io/tutorials/intro-to-r/) to get a grip of the basics. This tutorial will also incorparate various functions from the `dplyr` package, therefore the "Basic data manipulation" tutorial (https://ourcodingclub.github.io/tutorials/data-manip-intro/) will also be very useful if you've never used the `dplyr` package before.

<a name="1"></a>
### Part 2. Data preparation  

> **_TIP:_**
All the files you need to complete this tutorial can be downloaded from this <a href="https://github.com/EdDataScienceEES/tutorial-hollybee04.git" target="_blank" markdown="1">repository</a>. Click code, download the URL and paste in a new project in R Studio. 

Open a new script, write a title, your name, the date and load in the packages and data. Throughout this tutorial you can copy the code boxes into your own script. Remember # give extra context and explain what the code is doing! 

The UK Centre for Ecology and Hydrology (https://nrfa.ceh.ac.uk/data/search) collects precipitation and daily flow data across the whole of the UK, as well as detailed catchment info. For this tutorial, we're going to be using the Tweed at Peebles in Scotland. 

```r
# Tutorial: Understanding and Building a Rainfall-Runoff Model
# Written by ...
# Date 

---- Library ----

library(nasapower) # for downloading evapotranspiration data 
library(dplyr) # for data manipulation
library(ggplot2) # for data visualisation
library(lubridate) # for data handling

---- Load data ----

Flow <- read.csv("data/Daily_flow.csv")
Precipitation <- read.csv("data/Rainfall_Data.csv")

# Download Et data from NASA! (nasapower)

# Set coordinates for the Tweed catchment.

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

```r
---- Data preparation ----

# Remove first 19 rows of metadata

Flow_clean <- Flow[20:nrow(Flow), ] # This excludes the first 19 rows and only rows 20 and on are kept in the new data frame "Flow_clean".

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

This is because the date column in Flow and Precipitation (x$Date) is a character but the date column in Et is in 'date' form. This means we need to change the date column in Et to a character. 

```r
# Change Evapotranspiration_clean to a character to match with other data sets
Evapotranspiration_clean$Date <- as.character(Evapotranspiration_clean$Date)

# Try again!
Final_merged_data <- Merged_data %>%
  left_join(Evapotranspiration_clean, by = "Date")
```

You can also use this when you need to change a column to a factor or numeric: as.factor, as.numeric. 

OKAY! Now we have one big data set containing all the information we need. What's next?

Now we need to filter to the years we want to calibrate our model with (2015-2017). We can do this by using the `filter()` function which selects rows based on the conditions we set. 

```r
# Filter to 2015-2017 ----

# Filter for dates within the range
Filtered_data <- Final_merged_data %>% 
  filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2017-12-31"))
```

- Condition 1: "Date >= as.Date("2015-01-01")" keeps rows where the Date column is greater than or equal to January 1st 2015. "as.Date" ensures date is treated as a date and not just text.
- Condition 2: "Date <= as.Date("2017-12-31")" keeps rows where the Date column is less than or equal to December 31st 2017.

Now we have our dataset with all the information we need AND filtered to the right timeline. The next step is to ensure we have the correct units. 








The parameters are decided based on typical characteristics of your chosen catchment (In this tutorial, we'll be looking at the Tweed at Peebles catchment). Characteristics include:
- Topography - is your catchment pretty flat, steep hills, undulating terrain?
- Soil type - permeable/impermeable? This affects how well precipitation infiltrates into the soil.
- Evapotranspiration (Et) - More Et in summer means less precipitation reaching the surface, therefore less surface runoff.
- Land cover - Woodland? Agriculture? Urban? Woodland = Higher interception, Higher Et? Urban = impermeable roads and pavements = more surface runoff.

These are all factors you have to think about when deciding parameters. 
                   
