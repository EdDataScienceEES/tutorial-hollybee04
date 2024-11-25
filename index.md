---
layout: tutorial
title: How to build a Rainfall-Runoff Model
author: Holly Bacon
date: 2024-11-23 00:00:00
subtitle: Deciding parameters, visualising, calibrating and validating.
tags: Rainfall-Runoff, Hydrology, Calibration, Validation
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

#### <a href="#section3"> 2. Load packages</a>

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

### WHY THOUGH?

Runoff models aim to successfully track changes in water availability, floods and droughts over time (Jehanzaib et al, 2022). 

For example, imagine you're a hydrologist studying a Scottish catchment. You have access to precipitation and daily flow data from 1970-now. Based on your understanding of the catchment characteristics - such as, soil type, topography, land cover, evapotranspiration - you can create a model that reflects the observed trends and patterns within this catchment. By adjusting parameters, such as infiltration rates, interception rates and surface runoff coefficients, you can fine-tune the model to mimic how the hydrological cycle behaves in the region.

At the end, the model will provide a predicted flow, which can be plotted against the observed daily flow to see how well your model performs. Do the lines match up? Did it capture seasonal trends? If the lines match up, this means you've successfully created a Rainfall-Runoff model that captures the hydrological dynamics of your catchment. 

Once validated, the model becomes a powerful resource. It can be used to predict future flow rates based on estimated precipitation levels, providing valuable insights for mitigating flood risks and improving water resource management. By simulating different scenarios, such as changes in land use or climate, hydrologists can use runoff models in decision-making and improve resilience to extreme climatic events.

{% capture callout %} 
If you don't have much experience with R, you should check out some of the Coding Club tutorials such as, "Intro to R" (https://ourcodingclub.github.io/tutorials/intro-to-r/) to get a grip of the basics. This tutorial will also incorparate various functions from the `dplyr` package, therefore the "Basic data manipulation" tutorial (https://ourcodingclub.github.io/tutorials/data-manip-intro/) will also be very useful if you've never used the `dplyr` package before.
{% endcapture %}
{% include callout.html colour='callout' content=callout %}

{% capture callout %}
All the files you need to complete this tutorial can be downloaded from this <a href="https://github.com/EdDataScienceEES/tutorial-hollybee04.git" target="_blank" markdown="1">repository</a>. Click code, download the URL and paste in a new project in R Studio. 
{% endcapture %}
{% include callout.html colour='callout' content=callout %}

<a name="1"></a>
### Part 2. Data preparation  

Open a new script, write a title, your name, the date and load in the packages and data. Throughout this tutorial you can copy the code boxes into your own script. Remember # give extra context and explain what the code is doing! 

The UK Centre for Ecology and Hydrology (https://nrfa.ceh.ac.uk/data/search) collects precipitation and daily flow data across the whole of the UK, as well as detailed catchment info. For this tutorial, we're going to be using the Tweed at Peebles in Scotland. 

```
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

```
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

Great! Now we have all the data we need and it's pretty organised now. But it's all seperate. Lets combine the 3 datasets together using a cool `dplyr` function called `left_join` 

{% capture callout %}
Why I love `left_join()`: 
It's really easy to use and allows you to combine data sets together, but specifically when you want to keep the rows from the left data frame and add matching values from the right data frame. Where values don't match, an NA will appear. This is useful when you have collected data on multiple different variables and so you have may have separate data frames.
{% endcapture %}
{% include callout.html colour='callout' content=callout %}


The parameters are decided based on typical characteristics of your chosen catchment (In this tutorial, we'll be looking at the Tweed at Peebles catchment). Characteristics include:
- Topography - is your catchment pretty flat, steep hills, undulating terrain?
- Soil type - permeable/impermeable? This affects how well precipitation infiltrates into the soil.
- Evapotranspiration (Et) - More Et in summer means less precipitation reaching the surface, therefore less surface runoff.
- Land cover - Woodland? Agriculture? Urban? Woodland = Higher interception, Higher Et? Urban = impermeable roads and pavements = more surface runoff.

These are all factors you have to think about when deciding parameters. 










You can read this text, then delete it and replace it with your text about your tutorial: what are the aims, what code do you need to achieve them?
---------------------------
We are using `<a href="#section_number">text</a>` to create anchors within our text. For example, when you click on section one, the page will automatically go to where you have put `<a name="section_number"></a>`.

To create subheadings, you can use `#`, e.g. `# Subheading 1` creates a subheading with a large font size. The more hashtags you add, the smaller the text becomes. If you want to make text bold, you can surround it with `__text__`, which creates __text__. For italics, use only one understore around the text, e.g. `_text_`, _text_.

# Subheading 1
## Subheading 2
### Subheading 3

This is some introductory text for your tutorial. Explain the skills that will be learned and why they are important. Set the tutorial in context.

You can get all of the resources for this tutorial from <a href="https://github.com/ourcodingclub/CC-EAB-tut-ideas" target="_blank">this GitHub repository</a>. Clone and download the repo as a zip file, then unzip it.

<a name="section1"></a>

## 1. The first section


At the beginning of your tutorial you can ask people to open `RStudio`, create a new script by clicking on `File/ New File/ R Script` set the working directory and load some packages, for example `ggplot2` and `dplyr`. You can surround package names, functions, actions ("File/ New...") and small chunks of code with backticks, which defines them as inline code blocks and makes them stand out among the text, e.g. `ggplot2`.

When you have a larger chunk of code, you can paste the whole code in the `Markdown` document and add three backticks on the line before the code chunks starts and on the line after the code chunks ends. After the three backticks that go before your code chunk starts, you can specify in which language the code is written, in our case `R`.

To find the backticks on your keyboard, look towards the top left corner on a Windows computer, perhaps just above `Tab` and before the number one key. On a Mac, look around the left `Shift` key. You can also just copy the backticks from below.

```r
# Set the working directory
setwd("your_filepath")

# Load packages
library(ggplot2)
library(dplyr)
```

<a name="section2"></a>

## 2. The second section

You can add more text and code, e.g.

```r
# Create fake data
x_dat <- rnorm(n = 100, mean = 5, sd = 2)  # x data
y_dat <- rnorm(n = 100, mean = 10, sd = 0.2)  # y data
xy <- data.frame(x_dat, y_dat)  # combine into data frame
```

Here you can add some more text if you wish.

```r
xy_fil <- xy %>%  # Create object with the contents of `xy`
	filter(x_dat < 7.5)  # Keep rows where `x_dat` is less than 7.5
```

And finally, plot the data:

```r
ggplot(data = xy_fil, aes(x = x_dat, y = y_dat)) +  # Select the data to use
	geom_point() +  # Draw scatter points
	geom_smooth(method = "loess")  # Draw a loess curve
```

At this point it would be a good idea to include an image of what the plot is meant to look like so students can check they've done it right. Replace `IMAGE_NAME.png` with your own image file:

<center> <img src="{{ site.baseurl }}/IMAGE_NAME.png" alt="Img" style="width: 800px;"/> </center>

<a name="section1"></a>

## 3. The third section

More text, code and images.

This is the end of the tutorial. Summarise what the student has learned, possibly even with a list of learning outcomes. In this tutorial we learned:

##### - how to generate fake bivariate data
##### - how to create a scatterplot in ggplot2
##### - some of the different plot methods in ggplot2

We can also provide some useful links, include a contact form and a way to send feedback.

For more on `ggplot2`, read the official <a href="https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf" target="_blank">ggplot2 cheatsheet</a>.

Everything below this is footer material - text and links that appears at the end of all of your tutorials.

<hr>
<hr>

#### Check out our <a href="https://ourcodingclub.github.io/links/" target="_blank">Useful links</a> page where you can find loads of guides and cheatsheets.

#### If you have any questions about completing this tutorial, please contact us on ourcodingclub@gmail.com

#### <a href="INSERT_SURVEY_LINK" target="_blank">We would love to hear your feedback on the tutorial, whether you did it in the classroom or online!</a>

<ul class="social-icons">
	<li>
		<h3>
			<a href="https://twitter.com/our_codingclub" target="_blank">&nbsp;Follow our coding adventures on Twitter! <i class="fa fa-twitter"></i></a>
		</h3>
	</li>
</ul>

### &nbsp;&nbsp;Subscribe to our mailing list:
<div class="container">
	<div class="block">
        <!-- subscribe form start -->
		<div class="form-group">
			<form action="https://getsimpleform.com/messages?form_api_token=de1ba2f2f947822946fb6e835437ec78" method="post">
			<div class="form-group">
				<input type='text' class="form-control" name='Email' placeholder="Email" required/>
			</div>
			<div>
                        	<button class="btn btn-default" type='submit'>Subscribe</button>
                    	</div>
                	</form>
		</div>
	</div>
</div>
