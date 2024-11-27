---
layout: tutorial
title: Understanding and Building a Rainfall-Runoff Model
subtitle: Parameters, building and visualising
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

*Created by Holly Bacon* c
 
## Tutorial Aims:

1. Learn the importance of Rainfall-Runoff models, what they do and why we need them.
2. Understand how we decide model parameters.
3. Become confident in creating a Rainfall-Runoff model that can accurately predict flow values that match up with real values.

## Tutorial Steps:

#### <a href="#1"> 1. What is a Rainfall-Runoff Model?</a>

#### <a href="#a"> a) Why do we want to do this?</a>

#### <a href="#section2"> Part 2. Data preparationa</a>

#### <a href="#section2"> 2.1. Install packages and load data</a>



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

<a name="a"></a>
### a) Why do we want to do this?

Runoff models aim to successfully track changes in water availability, floods and droughts over time (Jehanzaib et al, 2022). 

For example, imagine you're a hydrologist studying a Scottish catchment. You have access to precipitation and daily flow data from 1970-now. Based on your understanding of the catchment characteristics - such as, soil type, topography, land cover, evapotranspiration - you can create a model that reflects the observed trends and patterns within this catchment. By adjusting parameters, such as infiltration rates, interception rates and surface runoff coefficients, you can fine-tune the model to mimic how the hydrological cycle behaves in the region.

At the end, the model will provide a predicted flow, which can be plotted against the observed daily flow to see how well your model performs. Do the lines match up? Did it capture seasonal trends? If the lines match up, this means you've successfully created a Rainfall-Runoff model that captures the hydrological dynamics of your catchment. 

Once validated, the model becomes a powerful resource. It can be used to predict future flow rates based on estimated precipitation levels, providing valuable insights for mitigating flood risks and improving water resource management. By simulating different scenarios, such as changes in land use or climate, hydrologists can use runoff models in decision-making and improve resilience to extreme climatic events.

> **_TIP:_**
If you don't have much experience with R, you should check out some of the Coding Club tutorials such as, "Intro to R" (https://ourcodingclub.github.io/tutorials/intro-to-r/) to get a grip of the basics. This tutorial will also incorparate various functions from the `dplyr` package, therefore the "Basic data manipulation" tutorial (https://ourcodingclub.github.io/tutorials/data-manip-intro/) will also be very useful if you've never used the `dplyr` package before.

<a name="2"></a>
### Part 2. Data preparation  

> **_TIP:_**
All the files you need to complete this tutorial can be downloaded from this <a href="https://github.com/EdDataScienceEES/tutorial-hollybee04.git" target="_blank" markdown="1">repository</a>. Click code, download the URL and paste in a new project in R Studio. 

- Land cover - Woodland? Agriculture? Urban? Woodland = Higher interception, Higher Et? Urban = impermeable roads and pavements = more surface runoff.

These are all factors you have to think about when deciding parameters. 
                   
<center><img src="{{ site.baseurl }}/tutheaderbl.png" alt="Img"></center>

To add images, replace `tutheaderbl1.png` with the file name of any image you upload to your GitHub repository.

### Tutorial Aims

#### <a href="#section1"> 1. The first section</a>

#### <a href="#section2"> 2. The second section</a>

#### <a href="#section3"> 3. The third section</a>

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
![image](https://github.com/user-attachments/assets/b9fedd91-ca2c-4fdd-a3f9-bfce70fc7a11)
