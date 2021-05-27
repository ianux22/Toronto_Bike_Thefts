# Bike thefts in Toronto

### [Click here for the code](https://github.com/ianux22/Toronto_Bike_Thefts/blob/main/Script_Bikes.R) ###
### [Click here for the Report](https://ianux22.wixsite.com/thepizzastatistician/post/study-on-toronto-s-bicycle-thefts) ###

## Step 1: Exploratory Data Analysis and visualizations ##

This project analyzes all bicycle theft occurrences reported to the Toronto Police Service from 2014 to 2019 using the R programming language.
In the first part of the project I downloaded the data from the city of [Toronto](https://open.toronto.ca/) about the bikes' theft and the city's shapefile in order to use it with the package ggplot2 in R for visualizations. Subsequently, I started loading the files in Excel and R for some EDA.
From the EDA, it's clear that the thefts follow a normal distribution and that most of the thefts happens in summer, especially downton in the evening.
using the package Ggplot2 I was able to create heatmaps to create maps able to show the most neighborhoods with most thefts.

## Step 2: Clustering and predictions ##

In this step, I used hierarchical clustering to create 4 clusters to group the neighborhoods according to their likelihood of theft.
Obviously, closer to downtown, higher the risk.
In the end, I created an XGBoost model to predict the thefts for summer 2019 using data from 2014 to spring 2019. Then I compared the predictions with the true values.

