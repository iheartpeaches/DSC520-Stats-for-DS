# Assignment: Housing Assignment
# Name: Swartzwelter, Myranda
# Date: 2021-02-13


#Set Working Directory
setwd("/Users/myrandaswartzwelter/Documents/Bellvue/Term-2/DSC-520/DSC520-Stats-for-DS/Assignments")

#Read in CSV
housing_df <- read.csv("assignment_week8_9/week-6-housing.csv")

#Import useful libraries
#install.packages('dplyr')
#install.packages('purrr')
#install.packages('stringr')

library('dplyr')
library('purrr')
library('stringr')

#Transforamtions / modifications:

#create a new column that calculates price per sqft
housing_df$price_per_sqft <-  housing_df$sale_price / housing_df$sq_ft_lot

#create new column that calculates total bathrooms
housing_df$total_bathrooms <- housing_df$bath_3qtr_count + housing_df$bath_full_count + housing_df$bath_half_count

#Create variable that contains sale date and sq foot lot
sdate_sq_ft <- select(housing_df, sale_price, sq_ft_lot)

simple_lm <-  lm(sale_price ~ sq_ft_lot , data=housing_df)


#Create variable that contains sale price and additional parameters
#Selecting number of bedrooms and bathrooms, year built and year renovated and sale date as those 
#are the parameters that I believe will have a relationship with price and are different across the different sales
many_params <- select(housing_df, sale_price, sq_ft_lot, bedrooms, bath_full_count, year_built, year_renovated)

multi_lm <- lm(sale_price ~  sq_ft_lot + bedrooms + bath_full_count + year_built + year_renovated, data=housing_df)

#Summary functions
summary(simple_lm)
#R2 Value: 0.01435
#adjusted R2 value: 0.001428

summary(multi_lm)
#R2 value: 0.1461
#Adjusted R2: 0.1458

#The R2 value is higher for the model with more parameters, indicating a better fit for the model
#Including the additional parameters did help the model fit

#Get the standardized betas for the simple model
simple_lm$coef

#get the standardized betas for the more complex model
multi_lm$coef
