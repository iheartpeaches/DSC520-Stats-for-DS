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
install.packages('car')

library('dplyr')
library('purrr')
library('stringr')
library('broom')
library('car')

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

#A standardized beta coefficient  compares the strength of the effect of each individualind variable to the 
#dependent variable. The higher the coefficient, the stronger the effect.

#So, in my model I would expect the number of bedrooms and bathrooms to be the 
#strongest indicator for sale price.

#Calculate confidence intervals

confint(simple_lm, "sq_ft_lot")

confint(multi_lm, "sq_ft_lot")
confint(multi_lm, "bedrooms")
confint(multi_lm, "bath_full_count")
confint(multi_lm, "year_built")
confint(multi_lm,"year_renovated")

#compare models using ANOVA() 
anova(simple_lm, multi_lm)

#Since we see a very small P Value of <2.2e-16 we can conclude that the more complex model has a better fit

# Assessing Outliers
#Look at leverage plots and outlier tests for each model
outlierTest(simple_lm) # Bonferonni p-value for most extreme obs
qqPlot(simple_lm, main="QQ Plot") #qq plot for resid
leveragePlots(simple_lm) # leverage plots to view outiers

simple_lm.diag.metrics <- augment(simple_lm)
head(simple_lm.diag.metrics)

outlierTest(multi_lm) # Bonferonni p-value for most extreme obs
qqPlot(multi_lm, main="QQ Plot") #qq plot for studentized resid
leveragePlots(multi_lm) # leverage plots to view outliers

multi_lm.diag.metrics <- augment(multi_lm)
head(multi_lm.diag.metrics)

#calculate residuals
simple_resid <- resid(simple_lm)
multi_resid <- resid(multi_lm)

#calculate standard residuals
rstandard(simple_lm)
rstandard(multi_lm)

large_resids_simple <- rstandard(simple_lm)[abs(rstandard(simple_lm)) > 2]

large_resids_multi <- rstandard(multi_lm)[abs(rstandard(multi_lm)) > 2]

#Simple regression cooks distance
cd_simple_add = cooks.distance(simple_lm)
sum(cd_simple_add > 4 / length(cd_simple_add))
large_cd_simple = cd_simple_add > 4 / length(cd_simple_add)
cd_simple_add[large_cd_simple]

#multiple parameters cooks distance
cd_multi_add = cooks.distance(multi_lm)
sum(cd_multi_add > 4 / length(cd_multi_add))
large_cd_multi = cd_multi_add > 4 / length(cd_multi_add)
cd_simple_add[large_cd_multi]

