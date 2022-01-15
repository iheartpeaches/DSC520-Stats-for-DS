# Assignment: ASSIGNMENT 5
# Name: Swartzwelter, Myranda
# Date: 2021-1-12

#install.packages('dplyr')
#install.packages('purrr')
#install.packages('stringr')
library('dplyr')
library('purrr')
library('stringr')

setwd("/Users/myrandaswartzwelter/Documents/Github/DSC520-Stats-for-DS")

housing_df <- read.csv('week-6-housing.csv')
str(housing_df)

##### Using dyplr, use the 6 functions to analyze/transform the data

#select
#select the columns sale_date and sale_price
select(housing_df, sale_date, sale_price)

#mutate
#create a new column that calculates price per sqft
mutate(housing_df, price_per_sqft =  sale_price / sq_ft_lot)

#summarize
#get the average lot sq ft
summarize(housing_df, mean(sq_ft_lot))

#group_by
#get the average sale price by date of sale
housing_df %>%
  group_by(sale_date) %>%
  summarize(avg_price = mean(sale_price))

#filter
#filter dataset for only houses that sold for more than 500,000
more_5000 <- filter(housing_df, sale_price > 500000)

#arrange
#order data set by sale price
arrange(housing_df, sale_price)


##### Using purrr perform 2 functions

#keep,
#taking the year built column, select only years built after the year 2000
keep(housing_df$year_built, housing_df$year_built > 2000)

#map
#get the square root of the sale price for the first 5 rows of the data set
map(housing_df$sale_price[1:5], sqrt)

### Use the cbind and rbind function

#cbind
housing_df <- cbind(housing_df, price_per_sqft =  housing_df$sale_price / housing_df$sq_ft_lot)

#rbind
new_row <- c('12/16/2020', 535000, 1, 1,' ' ,'R1', '123 ABC Dr.', 55108, 'St. Paul', 'Saint Paul', -122, 47, 9, 2230, 3, 2, 1, 0, 2004, 0,  'RA5', 5570, 'R', 5)
housing_df <- rbind(housing_df, new_row)

#split a string, and then concatenate it back together
my_string <- 'Hello, World!'
#split
split <- str_split(my_string, " ")

#concatenate again
new_string <- sapply(split, function(x) paste(x[1:2], collapse = ' '))



