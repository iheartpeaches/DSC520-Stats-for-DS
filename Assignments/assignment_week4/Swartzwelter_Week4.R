# Assignment: ASSIGNMENT 4
# Name: Swartzwelter, Myranda
# Date: 2021-1-07

setwd("/Users/myrandaswartzwelter/Documents/Github/DSC520-Stats-for-DS")


####### PART 1 ##########

scores_df1 <- read.csv('scores.csv')
str(scores_df1)


# #1. What are the observational units in this study?
# The observational units are the objects on which variables are defined and measured.
# In this study, the observational units are the students, so there are 38 observational units.
# 
# #2. Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
# There are 3 variables mentioned in the paragraph:
# Section: Categorical
# Course Grades: Quantitative
# Total Points Earned in Course: Quantitative

#3. Create one variable to hold a subset of your data set that contains only the Regular Section and one variable for the Sports Section.
sports_df <- subset(scores_df1, Section == "Sports", select = c(Count, Score))
regular_df <- subset(scores_df1, Section == "Regular", select = c(Count, Score))

#4. Use the Plot function to plot each Sections scores and the number of students achieving that score.
#Use additional Plot Arguments to label the graph and give each axis an appropriate label. 

plot(sports_df$Score, sports_df$Count, type = 'p', main = "Section Scores", xlab = "Scores", ylab = "Number of Students", col= "Black")
points(regular_df$Score, regular_df$Count, type = 'p', col="Red")

#a. Comparing and contrasting the point distributions between the two section, looking at both tendency and consistency: 
#Can you say that one section tended to score more points than the other? Justify and explain your answer.

#Tendency, specifically the central tendency is a way to describe a data set by a singular metric this can be the mean, mode, or median.
#Consistency refers to how consistent the data is. This can be looked at by the spread of the data.
#Looking at the graph, it looks like students in the Regular section tended to score higher than the students in the Sports section.
#The students in the Regular section were consistently higher than the students in the Sports section, whereas the Sports section had a wider range of scores.
# Although the max score for the Sports section was higher than the max score for the Regular section the scores of the Regular section tended higher than the Sports section.
#The min score for the Sports section was much lower than the min score for the Regular section. The majority of students in the Regular section scored higher than the majority
#of students in the Sports section.

#b. Did every student in one section score more points than every student in the other section? If not, explain what a statistical tendency means in this context.

#No not every student in one section scored more than every student in the other section. For this sitution, we want to look at the central tendency, and in this context the mean
# of the scores of the student to understand whether the average student in one section scored higher than the average student in another section. When we look at scores with this 
# lens, we can see that the average score for the Regular section (335) was higher than the average score for the Sports section (307).

# c. What could be one additional variable that was not mentioned in the narrative that could be influencing the point distributions between the two sections?
# There are many factors that could influence the distribution in points and scores that students are getting. An interesting variable to compare would be GPA of students in the 
# course prior to the course, or attendance by students, or even time of day of the class to see if any of these factors correlate with the point distributions.

###### Part 2 - using the housing data set#######

housing_df <- read.csv('week-6-housing.csv')
str(housing_df)

#Use the apply function on a variable in your dataset
#find the mean number of bedrooms
sq_ft_bedroom_means <- apply(housing_df[ ,c(14,15)],2,mean)
sq_ft_bedroom_means

#Use the aggregate function on a variable in your dataset
bedroom_mean <- aggregate(housing_df$bedrooms, list(housing_df$sitetype), mean)
bedroom_mean

#Use the plyr function on a variable in your dataset – more specifically, I want to see you split some data, perform a modification to the data, and then bring it back together

#create a function that counts the total number of bedrooms and bathrooms in the house
library(plyr)
total_rooms <- function(data)
{
  c(rooms=with(data, sum(bedrooms + bath_full_count + bath_half_count + bath_3qtr_count )))
}

#using ddply, count all bed and bathrooms and combine for site types
total_bed_bath <- ddply(housing_df, .variables = "sitetype", .fun=total_rooms)
total_bed_bath

#Check distributions of the data
#more than 5000 observations, so we can't use the shapiro test for normality but we can observe the histograms
library(ggplot2)

#bedrom distribution
ggplot(housing_df, aes(bedrooms)) + geom_histogram()

#saleprice distribution
ggplot(housing_df, aes(sale_price)) + geom_histogram()

#full bath count distribution
ggplot(housing_df, aes(bath_full_count)) + geom_histogram()

#sq ft lot distribution
ggplot(housing_df, aes(sq_ft_lot)) + geom_histogram()

#year built distribution
ggplot(housing_df, aes(year_built)) + geom_histogram()

#square feet total living distribution
ggplot(housing_df, aes(square_feet_total_living)) + geom_histogram()

#Check for outliers
#bedrooms variable
ggplot(housing_df) +  aes(x = "", y = bedrooms) + geom_boxplot()
boxplot.stats(housing_df$bedrooms)$out

#sale price variable
ggplot(housing_df) +  aes(x = "", y = sale_price) + geom_boxplot()
boxplot.stats(housing_df$sale_price)$out

#full bath count variable
ggplot(housing_df) +  aes(x = "", y = bath_full_count) + geom_boxplot()
boxplot.stats(housing_df$bath_full_count)$out

#sq ft lot variable
ggplot(housing_df) +  aes(x = "", y = sq_ft_lot) + geom_boxplot()
boxplot.stats(housing_df$sq_ft_lot)$out

#year built variable
ggplot(housing_df) +  aes(x = "", y = year_built) + geom_boxplot()
boxplot.stats(housing_df$year_built)$out

#square feet total living variable
ggplot(housing_df) +  aes(x = "", y = square_feet_total_living) + geom_boxplot()
boxplot.stats(housing_df$square_feet_total_living)$out


#Create at least 2 new variables
#create a variable that includes total # of bathrooms in the house
housing_df$total_bath <- housing_df$bath_full_count + housing_df$bath_3qtr_count + housing_df$bath_half_count

#create a variable that calculates the number of years after it was build that the house was renovated. 0 if the house was not renovated.
housing_df$years_between_ren <- (ifelse(housing_df$year_renovated == 0, 0, housing_df$year_renovated - housing_df$year_built))
