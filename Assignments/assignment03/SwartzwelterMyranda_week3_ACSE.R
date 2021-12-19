## Assignment: American Community Survey Exercise
# Name: Swartzwelter, Myranda
# Date: 2021-12-15

library(ggplot2)
library(pastecs)
library(moments)
install.packages('moments')
theme_set(theme_minimal())

setwd("/Users/myrandaswartzwelter/Documents/Github/DSC520-Stats-for-DS/Assignments/assignment03")

comm_df <- read.csv("acs-14-1yr-s0201.csv")
#Question 1 and 2
str(comm_df)
nrow(comm_df)
ncol(comm_df)
summary(comm_df)

#Question 3
ggplot(comm_df, aes(HSDegree)) + geom_histogram( bins = 20)  + ggtitle('Percent with HS Degree') + xlab('Percent with HS Degree') + ylab('Count')

#Question 4
ggplot(comm_df, aes(HSDegree)) + geom_histogram(aes(y = ..density..), bins = 20)  + ggtitle('Percent with HS Degree') + xlab('Percent with HS Degree') + ylab('Density') + stat_function(fun = dnorm, args = list(mean = mean(comm_df$HSDegree), sd = sd(comm_df$HSDegree)), size=3)

#Question 5
ggplot(comm_df, aes(HSDegree)) + geom_density() + ggtitle('Percent with HS Degree') + xlab('Percent with HS Degree') + ylab('Density')

#Question 8
stat.desc(comm_df['HSDegree'])

#Question 9
skewness(comm_df['HSDegree'])
kurtosis(comm_df['HSDegree'])

