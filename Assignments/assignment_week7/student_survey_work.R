# Assignment: Student Survey
# Name: Swartzwelter, Myranda
# Date: 2021-1-12

setwd("/Users/myrandaswartzwelter/Documents/Github/DSC520-Stats-for-DS/Assignments/assignment_week7")

library('GGally')


student_survey_df <- read.csv("student-survey.csv")
cor(student_survey_df)
GGally::ggpairs(student_survey_df)
cov(student_survey_df)
