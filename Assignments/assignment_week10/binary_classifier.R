### Myranda Swartzwelter
### DSC-520
### Week 10
### Binary Classifier 

library('car')
library('mlogit')
library('dplyr')
install.packages('class')


#read in dataset
setwd('/Users/myrandaswartzwelter/Documents/Bellvue/Term-2/DSC-520/DSC520-Stats-for-DS/Assignments/')
binary_df <- read.csv('assignment_week10/binary-classifier-data.csv')

#look at dataset
head(binary_df)

#create logistic model
model <- glm(label ~ x+y, data=binary_df, family = binomial)
summary(model)

#predict outcome
binary_df$test_prob <- predict(model, binary_df, type = 'response')

#turn probability into binary
binary_df <- binary_df  %>% mutate(test_prob = 1*(test_prob > .50))

#check how many predictions are different than actuals
binary_df$difference <- binary_df$label == binary_df$test_prob

#calculate accuracy
accuracy <- sum(binary_df$difference) / length((binary_df$difference))
accuracy <- 100*accuracy
accuracy

#Model is 58.3% accurate
