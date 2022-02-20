### Myranda Swartzwelter
### DSC-520
### Week 10
### Thoratic Surgery Analysis

#install necessary packages
library('car')
library('mlogit')
library('foreign')

#Use foreign to read in file
thoratic_surgery <- read.arff('assignment_week10/ThoraricSurgery.arff')

head(thoratic_surgery)


thoratic_surgery$Risk1Yr <- relevel(thoratic_surgery$Risk1Yr, 'T')

newModel <- glm(Risk1Yr ~ DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 + PRE17 + PRE19 + PRE25 + PRE30 + PRE32 + AGE, data = thoratic_surgery, family = binomial) 
summary(newModel)
bestModel <- glm(Risk1Yr ~ PRE9 + PRE14 + PRE17 + PRE30, data=thoratic_surgery, family=binomial)
summary(bestModel)
#According to the summary, which variables had the greatest effect on the survival rate?

#According to the summary the following variables have the greatest effect:
# PRE9 - Dyspnoea before surgery TRUE
# PRE14 OC14 - T in clinical TNM - size of the original tumour (largest size tumor)
# PRE17 - Type 2 DM - diabetes mellitus TRUE
# PRE30 Smoking - TRUE

#To compute the accuracy of your model, use the dataset to predict the outcome variable. The percent of correct predictions is the accuracy of your model.
#What is the accuracy of your model?

#fit model
thoratic_surgery$predicted_probabilities <- fitted(newModel)
thoratic_surgery$predicted_probabilities2 <- fitted(bestModel)

#the predict does the exact same thing as previous code
thoratic_surgery$predicted_2 <- predict(bestModel, thoratic_surgery, type = 'response')

#turn survival probability into true / false
thoratic_surgery <- thoratic_surgery  %>% mutate(predicted_probabilities_binary = 1*(predicted_probabilities2 > .50), survival_binary = 1*(Risk1Yr == "F") + 0)

#check differences between predicted and actual
thoratic_surgery$difference <- thoratic_surgery$predicted_probabilities_binary == thoratic_surgery$survival_binary

#calculate accuracy
accuracy <- sum(thoratic_surgery$difference) / length((thoratic_surgery$difference))
accuracy <- 100*accuracy
accuracy

#model is 84.5% accurate
