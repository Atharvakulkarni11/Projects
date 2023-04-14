###
# Module 2 in-class activity
###

#Using the following data and code, answer these questions:

#1.) What is the accuracy for predicting each of the three species?

    setoSA

#2.) What are the most important features?

#3.) What happens if you reduce or increase the number of trees used by randomForest?
    
    ##

library(tidyverse)
library(caret)
library(randomForest)

# Loading data
data(iris)

# Structure 
str(iris)

# Split the data into training and test set
set.seed(123)
training.samples <- iris$Species %>% 
  createDataPartition(p = 0.8, list = FALSE)
train  <- iris[training.samples, ]
test <- iris[-training.samples, ]

# Fitting Random Forest to the train dataset
classifier_RF = randomForest(x = train[-5],
                             y = train$Species,
                             ntree = 15)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-5])

# Confusion Matrix
confusion_mtx = table(test[, 5], y_pred)
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)
