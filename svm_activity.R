library(caret)
library(caTools)
library(e1071)
library(dplyr)

getwd()
setwd("C:\\Users\\athar\\Desktop\\ALY6040")
# Importing the dataset
dataset = read.csv('star_classification.csv')

head(dataset)
summary(dataset)

#drop columns that aren't numerical or useful (all fields with ID, plate)
dataset <- select(dataset, -c(obj_ID, run_ID, rerun_ID, field_ID, spec_obj_ID, plate, fiber_ID))

#train/test split
set.seed(123)
split = sample.split(dataset$class, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#fit an SVM model to training data
classifier = svm(formula = as.factor(class) ~ .,
                 data = training_set,
                 scale = TRUE,
                 type = 'C-classification',
                 kernel = 'linear')

classifier

# Predicting the Test set results
test_set_x <- select(test_set, -class)
y_pred = predict(classifier, newdata = test_set_x)

#accuracy
mean(y_pred == test_set$class)

# Making the Confusion Matrix
cm = table(test_set$class, y_pred)
cm

#take a subsample to tune params
training_set_tuning <- sample_frac(training_set, 0.2)

#try tuning the svm model params to improve performance
tune_classifier = tune.svm(as.factor(class)~.,
                           data = training_set_tuning,
                           type = 'C-classification',
                           kernel = 'linear',
                           cost = c(.01, .1, .5, 1, 2.5, 5, 7.5, 10, 20, 50), 
                           scale = TRUE)

tune_classifier

# use those suggested parameters
classifier_2 = svm(formula = as.factor(class) ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'linear',
                   cost = 0.5,
                   gamma = 0.01)

# Predicting the Test set results
y_pred_2 = predict(classifier_2, newdata = test_set_x)

#accuracy
mean(y_pred_2 == test_set$Purchased)