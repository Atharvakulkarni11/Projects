library(tidyverse)
library(caret)
library(MASS)
theme_set(theme_classic())
getwd()
setwd("C:\\Users\\athar\\Desktop\\ALY6040")

# Load the data
data("iris")
# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- iris$Species %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- iris[training.samples, ]
test.data <- iris[-training.samples, ]

# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

#check our work (why only use training data?)
apply(train.transformed[1:4], 2, mean)
apply(train.transformed[1:4], 2, sd)

###
# Linear discriminant analysis - LDA
###

# Fit the model
model <- lda(Species~., data = train.transformed)
# Make predictions
predictions <- model %>% predict(test.transformed)
# Model accuracy
mean(predictions$class==test.transformed$Species)

# dive into the model
model



plot(model)

names(predictions)


# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))


mean(predictions$class==test.transformed$Species)

#############################################################################################
#Quadratic discriminant analysis - QDA
#no assumption that the covariance matrix of the classes is the same
library(MASS)
# Fit the model
model <- qda(Species~., data = train.transformed)
model
# Make predictions
predictions <- model %>% predict(test.transformed)
# Model accuracy
mean(predictions$class == test.transformed$Species)


##############################################################################################
#Mixture discriminant analysis - MDA
# Each class is assumed to be a gaussian mixture of subclasses
library(mda)
# Fit the model
model <- mda(Species~., data = train.transformed)
model
# Make predictions
predicted.classes <- model %>% predict(test.transformed)
# Model accuracy
mean(predicted.classes == test.transformed$Species)


##############################################################################################
#Flexible discriminant analysis - FDA
# nonlinear combinations of predictors is used for boundaries

library(mda)
# Fit the model
model <- fda(Species~., data = train.transformed)
# Make predictions
predicted.classes <- model %>% predict(test.transformed)
# Model accuracy
mean(predicted.classes == test.transformed$Species)
model

###############################################################################################
#Regularized discriminant analysis
# regularization helps when p >> n

library(klaR)
# Fit the model
model <- rda(Species~., data = train.transformed)
# Make predictions
predictions <- model %>% predict(test.transformed)
# Model accuracy
mean(predictions$class == test.transformed$Species)

