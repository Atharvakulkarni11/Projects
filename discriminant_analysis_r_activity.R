library(tidyverse)
library(caret)
library(MASS)
theme_set(theme_classic())


# Load the data
df<- read.csv("car_silhouette.csv", header = TRUE)
summary(df)
unique(df$class)

# Split the data into training (80%) and test set (20%)

# normalize the data based on the params of the training set

# Fit an LDA model to the training data and predict performance on the test set

# Try another discriminant analysis model and see how well it performs
