install.packages('readr')
install.packages('ggplot2')
install.packages('mlbench')
install.packages('corrplot')
install.packages('Amelia')
install.packages('caret')
install.packages('plotly')
install.packages('caTools')
install.packages('reshape2')
install.packages('dplyr')


library(readr)
library(ggplot2)
library(corrplot)
library(mlbench)
library(Amelia)
library(plotly)
library(reshape2)
library(caret)
library(caTools)
library(dplyr)
getwd()
setwd('C:\\Users\\athar\\Desktop\\ALY6040')
getwd()
data<- read.csv('HousingBos.csv')


head(data)

summary(data)



## checking for NA values in the data provided with help of missmap in Amelia package 
missmap(data,legend=TRUE)

## checking duplicate values
sum(is.na(data))
sum(duplicated(data))

##spliting data in test and train sets

split <- sample.split(data,SplitRatio =0.75)

train <- subset(data,split==TRUE)
test <- subset(data,split==FALSE)


###Checking outiners 

par(mfrow = c(1, 4))
boxplot(data$crim, main='crim',col='Blue')
boxplot(data$zn, main='zn',col='Blue')
boxplot(data$rm, main='rm',col='Blue')
boxplot(data$black, main='black',col='Blue')


##correlation plot 

corrplot(cor(data))
corrplot(cor(data), method = "number", type = "upper", diag = FALSE)
##Scatter plot

dropList <- c('chas','rad','crim','zn','black')
#drop chas and rad because they are non numeric
#drop crim, zn and black because they have lot of outliers
boshousingplot <- data[,!colnames(data) %in% dropList]
splom(boshousingplot,col = 'Sky Blue')




