#####FINAL PROJECT by Atharva Kulkarni
###
#install.packages("magrittr")
#install.packages("dplyr")
library(magrittr)
library(dplyr)
#install.packages("DataCombine")
library(DataCombine)
#install.packages("gmodels")
library(gmodels)
library(MASS)
#install.packages("skimr")
library(skimr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(ggplot2)
library(janitor)

library(dplyr)
install.packages("devtools")




getwd()
LCData <- read.csv('LungCapDataCSV.csv')
names(LCData)
colnames(LCData)
#understanding the contents of data
head(LCData)
summary(LCData)
print(LCData$Smoke)
count(LCData,Smoke)

####creating frequency table:

table <- table(LCData$LungCap)
table
table1 <- table(LCData$Gender,LCData$Gender)
table1
table2 <- table(LCData$Smoke, LCData$Gender)
table2
table3 <- table(LCData$Age,LCData$Smoke)
table3
table4 <- table(LCData$LungCap,LCData$Gender)
table4
table5 <- table(LCData$Age,LCData$Caesarean)
table5
table6 <- table(LCData$Gender,LCData$Caesarean)
table6

####cross table

tabyl(LCData,LungCap,Age,Gender)
tabyl(LCData,Caesarean ,Age,Gender)


######descriptive statistics
###mean(LCData$LungCap)
mean(LCData$LungCap,trim=0.10)
median(LCData$LungCap)
#min and max 
min(LCData$LungCap)
max(LCData$LungCap)
range(LCData$LungCap)

##Linear regression AGE LUNG

plot(LCData$Age,LCData$LungCap,main="Relation between age and lung capacity"
     , col="green",las=1, xlab="Age of the patient", ylab="Lung capacity")
mod=lm(LCData$LungCap~LCData$Age) 
abline(mod,lwd=2,col="grey")

##visualization for AGE LUNG linear

ggplot(LCData, aes(x = LungCap, y = Age, color= LungCap))+
  geom_point( aes(fill = LungCap),alpha = 0.3) +
  ggtitle("Lung Capacity - Age") + xlab("Age") + ylab("Lung Capacity")+
  geom_smooth(method = lm, fill='red', color='pink')

ht1 <- LCData[c("Age","LungCap")]
head(ht1)
ggplot(ht1, aes(x = Age, y = LungCap, fill = Age)) + geom_boxplot(alpha = 0.3)+
  ggtitle("Lung Capacity of Smoker vs Non_Smoker")
summary(ht1)

##Linear regression Height LUNG
attach(lung)
plot(LCData$Height,LCData$LungCap,main="Relation between height and lung capacity"
     , col="red",las=1, xlab="Height of the patient", ylab="Lung capacity")
mod=lm(LCData$LungCap~LCData$Age) 

##visualization for HIEGHT LUNG linear

ggplot(LCData, aes(x = LungCap, y = Height, color= LungCap))+
  geom_point( aes(fill = LungCap),alpha = 0.3) +
  ggtitle("Lung Capacity - Height") + xlab("Height") + ylab("Lung Capacity")+
  geom_smooth(method = lm, fill='gray', color='black')

ht2 <- LCData[c("Height","LungCap")]
head(ht2)
ggplot(ht2, aes(x = Height, y = LungCap, fill = Height)) + geom_boxplot(alpha = 0.3)+
  ggtitle("Lung Capacity ")
summary(ht2)


#### correlation between data
cor(LCData$LungCap,LCData$Age)

##Graph for lung capacity 
Dt1 <- LCData[c("Smoke","LungCap")]
head(Dt1)
ggplot(Dt1, aes(x = Smoke, y = LungCap, fill = Smoke)) + geom_boxplot(alpha = 0.3)+
  ggtitle("Lung Capacity of Smoker v Non_Smoker")
summary(Dt1)

###

#one sample t test to compare the lung capacity of smoker vs average mean
Mean <- mean(LCData$LungCap)
Mean
LungCap_Smoker <- subset(LCData[c("LungCap","Smoke")],LCData$Smoke == 'yes')
summary(LungCap_Smoker)

LungCap_Smoker_Mean <- mean(LungCap_Smoker$LungCap)
LungCap_Smoker_Mean
sd(LungCap_Smoker$LungCap)
t.test(LungCap_Smoker$LungCap, mu = 7.86, alternative = 'greater')

#Data subset to compare between Gender and Lung Capacity
dt2 <- LCData[c("Gender","LungCap")]
head(dt2)
ggplot(dt2, aes(x = Gender, y = LungCap, fill = Gender)) + geom_boxplot()+
  ggtitle("Lung Capacity for Male vs Female")
summary(dt2)

###two sample t test - lung capacity for genders
LungCap_Gender <- subset(LCData[c("LungCap","Gender")])
LungCap_Male <- subset(LCData[c("LungCap","Gender")],LCData$Gender == 'male')
mean(LungCap_Male$LungCap)
summary(LungCap_Male)
sd(LungCap_Male$LungCap)
LungCap_Female <- subset(LCData[c("LungCap","Gender")],LCData$Gender == 'female')
mean(LungCap_Female$LungCap)
sd(LungCap_Female$Lung_Cap)
summary(LungCap_Female)
t.test(LungCap_Gender$LungCap ~ LungCap_Gender$Gender, alternative = 'two.sided')

#Data subset-compare  Lung Capacity v caesarean or not
dt3 <- LCData[c("Caesarean","LungCap")]
head(dt3)
ggplot(dt3, aes(x = Caesarean, y = LungCap, fill=Caesarean)) + geom_boxplot()+
  ggtitle("Lung Capacity for Caesarean v not")
summary(dt3)

# two sample t test for lung capacity based on caesarean
LungCap_Caesarean <- subset(LCData[c("LungCap","Caesarean")])
LungCapCeas_yes<- subset(LCData[c("LungCap","Caesarean")],LCData$Caesarean == 'yes')
mean(LungCapCeas_yes$LungCap)
summary(LungCapCeas_yes)
sd(LungCapCeas_yes$LungCap)
LungCapCeas_no <- subset(LCData[c("LungCap","Caesarean")],LCData$Caesarean == 'no')
mean(LungCapCeas_no$LungCap)
sd(LungCapCeas_no$LungCap)
summary(LungCapCeas_no)
t.test(LungCap_Caesarean$LungCap ~ LungCap_Caesarean$Caesarean, alternative = 'two.sided')


###correlation between lung capacity with age and height
Dt1<- LCData[,c('LungCap','Age','Height')]
sum <- cor(Dt1)
round(sum,2)

###visualization of correlation
#install.packages("corrplot")
library(corrplot)
corrplot(sum, type="upper",method = 'pie')


