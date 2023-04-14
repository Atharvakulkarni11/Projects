install.packages('mlbench')
install.packages('MASS')
install.packages('pROC')
library(mlbench)
library(MASS)
library(pROC)

#load some UCLA admittance data
df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(df)


#1.) Visualize and summarize the data to better understand it.



#2.) Look at a contingency table for admit versus rank.

df$admit <- as.factor(df$admit)
df$rank <- as.factor(df$rank)


xtabs(~diabetes + admit, data = df)
xtabs(~diabetes + rank, data = df)



#3.) Turn rank into a factor instead of a numerical value (hint: use as.factor)
df$rank <- as.factor(df$rank)


#4.) Fit a logistic regression model on admit.
logit <- glm(admit ~ gre+gpa+rank,data=df,family="binomial")
summary(logit)


#5.) What is the probability that Dave will get into UCLA? His data is provided below.
dave <- data.frame(gre=790,gpa=3.8,rank=as.factor(1))
predict(logit,dave)
predict(logit,dave, type = 'response')



