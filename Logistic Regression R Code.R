install.packages('mlbench')
install.packages('MASS')
install.packages('pROC')
library(mlbench)
library(MASS)
library(pROC)

#load the data and summarize
data(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)

#drop missing values and look again
newdata <- na.omit(PimaIndiansDiabetes2)
summary(newdata)

#make your plot window bigger to see this
par(mfrow = c(4,2))
for( i in 1:8){
  hist(newdata[,i], main = colnames(newdata)[i],xlab = colnames(newdata)[i], col = 'yellow')
}
pairs(newdata[1:8])

#age and pregnancy highly skewed
newdata$age_bucket <- as.factor(ifelse(newdata$age<=30,"20-30",ifelse(newdata$age<=40,"31-40",ifelse(newdata$age<=50,"41-50","50+"))))
newdata$preg_bucket <- as.factor(ifelse(newdata$pregnant<=5,"0–5",ifelse(newdata$pregnant<=10,"6–10","10+")))


#visualize the predictors by diabedes
par(mfrow = c(3,2))
boxplot(glucose~diabetes, ylab="Glucose", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(pressure~diabetes, ylab="Pressure", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(triceps~diabetes, ylab="triceps", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(insulin~diabetes, ylab="Insulin", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(mass~diabetes, ylab="Mass", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(pedigree~diabetes, ylab="Pedigree", xlab= "Diabetes", col="light blue",data = newdata)


#look at contingency tables for age and pregnancy buckets
xtabs(~diabetes + age_bucket, data = newdata)
xtabs(~diabetes + preg_bucket, data = newdata)


#only keep the age and preg buckets, drop originals
newdata2 <- newdata[,c("diabetes","glucose","pressure","triceps","insulin","mass","pedigree","age_bucket","preg_bucket")]


#fit a logistic regression model (binomial)
logit_1 <- glm(diabetes~., family = binomial,data = newdata2)
summary(logit_1)


#use a stepwise feature selection function to find lowest AIC
logit_2 <- stepAIC(logit_1)

logit_1$aic
logit_2$aic


summary(logit_2)

#look at fitted values
summary(logit_2$fitted.values)

hist(logit_2$fitted.values,main = " Histogram ",xlab = "Probability of 'pos' diabetes", col = 'light green')


#turn into binary prediction
newdata2$Predict <- ifelse(logit_2$fitted.values >0.5,"pos","neg")

#see results
mytable <- table(newdata2$diabetes,newdata2$Predict)
rownames(mytable) <- c("Obs. neg","Obs. pos")
colnames(mytable) <- c("Pred. neg","Pred. pos")
mytable

efficiency <- sum(diag(mytable))/sum(mytable)
efficiency


#measure AUC for sensitivity and specificity
roc(diabetes~logit_2$fitted.values, data = newdata2, plot = TRUE, main = "ROC CURVE", col= "blue")
auc(diabetes~logit_2$fitted.values, data = newdata2)

