
#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
install.packages('readxl')

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(readxl)

#Reading the data set as a dataframe
getwd()
setwd("C:\\Users\\athar\\Desktop\\ALY6040\\Lec2")
cars <- read.csv(file = "car_evaluation.csv", stringsAsFactors = FALSE, header = TRUE)

# structure of the data
str(cars)
table(cars$decision)

# number of rows with missing values
nrow(cars) - sum(complete.cases(cars))

#only keep acceptable and unacceptable
cars <- cars[ cars$decision %in% c("unacc", "acc"), ]

#look at how one feature might impact the decision
table(cars$decision, cars$safety)

number.perfect.splits <- apply(X=cars[1:6], MARGIN = 2, FUN = function(col){
  t <- table(cars$decision,col)
  sum(t == 0)
})


# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")



#data splicing
set.seed(12345)
train <- sample(1:nrow(cars),size = ceiling(0.80*nrow(cars)),replace = FALSE)
# training set
cars_train <- cars[train,]
# test set
cars_test <- cars[-train,]


# building the classification tree with rpart
tree <- rpart(decision~.,
              data=cars_train,
              method = "class")


# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)


# choosing the best complexity parameter "cp" to prune the tree
cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
# tree prunning using the best complexity parameter. For more in
tree <- prune(tree, cp=cp.optim)

#Testing the model
pred <- predict(object=tree,cars_test[1:6],type="class")


#Calculating accuracy
t <- table(cars_test$decision,pred) 
confusionMatrix(t) 
