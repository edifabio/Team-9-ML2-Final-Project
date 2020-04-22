##################################################
### prelims                                    ###
##################################################

### clear ###
rm(list=ls())

setwd("C:/Users/Owner/Google Drive/WM/Spring 2020/Machine Learning II (BUAD 5082)/Assignments/TP2")

### packages ###
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c('e1071')  
installIfAbsentAndLoad(needed)

### set seed as 6 ###
set.seed(6)

### bring in data ###

train <- data.frame(read.csv("train_small.csv",header = T))
test <- data.frame(read.csv("test_small.csv",header = T))

### convert categoricals to factors ###

train$place_id <- as.factor(train$place_id)

### shrink data set ###
    # SVM requires an incredibly high number of computations to run. this must be shrunk to perform
    # will return following error: Error: cannot allocate vector of size xxx GB

rows <- nrow(train)
shrink.indices <- sample(rows, .02 * rows)
train <- train[shrink.indices,]

rows <- nrow(test)
shrink.indices <- sample(rows, .02 * rows)
test <- test[shrink.indices,]

### create validate set

rows <- nrow(train)
train.indices <- sample(rows, .8 * rows)
train <- train[train.indices,]
val <- train[-train.indices,]

##################################################
### Testing                                    ###
##################################################

### full radial ###
svm.rad <- svm(place_id~.,data=train,kernel="radial",cost=1,gamma=1,scale=T)

# make predictions 
ypred.rad <- predict(svm.rad,val[,1:5])

# determine accuracy
acc.rad <- mean(ifelse(val$place_id==ypred.rad,1,0))

### more flexible ###
svm.rad.flex <- svm(place_id~.,data=train,kernel="radial",cost=1,gamma=5,scale=T)

# make predictions 
ypred.rad.flex <- predict(svm.rad.flex,val[,1:5])

# determine accuracy
acc.rad.flex <- mean(ifelse(val$place_id==ypred.rad.flex,1,0))

### reducing training errors ###
svm.rad.lcost <- svm(place_id~x + y,data=train,kernel="radial",cost=.01,gamma=5,scale=T)

# make predictions 
ypred.rad.lcost <- predict(svm.rad.lcost,val[,1:5])

# determine accuracy
acc.rad.lcost <- mean(ifelse(val$place_id==ypred.rad.lcost,1,0))

### increasing training errors ###
svm.rad.hcost <- svm(place_id~x + y,data=train,kernel="radial",cost=10,gamma=5,scale=T)

# make predictions 
ypred.rad.hcost <- predict(svm.rad.hcost,val[,1:5])

# determine accuracy
acc.rad.hcost <- mean(ifelse(val$place_id==ypred.rad.hcost,1,0))

##################################################
### Determine best                             ###
##################################################

results.df <- data.frame("model"=c("Regular Radial",
                                   "More Flexible, Gamma = 5",
                                   "Low Cost, cost = .01",
                                   "High Cost, cost = 10"),
                         "error rate"=c(1-acc.rad,
                                        1-acc.rad.flex,
                                        1-acc.rad.lcost,
                                        1-acc.rad.hcost))

print(results.df)
# best model is More Flexible model with gamma = .5 and error rate of about 6%

##################################################
### Making Predictions                         ###
##################################################

ypred.best <- predict(svm.rad.flex,test)
test[,6] <- ypred.best

### display the results ###
names(test)[6] <- "predicted_place_id"
head(test,20)

### plot ###
plot(x=test$x,y=test$y,col=test$predicted_place_id,main = "Predicted Test Values from SVM")
