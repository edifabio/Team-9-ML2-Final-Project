##############
## KNN #######
#############

rm(list=ls())

# set working directory 
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c('e1071','class')  
installIfAbsentAndLoad(needed)

set.seed(6)

# load data
train <- data.frame(read.csv("train_small.csv",header = T))
test <- data.frame(read.csv("test_small.csv",header = T))
# validate <- data.frame(read.csv("val_small.csv",header = T))

rows <- nrow(train)
shrink.indices <- sample(rows, .02 * rows)
train <- train[shrink.indices,]

rows <- nrow(train)
train.indices <- sample(rows, .8 * rows)
train <- train[train.indices,]
validate <- train[-train.indices,]

rows <- nrow(test)
shrink.indices <- sample(rows, .02 * rows)
test <- test[shrink.indices,]


train$place_id <- as.factor(train$place_id)
validate$place_id <- as.factor(validate$place_id)

# create train, validate and test set
train.x <- cbind(train$x, train$y, train$accuracy, train$hour, train$DayOfWeek)
train.y <- train$place_id
validate.x <- cbind(validate$x, validate$y, validate$accuracy, validate$hour, validate$DayOfWeek)
validate.y <- validate$place_id
test.x <- cbind(test$x, test$accuracy, test$hour, test$DayOfWeek)

##############################################
#error = c(1:20)
#for (k in 1:20) {
#  knn.pred <- knn(train.x, validate.x, train.y, k=k)
#  error[k] <- mean(knn.pred != validate.y)
#}
#plot(c(1:20),error)
##############################################

  
# try k = 1 
knn.pred <- knn(train.x, validate.x, train.y, k=1)
df <- cbind.data.frame(knn.pred,validate.y)

size <- nrow(df)
hold <- rep(0,size)
for (i in 1:size) {
  # if the values are the same store a one. if not store a zero. the mean of these will be accuracy
  if (df[i,1]==df[i,2]) {hold[i]=1} else {hold[i]=0}
}

(acc.kone <- mean(hold)) # accuracy rate 
(err.kone <- 1 - acc.kone)

# k = 3 
knn.pred <- knn(train.x, validate.x, train.y, k=3)
df <- cbind.data.frame(knn.pred,validate.y)

size <- nrow(df)
hold <- rep(0,size)
for (i in 1:size) {
  # if the values are the same store a one. if not store a zero. the mean of these will be accuracy
  if (df[i,1]==df[i,2]) {hold[i]=1} else {hold[i]=0}
}

(acc.kone <- mean(hold)) # accuracy rate 
(err.kone <- 1 - acc.kone)


# k = 5 
knn.pred <- knn(train.x, validate.x, train.y, k=5)
df <- cbind.data.frame(knn.pred,validate.y)

size <- nrow(df)
hold <- rep(0,size)
for (i in 1:size) {
  # if the values are the same store a one. if not store a zero. the mean of these will be accuracy
  if (df[i,1]==df[i,2]) {hold[i]=1} else {hold[i]=0}
}

(acc.kone <- mean(hold)) # accuracy rate 
(err.kone <- 1 - acc.kone)

# try k = 10 
knn.pred <- knn(train.x, validate.x, train.y, k=10)
df <- cbind.data.frame(knn.pred,validate.y)

size <- nrow(df)
hold <- rep(0,size)
for (i in 1:size) {
  # if the values are the same store a one. if not store a zero. the mean of these will be accuracy
  if (df[i,1]==df[i,2]) {hold[i]=1} else {hold[i]=0}
}

(acc.kone <- mean(hold)) # accuracy rate 
(err.kone <- 1 - acc.kone)

