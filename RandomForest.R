#############################################
##### Random Forest #########################
#############################################
rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c('e1071', 'randomForest')  
installIfAbsentAndLoad(needed)

# set seed to 6
set.seed(6)

# import data
train <- data.frame(read.csv("train_small.csv",header = T))
n <- nrow(train)
train.indices <- sample(n, .8 * n)
train <- train[train.indices,]
val <- train[-train.indices,]

# convert to factor
train$place_id <- as.factor(train$place_id)

# random forest
RF = randomForest(place_id~., data=train, ntree=500,mtry=4, importance=TRUE, replace=TRUE)
RF

# predict on test sed
RF.yhat = predict(RF, newdata=val)

# train vs test
plot(RF.yhat, train$place_id)

# confusion matrix
(table(RF.yhat, train$place_id))

# variable importance
(RF$importance)
varImpPlot(RF)

# error rate
error.rate <- mean(RF.yhat != val$place_id)
error.rate

train %>% 
  ggplot(aes(x = place_id)) + geom_bar(aes(fill = Correct)) + 
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Prediction Accuracy by ID and Popularity") +
  scale_fill_brewer(palette = "Set1")