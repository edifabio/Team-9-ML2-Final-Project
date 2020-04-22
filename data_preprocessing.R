rm(list=ls())

set.seed(6)

### bring in our data ###

original_train <- read.csv(file="train.csv",sep=",",header=T)
original_test <- read.csv(file="test.csv",sep=",",header=T)

### reduce range of data to 1 x 1 kilometer square ###

original_train.df <- data.frame(original_train)
small_train <- subset.data.frame(original_train.df,original_train.df$x>1)
small_train <- subset.data.frame(small_train,small_train$x<1.5)
small_train <- subset.data.frame(small_train,small_train$y<2.5)
small_train <- subset.data.frame(small_train,small_train$y>2)

original_test.df <- data.frame(original_test)
small_test <- subset.data.frame(original_test.df,original_test.df$x>1)
small_test <- subset.data.frame(small_test,small_test$x<1.5)
small_test <- subset.data.frame(small_test,small_test$y<2.5)
small_test <- subset.data.frame(small_test,small_test$y>2)

### working with time ###

hours_in_day <- 24
minutes_in_day <- hours_in_day * 60

small_train$hour <- (small_train$time/60) %% hours_in_day
small_train$DayOfWeek <- floor((small_train$time/(minutes_in_day)) %% 7) + 1

small_test$hour <- (small_test$time/60) %% hours_in_day
small_test$DayOfWeek <- floor((small_test$time/(minutes_in_day)) %% 7) + 1

### reorder data ###

small_train <- small_train[,c(1,2,3,4,5,7,8,6)]

### create validation set ###

rows <- nrow(small_train)
train.indices <- sample(rows, .8 * rows)
train <- small_train[train.indices,]
val <- small_train[-train.indices,]
test <- small_test

### remove redunant information

drops <- c("row_id","time")
train <- small_train[,!(names(small_train) %in% drops)]
val <- val[,!(names(val) %in% drops)]
test <- small_test[,!(names(test) %in% drops)]

### convert to factors ###

train$place_id <- as.factor(train$place_id)
val$place_id <- as.factor(val$place_id)

### export ###

write.csv(train,"train_small.csv",row.names = F)
write.csv(val,"val_small.csv",row.names = F)
write.csv(test,"test_small.csv",row.names = F)
