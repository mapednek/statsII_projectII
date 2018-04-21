library(ggplot2)
library(scales)
library(plyr)
library(caret)
library(randomForest)
library(e1071)
library(nnet)

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#set test column
train$test <- FALSE
test$test <- TRUE

#merge to remove some variables
merged <- rbind(train, test)
merged <- merged[ , -which(names(merged) %in% c("workclass","hours_per_week", "native_country", "capital_gain", "capital_loss"))]

# separate train and test sets
train <- subset(merged, merged$test == FALSE)
train$test <- NULL

test <- subset(merged, merged$test == TRUE)
test$test <- NULL
row.names(test) <- 1:nrow(test)

# oversampling dataset
oversampleTrain <- subset(train, train$income == ">50K")
rownames(oversampleTrain) <- 1:nrow(oversampleTrain)

partition <- createDataPartition(y = oversampleTrain$income, times = 1, p = 0.8, list = FALSE)


train.oversampled <- rbind(train, oversampleTrain[partition, ])
rownames(train.oversampled) <- 1:nrow(train.oversampled)
rm(oversampleTrain)

#######
# Normal train set
#######

# running random forest with normal train set
set.seed(1234)

rfmodel <- randomForest(formula = income ~ age + education + education_num + marital_status + occupation + 
                          relationship + race + sex + workclass_category + hours_per_week_category + 
                          global_region + capital_gain_category + capital_loss_category,
                          data = train)

# training accuracy
mean(predict(rfmodel, newdata = train) == train$income)

#test accuracy
mean(predict(rfmodel, newdata = test) == test$income)

#plot
plot(rfmodel)

#confusion matrix
confusionMatrix(data = predict(rfmodel, newdata = train), reference = train$income, positive = levels(test$income)[2])

#######
# Oversampled
#######

# running random forest with oversampled train set
set.seed(1234)

rfmodeloversample <- randomForest(formula = income ~ age + education + education_num + marital_status + occupation + 
  relationship + race + sex + workclass_category + hours_per_week_category + 
  global_region + capital_gain_category + capital_loss_category,
  data = train.oversampled)

# training accuracy
mean(predict(rfmodeloversample, newdata = train.oversampled) == train.oversampled$income)

#test accuracy
mean(predict(rfmodeloversample, newdata = test) == test$income)

#plot
plot(rfmodeloversample)

#confusion matrix
confusionMatrix(data = predict(rfmodeloversample, newdata = train.oversampled), reference = train.oversampled$income, positive = levels(test$income)[2])

