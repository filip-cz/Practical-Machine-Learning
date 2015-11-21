library(caret)
setwd("C:/Users/fi062864/Desktop/kaggle/coursera/")
train <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")

set.seed(333)
inTrain <- createDataPartition(y=train$classe, p=0.7, list=F)
training <- train[inTrain, ]
testing <- train[-inTrain, ]

nzv <- nearZeroVar(training)
training <- training[, -nzv]
testing <- testing[, -nzv]

# remove variables that are almost always NA
mostlyNA <- sapply(training, function(x) mean(is.na(x))) > 0.95
training <- training[, mostlyNA==F]
testing <- testing[, mostlyNA==F]

# remove variables that don't make intuitive sense for prediction (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp), which happen to be the first five variables
training <- training[, -(1:5)]
testing <- testing[, -(1:5)]

# instruct train to use 3-fold CV to select optimal tuning parameters
fitControl <- trainControl(method="cv", number=3, verboseIter=F)

# fit model on training
fit <- train(classe ~ ., data=training, method="rf", trControl=fitControl)

fit$finalModel

# use model to predict classe in validation set (ptrain2)
preds <- predict(fit, newdata=testing)

# show confusion matrix to get estimate of out-of-sample error
confusionMatrix(testing$classe, preds)


# final predictions
test_pred <- test
test_pred$pred <- predict(fit, newdata=test)
test_pred$pred


# result function
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(test_pred$pred)
