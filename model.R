library(randomForest)
library(xgboost)
library(e1071)
library(ROCR)
library(caret)

accuracy <- function(name, model, xtest, ytest) {
  pred = prediction(as.numeric(predict(model, xtest)), as.numeric(ytest))
  auc = performance(pred, measure = "auc")@y.values[[1]]
  print(paste('AUC for ', name, ' = ', auc))
  auc
}

data <- read.csv('data.csv')[2:32]
size <- nrow(data)

trainIds <- sample(c(1:size), floor(0.8 * size))
training <- data[trainIds,]
test <- data[setdiff(c(1:size), trainIds),]

rF <- caret::train(diagnosis ~ ., training,
                   method = "rf",
                   trControl = trainControl(classProbs = T),
                   metric = 'ROC')

svm <- caret::train(diagnosis ~ ., training, method = 'svmLinearWeights')

confusionMatrix(predict(rF, newdata = test), test$diagnosis)
confusionMatrix(predict(svm, newdata = test), test$diagnosis)