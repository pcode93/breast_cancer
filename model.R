library(caret)

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

print(confusionMatrix(predict(rF, newdata = test), test$diagnosis))
print(confusionMatrix(predict(svm, newdata = test), test$diagnosis))