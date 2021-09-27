library(kernlab)

library(e1071)





data(iris)

rows.iris <- sample.int(nrow(iris), size = round(nrow(iris)/3), replace = F)

iris.train <- iris[-rows.iris,]

iris.test <- iris[rows.iris,]

svm.model <- svm(x = iris.train[,-5], y = iris.train[,5])

svm.result <- predict(svm.model,newdata = iris.test[,-5])

table(svm.result,iris.test$Species)

acc <- sum(svm.result==iris.test$Species)/length(iris.test$Species)

acc

print(svm.model)







data(spam)

rows.spam <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)

spam.train <- spam[-rows.spam,]

spam.test <- spam[rows.spam,]

svm.model <- svm(x = spam.train[,-58], y = spam.train[,58])

svm.result <- predict(svm.model,newdata = spam.test[,-58])

table(svm.result,spam.test$type)

acc <- sum(svm.result==spam.test$type)/length(spam.test$type)

acc

print(svm.model)
