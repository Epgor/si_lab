library(rpart)

library(kernlab)

library(adabag)

library(randomForest)

library(mlbench)

library(C50)



model.rpart <-rpart(Species~., data=iris)

iris.c50 <-C5.0(Species~., data=iris)



plot(model.rpart)
drawtreemodel(model.rpart)

text(model.rpart)

plot(iris.c50)

text(iris.c50)





data(spam)

rows <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)

spam.train <- spam[-rows,]

spam.test <- spam[rows,]







spam.cart.model <-rpart(formula = type~., data = spam.train)



spam.cart.predict <- predict(spam.cart.model, newdata = spam.test[,-58], type = "class")

error.cart <- sum(spam.test[,58] != spam.cart.predict)/length(spam.cart.predict)



plot(spam.cart.model)

text(spam.cart.model)

error.cart



spam.c50.model <-C5.0(formula = type~., data = spam.train)

plot(spam.c50.model)

text(spam.c50.model)

spam.c50.predict <- predict(spam.c50.model, newdata = spam.test[,-58], type = "class")

error.c50 <- sum(spam.test[,58] != spam.c50.predict)/length(spam.c50.predict)

error.c50



spam.model.bagging<-bagging (formula = type~., data = spam.train, nbagg = 100)

spam.bagging.predict <- predict(spam.model.bagging, newdata = spam.test[,-58])

error.bagging2 <- sum(spam.test[,58] != spam.bagging.predict$class)/length(spam.bagging.predict$class)

error.bagging2







model.boosting <- boosting(formula = type~., data = spam.train, mfinal = 100)

boosting.result <- predict(model.boosting, newdata = spam.test)

error.boosting1 <- sum(spam.test[,58] != boosting.result$class)/length(boosting.result$class)

error.boosting1





model.rf <- randomForest( x = spam.train[, -58], y = spam.train[,58],do.trace = 25, ntree = 500, importance = T)

rf.result <- predict(model.rf, newdata = spam.test[,-58])

error.rf1 <- sum(spam.test[,58] != rf.result)/length(rf.result)

error.rf1

plot(model.rf)
plotcp(model.rf)



suma = 0
data(spam)
folds <- cut(seq(1,nrow(spam)), breaks=10, labels=FALSE)
for(i in 1:10)
{
  rows <- which(folds==i, arr.ind=TRUE)
  spam.train <- spam[-rows,]
  spam.test <- spam[rows,]
  spam.cart.model <- rpart(formula = type~., data = spam.train)
  spam.cart.predict <- predict(spam.cart.model, newdata = spam.test[, -58], type = "class")
  
  error.cart <- sum(spam.test[,58] != spam.cart.predict)/length(spam.cart.predict)
  suma <- suma + error.cart
  print(error.cart)

}
suma/10



suma = 0
folds <- cut(seq(1,nrow(spam)), breaks=10, labels=FALSE)
for(i in 1:10)
{
  rows <- which(folds==i, arr.ind=TRUE)
  spam.train <- spam[-rows,]
  spam.test <- spam[rows,]
  spam.model.bagging <- bagging (formula = type~., data = spam.train, mfinal = 50)
  
  spam.bagging.predict <- predict(spam.model.bagging, newdata =spam.test[,-58])
  
  error.bagging2 <- sum(spam.test[,58] != spam.bagging.predict$class)/length(spam.bagging.predict$class)
  
  suma <- suma + error.bagging2
  print(error.bagging2)

}
suma/10




suma = 0
folds <- cut(seq(1,nrow(spam)), breaks=10, labels=FALSE)
for(i in 1:10)
{
  rows <- which(folds==i, arr.ind=TRUE)
  spam.train <- spam[-rows,]
  spam.test <- spam[rows,]
  model.boosting <- boosting(formula = type~., data = spam.train,mfinal = 50)
  
  boosting.result <- predict(model.boosting, newdata = spam.test)
  error.boosting1 <- sum(spam.test[,58] != boosting.result$class)/length(boosting.result$class)
  
  print(error.boosting1)
  suma <- suma + error.boosting1
  
}
suma/10




suma = 0
folds <- cut(seq(1,nrow(spam)), breaks=10, labels=FALSE)
for(i in 1:10)
{
  rows <- which(folds==i, arr.ind=TRUE)
  spam.train <- spam[-rows,]
  spam.test <- spam[rows,]
  model.rf <- randomForest(x=spam.train[,-58], y = spam.train[,58],ntree = 200, do.trace = 100)
  
  rf.result <- predict(model.rf, newdata = spam.test[,-58])
  error.rf1 <- sum(spam.test[,58] != rf.result)/length(rf.result)
  suma <- suma + error.rf1
  
}
suma/10

