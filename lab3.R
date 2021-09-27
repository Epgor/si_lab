library(e1071)

rows.iris <- sample.int(nrow(iris), size = round(nrow(iris)/3), replace = F)

iris.train <- iris[-rows.iris,-5]

iris.test <- iris[rows.iris,-5]

cl.train.iris <-iris[-rows.iris,5]

cl.test.iris <- iris[rows.iris,5]

model.bayes.iris <- naiveBayes( x = iris.train, y = cl.train.iris )

bayes.result <- predict(model.bayes.iris,newdata = iris.test)

error.iris <- sum(cl.test.iris != bayes.result)/length(bayes.result)

error.iris

table(cl.test.iris, bayes.result)


suma = 0
folds<-cut(seq(1,nrow(iris)), breaks=10, labels=FALSE)
for(i in 1:10)
{
  rows<-which(folds==i, arr.ind=TRUE)
  iris.train<-iris[-rows,]
  iris.test<-iris[rows,]
  model.bayes.iris<-naiveBayes(x=iris.train[,-5], y=iris.train[,5], k= 10)
  bayes.result <- predict(model.bayes.iris,newdata = iris.test[,-5])
  error <- 1-sum(bayes.result == iris.test[,5])/length(iris.test[,5])
  sum(bayes.result!=iris.test[,5])
  table(bayes.result, iris.test$Species)
  suma<-suma+error
}
suma/10



rows.spam <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)

spam.train <- spam[-rows.spam,-58]

spam.test <- spam[rows.spam,-58]

cl.train.spam <-spam[-rows.spam,58]

cl.test.spam <- spam[rows.spam,58]

model.bayes.spam <- naiveBayes( x = spam.train, y = cl.train.spam )

bayes.result <- predict(model.bayes.spam,newdata = spam.test)

error.spam <- sum(cl.test.spam != bayes.result)/length(bayes.result)

table(cl.test.spam, bayes.result)

error.spam


suma = 0
folds<-cut(seq(1,nrow(spam)), breaks=10, labels=FALSE)
for(i in 1:10)
{
  rows<-which(folds==i, arr.ind=TRUE)
  spam.train<-spam[-rows,]
  spam.test<-spam[rows,]
  model.bayes.spam<-naiveBayes(x=spam.train[,-58], y=spam.train[,58], k= 10)
  bayes.result <- predict(model.bayes.spam,newdata = spam.test[,-58])
  error <- 1-sum(bayes.result == spam.test[,58])/length(spam.test[,58])
  sum(bayes.result!=spam.test[,58])
  table(bayes.result, spam.test$type)
  suma<-suma+error
}
suma/10
suma
