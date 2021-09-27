install.packages('class')
library(class)
rows.iris <- sample.int(nrow(iris), size = round(nrow(iris)/3), replace = F)

iris.train <- iris[-rows.iris,-5]

iris.test <- iris[rows.iris,-5]

cl.train.iris <-iris[-rows.iris,5]

cl.test.iris <- iris[rows.iris,5]


knn.result.iris <- knn(train = iris.train, test = iris.test, cl = cl.train.iris, k = 5)

error.iris <- sum(cl.test.iris != knn.result.iris)/length(knn.result.iris)

error.iris

table(cl.test.iris, knn.result.iris)

#knn.result.iris


View(iris)

library(kernlab)

data("spam")

View(spam)

rows.spam <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)

ncol(spam)

spam.train <- spam[-rows.spam,-58]

spam.test <- spam[rows.spam,-58]

cl.train.spam <-spam[-rows.spam,58]

cl.test.spam <- spam[rows.spam,58]

knn.result.spam <- knn(train = spam.train, test = spam.test, cl = cl.train.spam, k = 5)

error.spam <- sum(cl.test.spam != knn.result.spam)/length(knn.result.spam)

table(cl.test.spam, knn.result.spam)


suma = 0
folds<-cut(seq(1,nrow(iris)), breaks=10, labels=FALSE)
for(i in 1:10)
{
  rows<-which(folds==i, arr.ind=TRUE)
  iris.train<-iris[-rows,]
  iris.test<-iris[rows,]
  knn.result<-knn(train=iris.train[,-5], test=iris.test[,-5], cl=iris.train[,5], k= 10)
  error <- 1-sum(knn.result == iris.test[,5])/length(iris.test[,5])
  sum(knn.result!=iris.test[,5])
  table(knn.result, iris.test$Species)
  suma<-suma+error
}
suma/10



suma = 0
folds<-cut(seq(1,nrow(spam)), breaks=10, labels=FALSE)
for(i in 1:10)
{
  rows<-which(folds==i, arr.ind=TRUE)
  spam.train<-spam[-rows,]
  spam.test<-spam[rows,]
  knn.result<-knn(train=spam.train[,-58], test=spam.test[,-58], cl=spam.train[,58], k= 10)
  error <- 1-sum(knn.result == spam.test[,58])/length(spam.test[,58])
  sum(knn.result!=spam.test[,58])
  table(knn.result, spam.test$type)
  suma<-suma+error
}
suma/10
i = 0.277374
i
