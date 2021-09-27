library(neuralnet)

library(kernlab)



rows.iris <- sample.int(nrow(iris), size = round(nrow(iris)/3), replace = F)

iris.train <- iris[-rows.iris,]

iris.test <- iris[rows.iris,]

model <- neuralnet(formula = Species~., data = iris.train, linear.output = F, hidden = c(3,3))

result <- predict(model, iris.test)

plot(model)

classes <- apply(result, 1, which.max)

table(iris.test$Species, apply(result, 1, which.max))







data(spam)

rows.spam <- sample.int(nrow(spam), size = round(nrow(spam)/3), replace = F)

spam.train <- spam[-rows.spam,]

spam.test <- spam[rows.spam,]

model <- neuralnet(formula = type~., data = spam.train, linear.output = F, hidden = c(1))

result <- predict(model, spam.test)

plot(model)

table(spam.test$type, apply(result, 1, which.max))