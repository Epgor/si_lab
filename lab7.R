library(pvclust)

library(mclust)


scale.iris <- scale(iris[,1:4])

kmean.results <- kmeans(scale.iris, centers = 3)


d <- dist(scale.iris, method = "euclidean")

hclust.result <- hclust(d,method="ward.D")

plot(hclust.result)



pvclust.result <- pvclust(scale.iris, method.hclust="ward.D", method.dist="euclidean")

plot(pvclust.result)

pvrect(pvclust.result, alpha=.95)



mclust.result <- Mclust(scale.iris)

plot(mclust.result) # plot results

summary(mclust.result)






scale.iris <- scale(iris[,1:4])
iris.clusters <- kmeans(scale.iris, 6)
table(iris.clusters$cluster, iris$Species)
plot(iris.clusters$cluster)
iris.clusters$size
iris.clusters$iter
iris.clusters$centers




library(MASS)




scale.iris <- scale(iris[,1:4])
d <- dist(scale.iris, method="euclidean")
hclust.result <- hclust(d, method="ward.D")
plot(hclust.result)





scale.iris <- scale(iris[,1:4])
d <- dist(scale.iris, method="euclidean")
hclust.result <- hclust(d, method="ward.D2")
plot(hclust.result)



