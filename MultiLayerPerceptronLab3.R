data(iris)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3))
trainset = iris[ind == 1,]
testset = iris[ind == 2,]
install.packages("neuralnet")
library(neuralnet)
trainset$setosa = trainset$Species == "setosa"
trainset$virginica = trainset$Species == "virginica"
trainset$versicolor = trainset$Species == "versicolor"
network = neuralnet(versicolor + virginica + setosa~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, trainset, hidden=3)
network
network$result.matrix
head(network$generalized.weights[[1]])
plot(network)
par(mfrow=c(2,2))
gwplot(network,selected.covariate="Petal.Width")
gwplot(network,selected.covariate="Sepal.Width")
