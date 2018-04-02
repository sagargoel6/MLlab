install.packages("HDclassif")
library(HDclassif)
data(wine)
str(wine)
names(wine) <- c("Type", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash",
                  "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                  "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines",
                  "Proline")
library(stats)
wine_pca <- prcomp(wine, center = TRUE, scale = TRUE)
summary(wine_pca)
biplot(wine_pca)
wine[c(4, 19),]
biplot(wine_pca, xlabs = rep("", nrow(wine)))