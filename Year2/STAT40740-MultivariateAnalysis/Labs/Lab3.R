setwd("C:/Users/milosma/OneDrive/Documents/MSc in Data Analytics/Year 2 - Semester 2/STAT40740 - Multivariate Analysis/Lab 03")
rm(list=ls())

# 1 Clustering: k-means clustering
data(faithful)
head(faithful)
WGSS <- rep(0,10)
n <- nrow(faithful)
WGSS[1] <- (n-1)*sum(apply(faithful, 2, var))
for (k in 2:10) {
  WGSS[k] <- sum(kmeans(faithful, centers = k)$withinss)
}

plot(1:10, WGSS, type = "b", xlab = "x", ylab = "Within group sum of squares")

K <- 2
cl <- kmeans(faithful, centers = K)
table(cl$cluster)
plot(faithful, col = cl$cluster)
points(cl$centers, col = 1:K, pch = 8)

# 2 Rand Index
hcl <- cutree(hclust(dist(faithful)),2)
pcl <- kmeans(faithful, centers = 2)
tab <- table(hcl, pcl$cluster)
tab
install.packages("e1071")
library(e1071)
library(help=e1071)
agreement <- classAgreement(tab)
agreement$rand

install.packages("mclust")
library(mclust)
library(help=mclust)
adjRand <- adjustedRandIndex(hcl, pcl$cluster)
adjRand
