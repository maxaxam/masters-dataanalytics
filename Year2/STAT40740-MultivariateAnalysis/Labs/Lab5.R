# k nearest neighbours classification

# 1 Generating multivariate data
library(MASS)
N <- 900
muA <- c(0,0)
SigmaA <- matrix(c(10,3,3,2),2,2)
x <- mvrnorm(n=N, muA, SigmaA)

plot(x)
kdfitx <- kde2d(x[,1],x[,2], n=10)
contour(kdfitx, add=TRUE, col="red", nlevels=6)

muB <- c(3,3)
SigmaB <- matrix(c(10,3,3,2),2,2)
y <- mvrnorm(n=N, muB, SigmaB)

z <- rbind(x, y)
dim(z)
cls <- c(rep(1,N), rep(2,N))
z.dat <- data.frame(z, cls)

plot(z.dat[,1], z.dat[,2], pch = z.dat$cls, col = z.dat$cls)
kdfity <- kde2d(y[,1],y[,2], n=10)

contour(kdfitx, add=TRUE, col="green", nlevels=6)
contour(kdfity, add=TRUE, col="blue", nlevels=6)

# 2 Nearest neighbour classifier

library(class)
index <- c(1:600, 901:1500)
train <- z.dat[index, 1:2]
test <- z.dat[-index, 1:2]
knn(train, test, cl = z.dat[index, 3], k=3)

# 3 Misclassification rate

result <- knn(train, test, cl=z.dat[index, 3], k=3)
(nrow(test) - sum(diag(table(result, z.dat[-index, 3])))) / nrow(test)

miss <- rep(0, 10)
for (k in c(1:10)) {
  result <- knn(train, test, cl=z.dat[index, 3], k=k)
  miss[k]<-(nrow(test) - sum(diag(table(result, z.dat[-index, 3])))) / nrow(test)
}

plot(c(1:10), miss)
lines(c(1:10), miss)

unknown1 <- mvrnorm(n=4, muA, SigmaA)
unknown2 <- mvrnorm(n=4, muB, SigmaB)
unknown <- rbind(unknown1, unknown2)
knn(train, unknown, cl=z.dat[index, 3], k=10)

# 4 Cross validation, other data and other functions
# IRIS

iris
library(ggvis)
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()
str(iris)
head(iris)
table(iris$Species)
round(prop.table(table(iris$Species)) * 100, digits = 1)
summary(iris)
summary(iris[c("Petal.Width", "Sepal.Width")])
library(class)
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_norm)
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]
iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
iris_pred
library(gmodels)
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
