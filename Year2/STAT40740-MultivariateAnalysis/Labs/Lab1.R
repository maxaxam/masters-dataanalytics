setwd("C:/Users/milosma/OneDrive/Documents/MSc in Data Analytics/Year 2 - Semester 2/STAT40740 - Multivariate Analysis/Lab 01")

# 1 R basics

2+2
exp(-2)
2*3*4*5
pi
1000*(1+0.75)^5-1000
x <- 10
x
x+x
sqrt(x)

# See all the packages that are currently loaded into workspace
search()

# Help
help(sqrt)
?sqrt
help("[[")
help.start()

# 2 Objects
# 2.1 Vectors
my.vec1 <- c(1, 2, 3, 4)
my.vec2 <- c(1:4)
my.vec3 <- c("train", "house", "car")
my.vec4 <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
my.vec1[1]
my.vec2[c(1,3)]
my.vec1[1] <- 10
my.vec1

# 2.2 Matrices
A <- matrix(c(1,2,2,5), nrow = 2, ncol = 2, byrow = TRUE)
A
A[1,]
A[,2]
A[1,1]
res <- eigen(A)
res

res$values
res$values[1]
res$vectors
res$vectors[1,1]
eigenTest.leftFirst <- A%*%res$vectors[,1]
eigenTest.rightFirst <- res$values[1]*t(t(res$vectors[,1]))
eigenTest.leftFirst - eigenTest.rightFirst

eigenTest.leftSecond <- A%*%res$vectors[,2]
eigenTest.rightSecond <- res$values[2]*t(t(res$vectors[,2]))
eigenTest.leftSecond - eigenTest.rightSecond


# 2.3 Data frames
elasticband <- data.frame(stretch = c(46,54,48,50,44,42,52), 
                          distance = c(148,182,173,166,109,141,166), 
                          ID = c("A","A","B","B","C","C","D"))
elasticband

elasticband$stretch
elasticband[,1]
elasticband[,c(2,3)]
elasticband[,c("distance", "ID")]
elasticband[1,]
elasticband[c(1,3,5),]
names(elasticband)
ls()
rm(list=ls())

# 3 Importing/exporting data
music <- read.csv("MusicData.csv")
music
dim(music)

write.table(music, file="music.csv", sep=",")

# 4 Plots
x <- sample(c(-1000:1000), size = 200, replace = FALSE)
x
x <- sort(x)
y <- x^2 + 3*x + 1
plot(x, y, type = "l", col = "red")

pairs(music[,4:8])

songType <- as.numeric(music[,3])
songType
music[,3]
pairs(music[,4:8], col = songType)

pairs(music[,4:8], col = songType, pch = songType)

library(MASS)
whiteside
typeof(whiteside)
summary(whiteside)
names(whiteside)
hist(whiteside$Temp)
plot(whiteside$Temp, whiteside$Gas, col = whiteside$Insul)
summary(whiteside)