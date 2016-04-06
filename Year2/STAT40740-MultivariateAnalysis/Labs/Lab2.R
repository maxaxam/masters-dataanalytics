setwd("C:/Users/milosma/OneDrive/Documents/MSc in Data Analytics/Year 2 - Semester 2/STAT40740 - Multivariate Analysis/Lab 02")

# 1 Dissimilarity matrices
data(iris)
ls()
data()

iris[1:5,]
x <- iris[1:5, 1:4]
dist.eucl <- dist(x, method = "euclidian")
dist.eucl

# 2 Hierarchical clustering
oliveOil <- read.csv("OliveOilData.csv")
oliveOil
head(oliveOil)
acids <- oliveOil[,3:10]
cl.average <- hclust(dist(acids), method = "average")
plot(cl.average)

cl.single <- hclust(dist(acids), method = "single")
plot(cl.single)

cl.complete <- hclust(dist(acids), method = "complete")
plot(cl.complete)

# 3 Cutting a dendogram to produce groups of data

hcl <- cutree(cl.average, k = 3)
hcl
table(hcl)
table(hcl, oliveOil[,1])

# 4 Functions apply and sweep
StDev <- apply(acids, 2, sd)
StDev
apply(acids, 2, mean)

stdacids <- sweep(acids, 2, StDev, "/")
stdacids

cl.stdaverage <- hclust(dist(stdacids), method = "average")
plot(cl.stdaverage)

cl.stdsingle <- hclust(dist(stdacids), method = "single")
plot(cl.stdsingle)

cl.stdcomplete <- hclust(dist(stdacids), method = "complete")
plot(cl.stdcomplete)

stdhcl <- cutree(cl.stdaverage, k = 3)
stdhcl
table(stdhcl)
table(stdhcl, oliveOil[,1])
table(hcl, oliveOil[,1])

hcl <- cutree(cl.complete, k = 3)
stdhcl <- cutree(cl.stdcomplete, k = 3)

table(stdhcl, oliveOil[,1])
table(hcl, oliveOil[,1])