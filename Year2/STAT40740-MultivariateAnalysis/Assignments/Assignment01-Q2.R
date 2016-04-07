# Name: Milos Maksimovic
# Student Number: 14205449

# Question 2
setwd("Assignment01")

# Read the data
potteryData <- read.csv("PotteryData.csv")

# Explore the data
potteryData
typeof(potteryData)
summary(potteryData)

potteryData.chemicals <- potteryData[1:9]

# (a)
# Set seed so we can repeat the results
seed <- 1000

# Function for plotting Within sum of squares for different K values, returns WGSS vector
plotWGSS <- function(data, K, seed = 123) {
  # Setting seed to the same value so we can reproduce results
  set.seed(seed)
  WGSS <- rep(0,K)
  n <- nrow(data)
  # Find WGSS for k = 1
  WGSS[1] <- (n-1)*sum(apply(data, 2, var))
  # Find WGSS for different values of k
  for (k in 2:10) {
    WGSS[k] <- sum(kmeans(data, centers = k)$withinss)
  }
  
  # Plot the WGSS graph
  plot(1:K, WGSS, type = "b", main = " Within group sum of squares for different k values", xlab = "k", ylab = "Within group sum of squares", col = "blue")
  return(WGSS)
}

# plot WGSS graph for chemicals
plotWGSS(potteryData.chemicals, 10, seed)

# Based on the Within sum of square graph for different k-values we see that k-means stabilizes for k = 4 so we choose K=4 for further calculations.

# Function for calculating k-means and plotting the clusters that are discovered using k-means
calcAndPlotKmeans <- function(data, K, seed = 123) {
  set.seed(seed)
  res <- kmeans(data, centers = K)
  table(res$cluster)
  plot(data, col = res$cluster)
  points(res$centers, col = 1:K, pch = 8)
  return (res)
}

# Calculate clusters for k = 4
K <- 4
potteryData.kmeans4<-calcAndPlotKmeans(potteryData.chemicals, K, seed)
table(potteryData.kmeans4$cluster)

# (b)
potteryData.hclust.average <- hclust(dist(potteryData.chemicals), method = "average")
plot(potteryData.hclust.average)
# From the dendrogram, it looks like it is reasonable to cut dedrogram at height 4, which gives us 3 clusters
hcl <- cutree(potteryData.hclust.average, h = 4)
table(hcl)

# (c)
# Load library mclust
library(mclust)
# Calculate and print Adjusted Rand Index
adjRand <- adjustedRandIndex(hcl, potteryData.kmeans4$cluster)
adjRand
table(hcl, potteryData.kmeans4$cluster)


# (d)
# Relationship between k-means clusters and klin variable
table(potteryData.kmeans4$cluster, potteryData$kiln)
# Relationship between clusters created using hierarchical clustering and klin variable
table(hcl, potteryData$kiln)