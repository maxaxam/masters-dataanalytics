# Name: Milos Maksimovic
# Student Number: 14205449

# Question 2

# Clear variables and Set working directory
rm(list=ls())
setwd("Assignment02")

library(class)
library(klaR)
library(cluster)
library(fpc)

# (a)

# Load the voting data

load("2016_First6Votes_PresentAbsent.Rdata")
head(votes)

votes.tab.ED1 <- table(votes$ED1)
votes.tab.ED2 <- table(votes$ED2)
votes.tab.Credit <- table(votes$Credit)
votes.tab.Confidence1 <- table(votes$Confidence1)
votes.tab.Confidence2 <- table(votes$Confidence2)
votes.tab.Trade <- table(votes$Trade)

votes.tab.Combined <- cbind(votes.tab.ED1, votes.tab.ED2, votes.tab.Credit, votes.tab.Confidence1, votes.tab.Confidence2, votes.tab.Trade)
colnames(votes.tab.Combined) <- c("ED1", "ED2", "Credit", "Confidence1", "Confidence2", "Trade")
barplot(votes.tab.Combined, legend = rownames(votes.tab.Combined), axes = FALSE)
axis(2,at=seq(0,180,20))

# Function for plotting Within sum of squares for different K values, returns WGSS vector
plotWithinDiff <- function(data, K, seed = 123) {
  # Setting seed to the same value so we can reproduce results
  set.seed(seed)
  withindiffs <- rep(0,K)
  # Find withindiff for different values of k
  for (k in 1:K) {
    withindiffs[k] <- sum(kmodes(votes, k)$withindiff)
  }
  
  # Plot the withindiff graph
  plot(1:K, withindiffs, type = "b", main = " The within-cluster simple-matching distance for each cluster for different k values", xlab = "k", ylab = "The within-cluster simple-matching distance for each cluster", col = "blue")
  return(withindiffs)
}

plotWithinDiff(votes, 20, 531) #123 (9) or 100 (7)

plotSilhouetteCoefficient <- function(dist, K) {
  # Not seed dependant
  # set.seed(seed)
  silwidth <- rep(0,K)
  # d<-dist(data, method="binary")
  # Find Silhouette Coefficient for different values of k
  for (k in 2:K) {
    silwidth[k] <-pam(dist,k)$silinfo$avg.width
  }
  
  # Plot the Silhouette Coefficient graph
  plot(1:K, silwidth, type = "b", main = " Silhouette Coefficient average width for different k values", xlab = "k", ylab = "The within-cluster simple-matching distance for each cluster", col = "blue")
  return(silwidth)
}

votes.pam <- votes-1 # Adjust values to 0 and 1 so binary (Jaccard) measure can be used

d<-dist(votes.pam, method="binary")

plotSilhouetteCoefficient(d, 20)

fitpam<-pam(d,k=7)



