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
library(poLCA)

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

votes.binary <- votes-1 # Adjust values to 0 and 1 so binary (Jaccard) measure can be used

votes.binary.dist <- dist(votes.binary, method="binary")

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
  plot(1:K, withindiffs, type = "b", main = " The within-cluster simple-matching distance for each cluster for different k values", xlab = "k", ylab = "Within-cluster simple-matching distance", col = "blue")
  return(withindiffs)
}

plotWithinDiff(votes, 10, 100) #123 (9) or 100 (7)

# Function for plotting silhouette coefficients average width for different K values
# To be used for choosing k (number of clusters) for pam
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
  plot(1:K, silwidth, type = "b", main = " Silhouette Coefficient average width for different k values", xlab = "k", ylab = "Silhouette Coefficient Average Width", col = "blue")
  return(silwidth)
}

plotSilhouetteCoefficient(votes.binary.dist, 10)

fitpam<-pam(votes.binary.dist,k=7)

votes.hclust.average <- hclust(votes.binary.dist, method = "average")
plot(votes.hclust.average)
# From the dendrogram, it looks like it 
# is reasonable to cut dedrogram at height 0.8, 
# which gives us 3 clusters
votes.hclust.average.hcl <- cutree(votes.hclust.average, h = 0.8) # or at 0.7 (5 clusters)
table(votes.hclust.average.hcl)

votes.hclust.single <- hclust(votes.binary.dist, method = "single")
plot(votes.hclust.single)
# From the dendrogram, it looks like it 
# is reasonable to cut dedrogram at height 0.6, 
# which gives us 2 clusters
votes.hclust.single.hcl <- cutree(votes.hclust.single, h = 0.6) # or at 0.4 (12 clusters)
table(votes.hclust.single.hcl)

votes.hclust.complete <- hclust(votes.binary.dist, method = "complete")
plot(votes.hclust.complete)
# From the dendrogram, it looks like it 
# is reasonable to cut dedrogram at height 0.55, 
# which gives us 12 clusters
votes.hclust.complete.hcl <- cutree(votes.hclust.complete, h = 0.55) # or at 0.7
table(votes.hclust.complete.hcl)

# (b)

# Plot goodness of fit (bic, aic, Gsq, Chisq) measures for different values of nclass
poLCAplotGoodnessOfFit <- function(f, data, K, nrep, maxiter) {
  goodFit.bic <- rep(0,K)
  goodFit.aic <- rep(0,K)
  goodFit.Gsq <- rep(0,K)
  goodFit.Chisq <- rep(0,K)
  for (k in 2:K) {
    lc <- poLCA(f, data, nclass = k, nrep = nrep, maxiter = maxiter, verbose = FALSE)
    goodFit.bic[k] <- lc$bic
    goodFit.aic[k] <- lc$aic
    goodFit.Gsq[k] <- lc$Gsq
    goodFit.Chisq[k] <- lc$Chisq
  }
  
  # Plot all goodness of fit measures
  par(mfrow=c(2, 2))
  plot(2:K, goodFit.bic[2:K], type = "b", main = "BIC for different k values", xlab = "k", ylab = "BIC", col = "blue")
  plot(2:K, goodFit.aic[2:K], type = "b", main = "AIC for different k values", xlab = "k", ylab = "AIC", col = "red")
  plot(2:K, goodFit.Gsq[2:K], type = "b", main = "Likelihood ratio for different k values", xlab = "Gsq", ylab = "BIC", col = "green")
  plot(2:K, goodFit.Chisq[2:K], type = "b", main = "Pearson Chi-square for different k values", xlab = "Chisq", ylab = "BIC", col = "purple")
  par(mfrow=c(1, 1))
  goodFit <- rbind(goodFit.bic, goodFit.aic, goodFit.Gsq, goodFit.Chisq)
  rownames(goodFit) <- c("bic", "aic", "Gsq", "Chisq")
  
  return(goodFit)
}

# Model formula
f <- cbind(ED1, ED2, Credit, Confidence1, Confidence2, Trade) ~ 1
nrep <- 10
maxiter <- 3000
poLCAplotGoodnessOfFit(f, data = votes, 10, nrep, maxiter)

# Choosing nclass = 4 as that is when both BIC and AIC are smallest 
# and Gsq and Chisq also start stabilizing (elbow is created)
poLCA(f, votes, nclass = 4, nrep = nrep, maxiter = maxiter, graphs = TRUE)
