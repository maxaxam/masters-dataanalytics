# Name: Milos Maksimovic
# Student Number: 14205449

# Question 1

# Clear variables and Set working directory
rm(list=ls())
setwd("Assignment02")

# Read the data
prices <- read.csv("prices.csv", header = TRUE, sep = ",")

# Split the data between Menlo Park and Palo Alto
prices.MenloPark <- prices[prices$Area == "MP", 2:4]
prices.PaloAlto <- prices[prices$Area == "PA", 2:4]

# Function for finding common covariance matrix
commonCovarianceMatrix <- function(S1, S2, n1, n2) {
  if ((ncol(S1) != ncol(S2)) || (nrow(S1) != nrow(S2))) {
    stop("commonCovarianceMatrix: Matrix S1 and S2 need to have same dimensions")
  }
  
  S <- ((n1-1)*S1 + (n2-1)*S2) / (n1 + n2 - 2)
  return(S)
}

# Test function for finding common covariance matrix (for testing we are using regular values)
commonCovarianceMatrix(matrix(c(1,2,3,4,5,6,7,8,9), byrow=TRUE, nrow=3, ncol=3), matrix(c(2,2,2,2,2,2,2,2,2), byrow=TRUE, nrow=3, ncol=3), 5, 2)

# Stats package has function mahalanobis for calculating Mahalanobis Distance.
# However, documentation is not totally clear how to use it for calculating distance between groups
# so function will be developed to do that calculation for us and results will be compared with the in built function
# Function for calculating mahalanobis distance
mahalanobisDistance <- function(x1, x2) {
  if (ncol(x1) != ncol(x2)) {
    stop("mahalanobisDistance: Number of columns of x1 and x2 for Mahalanobis distance must be the same")
  }
  
  n1 <- nrow(x1)
  n2 <- nrow(x2)
  S1 <- cov(x1)
  S2 <- cov(x2)
  S <- commonCovarianceMatrix(S1, S2, n1, n2)
  mu1 <- colMeans(x1)
  mu2 <- colMeans(x2)
  D_2 <- t(mu1 - mu2) %*% solve(S) %*% (mu1 - mu2)
  return (D_2)
}

# We can validate our function and usage of in-built function by comparing their values
mah.developed <- mahalanobisDistance(prices.MenloPark, prices.PaloAlto)
mah.inbuilt <- mahalanobis(colMeans(prices.MenloPark), colMeans(prices.PaloAlto), commonCovarianceMatrix(cov(prices.MenloPark), cov(prices.PaloAlto), nrow(prices.MenloPark), nrow(prices.PaloAlto)))
mah.developed ==  mah.inbuilt

# The values are the same so we can use either function

# Calculate Hotellings t-test value
hotellingsT_2 <- function(x1, x2) {
  if (ncol(x1) != ncol(x2)) {
    stop("hotellingsT_2: Number of columns of x1 and x2 must be the same")
  }
  
  n1 <- nrow(x1)
  n2 <- nrow(x2)
  T_2 <- (n1*n2)*mahalanobisDistance(x1, x2) / (n1 + n2)
  return (T_2)
}

# Method to check if two populations are significantly different. Returns TRUE if they are different (i.e. if null hypothesis is rejected)
# otherwise returns FALSE (i.e. Fail to reject null hypothesis)
significantlyDifferent <- function(x1, x2, alpha = 0.05) {
  if (ncol(x1) != ncol(x2)) {
    stop("significantlyDifferent: Number of properties (columns) of x1 and x2 must be the same")
  }
  
  n1 <- nrow(x1)
  n2 <- nrow(x2)
  p <- ncol(x1)
  T_2 <- hotellingsT_2(x1, x2)
  F <- (n1 + n2 - p - 1) * T_2 / ((n1 + n2 -2)*p)
  df1 <- p
  df2 <- n1+n2-p-1
  Fcrit <- qf(1-alpha, df1, df2)
  print(F)
  print(df1)
  print(df2)
  print(Fcrit)
  
  if (F > Fcrit) {
    print(paste("Reject Null Hypothesis as F=", F, "is bigger than critical value", Fcrit))
  }
  else {
    print(paste("Fail to Reject Null Hypothesis as F=", F, "is smaller than critical value", Fcrit))
  }
  return (F > Fcrit)
}

significantlyDifferent(prices.MenloPark, prices.PaloAlto)

# As the return value is FALSE we failed to reject that two populations (Menlo Park and Palo Alto) are significantly different