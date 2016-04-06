# Name: Milos Maksimovic
# Student Number: 14205449

# Question 1
setwd("Assignment01")

# Read the data
potteryData <- read.csv("PotteryData.csv")

# Explore the data
potteryData
typeof(potteryData)
summary(potteryData)

# (a) (i)
# Save 9 chemicals
potteryData.chemicals <- potteryData[1:9]
potteryData.chemicals

# Covariance matrix
potteryData.chemicals.covMatrix <- cov(potteryData.chemicals)

# Print covariance matrix
potteryData.chemicals.covMatrix
write.table(potteryData.chemicals.covMatrix, file = "covMatrixChemicals.csv", sep = ",")

# (a) (ii)
# Find Eigen values and vectors
potteryData.chemicals.eigen <- eigen(potteryData.chemicals.covMatrix)
potteryData.chemicals.eigen$values
potteryData.chemicals.eigen$vectors
# Print first two Eigen values
potteryData.chemicals.eigen$values[1:2]
# Print first two Eigen vectors
potteryData.chemicals.eigen$vectors[,1:2]

# Function to verify if value and vector are eigen value and eigen vector for 
# the specified matrix mat
verifyEigen <- function(mat, eigenValue, eigenVector) {
  # Print Eigen value and Eigen vector
  paste("Eigen Value: ", eigenValue, sep = "")
  paste("Eigen Vector: ", eigenVector, sep = "")
  
  # Left hand side of eigen equation
  eigenTest.left <- as.matrix(mat)%*%as.matrix(eigenVector)
  # Right hand side of eigen equation
  eigenTest.right <- eigenValue*as.matrix(eigenVector)
  # Round difference between left and right hand side so imprecision in multiplication is eliminated
  res <- round(eigenTest.left - eigenTest.right,10) 
  # Return true or false depending if value and vector are Eigen.
  return (all(res == 0))
}

# Verify if first value and vector are Eigen
verifyEigen(potteryData.chemicals.covMatrix,
            potteryData.chemicals.eigen$values[1], 
            potteryData.chemicals.eigen$vectors[,1])

# Verify if second value and vector are Eigen
verifyEigen(potteryData.chemicals.covMatrix,
            potteryData.chemicals.eigen$values[2], 
            potteryData.chemicals.eigen$vectors[,2])

# (a) (iii)
# Function to test if two vectors are Orthogonal
verifyOrthogonal <- function(u, v) {
  ut_v = t(u)%*%v
  vt_u = t(v)%*%u
  # Rounding so comparison to zero works
  return (round(ut_v, 10) == 0 && round(vt_u, 10) == 0)
}

verifyOrthogonal(potteryData.chemicals.eigen$vectors[,1], 
                 potteryData.chemicals.eigen$vectors[,2])

# Function to test if two vectors are Orthonormal
verifyOrthonormal <- function(u, v) {
  ut_u = t(u)%*%u
  vt_v = t(v)%*%v
  # Rounding so comparison to zero works
  return (verifyOrthogonal(u,v) && round(ut_u, 10) == 1 && round(vt_v, 10) == 1)
}

verifyOrthonormal(potteryData.chemicals.eigen$vectors[,1], 
                  potteryData.chemicals.eigen$vectors[,2])

# (a) (iv)
# ANSWER: I would not standardize the data as for all the oxides we are measuring
# atomic absorption spectrophotometry. As the measure is using same unit for measurement
# and we are measuring same property for different oxides I think the data doesn't need
# to be standardize. If we standardize it oxides that have a higher measure might lose importance.
# But ideally, we should get an expert in the field to explain us differences in values so we can understand should we 
# standardize the data or not.

# (b)
# Save the input values in variables
x1.mean <- 10
x1.var <- 11
x2.mean <- 8
x2.var <- 14
x1x2.cov <- 2
# (b) (i)
a1 = 1
a2 = -1
# Calculate mean of X1-X2
diff_x1_x2.mean <- a1*x1.mean + a2*x2.mean
# Calculate variance of X1-X2
diff_x1_x2.var <- a1^2*(x1.var)^2 + a2^2*(x2.var)^2 + 2*a1*a2*x1x2.cov
diff_x1_x2.mean
diff_x1_x2.var

# (b) (ii)
# Calculate mean and variance of U=X1-X2
u.mean <- x1.mean - x2.mean
u.var <- (x1.var)^2 + (x2.var)^2 - 2*x1x2.cov

# Calculate mean and variance of V=X1-2*X2
v.mean <- x1.mean - 2*x2.mean
v.var <- (x1.var)^2 + 4*(x2.var)^2 - 2*2*x1x2.cov
# Print the values
u.mean
u.var
v.mean
v.var
# After doing the math on how to calculate covariance of U and V on paper below formula is the result.
uv.cov = x1.var + 2*x2.var - 3*x1x2.cov
# Correlation between U and V
uv.corr = uv.cov / sqrt(u.var*v.var)
uv.corr
