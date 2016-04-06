# Name: Milos Maksimovic
# Student Number: 14205449

# Question 3
rm(list=ls())

setwd("Assignment01")

# Read and investigate the data
pima <- read.csv("Pima.csv")
head(pima)
summary(pima)

# Load MASS library
library(MASS)

# (a)
qda.res <- qda(type~npreg + glu + bp + skin + bmi + ped + age, data = pima)
attributes(qda.res)

N <- nrow(pima)
K <- length(levels(pima$type))

# Create subset dataset containing only women how have or don't have diabetes
pima.no <- subset(pima, type == "No")
pima.yes <- subset(pima, type == "Yes")
# Calculate covariance matrix for each subset of pima data set
pima.no.cov <- cov(pima.no[1:7])
pima.yes.cov <- cov(pima.yes[1:7])

# Function to calculate quadratic discriminant function
qdf <- function(x, prior, mu, covar) {
  x <- matrix(as.numeric(x), ncol = 1)
  
  # Find inverse of covar matrix
  covar_inv = solve(covar)
  # Find determinant of covar matrix
  covar_det = det(covar)
  return(log(prior) - 0.5*log(covar_det) - 0.5*(t(x-mu)%*%covar_inv%*%(x-mu)))
}

# Add a new member
new_member = data.frame(npreg=7, glu=87, bp=50, skin=33, bmi=33.9, ped=0.826, age=30)

# Function for deciding which covariance matrix to use based on k value
pima_cov_fun <- function(k) {
  if (k == 1) {
    # Covariance to be used is for type "No" 
    temp_cov <- pima.no.cov
  }
  else {
    # Covariance to be used is for type "Yes"
    temp_cov <- pima.yes.cov
  }
  
  return(temp_cov)
}

# Function for calculating qdf for new_member based on current qda results, number of groups K and with specified covariance function
# Return result is a vector containg qdf value for new_member for all k=1..K
calculate_qdf <- function(new_member, qda.res, K, cov_fun) {
  calc_qdf <- rep(0, K)
  # Calculating value of quadratic discriminant function for all types
  for (k in 1:K) {
    # Depending on the value of k we decide which covariance we should get.
    temp_cov <- cov_fun(k)
    
    # Call qdf for sepcific values
    calc_qdf[k] <- qdf(new_member, qda.res$prior[k], qda.res$means[k,], temp_cov)
  }
  return(calc_qdf)
}

# Calculate qdf and print values
calc_qdf <- calculate_qdf(new_member, qda.res, K, pima_cov_fun)
paste("Value of qdf for new member for belonging to type No: ", round(calc_qdf[1],5))
paste("Value of qdf for new member for belonging to type Yes: ", round(calc_qdf[2],5))
# New population member does not suffer from diabetes.
levels(pima$type)[calc_qdf == max(calc_qdf)]

# (b)

# Input test data
pima.test <- data.frame(npreg = c(2, 9, 10, 5, 1),
                        glu = c(88, 170, 101, 121, 93),
                        bp = c(58, 74, 76, 72, 70),
                        skin = c(26, 31, 48, 23, 31),
                        bmi = c(28.4, 44.0, 32.9, 26.2, 30.4),
                        ped = c(0.776, 0.403, 0.171, 0.245, 0.315),
                        age = c(22, 43, 63, 30, 23),
                        type = c("No", "Yes", "No", "No", "No"))

# Decide to which type each observation is test data belongs to based on qdf
T <- nrow(pima.test)
# Variable for storing response type
response_type <- rep(0, T)
for (t in 1:T) {
  # Calculate qdf
  calc_qdf <- calculate_qdf(pima.test[t,1:7], qda.res, K, pima_cov_fun)
  # Save the predicted response type
  response_type[t] <- levels(pima$type)[calc_qdf == max(calc_qdf)]
}

# Create a table of response type vs. actual type from test data
miscl_table <- table(response_type, pima.test$type)
miscl_table
# Calculate missclassification rate as one minus sum of the diagonal divided by sum of all elements in the table
misclassification_rate <- 1-sum(diag(miscl_table))/sum(miscl_table)
misclassification_rate
  