inv.exp <- function(n, lambda) {       # Define the function inv.exp
  u <- runif(n)                        # Generate a U(0,1) number
  x <- -log(1-u)/lambda                # Transform to an exponential
  x                                    # Return x
}

inv.gamma.int <- function(n, k, lambda) {
  x <- matrix(inv.exp(n = n*k, lambda = lambda), ncol=k)
  apply(x, 1, sum)
}

# 1 Modify the code to create a sample of size 2000 
#   from the Ga(3, 0.5) distribution

y <- inv.gamma.int(2000, 3, 0.5)       # Calling method of inversion for sample of size 2000 and for Ga(3,0.5) distribution

# 2 Plot a histogram and compare it to the density of Ga(3, 0.5)

# Plot a histogram
hist(y, freq = FALSE, ylim = c(0,0.14))  
# Create a sequence
t <- 0:300 / 10
# Add the density to the histogram
lines(t, dgamma(t, shape = 3, rate = 0.5), lwd = 2)

# 3 Carry out a Kolmogorov-Smirnov test to check your output
ks.test(y, pgamma, 3, 0.5)

# 4 Rejection sampling for Ga(3.5, 3) distribution.

# Set alpha and lambda
alpha <- 3.5
lambda <- 3


rejection.gamma <- function(n, alpha, lambda) {
  k <- floor(alpha)
  
  # Based on analytical calculation x_M for which M has maximum value 
  # is equal to alpha - floor(alpha)
  x_M <- alpha - k
  # Calculate value of Ga(3.5,3) in for x_M
  dgamma_f <- dgamma(x_M, shape = alpha, rate = lambda)
  # Calculate value of Ga(floor(3.5),2) in for x_M
  dgamma_h <- dgamma(x_M, shape = k, rate = lambda-1)
  M <- dgamma_f / dgamma_h
  
  r <- NULL
  rx <- NULL
  ry <- NULL
  total <- 0
  for (i in 1:n) {
    t <- -1
    while (t < 0) {
      # Count total number of tries
      total <- total + 1
      # X~h(x) - get X using method of inversion from Ga(floor(alpha), lambda-1)
      x <- inv.gamma.int(1, k = k, lambda = lambda - 1)
      rx[total] <- x
      ry[total] <- y
      # Get Y from U(0,1)*Ga(floor(alpha), lambda-1)
      y <- runif(1, 0, M*dgamma(x, shape = k, rate = lambda-1))
      # Calculate difference between value from Ga(alpha, lambda) for value X and Y
      t <- dgamma(x, shape = alpha, rate = lambda) - y
    }
    
    # Save x
    r[i] <- x
  }
  
  # Return the list of interesting values for this rejection sampling
  return(list(
    n = n,
    alpha = alpha,
    lambda = lambda,
    k = k,
    M = M,
    sample = r, # accepted values of x
    allX = rx, # all sampled x
    allY = ry, # all sampled y
    totalSamples = total,  # total number of generated samples
    acceptedSamples = length(r), # total number of accepted samples
    rejectedSamples = total - length(r), # total number of rejected samples
    proportionalAcceptedSamples = length(r) / total)) # proportion of accepted samples
}

res <- rejection.gamma(2000, alpha, lambda)

# Plot a histogram
hist(res$sample, freq = FALSE, xlim = c(0, 6), ylim = c(0,0.8), main = "Histogram of sampled values", xlab = "t", ylab = "density")
# Create a sequence
t <- 0:60 / 10
lines(t, dgamma(t, shape = alpha, rate = lambda), col = 'blue', lwd = 2)
lines(density(res$sample), col = 'red', lwd = 2, lty = 2)

# Kolmogorov-Smirnov test to check output
ks.test(res$sample, pgamma, alpha, lambda)

res$totalSamples
res$proportionalAcceptedSamples
res$M
1/res$M