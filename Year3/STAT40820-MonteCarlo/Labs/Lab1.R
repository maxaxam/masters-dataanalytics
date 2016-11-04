lambda <- 1                            # Define rate
u <- runif(1)                          # Generate a U(0,1) number
x <- -log(1-u)/lambda                  # Transform to an exponential
x                                      # Output to screen

n <- 20                                # Define the sample size
lambda <- 1                            # Define the rate
u <- runif(n)                          # Vector of n U(0,1) numbers
x <- -log(1-u)/lambda                  # Transform to vector of exponentials
x                                      # Output to screen

inv.exp <- function(n, lambda) {       # Define the function inv.exp
  u <- runif(n)                        # Generate a U(0,1) number
  x <- -log(1-u)/lambda                # Transform to an exponential
  x                                    # Return x
}

inv.exp(n = 20, lambda = 1)

x <- inv.exp(n = 1000, lambda = 0.5)   # Generate a large sample
hist(x, freq = FALSE)                  # Plot a histogram
t <- 0:150 / 10                        # Create a sequence
lines(t, dexp(t, rate = 0.5), lwd = 2) # Add the density to the histogram

ks.test(x, pexp, rate = 0.5)           # K-S test

k <- 4
lambda <- 1
x <- inv.exp(n = k, lambda = lambda)   # Draw an exp number
y <- sum(x)
y

n <- 20                                # define n                                           
k <- 4                                 # define k
lambda <- 1                            # define lambda
x <- matrix(inv.exp(n = n*k, lambda = lambda), ncol = k)
y <- apply(x, 1, sum)                  # sum up each row
y                                      # output y

inv.gamma.int <- function(n, k, lambda) {
  x <- matrix(inv.exp(n = n*k, lambda = lambda), ncol=k)
  apply(x, 1, sum)
}

y <- inv.gamma.int(20, 4, 1)          # test out the function