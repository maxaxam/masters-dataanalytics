# Exercise 1
# Gibbs sampler
# iters - number of iterations
# y - data vector
# mu - initial values for mu
# tau - initial values for tau
gibbs <- function(iters, y, mu=0, tau=1, 
                  alpha0 = 0.00001, beta0 = 0.00001, 
                  mu0 = 0.0, tau0 = 0.00001,
                  plot_trace = TRUE, plot_hist = TRUE,
                  print_posterior = TRUE, print_prob = TRUE) {
    x <- array(0, c(iters+1, 2))
  x[1,1] <- mu
  x[1,2] <- tau
  
  n <- length(y)
  ybar <- mean(y)
  
  for (t in 2:(iters+1)) {
    x[t,1] <- rnorm(1,
                    (n*ybar*x[t-1,2]+mu0*tau0)/(n*x[t-1,2]+tau0),
                    sqrt(1/(n*x[t-1,2]+tau0)))
    sn <- sum((y-x[t,1])^2)
    x[t,2] <- rgamma(1, alpha0+n/2)/(beta0+sn/2)
  }
  
  par(mfrow=c(1,2), oma=c(0,0,2,0))
  
  if (plot_trace == TRUE) {
    plot(1:length(x[,1]), x[,1], type = 'l', lty = 1, xlab = 't', ylab = 'mu')
    title('Trace plot of mu')
    plot(1:length(x[,2]), 1/x[,2], type = 'l', lty = 1, xlab = 't', ylab = 'sigma^2')
    title('Trace plot of sigma^2')
    title(paste('alpha0 =', alpha0, 'beta0 =', beta0,
                'mu0 =', mu0, 'tau0 =', tau0), outer = TRUE)
  }
  
  if (plot_hist == TRUE) {
    hist(x[,1], freq = FALSE, xlab = "mu", main = NULL, ylab = 'density', ylim = c(0,1))
    title('Histogram of mu')
    lines(density(x[,1]), col = 'red', lwd = 2, lty = 2)
    hist(1/x[,2], freq = FALSE, xlab = "sigma^2", main = NULL, ylab = 'density', ylim = c(0,1))
    title('Histogram of sigma^2')
    lines(density(1/x[,2]), col = 'red', lwd = 2, lty = 2)
    title(paste('alpha0 =', alpha0, 'beta0 =', beta0,
                'mu0 =', mu0, 'tau0 =', tau0), outer = TRUE)
  }
  
  mu_posterior_mean <- x[iters+1,1]
  mu_posterior_var <- (x[iters+1,1] - mean(x[,1]))^2
  sigma2_posterior_mean <- 1/x[iters+1,2]
  sigma2_posterior_var <- (1/x[iters+1,2] - 1/mean(x[,2]))^2
  
  if (print_posterior == TRUE) {
    print(paste('posterior_mean(mu) =', mu_posterior_mean))
    print(paste('posterior_var(mu) =', mu_posterior_var))
    print(paste('posterior_mean(sigma^2) =', sigma2_posterior_mean))
    print(paste('posterior_var(sigma^2) =', sigma2_posterior_var))
  }

  if (print_prob == TRUE) {
    print(paste('P(mu>2.5) =', sum(x[,1]>2.5) / length(x[,1])))
    print(paste('P(sigma^2>2.5) =', sum(1/x[,2]>2.5) / length(x[,2])))
  }

  return(list(
    iters = iters,
    y = y,
    mu = mu,
    tau = tau,
    alpha0 = alpha0,
    beta0 = beta0,
    mu0 = mu0,
    tau0 = tau0,
    mu_mc = x[,1],
    tau_mc = x[,2],
    mu_posterior_mean = mu_posterior_mean,
    mu_posterior_var = mu_posterior_var,
    sigma2_posterior_mean = sigma2_posterior_mean,
    sigma2_posterior_var = sigma2_posterior_var,
    prob_mu_bigger = sum(x[,1]>2.5) / length(x[,1]),
    prob_tau_bigger = sum(1/x[,2]>2.5) / length(x[,2])
  ))
}

# Exercise 2
y <- rnorm(20, 2, 2)
output <- gibbs(5000, y, 5, 5)

# Exercise 3
output$mu_posterior_mean
output$mu_posterior_var
output$sigma2_posterior_mean
output$sigma2_posterior_var
output$prob_mu_bigger
output$prob_tau_bigger

# Exercise 4
for (i in c(0:8)) {
  for (j in c(0:5)) {
      alpha0 =0.00001
      beta0 = 0.00001
      mu0 = 0.0+i*0.25
      tau0 = 0.00001*10^j
      output <-
        gibbs(iters = 5000, y = y, mu = 5, tau = 5,
              alpha0 = alpha0, beta0 = beta0,
              mu0 = mu0, tau0 = tau0,
              plot_trace = FALSE, plot_hist = FALSE,
              print_posterior = FALSE, print_prob = FALSE)
        par(mfrow=c(1,2), oma=c(0,0,2,0))
        plot(1:length(output$mu_mc), output$mu_mc, 
             type = 'l', lty = 1, xlab = 't', ylab = 'mu')
        title('Trace plot of mu')
        hist(output$mu_mc, breaks = 30, xlim = c(0,5), xlab = "mu",
             ylim = c(0,1000), main = NULL)
        title('Histogram of mu')
        title(paste('alpha0 =', alpha0, 'beta0 =', beta0,
                    'mu0 =', mu0, 'tau0 =', tau0), outer = TRUE)
  }
}
