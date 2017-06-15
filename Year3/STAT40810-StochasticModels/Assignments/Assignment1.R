dZeroTruncatedPoisson <- function(x,lambda)
{
  numer <- lambda^x
  denom <- factorial(x)*(exp(lambda) - 1)
  res <- numer/denom
  
  res
}

dZeroTruncatedPoissonGamma <- function(x,alpha,beta)
{
  numer <- gamma(x + alpha) * beta^x
  denom <- gamma(alpha) * (1 + beta)^(x+alpha) * gamma(x + 1) * (1 - (1 + beta)^(-alpha))
  res <- numer/denom
  res
}

loglikZeroTruncatedPoisson <- function(theta,x)
{
  lambda <- theta[1]
  sum(log(dZeroTruncatedPoisson(x = x,lambda = lambda)))
}

loglikZeroTruncatedPoissonGamma <- function(theta,x)
{
  alpha <- theta[1]
  beta <- theta[2]
  sum(log(dZeroTruncatedPoissonGamma(x = x,alpha = alpha, beta = beta)))
}

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 15, 17, 18, 22, 24, 25, 27, 30, 33, 34, 47, 50, 71, 104)
freq <- c(433, 101, 42, 27, 14, 15, 5, 5, 5, 4, 2, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1)
prob <- freq / sum(freq)
y <- rep(x,freq)
mean(y)
var(y)

lambda0 <- mean(y)
zeroTruncatedPoisson.fit <- optim(par=lambda0, loglikZeroTruncatedPoisson, method="BFGS", x=y, control=list(fnscale=-1), hessian=TRUE)
zeroTruncatedPoisson.fit

# Zero Truncated Poisson Gamma
set.seed(12)
alpha0 = 6.27e-10
N=40
found = 0

# Validate starting points
for (i in 1:N) {
  beta0 = 3.54e-10
  for (j in 1:N) {
    theta0 <- c(alpha0, beta0)
    try(
      { 
        tempfit <- optim(par=theta0, loglikZeroTruncatedPoissonGamma, method="BFGS", x=y, control=list(fnscale=-1), hessian=TRUE); 
        if (tempfit$convergence == 0)
        {
          print("****************");
          print(paste0("alpha0 = ", alpha0," beta0 = ", beta0))
          print(paste0("alpha.est = ", tempfit$par[1]," beta.est = ", tempfit$par[2]))
          print("****************");
          zeroTruncatedPoissonGamma.fit = tempfit
         }
      }, silent = TRUE)
    beta0 = beta0 * runif(1, 1.5, 3)
  }
  alpha0 = alpha0 * runif(1, 1.5, 3)
}

theta0 <- c(0.00378, 0.556)
zeroTruncatedPoissonGamma.fit <- optim(par=theta0, loglikZeroTruncatedPoissonGamma, method="BFGS", x=y, control=list(fnscale=-1), hessian=TRUE)


lambda.est <- zeroTruncatedPoisson.fit$par[1]
zeroTruncatedPoisson.loglik <- loglikZeroTruncatedPoisson(x, lambda.est)
print(paste0("Estimate for lambda = ", lambda.est))
print(paste0("LogLik for Zero truncated Poisson = ", zeroTruncatedPoisson.loglik))
inf <- -zeroTruncatedPoisson.fit$hessian
zeroTruncatedPoisson.se <- sqrt(diag(solve(inf)))
zeroTruncatedPoisson.cilower <- lambda.est - 1.96 * zeroTruncatedPoisson.se
zeroTruncatedPoisson.ciupper <- lambda.est + 1.96 * zeroTruncatedPoisson.se

print(paste0("95% CI for lambda is (", zeroTruncatedPoisson.cilower, ", ", zeroTruncatedPoisson.ciupper,")"))

alpha.est <- zeroTruncatedPoissonGamma.fit$par[1]
beta.est <- zeroTruncatedPoissonGamma.fit$par[2]
zeroTruncatedPoissonGamma.loglik <- loglikZeroTruncatedPoissonGamma(x, c(alpha.est, beta.est))
print(paste0("Estimate for alpha = ", alpha.est))
print(paste0("Estimate for beta = ", beta.est))
print(paste0("LogLik for Zero truncated Poisson Gamma = ", zeroTruncatedPoissonGamma.loglik))
inf <- -zeroTruncatedPoissonGamma.fit$hessian
zeroTruncatedPoissonGamma.se <- sqrt(diag(solve(inf)))
zeroTruncatedPoissonGamma.alphacilower <- alpha.est - 1.96 * zeroTruncatedPoissonGamma.se[1]
zeroTruncatedPoissonGamma.alphaciupper <- alpha.est + 1.96 * zeroTruncatedPoissonGamma.se[1]
zeroTruncatedPoissonGamma.betacilower <- beta.est - 1.96 * zeroTruncatedPoissonGamma.se[2]
zeroTruncatedPoissonGamma.betaciupper <- beta.est + 1.96 * zeroTruncatedPoissonGamma.se[2]

print(paste0("95% CI for alpha is (", zeroTruncatedPoissonGamma.alphacilower, ", ", zeroTruncatedPoissonGamma.alphaciupper,")"))
print(paste0("95% CI for beta is (", zeroTruncatedPoissonGamma.betacilower, ", ", zeroTruncatedPoissonGamma.betaciupper,")"))

# Model comparison
plot(x, prob)
points(x, dZeroTruncatedPoisson(x, lambda = lambda.est), col="green")
points(x, dZeroTruncatedPoissonGamma(x, alpha = alpha.est, beta = beta.est), col="red")

# AIC
zeroTruncatedPoisson.aic <- 2* zeroTruncatedPoisson.loglik - 2*1
zeroTruncatedPoissonGamma.aic <- 2* zeroTruncatedPoissonGamma.loglik - 2*2

print(paste0("AIC for zero truncated Poisson = ", zeroTruncatedPoisson.aic))
print(paste0("AIC for zero truncated Poisson-Gamma = ", zeroTruncatedPoissonGamma.aic))
