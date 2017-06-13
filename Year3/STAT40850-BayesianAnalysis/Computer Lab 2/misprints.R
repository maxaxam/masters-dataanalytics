library(rjags)

x_misprints = c(3, 4, 2, 1, 2, 3)
### Question 1 ###

#Data Section
misprints_data1 = list(N=6, x = x_misprints)

# hyperparameters (parameters for the priors)
alpha0 = 9
beta0 = 6
misprints_hypers1 = list(alpha0=alpha0, beta0=beta0)

# save in a single list
misprints1 = append(misprints_data1, misprints_hypers1)

jmodel1 = jags.model(file="misprints1.model", data=misprints1)
samps1 = jags.samples(jmodel1, "lambda", n.iter=10000)

title1 = paste("Comparison of JAGS model with Gamma(", alpha0, ",", beta0, 
              ") prior and Theoretical posterior", sep="")
plot(density(samps1$lambda), col=3, lwd=2, xlab="misprints", main=title1)

# now compare to the theoretical results
alpha0 = 9
beta0 = 6
curve(dgamma(x, 24, 12), add=TRUE, col=2, lwd=2, lty=2) # theoretical posterior
legend("topright", c("JAGS posterior","Theoretical posterior"), lty=c(1,2), col=c(3,2), lwd=c(2,2))

cat("Mean of JAGS posterior: ", mean(samps1$lambda))
cat("Variance of JAGS posterior: ", var(samps1$lambda))

### Question 2 ###
misprints_data2 = list(N=6, x = x_misprints)

# hyperparameters (parameters for the priors)
a0 = 0
b0 = 10
alpha_jeffreys = 0.5
beta_jeffreys = 0.00001
misprints_hypers2 = 
  list(alpha0=alpha0, beta0=beta0, a0=a0, b0=b0, alpha_jeffreys = alpha_jeffreys, beta_jeffreys = beta_jeffreys)

# save in a single list
misprints2 = append(misprints_data2, misprints_hypers2)

jmodel2 = jags.model(file="misprints2.model", data=misprints2)
samps2 = jags.samples(jmodel2, "lambda", n.iter=10000)

# Comparison
xmin=min(samps2$lambda)
xmax=max(samps2$lambda)
title2 = paste("Comparison of JAGS models with Gamma(", alpha0, ",", beta0, 
              "), Uniform(", a0, ",", b0, 
              ") and Jeffrey's priors", sep="")
plot(density(samps2$lambda[1,,1]), col=2, lwd=2, xlab="misprints", main=title2, xlim=c(xmin,xmax))
lines(density(samps2$lambda[2,,1]), col=3, lwd=2)
lines(density(samps2$lambda[3,,1]), col=4, lwd=2)
legend("topright", 
       c(paste("JAGS models with Gamma(", alpha0, ",", beta0, ") prior", sep = ""),
         paste("JAGS models with Uniform(", a0, ",", b0,") prior", sep = ""), 
         paste("JAGS models with Gamma(", alpha_jeffreys, ",", beta_jeffreys, ") prior", sep = "")),
       lty=c(1,1,1), col=c(2,3,4), lwd=c(2,2,2))

cat(paste("JAGS model with p(lambda)=Gamma(", alpha0, ",", beta0, ") mean / var = ", sep=""), 
    mean(samps2$lambda[1,,1]), var(samps2$lambda[1,,1]), "\n")
cat(paste("JAGS model with p(lambda)=Uniform(", a0, ",", b0, ") mean / var = ", sep=""), 
    mean(samps2$lambda[2,,1]), var(samps2$lambda[2,,1]), "\n")
cat(paste("JAGS model with p(lambda)=Gamma(", alpha_jeffreys, ",", beta_jeffreys, ") mean / var = ", sep=""), 
    mean(samps2$lambda[3,,1]), var(samps2$lambda[3,,1]), "\n")

plot(1, main=paste("Plot of Uniform(", a0, ",", b0, ") and Jeffrey's prior", sep=""), 
     xlim = c(0,10), ylim = c(0,0.15), xlab = "x", ylab = "density")
curve(dunif(x, a0, b0), add=TRUE, col=2, lwd=2, lty=2) # Ubiform
curve(dgamma(x, alpha_jeffreys, beta_jeffreys), add=TRUE, col=3, lwd=2, lty=2) # Jeffreys
legend("topright", 
       c(paste("Uniform(", a0, ",", b0,") prior", sep = ""), 
         paste("Jeffreys' prior - Gamma(", alpha_jeffreys, ",", beta_jeffreys, ")", sep = "")),
       lty=c(1,1), col=c(2,3), lwd=c(2,2))


### Question 3 ###
# New observations with posterior distrbution from Question 1 as a prior
x_newmisprints = c(2,1,6,0)
misprints_data3 = list(N=4, x = x_newmisprints)
# hyperparameters (parameters for the priors)
beta1 = mean(samps1$lambda) / var(samps1$lambda)
alpha1 = mean(samps1$lambda) * beta1
misprints_hypers3 = list(alpha0=alpha1, beta0=beta1)
# save in a single list
misprints3 = append(misprints_data3, misprints_hypers3)
jmodel3 = jags.model(file="misprints1.model", data=misprints3)
samps3 = jags.samples(jmodel3, "lambda", n.iter=10000)


# All observations with original Prior of Gamma(9,6)
misprints_data4 = list(N=10, x = c(x_misprints, x_newmisprints))
# hyperparameters (parameters for the priors)
misprints_hypers4 = list(alpha0=9, beta0=6)
# save in a single list
misprints4 = append(misprints_data4, misprints_hypers4)
jmodel4 = jags.model(file="misprints1.model", data=misprints4)
samps4 = jags.samples(jmodel4, "lambda", n.iter=10000)

# Comparison
title3 = "Comparison of JAGS models with new data only and all observed data"
plot(density(samps3$lambda), col=2, lwd=2, xlab="misprints", main=title3)
lines(density(samps4$lambda), col=3, lwd=2)
legend("topright", c("JAGS model (new data)", "JAGS model (all observed data)"), lty=c(2,2), col=c(2,3), lwd=c(2,2))

cat(paste("JAGS model (new data) with p(lambda)=Gamma(", alpha1, ",", beta1, ") mean / var = ", sep=""), 
    mean(samps3$lambda), var(samps3$lambda), "\n")
cat(paste("JAGS model (all observed data) p(lambda)=Gamma(", alpha0, ",", beta0, ") mean / var = ", sep=""), 
    mean(samps4$lambda), var(samps4$lambda), "\n")

