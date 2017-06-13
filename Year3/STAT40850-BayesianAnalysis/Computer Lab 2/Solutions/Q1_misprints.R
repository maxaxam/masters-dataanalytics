library(rjags)
source("misprints_data_hypers.R")

jmodel=jags.model(file="Q1_misprints.model", data=misprints)
samps=jags.samples(jmodel,"lambda",n.iter=1e4)

xmin=min(samps$lambda)
xmax=max(samps$lambda)
plot(density(samps$lambda),col=3,lwd=2,xlab="mean misprints",main="",xlim=c(xmin,xmax))
# now compare to the theoretical results
p_a=misprints$a+sum(misprints$x)
p_b=misprints$b+length(misprints$x)
curve(dgamma(x, p_a, p_b),add=T,col=2,lwd=2,lty=2,xmin,xmax) # theoretical posterior
legend("topright", c("theoretical posterior","JAGS posterior"), lty=c(1,1), col=c(2,3), lwd=c(2,2))

cat("JAGS mean / var = ", mean(samps$lambda), var(samps$lambda), "\n")
cat("theoretical mean / var = ", p_a/p_b, p_a/(p_b^2), "\n")
