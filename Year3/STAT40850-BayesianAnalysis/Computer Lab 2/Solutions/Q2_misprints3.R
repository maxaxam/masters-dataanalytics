library(rjags)
source("misprints_data_hypers.R")

jmodel=jags.model(file="Q2_misprints3.model", data=misprints)
samps=jags.samples(jmodel,"lambda",n.iter=1e4)

xmin=min(samps$lambda)
xmax=max(samps$lambda)
plot(density(samps$lambda[1,,1]),col=2,lwd=2,xlab="mean misprints",main="",xlim=c(xmin,xmax))
lines(density(samps$lambda[2,,1]),col=3,lwd=2)
lines(density(samps$lambda[3,,1]),col=4,lwd=2)
legend("topright", lty=c(1,1,1), col=c(2,3,4), lwd=c(2,2,2),
       legend=c(expression(paste(lambda,"~Gamma(9,6)")), c(expression(paste(lambda,"~Unif(0,10)")), c(expression(paste(lambda,"~Gamma(0.5,1e-6)"))))))


