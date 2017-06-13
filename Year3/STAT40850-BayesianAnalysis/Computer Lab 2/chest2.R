library(rjags)
source("chest_data_hypers.R")

jmodel=jags.model(file="chest2.model", data=chests)
samps=jags.samples(jmodel,"theta",n.iter=1e4)

xmin=min(samps$theta)
xmax=max(samps$theta)
plot(density(samps$theta[1,,1]),col=2,lwd=2,xlab="mean chest",main="",xlim=c(xmin,xmax))
lines(density(samps$theta[2,,1]),col=3,lwd=2)
legend("topleft", c("JAGS model 1", "JAGS model 2"), lty=c(2,2), col=c(2,3), lwd=c(2,2))

cat("JAGS model with p(theta)=Normal(38,9)   mean / var = ", mean(samps$theta[1,,1]), var(samps$theta[1,,1]), "\n")
cat("JAGS model with p(theta)=uniform(0,100) mean / var = ", mean(samps$theta[2,,1]), var(samps$theta[2,,1]), "\n")

