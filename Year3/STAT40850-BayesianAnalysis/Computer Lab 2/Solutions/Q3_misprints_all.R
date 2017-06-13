library(rjags)
#Data 
misprints=list(x_all=c(3,4,2,1,2,3,2,1,6,0),x_new=c(2,1,6,0))
# x_all show the full dataset; x_new is only the last 4 pages but the posterior for the first 6 is used as the prior

jmodel=jags.model(file="Q3_misprints_all.model", data=misprints)
samps=jags.samples(jmodel,"lambda",n.iter=1e5)

xmin=min(samps$lambda)
xmax=max(samps$lambda)
plot(density(samps$lambda[1,,1]),col=2,lwd=2,xlab="mean misprints",main="",xlim=c(xmin,xmax))
lines(density(samps$lambda[2,,1]),col=3,lwd=2)
legend("topright", lty=c(1,1,1), col=c(2,3), lwd=c(2,2),
       legend=c(expression(paste(lambda,"~Gamma(9,6); full dataset")), c(expression(paste(lambda,"~Gamma(24,12); new data only")))))




