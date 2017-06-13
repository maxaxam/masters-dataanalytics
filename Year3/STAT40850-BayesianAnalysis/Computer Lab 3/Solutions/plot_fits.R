# re-run the models tracing only the fitted probability of success curve
putting_linear.samps=coda.samples(putting_linear.model,variable.names=c("p"),1e4)
putting_quadratic.samps=coda.samples(putting_quadratic.model,variable.names=c("p"),1e4)
putting_cubic.samps=coda.samples(putting_cubic.model,variable.names=c("p"),1e4)
putting_quartic.samps=coda.samples(putting_quartic.model,variable.names=c("p"),1e4)

chain=1 # look at the first chain only for simplicity
par(mfrow=c(1,2)) # create a two part plot window

# plot the data first, then overlay the posterior mean fitted curves
plot(putting.data$dist,putting.data$Nsucc/putting.data$Ntrys,main="Posterior means",xlab="distance",ylab="p(success)")
lines(putting.data$dist,colMeans(putting_linear.samps[[chain]]),col=1)
lines(putting.data$dist,colMeans(putting_quadratic.samps[[chain]]),col=2)
lines(putting.data$dist,colMeans(putting_cubic.samps[[chain]]),col=3)
lines(putting.data$dist,colMeans(putting_quartic.samps[[chain]]),col=4)
legend("topright", c("data","linear","quadratic","cubic","quartic"),pch=c(1,-1,-1,-1,-1),col=c(1,1,2,3,4),lty=c(0,1,1,1,1))

# now just the best model (quartic according to DIC) 
plot(putting.data$dist,putting.data$Nsucc/putting.data$Ntrys,main="Quartic Posterior",xlab="distance",ylab="p(success)")
lines(putting.data$dist,colMeans(putting_quartic.samps[[chain]]),col=4)
# calculate credible intervals at each distance then add upper and lower parts to the posterior mean plot
quartic_HPD=HPDinterval(putting_quartic.samps[[chain]])
lines(putting.data$dist,quartic_HPD[,1],col=4,lty=2)
lines(putting.data$dist,quartic_HPD[,2],col=4,lty=2)
legend("topright", c("data","quartic-mean","quartic-95%"),pch=c(1,-1,-1,-1),col=c(1,4,4,4),lty=c(0,1,2,2))
