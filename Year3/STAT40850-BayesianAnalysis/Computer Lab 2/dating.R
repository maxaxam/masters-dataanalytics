library(rjags)
volcano=list(x=421, phi=64, theta0=370, phi0=400)
#inits=list(theta=5)

jmodel=jags.model(file="dating.model", data=volcano)#, inits)
samps=jags.samples(jmodel,"theta",n.iter=1e4)

plot(density(samps$theta),col=3,lwd=2,xlab="date",main="", xlim=c(300,460))

# now compare to the theoretical results
phi0=20^2
tau0=1/phi0
phi=8^2
tau=1/phi
theta0=370
newx=421
p_tau=tau+tau0
p_mu=(tau*newx+tau0*theta0)/p_tau
curve(dnorm(x, theta0, sqrt(1/tau0)),add=T,col=4,lwd=2,lty=2) # prior
curve(dnorm(x, newx, sqrt(1/tau)),add=T,col=2,lwd=2,lty=2) # likelihood (renormalised)
curve(dnorm(x, p_mu, sqrt(1/p_tau)),add=T,col=5,lwd=2,lty=2) # theoretical posterior
legend("topleft", c("prior", "likelihood", "JAGS posterior","theoretical posterior"), lty=c(2,2,1,2), col=c(4,2,3,5), lwd=c(2,2,2,2))

