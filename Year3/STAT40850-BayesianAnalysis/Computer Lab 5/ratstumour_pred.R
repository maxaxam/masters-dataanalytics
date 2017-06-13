require(rjags)
# first the number of tumours
y=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,2,2,2,1,2,2,2,2,2,2,5,2,5,3,2,7,7,3,3,2,9,4,4,4,4,4,4,4,10,10,4,4,4,5,11,12,5,5,6,5,6,6,6,6,16,15,15,9)
# now the number of rats in each experiment
n=c(20,19,20,18,20,18,20,17,20,20,20,19,19,19,20,20,20,20,19,19,18,18,25,24,23,10,20,20,20,20,20,20,49,19,46,27,17,49,47,20,20,13,48,20,20,20,20,20,20,20,50,48,19,19,19,22,46,49,20,20,23,19,22,20,20,20,52,47,46,24)
M=length(y) # number of experiments
rats_tumours=list(M=70, y=y, n=n)

# two overdispersed sets of initial parameter samples
inits1=list(alpha=0.1,beta=0.1,pi=rep(0.1,M))
inits2=list(alpha=10,beta=10,pi=rep(0.9,M))

ratsmodel=jags.model("ratstumour_pred.model",rats_tumours,inits=list(inits1,inits2),n.chains=2,n.adapt=1e3)

rats.samps=coda.samples(ratsmodel,variable.names=c("alpha","beta","pi","ystar"),n.iter=1e4-1e3,thin=10)

yindex=grep("ystar",colnames(rats.samps[[1]])) # find which variables in the output relate to ystar
plot(rats_tumours$y, colMeans(rats.samps[[1]][,yindex])) # plot the mean of the posterior samples against the data

ystarmeans = colMeans(rats.samps[[1]][,yindex])
ystarmeans[56]
y[56]
ystarmeans[70]
y[70]
