require(rjags)
# first the number of tumours
y=c(0,0,1,2,3,4,6,0,0,1,1,2,10,5,0,0,2,5,9,4,6,0,0,2,2,10,4,6,0,1,2,5,4,4,6,0,1,2,3,4,5,6,0,1,2,2,4,11,16,0,1,2,7,4,12,15,0,1,2,7,4,5,15,0,1,2,3,4,5,9)
# now the number of rats in each experiment
n=c(20,19,18,20,20,20,23,20,18,18,10,13,48,19,20,18,25,49,48,19,22,20,17,24,19,50,19,20,20,20,23,46,20,19,20,20,20,20,27,20,22,20,20,20,20,17,20,46,52,19,20,20,49,20,49,47,19,19,20,47,20,20,46,19,19,20,20,20,20,24)
M=length(y) # number of experiments
rats_tumours=list(M=70, y=y, n=n)

inits1=list(alpha=0.1,beta=0.1,pi=rep(0.1,M))
inits2=list(alpha=10,beta=10,pi=rep(0.9,M))

ratsmodel=jags.model("ratstumours.model",rats_tumours,inits=inits1)

rats.samps=coda.samples(ratsmodel,variable.names=c("alpha","beta","pi"),1e3)
