require(rjags)

# the data
tmp=read.delim("mining.dat")
# create a list that can be passed to jags.model
mining.data=list(N=nrow(tmp), Disasters=tmp$Disasters)

# build the model in mining.model; use 2 MCMC chains
mining.model=jags.model("mining.model",mining.data,n.chains=2)
# sample from the posterior, tracing the two rates parameters and the year of the change point
mining.samps=coda.samples(mining.model,variable.names=c("lambda","gamma","k"),1e4)

# plot the trace and density plots for the 3 variables
plot(mining.samps[[1]])

chain=1
cat("mean k C.I. =", HPDinterval(mining.samps[[chain]],prob=0.95)[grep("k",colnames(mining.samps[[chain]])),] + 1851,"\n")
