require(rjags)

# The data
tmp=read.table("lr1.dat")
N=nrow(tmp)
X=tmp[,1]
Y=tmp[,2]
lr1.data=list(N=N, X=X, Y=Y)

lr1.model=jags.model("lr1.model",lr1.data,n.chains=2)
lr1.samps=coda.samples(lr1.model,c("alpha","beta1","beta2","sigma","mu"),1e4)

plot(lr1.samps)

par(mfrow=c(1,1))

crosscorr(lr1.samps)
crosscorr.plot(lr1.samps)

lr1.summary <- summary(lr1.samps)
lr1.means <- lr1.summary$statistics[,"Mean"]
lr1.mumeans <- lr1.means[(4:(4+N-1))]

par(mfrow=c(1,1))
plot(X, lr1.mumeans)

