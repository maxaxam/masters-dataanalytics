require(rjags)

# The data
tmp=read.table("lr1.dat")
N=nrow(tmp)
X=tmp[,1]
Y=tmp[,2]
lr1.data=list(N=30, X=tmp[,1],Y=tmp[,2])

lr2.model=jags.model("lr2.model",lr1.data,n.chains=2)
lr2.samps=coda.samples(lr2.model,c("alpha","beta1","beta2","sigma","mu"),1e4)

par(mfrow=c(1,1))

crosscorr(lr2.samps)
crosscorr.plot(lr2.samps)

lr2.summary <- summary(lr2.samps)
lr2.means <- lr2.summary$statistics[,"Mean"]
lr2.mumeans <- lr2.means[(4:(4+N-1))]

par(mfrow=c(1,1))
plot(X, lr2.mumeans)

