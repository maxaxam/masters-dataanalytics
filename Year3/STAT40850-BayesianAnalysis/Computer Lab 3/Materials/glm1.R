require(rjags)

# the data
tmp=read.delim("mining.dat")
glm1.data=list(N=nrow(tmp), Year=tmp$Year, Disasters=tmp$Disasters)

glm1.model=jags.model("glm1.model",glm1.data,n.chains=2)
glm1.samps=coda.samples(glm1.model,variable.names=c("alpha","beta", "lambda", "ystar"),1e4)

par(mfrow=c(1,1))
#plot(glm1.samps)

crosscorr(glm1.samps)
crosscorr.plot(glm1.samps)


glm1.summary <- summary(glm1.samps)
glm1.means <- glm1.summary$statistics[,"Mean"]


mt <- as.matrix(output)
m <- apply(mt, 2, mean)

glm1.dic = dic.samples(glm1.model, 1e4)
