require(rjags)

# the data
tmp=read.delim("mining.dat")
glm2.data=list(N=nrow(tmp), Disasters=tmp$Disasters)

glm2.model=jags.model("glm2.model",glm2.data,n.chains=2)
glm2.samps=coda.samples(glm2.model,variable.names=c("alpha","lambda"),1e4)


glm2.dic = dic.samples(glm2.model, 1e4)
glm2.jagssamps=jags.samples(glm2.model,variable.names=c("alpha","lambda","deviance","pD"),1e4)
