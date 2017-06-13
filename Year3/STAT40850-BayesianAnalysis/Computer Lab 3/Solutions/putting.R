require(rjags)
# read in the data
tmp=read.table("putting.dat",header=T)
# put in a list
putting.data=append(list(Ndists=nrow(tmp)),tmp)

# run a series of models of growing complexity. Each will be a polynomial regression model with 1 more power than the last.
putting_linear.model=jags.model("putting_linear.model",putting.data,n.chains=2)
putting_linear.samps=coda.samples(putting_linear.model,variable.names=c("alpha","beta","pstar5","pstar10","pstar30"),1e4)
putting_linear.dic=dic.samples(putting_linear.model,1e4)

putting_quadratic.model=jags.model("putting_quadratic.model",putting.data,n.chains=2)
putting_quadratic.samps=coda.samples(putting_quadratic.model,variable.names=c("alpha","beta1","beta2","pstar5","pstar10","pstar30"),1e4)
putting_quadratic.dic=dic.samples(putting_quadratic.model,1e4)

putting_cubic.model=jags.model("putting_cubic.model",putting.data,n.chains=2)
putting_cubic.samps=coda.samples(putting_cubic.model,variable.names=c("alpha","beta1","beta2","beta3","pstar5","pstar10","pstar30"),1e4)
putting_cubic.dic=dic.samples(putting_cubic.model,1e4)

putting_quartic.model=jags.model("putting_quartic.model",putting.data,n.chains=2)
putting_quartic.samps=coda.samples(putting_quartic.model,variable.names=c("alpha","beta1","beta2","beta3","beta4","pstar5","pstar10","pstar30"),1e4)
putting_quartic.dic=dic.samples(putting_quartic.model,1e4)

putting_quintic.model=jags.model("putting_quintic.model",putting.data,n.chains=2)
putting_quintic.samps=coda.samples(putting_quintic.model,variable.names=c("alpha","beta1","beta2","beta3","beta4","beta5","pstar5","pstar10","pstar30"),1e4)
putting_quintic.dic=dic.samples(putting_quintic.model,1e4)

putting_inv.model=jags.model("putting_inv.model",putting.data,n.chains=2)
putting_inv.samps=coda.samples(putting_inv.model,variable.names=c("alpha","beta","pstar5","pstar10","pstar30"),1e4)
putting_inv.dic=dic.samples(putting_inv.model,1e4)
