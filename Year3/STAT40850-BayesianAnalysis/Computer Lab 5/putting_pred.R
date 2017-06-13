require(rjags)
# read in the data
tmp=read.table("putting.dat",header=T)
# put in a list
putting.data=append(list(Ndists=nrow(tmp)),tmp)

putting_quadratic.model=jags.model("putting_quadratic.model",putting.data,n.chains=2)
putting_quadratic.samps=coda.samples(putting_quadratic.model,variable.names=c("alpha","beta1","beta2", "Nsuccstar"),1e4)

Nsuccindex=grep("Nsuccstar",colnames(putting_quadratic.samps[[1]])) # find which variables in the output relate to ystar
plot(putting.data$Nsucc, colMeans(putting_quadratic.samps[[1]][,Nsuccindex])) # plot the mean of the posterior samples against the data
abline(0, 1)
