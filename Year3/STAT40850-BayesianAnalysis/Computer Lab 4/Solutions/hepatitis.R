require(rjags)

Y=read.table("hepatitis.dat",header=TRUE) # the raw data
hep.data=list(Y=Y,N=nrow(Y),M=ncol(Y),logt=log(c(1.0, 2.5, 5.0, 7.0, 10.0, 15.0, 20.0)))

hep.model=jags.model("hepatitis.model",hep.data,n.chains=4,n.adapt=1e4)

hep.samps=coda.samples(hep.model,variable.names=c("ma","mb","phi","phia","phib"),n.iter=1e4)#,thin=10)

# note mean of a mean of obs is the same as mean of all obs but quantiles of means are not quantiles of all obs
# so replacing ma with alpha is gives the same result for the mean but different for the Credible Interval
# once converged just use the first chain for summaries
chain=1
cat("Posterior mean of mean alpha = ",  mean(sapply(hep.samps,function(x) x[,grep("ma",colnames(x))])), "\n") # note that replacing ma with alpha gives same result
# can use quantile of samples as follows for credible intervals
#quantile(sapply(hep.samps,function(x) x[,grep("ma",colnames(x))]),prob=c(0.025,0.975)) 
# or just use HPDinterval
cat("mean alpha C.I. =", HPDinterval(hep.samps[[chain]],prob=0.95)[grep("ma",colnames(hep.samps[[chain]])),],"\n")
cat("Posterior mean of mean beta = ",  mean(sapply(hep.samps,function(x) x[,grep("mb",colnames(x))])), "\n") # note that replacing mb with beta gives same result
cat("mean beta C.I. =", HPDinterval(hep.samps[[chain]],prob=0.95)[grep("mb",colnames(hep.samps[[chain]])),],"\n")

#print(gelman.diag(hep.samps,transform=TRUE)) # print the BGR statistics
gelman.plot(hep.samps,ask=TRUE,transform=TRUE) # examine the BGR statistics graphically
