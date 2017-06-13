require(rjags)

# the data
golfers_data=read.delim("golfers.dat")
golfers_data$ProportionSuccess = golfers_data$Successes / golfers_data$Tries


golfers.data = list(
  N=nrow(golfers_data), Distance=golfers_data$Distance, Tries=golfers_data$Tries, Successes = golfers_data$Successes)


golfers.model=jags.model("golfers.model", golfers.data, n.chains=3)
golfers.samps=coda.samples(golfers.model,variable.names=c("alpha","beta", "p"),1e4)
