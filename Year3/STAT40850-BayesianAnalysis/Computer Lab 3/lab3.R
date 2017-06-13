require(rjags)

# the data
golfers_data=read.delim("golfers.dat")
golfers_data$ProportionSuccess = golfers_data$Successes / golfers_data$Tries


golfers.data = list(
  N=nrow(golfers_data), 
  Distance=golfers_data$Distance, 
  Tries=golfers_data$Tries, 
  Successes = golfers_data$Successes)

# Models and sampling

golfers.model_bin1=jags.model("golfers_bin1.model", golfers.data, n.chains=3)
golfers.samps_bin1=coda.samples(golfers.model_bin1,variable.names=c("alpha", "y5", "y10", "y30"),1e4)

golfers.model_bin2=jags.model("golfers_bin2.model", golfers.data, n.chains=3)
golfers.samps_bin2=coda.samples(golfers.model_bin2,variable.names=c("alpha","beta", "y5", "y10", "y30"),1e4)

golfers.model_bin3=jags.model("golfers_bin3.model", golfers.data, n.chains=3)
golfers.samps_bin3=coda.samples(golfers.model_bin3,
                                variable.names=c("alpha","beta1", "beta2", "y5", "y10", "y30"),1e4)

golfers.model_bin4=jags.model("golfers_bin4.model", golfers.data, n.chains=3)
golfers.samps_bin4=coda.samples(golfers.model_bin4,
                                variable.names=c("alpha","beta1", "beta2", "beta3",
                                                 "y5", "y10", "y30"),1e4)

# DIC
golfers.dic_bin1 = dic.samples(golfers.model_bin1, 1e4)
golfers.dic_bin2 = dic.samples(golfers.model_bin2, 1e4)
golfers.dic_bin3 = dic.samples(golfers.model_bin3, 1e4)
golfers.dic_bin4 = dic.samples(golfers.model_bin4, 1e4)

golfers.dic_bin1
golfers.dic_bin2
golfers.dic_bin3
golfers.dic_bin4

# Sumamry statistics

golfers.summary_bin1 <- summary(golfers.samps_bin1)
golfers.means_bin1 <- golfers.summary_bin1$statistics[,"Mean"]

golfers.summary_bin2 <- summary(golfers.samps_bin2)
golfers.means_bin2 <- golfers.summary_bin2$statistics[,"Mean"]

golfers.summary_bin3 <- summary(golfers.samps_bin3)
golfers.means_bin3 <- golfers.summary_bin3$statistics[,"Mean"]

golfers.summary_bin4 <- summary(golfers.samps_bin4)
golfers.means_bin4 <- golfers.summary_bin4$statistics[,"Mean"]

# Print means
golfers.means_bin1
golfers.means_bin2
golfers.means_bin3
golfers.means_bin4

# Print quantiles
golfers.summary_bin1$quantiles
golfers.summary_bin2$quantiles
golfers.summary_bin3$quantiles
golfers.summary_bin4$quantiles

par(mfrow=c(1,1))

crosscorr(golfers.samps_bin4)
crosscorr.plot(golfers.samps_bin4)
