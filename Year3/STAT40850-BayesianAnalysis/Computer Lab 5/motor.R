require(rjags)
require(splines)
motor = read.table("motor.dat",header=T)
motor = append(list(N=nrow(motor)), motor)
K = 20
dic = list()
for (k in (4:K)) # starting at 4 as R complains everything lower is too small
{
  X = bs(motor$times, k, intercept = TRUE)
  motor.data = append(list(X=X),motor)
  motor.data = append(list(K=k),motor.data)

  motor.model = jags.model("motor.model", motor.data,n.chains=2)
  motor.samps = coda.samples(motor.model, variable.names=c("tau","lambda","b", "accelstar"),1e4)
  motor.gelmman = gelman.diag(motor.samps,transform=TRUE)
  motor.dic = dic.samples(motor.model, 1e4)
  dic = append(dic, list(motor.dic))
  accelindex=grep("accelstar",colnames(motor.samps[[1]])) # find which variables in the output relate to ystar
  plot(motor.data$accel, colMeans(motor.samps[[1]][,accelindex]), main = paste("K = ", k)) # plot the mean of the posterior samples against the data
  abline(0, 1)
}

dic # check DIC values. Smaller dic values are giving bad results

k = 7

X = bs(motor$times, k, intercept = TRUE)
motor.data = append(list(X=X),motor)
motor.data = append(list(K=k),motor.data)

motor.model = jags.model("motor.model", motor.data,n.chains=2)
motor.samps = coda.samples(motor.model, variable.names=c("tau","lambda","b", "accelstar"),1e4)

# I tried to run gelman.diag and gelman.plot but it would not finish on my machine
#motor.gelmman = gelman.diag(motor.samps,transform=TRUE)
#acfplot(motor.samps)
#gelman.plot(motor.samps,ask=TRUE,transform=TRUE) # examine the BGR statistics graphically

accelindex=grep("accelstar",colnames(motor.samps[[1]])) # find which variables in the output relate to ystar
plot(motor.data$accel, colMeans(motor.samps[[1]][,accelindex]), main = paste("K = ", k)) # plot the mean of the posterior samples against the data
abline(0, 1)

chain=1
plot(motor.data$times, motor.data$accel)
plot(motor.data$times, colMeans(motor.samps[[1]][,accelindex]))

chain=1
cat("Posterior mean of mean tau = ",  mean(sapply(motor.samps,function(x) x[,grep("tau",colnames(x))])), "\n")
cat("mean tau C.I. =", HPDinterval(motor.samps[[chain]],prob=0.95)[grep("tau",colnames(motor.samps[[chain]])),],"\n")
cat("Posterior mean of mean llambda = ",  mean(sapply(motor.samps,function(x) x[,grep("lambda",colnames(x))])), "\n")
cat("mean beta C.I. =", HPDinterval(motor.samps[[chain]],prob=0.95)[grep("lambda",colnames(motor.samps[[chain]])),],"\n")

