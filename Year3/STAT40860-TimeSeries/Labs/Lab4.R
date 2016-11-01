#install.packages("forecast")
#install.packages("lmtest")

library(forecast)
library(lmtest)

data <- read.csv("Data/lab4.csv", header = TRUE, sep = ",")
x1 <- data$Series.1
x2 <- data$Series.2

# Series 1

ts.plot(x1)
t1 <- seq(1, length(x1))
linmod1 <- lm(x1~t1)
linmod1

ts.plot(linmod1$residuals)

x1_detrended <- linmod1$residuals
ts.plot(x1_detrended)

x1_detrended

acf(x1_detrended)
# ACF - MA(2), q = 2 , q > 2 null
pacf(x1_detrended)
# PACF - exponential decay - MA model

# Original
model1 <- arima(x1_detrended, order=c(0,0,2))
res1 <- model1$residuals
coeftest(model1)
fit1 <- fitted(model1)
acf(res1)
pacf(res1)
qqnorm(res1)
hist(res1)
plot(fit1, res1)
plot(t1, res1, type="line")

# overfit

# ARIMA(1,0,2)
model1 <- arima(x1_detrended, order=c(1,0,2))
res1 <- model1$residuals
coeftest(model1)
fit1 <- fitted(model1)
acf(res1)
pacf(res1)
qqnorm(res1)
hist(res1)
plot(fit1, res1)
plot(t1, res1, type="line")
# AR1 significant

# ARIMA(0,0,3)
model1 <- arima(x1_detrended, order=c(0,0,3))
res1 <- model1$residuals
coeftest(model1)
fit1 <- fitted(model1)
acf(res1)
pacf(res1)
qqnorm(res1)
hist(res1)
plot(fit1, res1)
plot(t1, res1, type="line")
# MA3 not significant

# ARIMA(2,0,2)
model1 <- arima(x1_detrended, order=c(2,0,2))
res1 <- model1$residuals
coeftest(model1)
fit1 <- fitted(model1)
acf(res1)
pacf(res1)
qqnorm(res1)
hist(res1)
plot(fit1, res1)
plot(t1, res1, type="line")
# AR2 not significant

# Final model
# ARIMA(1,0,2)
model1 <- arima(x1_detrended, order=c(1,0,2))
res1 <- model1$residuals
coeftest(model1)
fit1 <- fitted(model1)
acf(res1)
pacf(res1)
qqnorm(res1)
hist(res1)
plot(fit1, res1)
plot(t1, res1, type="line")
linmod1
model1

#Ljung-Box Piere Statistic
Box.test(res1, type="Ljung", lag=10)
Box.test(res1, type="Ljung", lag=50)
Box.test(res1, type="Ljung", lag=100)
Box.test(res1, type="Ljung", lag=300)

# Series 2

ts.plot(x2)
t2 <- seq(1, length(x2))
linmod2 <- lm(x2~t2)
linmod2

x2_detrended <- linmod2$residuals
ts.plot(x2_detrended)

acf(x2_detrended)
# ACF - slow decay - needs differencing 
pacf(x2_detrended)
# PACF -

#Differenciate
x2_detrended_diff1 <- diff(x2_detrended, difference=1)
ts.plot(x2_detrended_diff1)
x2_detrended_diff2 <- diff(x2_detrended, difference=2)
ts.plot(x2_detrended_diff2)
# d = 2 - stationary

acf(x2_detrended_diff2)
# MA(2) - q=2 not null, q > 2 null
pacf(x2_detrended_diff2)
# exponentila decay - suggests MA model

# Original
# ARIMA(0,2,2)
model2 <- arima(x2_detrended_diff2, order=c(0,2,2))
res2 <- model2$residuals
coeftest(model2)
fit2 <- fitted(model2)
acf(res2)
pacf(res2)
qqnorm(res2)
hist(res2)
plot(fit2, res2)
plot(t2[0:398], res2, type="line")
# overfit

# ARIMA(1,2,2)
model2 <- arima(x2_detrended_diff2, order=c(1,2,2))
res2 <- model2$residuals
coeftest(model2)
# AR1 significant

# ARIMA(0,0,3)
model2 <- arima(x2_detrended_diff2, order=c(0,2,3))
res2 <- model2$residuals
coeftest(model2)
# MA3 significant

# ARIMA(1,2,3)
model2 <- arima(x2_detrended_diff2, order=c(1,2,3))
res2 <- model2$residuals
coeftest(model2)
# AR1 not significant

# ARIMA(2,2,3)
model2 <- arima(x2_detrended_diff2, order=c(2,2,3))
res2 <- model2$residuals
coeftest(model2)
# AR2 not significant

# ARIMA(2,2,3)
model2 <- arima(x2_detrended_diff2, order=c(1,2,4))
res2 <- model2$residuals
coeftest(model2)
# MA4 not significant

# Keep ARIMA(1,2,3)
# ARIMA(1,2,3)
model2_1 <- arima(x2_detrended_diff2, order=c(1,2,3))
res2_1 <- model2_1$residuals
coeftest(model2_1)
fit2_1 <- fitted(model2_1)
acf(res2_1)
pacf(res2_1)
qqnorm(res2_1)
hist(res2_1)
plot(fit2_1, res2_1)
plot(t2[0:398], res2_1, type="line")
linmod2
model2_1

# Final Model
# ARIMA(0,2,3)
model2_2 <- arima(x2_detrended_diff2, order=c(0,2,3))
res2_2 <- model2_2$residuals
coeftest(model2_2)
fit2_2 <- fitted(model2_2)
acf(res2_2)
pacf(res2_2)
qqnorm(res2_2)
hist(res2_2)
plot(fit2_2, res2_2)
plot(t2[0:398], res2_2, type="line")
linmod2
model2_2

#Ljung-Box Piere Statistic
Box.test(res2_1, type="Ljung", lag=10)
Box.test(res2_1, type="Ljung", lag=50)
Box.test(res2_1, type="Ljung", lag=100)
Box.test(res2_1, type="Ljung", lag=300)
