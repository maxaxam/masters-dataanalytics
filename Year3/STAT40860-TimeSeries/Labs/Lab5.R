#install.packages("forecast")
#install.packages("lmtest")

library(forecast)
library(lmtest)

data <- read.csv("Data/lab5.csv", header = TRUE, sep = ",")

# Series_1

x_1 <- data$Series.1

ts.plot(x_1)
t_1 <- seq(1, length(x_1))
linmod_1 <- lm(x_1~t_1)
linmod_1

# x2_detrended <- linmod1$residuals
# ts.plot(x2_detrended)

# acf(x2_detrended)
# ACF -
# pacf(x2_detrended)
# PACF -

#Differenciate
#x2_detrended_diff1 <- diff(x2_detrended, difference=1)
#ts.plot(x2_detrended_diff1)
#x2_detrended_diff2 <- diff(x2_detrended, difference=2)
#ts.plot(x2_detrended_diff2)
# d = 2 - stationary

# acf(x2_detrended_diff2)
# MA(2) - q=2 not null, q > 2 null
# pacf(x2_detrended_diff2)
# exponentila decay - suggests MA model

# Model
# ARIMA(1,0,1)
model_1 <- arima(x_1, order=c(1,0,1))
res_1 <- model_1$residuals
coeftest(model_1)
fit_1 <- fitted(model_1)
ts.plot(res_1)
acf(res_1)
pacf(res_1)
qqnorm(res_1)
hist(res_1)
# overfit

# ARIMA(2,0,1)
model_1_overAR <- arima(x_1, order=c(2,0,1))
res_1_overAR <- model_1_overAR$residuals
coeftest(model_1_overAR)


#Ljung-Box Piere Statistic
Box.test(res_1, type="Ljung", lag=10)
Box.test(res_1, type="Ljung", lag=50)
Box.test(res_1, type="Ljung", lag=100)

# Forecast
x_1_forecast <- forecast(model_1, h=4)
summary(x_1_forecast)
plot(x_1_forecast)


# Series_2

x_2 <- data$Series.2[1:145]

ts.plot(x_2)
t_2 <- seq(1, length(x_2))
linmod_2 <- lm(x_2~t_2)
linmod_2

# Model
# ARIMA(3,1,0)
model_2 <- arima(x_2, order=c(3,1,0))
res_2 <- model_2$residuals
coeftest(model_2)
fit_2 <- fitted(model_2)
ts.plot(res_2)
acf(res_2)
pacf(res_2)
qqnorm(res_2)
hist(res_2)

#Ljung-Box Piere Statistic
Box.test(res_2, type="Ljung", lag=10)
Box.test(res_2, type="Ljung", lag=50)
Box.test(res_2, type="Ljung", lag=100)

# Forecast
x_2_forecast <- forecast(model_2, h=5)
x_2_forecast
plot(x_2_forecast)
summary(x_2_forecast)

data$Series.2[146:150]


# Series_3

x_3 <- data$Series.3[1:195]

ts.plot(x_3)
t_3 <- seq(1, length(x_3))
linmod_3 <- lm(x_3~t_3)
linmod_3

# Model
# ARIMA(0,0,2)
model_3 <- arima(x_3, order=c(0,0,2), include.mean = FALSE)
res_3 <- model_3$residuals
coeftest(model_3)
fit_3 <- fitted(model_3)
ts.plot(res_3)
acf(res_3)
pacf(res_3)
qqnorm(res_3)
hist(res_3)

#Ljung-Box Piere Statistic
Box.test(res_3, type="Ljung", lag=10)
Box.test(res_3, type="Ljung", lag=50)
Box.test(res_3, type="Ljung", lag=100)

# Forecast
x_3_forecast <- forecast(model_3, h=4)
plot(x_3_forecast)
summary(x_3_forecast)

data$Series.3[196:199]

# ARIMA(0,0,3)
model_3_1 <- arima(x_3, order=c(0,0,3), include.mean = FALSE)
# Forecast
x_3_forecast_1 <- forecast(model_3_1, h=4)
summary(x_3_forecast_1)
