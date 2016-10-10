series <- read.csv("Data/lab2.csv", header = TRUE, sep = ",")

apply(series, 2, function(x) length(which(!is.na(x))))

series1 <- series$Series1
series2 <- series$Series2[1:100]
series3 <- series$Series3
series4 <- series$Series4

ts.plot(series1, ylab = "Series1") # stationary
ts.plot(series2, ylab = "Series2") # stationary
ts.plot(series3, ylab = "Series3") # not stationary
ts.plot(series4, ylab = "Series4") # not stationary

# series 1 - difference 0
# series 2 - difference 0
# series 3 - difference 1
ts.plot(diff(series3, difference=1))
# series 4 - difference 2
ts.plot(diff(series4, difference=2))

fit1 <- arima(series1, order = c(1,0,1))
fit1
mean(series1)
acf(residuals(fit1))

fit2 <- arima(series2, order = c(1,0,1))
fit2
mean(series2)
acf(residuals(fit2))

fit3 <- arima(series3, order = c(1,1,1))
fit3
mean(series3)
acf(residuals(fit3))

fit4 <- arima(series4, order = c(1,2,1))
fit4
mean(series4)
acf(residuals(fit4))
