#install.packages("forecast")
#install.packages("lmtest")

library(forecast)
library(lmtest)

data <- read.csv("Data/lab3.csv", header = TRUE, sep = "\t")
x1 <- data$Series.1
x2 <- data$Series.2
x3 <- data$Series.3
x4 <- data$Series.4
x5 <- data$Series.5
x6 <- data$Series.6

# Plot of x-series
ts.plot(x1)
ts.plot(x2)
ts.plot(x3)
ts.plot(x4)
ts.plot(x5)
ts.plot(x6)

# time variable
t1 <- seq(1, length(x1))
t2 <- seq(1, length(x2))
t3 <- seq(1, length(x3))
t4 <- seq(1, length(x4))
t5 <- seq(1, length(x5))
t6 <- seq(1, length(x6))

linmod1 <- lm(x1~t1)
linmod1

linmod2 <- lm(x2~t2)
linmod2

linmod3 <- lm(x3~t3)
linmod3

linmod4 <- lm(x4~t4)
linmod4

linmod5 <- lm(x5~t5)
linmod5

linmod6 <- lm(x6~t6)
linmod6

# stationarity of de-trended time series
ts.plot(linmod1$residuals)
ts.plot(linmod2$residuals)
ts.plot(linmod3$residuals)
ts.plot(linmod4$residuals)
ts.plot(linmod5$residuals)
ts.plot(linmod6$residuals)

# x1 - difference 0
# x2 - difference 1
ts.plot(diff(x2, difference=1))
# x3 - difference 1
ts.plot(diff(x3, difference=1))
# x4 - difference 0
# x5 - difference 0
ts.plot(diff(x5, difference=2))
# x6 - difference ???
ts.plot(diff(x6, difference=1))

# ARIMA
model3 <- arima(x3, order=c(0,1,1))
model3
coeftest(model3)

# overfit AR
model3_overAR <- arima(x3, order=c(1,1,1))
model3_overAR
coeftest(model3_overAR)

# overfit MA
model3_overMA <- arima(x3, order=c(0,1,2))
model3_overMA
coeftest(model3_overMA)

mean(x3)
acf(residuals(model3))
