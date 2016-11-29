library(forecast)
library(lmtest)

timeseries_plots <- function(timeseries, main, oneplot = FALSE) {
  if (oneplot == TRUE) {
    par(mfrow=c(3,1))
  }
  else {
    par(mfrow=c(1,1))
  }
  
  ts.plot(timeseries, main = paste("Times Series plot for", main))
  acf(timeseries, main = paste("ACF plot for", main))
  pacf(timeseries, main = paste("PACF plot for", main))
  
  par(mfrow=c(1,1))
}


data <- read.csv("monthly-traffic-fatalities-in-on.csv", header = TRUE, sep = ",")
print("Data set - Number of monthly traffic fatalities in Ontario, Canada")
n <- length(data$Monthly.traffic.fatalities.in.Ontario.1960.1974)
print(paste("Number of observations:", n))
fatalities <- data$Monthly.traffic.fatalities.in.Ontario.1960.1974[1:(n-2)]
fatalities_excluded <- data$Monthly.traffic.fatalities.in.Ontario.1960.1974[(n-1):n]

# Time series plots
timeseries_plots(
  timeseries = fatalities, 
  main = "Number of monthly traffic fatalities in Ontario, Canada",
  oneplot = FALSE)

t <- seq(1, length(fatalities))
linmod <- lm(fatalities~t)

# Detrended - Linear Trend
fatalities_detrended <- linmod$residuals
timeseries_plots(
  timeseries = fatalities_detrended, 
  main = "Detrended series",
  oneplot = TRUE)

for (d in c(1:5)) {
  fatalities_detrended_diff <- diff(fatalities_detrended, difference = d)
  fatalities_detrended <- linmod$residuals
  timeseries_plots(
    timeseries = fatalities_detrended_diff, 
    main = paste("Detrended differenciated series with d = ", d),
    oneplot = TRUE)
}

# Differenciate for d = 4
d = 4
fatalities_detrended_diff <- diff(fatalities_detrended, difference = d)
fatalities_detrended <- linmod$residuals
timeseries_plots(
  timeseries = fatalities_detrended_diff, 
  main = paste("Detrended differenciated series with d = ", d),
  oneplot = FALSE)

arima_model_diagnostics <- function(series, p, d, q, plots = TRUE, ljung_box_pierce_test = TRUE) {
  model <- arima(series, order=c(p,d,q))
  res <- model$residuals
  print(coeftest(model))
  fit <- fitted(model)
  
  if (plots == TRUE) {
    ts.plot(res, main = paste("Residuals time series for ARIMA(",p,",",d,",",q,")", sep = ""))
    acf(res,main = paste("ACF of residuals for ARIMA(",p,",",d,",",q,")", sep = ""))
    # pacf(res, main = paste("PACF for ARIMA(",p,",",d,",",q))
    qqnorm(res, main = paste("QQ plot of residuals for ARIMA(",p,",",d,",",q,")", sep = ""))
    hist(res, main = paste("Historgram of residuals for ARIMA(",p,",",d,",",q,")", sep = ""))
    plot(fit, res, main = paste("Residuals vs Fitted values for ARIMA(",p,",",d,",",q,")", sep = ""))
  }
  
  if (ljung_box_pierce_test == TRUE) {
    print(Box.test(res, type="Ljung", lag=10))
    print(Box.test(res, type="Ljung", lag=50))
    print(Box.test(res, type="Ljung", lag=100))
  }
  
  return(model)
}

# From the previous ARIMA(1,4,1)
p = 1
d = 4
q = 1
arima_model_diagnostics(fatalities_detrended_diff, p, d, q, TRUE, TRUE)

# Overfitting - ARIMA (2,4,1)
p = 2
d = 4
q = 1
arima_model_diagnostics(fatalities_detrended_diff, p, d, q, TRUE, TRUE)

# Overfitting - ARIMA (1,4,2)
p = 1
d = 4
q = 2
arima_model_diagnostics(fatalities_detrended_diff, p, d, q, TRUE, TRUE)

# Overfitting - ARIMA (2,4,2)
p = 2
d = 4
q = 2
arima_model_diagnostics(fatalities_detrended_diff, p, d, q, TRUE, TRUE)

# Overfitting - ARIMA (3,4,2)
p = 3
d = 4
q = 2
arima_model_diagnostics(fatalities_detrended_diff, p, d, q, TRUE, TRUE)

# Overfitting - ARIMA (2,4,3)
p = 2
d = 4
q = 3
arima_model_diagnostics(fatalities_detrended_diff, p, d, q, TRUE, TRUE)

# Overfitting - ARIMA (3,4,3)
p = 3
d = 4
q = 3
arima_model_diagnostics(fatalities_detrended_diff, p, d, q, TRUE, TRUE)

# Final Model
p = 3
d = 4
q = 2
model <- arima_model_diagnostics(fatalities_detrended_diff, p, d, q, TRUE, TRUE)

# Forecast
fatalities_excluded_forecast <- forecast(model, h=2)
fatalities_excluded_forecast
plot(fatalities_excluded_forecast)
summary(fatalities_excluded_forecast)
fatalities_excluded[1] - 100.2927062 - 0.3547736*(n-1)
fatalities_excluded[2] - 100.2927062 - 0.3547736*(n)
