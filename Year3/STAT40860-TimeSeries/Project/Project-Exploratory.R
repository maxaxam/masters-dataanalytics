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


data <- read.csv("winter-negative-temperature-sum-.csv", header = TRUE, sep = ",")

series <- data$Winter.negative.temperature.sum..in.deg..C...1781...1988

ts.plot(series)
t <- seq(1,length(series))
linmod <- lm(series~t)
linmod
timeseries_plots(diff(linmod$residuals, difference = 1), main = "Winter Negative Temparatur Sum in C")

arima_model_diagnostics(diff(linmod$residuals, difference = 1), 2, 1, 2, TRUE, FALSE)

# monthly-number-of-employed-perso
data <- read.csv("monthly-number-of-employed-perso.csv", header = TRUE, sep = ",")

series <- data$Monthly.number.of.employed.persons.in.Australia

timeseries_plots(series, main = "Number of employed people in Australia", oneplot = TRUE)
t <- seq(1,length(series))
linmod <- lm(series~t)
linmod
timeseries_plots(linmod$residuals, main = "Number of employed people in Australia", oneplot = TRUE)
timeseries_plots(diff(linmod$residuals, difference = 3), main = "Number of employed people in Australia", oneplot = TRUE)

arima_model_diagnostics(diff(linmod$residuals, difference = 1), 2, 1, 2, TRUE, FALSE)
