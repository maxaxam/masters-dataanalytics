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