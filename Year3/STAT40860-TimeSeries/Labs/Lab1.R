series <- read.csv("Data/lab1.csv", header = TRUE, sep = ",")

ts.plot(series$Series.1[1:121], ylab = "Series.1")
ts.plot(series$Series.2[1:121], ylab = "Series.2")
ts.plot(series$Series.3[1:121], ylab = "Series.3")
ts.plot(series$Series.4[1:121], ylab = "Series.4")
ts.plot(series$Series.5, ylab = "Series.5")
ts.plot(series$Series.6, ylab = "Series.6")

