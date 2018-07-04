install.packages("forecast")
install.packages("gtrendsR")
install.packages("reshape2")
install.packages('tseries') 

library(forecast)
library(gtrendsR)
library(reshape2)
require(tseries) 

#Google trends from 2017-01-15 to 2017-03-30

google.trends = gtrends(c("suicide + sucide -squad -blue -whale"), gprop = "web", time = "2017-01-15 2017-03-30")[[1]]
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date
google.trends$date = NULL

plot.ts(google.trends)

#Actual results from 2017-03-31 to 2017-04-18

actual.trends = gtrends(c("suicide + sucide -squad -blue -whale"), gprop = "web", time = "2017-03-31 2017-04-18")[[1]]
actual.trends = dcast(actual.trends, date ~ keyword + geo, value.var = "hits")
rownames(actual.trends) = actual.trends$date
actual.trends$date = NULL

actual.trends

plot.ts(actual.trends)

#Creating time series 
timeseries <- ts(google.trends)
timeseries1 <- ts(actual.trends, start = 76 )

#Check stationarity using Augmented Dickey-Fuller Test
adf.test(timeseries, alternative = "stationary", k=0)

#Using ACF and PACF
ts_acf <- acf(timeseries) #0
ts_pacf <- pacf(timeseries) #3

#Using different p,d and q values and then predicting the values.
fit <- arima(timeseries, c(3,1,0))
fit <- arima(timeseries, c(3,1,1))

#Using auto arima 
fit <- auto.arima(timeseries, seasonal=FALSE) 
fit

#Prediting the values using any of the fit above
pred <- predict(fit, n.ahead = 19)
ts.plot(timeseries,pred$pred, lty = c(1,3), col=c(3,2), lwd = 2)
lines(timeseries1, col = "blue")

#Saving it in a data frame
v1 <- unlist(pred, use.names = F)
v1 <- v1[1:19]
v2 <- as.numeric(timeseries1)
final <- data.frame(v2,v1)
names(final) <- c("actual","prediction")
View(final)

#Calculating the error

error <- sqrt((final$actual-final$prediction)^2)
error <- sum(error)
error
