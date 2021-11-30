rm(list=ls(all=TRUE))
setwd("C:/Users/Akisha Singh/Downloads")

#TIME SERIES FORECASTING

library("forecast")
library("stats")
library("data.table")
library("TTR")
install.packages("forecast")
library(forecast)




#For monthly time series data, 
#you set frequency=12, 
#while for quarterly time series data, 
#you set frequency=4

#You can also specify the first 
#year that the data was collected, 
#and the first interval in that year 
#by using the 'start'
#parameter in the ts() function. For example, if the first data 
#point corresponds to the second quarter of 1986, you would set 
#start=c(1986,2).


um_data = read.csv("Monthwise.csv")
View(um_data)

install.packages("xts")
library("xts")

um_timeseries <- ts(um_data$Total, frequency = 7, start = c(1,1), end = c(13,5))
um_timeseries
par(mfrow=c(1,1))
plot.ts(um_timeseries, xlab="Weeks")
d <- decompose(um_timeseries)
plot(d)

library(ggplot2)
library("forecast")


#splitting the data
nValid <- 4
nTrain <- length(um_timeseries) - nValid
train.ts <- window(um_timeseries, start = c(1, 1), end = c(1, nTrain))

valid.ts <- window(um_timeseries, start = c(1, nTrain + 1),
                   end = c(1, nTrain + nValid))



#applying roll means 
um <- rollmean(train.ts, k=7, align = "right")
um


plot(train.ts, ylim = c(2000,8000))
lines(um, col="blue")
lines(valid.ts, col="red")


#predicting further
um.ma <- rollmean(um_timeseries, k=4, align = "right")
plot(um.ma, ylim = c(2000,8000), col="blue")
accuracy(forecast(um),valid.ts)
#predict(um.ma)



#ARIMA
futurVal <- forecast(um_timeseries,h=7, level=c(99.5))
plot(futurVal)

#regression model
train.lm <- tslm(train.ts ~ trend + season)
accuracy(forecast(train.lm), valid.ts)
s <- forecast(train.lm)
plot(s)

train.lm$residuals
plot(train.lm$residuals)
Acf(train.lm$residuals, lag.max=1)
Acf(s$residuals, lag.max=1)

#exponential Smoothing
train.ets <- ets(train.ts, model = "MAA")
ets.pred <- forecast(train.ets, h=nValid, level=0)
accuracy(ets.pred, valid.ts)

plot(train.ets)
plot(ets.pred)


#Autocorrelation
Acf(um_timeseries, lag.max=1)

#naive
forecast <- c(NA, um_timeseries[-length(um_timeseries)])
forecast

plot(um_timeseries, type='l', col = 'red', main='Actual vs. Forecasted Sales',
     xlab='Sales Period', ylab='Sales')

#add line for forecasted sales
lines(forecast, type='l', col = 'blue')

par(mfrow=c(1,1))
res <- residuals(naive(um))
plot(res)
plot(um,col="blue")

