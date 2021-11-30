setwd("C:/Users/Akisha Singh/Downloads")
m_data = read.csv("Food & Beverages sales.csv")
View(m_data)

m_timeseries <- ts(m_data$Total, frequency = 1, start = c(1,1), end = c(89,1))
m_timeseries
par(mfrow=c(1,1))
plot.ts(m_timeseries)

#splitting the data
nValid <- 4
nTrain <- length(m_timeseries) - nValid
train.ts <- window(m_timeseries, start = c(1, 1), end = c(1, nTrain))

valid.ts <- window(m_timeseries, start = c(1, nTrain + 1),
                   end = c(1, nTrain + nValid))

#rollmeans
m <- rollmean(train.ts, k=4, align = "right")
m

plot(train.ts, ylim = c(200,800))
lines(m, col="blue")
lines(valid.ts, col = "red")

um.ma <- rollmean(m_timeseries, k=4, align = "right")
plot(um.ma, ylim = c(200,800), col="red", xlim=c(60,100))
predict(um.ma)
accuracy(forecast(m),valid.ts)

#ARIMA
futurVal <- forecast(m_timeseries,h=7, level=c(99.5))
plot(futurVal)

#naive
forecast <- c(NA, m_timeseries[-length(m_timeseries)])
forecast

plot(m_timeseries, type='l', col = 'black', main='Actual vs. Forecasted Sales',
     xlab='Sales Period', ylab='Sales')

#add line for forecasted sales
lines(forecast, type='l', col = 'red')


#predicting further
um.ma <- rollmean(um_timeseries, k=4, align = "right")
plot(um.ma, ylim = c(2000,8000))
predict(um.ma)

#regression model
train.lm <- tslm(train.ts ~ trend + season)
accuracy(forecast(train.lm), valid.ts)
rr <- forecast(train.lm)
plot(rr)
plot(train.lm)
train.lm$residuals
Acf(train.lm$residuals, lag.max=1)

#exponential Smoothing
train.ets <- ets(train.ts, model = "MAA")
ets.pred <- forecast(train.ets, h=nValid, level=0)
accuracy(ets.pred, valid.ts)

plot(train.ets)
plot(ets.pred)


