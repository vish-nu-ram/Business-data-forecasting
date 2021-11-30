library(zoo)
library(tseries)
# install.packages("TSstudio")
library(TSstudio)
library(forecast)
library(dplyr)
library(ggplot2)
df <- read.csv("C:/Users/Sarath/Desktop/eurusd_hour (3).csv")



view(df)
length(df)
range(df$BO)





#the data frame
df50weeks <- tail(df, n = 24*7*50)






View(df50weeks)
head(df50weeks)
tail(df50weeks)





#aggregating the data so that we have single value for a single day





meanBO = mean(df50weeks$BO)





daywise = df50weeks %>% group_by(Date) %>% summarise(mean(BO))
#daywise_ts = ts(daywise)
#plot(daywise_ts)
daywise1 <- daywise[1:80,] #exclude the noisy part at the end from 360 onwards
plot.ts(daywise1)
daywise_ts2 = daywise1[,-1]
plot = plot.ts(daywise_ts2)





#daywise$Date <- as.Date(daywise$Date)
daywise_ts = ts(daywise_ts2$`mean(BO)`, start = 1, frequency = 7)
plot(daywise_ts)
View(daywise_ts)





#partioning the data
train_data = head(daywise_ts, n=66,start = 0, end = 66)
valid_data = tail(daywise_ts, n=14, start = 66, end = 80)





plot(train_data)
plot(valid_data)





#run ts on train and valid
train_data_ts = ts(train_data)
valid_data_ts = ts(valid_data, start = 66, end = 80)





plot(train_data_ts)
plot(valid_data_ts)




training <- train_data_ts
testing <- valid_data_ts



length(training)
length(testing)
acf(training)
pacf(training)



adf.test(training)




train.lm.trend.season <- tslm(training ~ trend + I(trend^2 ))
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = 100, level = 0)
summary(train.lm.trend.season)



# Plot the model
par(mfrow = c(2,1))
plot(train.lm.trend.season.pred, ylab = "BO",
     xlab = "Time", bty = "l", main = "", flty = 2)
lines(train.lm.trend.season$fitted, lwd = 2, col = "red")
lines(testing, lwd = 2, col = "red")



plot(train.lm.trend.season$residuals, ylab = "Forecast Errors",
     xlab = "Time", bty = "l", main = "")






train.res.ar <- Arima(train.lm.trend.season$residuals, order = c(1,0,0))
tsdiag(train.res.ar)
train.res.ar.pred <- forecast(train.res.ar)
valid.res.ar.pred <- forecast(train.res.ar, h = 1)
summary(train.res.ar)



Acf(train.res.ar$residuals, lag.max = 12, main = "")




plot(train.lm.trend.season$residuals, ylab = "Residuals",
     xlab = "Time", bty = "l", main = "")
lines(train.res.ar.pred$fitted, lwd = 2, col = "red")



train.res.ar.pred <- predict(train.res.ar)
valid.res.ar.pred <- predict(train.res.ar, n.ahead = 7)
a<-ts(valid.res.ar.pred)
summary(train.res.ar)
range(train.res.ar$residuals)



Acf(train.res.ar$residuals, lag.max = 12, main = "")
plot(train.lm.trend.season$residuals, xlim = c(1, length(train.lm.trend.season$residuals) + 15),
     ylim = c(min(train.lm.trend.season$residuals), max(train.lm.trend.season$residuals)))
lines(valid.res.ar.pred$pred, col = "blue", lwd = 5)
accuracy(valid.res.ar.pred$pred,testing)