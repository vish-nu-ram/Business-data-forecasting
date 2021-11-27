library(tseries)
library(TSstudio)
library(zoo)
library(forecast)
library(dplyr)

df <- read.csv("eurusd_hour.csv")
View(df)
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
#exclude the noisy part at the end from 360 onwards
plot.ts(daywise$`mean(BO)`)
daywise1 <- daywise[1:80,]
daywise_ts2 = daywise1[,-1]
daywise_ts2 <- round(daywise_ts2,4)
plot = plot.ts(daywise_ts2)
daywise_ts = ts(round(daywise1$`mean(BO)`,4), frequency = 7)

#partioning the data
train_data = ts(head(daywise_ts, n=50))
valid_data = ts(tail(daywise_ts, n=20),start = 51, end = 70)

class(valid_data)
class(train_data)
plot(train_data)
plot(valid_data)

dcomp <- stl(daywise_ts, s.window = "periodic")

plot(dcomp)

adf.test(train_data)
 training_d1 <- diff(train_data, differences = 1)
adf.test(training_d1)
plot(training_d1)

dcomp <- stl(training_d1)
plot(dcomp)



pacf(training_d1)
pacf(train_data)


acf(training_d1)
acf(train_data)


#(5,1,1)
ts_Mod1 <- arima(train_data, order = c(5,1,1))
print(ts_Mod1)

#valForcast <- forecast()
length(valid_data)

ts_For1 <- forecast(ts_Mod1, h= 50)
plot(ts_For1)
lines(ts_For1$fitted,, lwd = 1, col = "blue")
lines(valid_data)

#Model is fitting very well, but unable to predict further.

#(5,1,9)
ts_Mod2 <- arima(train_data, order = c(1,1,0))
print(ts_Mod2)

#valForcast <- forecast()
length(valid_data)

ts_For2 <- forecast(ts_Mod2, h= 50)
plot(ts_For2)
lines(ts_For2$fitted, lwd = 1, col = "blue")
lines(valid_data)

ts_Mod4<- auto.arima(train_data, trace = TRUE)
ts_For4 <- forecast(ts_Mod4, h= 20)
plot(ts_For4)
lines(ts_For4$fitted, lwd = 1, col = "blue")
lines(valid_data)

accuracy(ts_For4, valid_data)
accuracy(ts_For2, valid_data)
accuracy(ts_For, valid_data)
accuracy(ts_For1, valid_data)
# Even with Auto arima we get 1,1,0 as the hyperparameter. 


#predicting for 7 days with arima 1,1,0
ts_ModFinal <- arima(daywise_ts, order = c(1,1,0))
ts_ForFinal <- forecast(ts_ModFinal,h=7)
plot(ts_ForFinal, xlim = c(48,53), ylim = c(1.1,1.125))
lines(ts_ForFinal$fitted,, lwd = 1, col = "blue")
lines(valid_data)
