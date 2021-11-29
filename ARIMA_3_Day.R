library(tseries)
library(TSstudio)
library(zoo)
library(forecast)
library(dplyr)

df <- read.csv("eurusd_hour.csv")
View(df)
length(df)
range(df$BO)

#Reading data from the last 50 weeks of the time series
df50weeks <- tail(df, n = 24*7*50)

View(df50weeks)
head(df50weeks)
tail(df50weeks)
#aggregating the data so that we have single value for a single day
meanBO = mean(df50weeks$BO)
daywise = df50weeks %>% group_by(Date) %>% summarise(mean(BO))
#Picking 80 days( 66 to train, 14 to validate)
plot.ts(daywise$`mean(BO)`)
daywise1 <- daywise[1:80,]
daywise_ts = ts(round(daywise1$`mean(BO)`,4), frequency = 7)
plot.ts(daywise_ts)
#partioning the data
train_data = ts(head(daywise_ts, n=66))
valid_data = ts(tail(daywise_ts, n=14),start = 67, end = 80)

class(valid_data)
class(train_data)
plot(train_data)
plot(valid_data)

dcomp <- stl(daywise_ts, s.window = "periodic")

plot(dcomp)

adf.test(train_data)
#ADF reveale that data is stationary so no need to difference

#training_d1 <- diff(train_data, differences = 2)  # Differencing data 2 times
#adf.test(training_d1)

#dcomp <- stl(training_d1)
#plot(dcomp)

#pacf(training_d1) #PACF On differenced data
pacf(train_data)

#acf(training_d1) #ACF On differenced data
acf(train_data)


#Based on the plots for ACF and PACF we try the model
ts_Mod1 <- arima(train_data, order = c(1,0,5))
print(ts_Mod1)

#valForcast <- forecast()
length(valid_data)

ts_For1 <- forecast(ts_Mod1, h= length(valid_data))
plot(ts_For1, ylim = c(1.1,1.2))
lines(ts_For1$fitted,, lwd = 1, col = "blue")
lines(valid_data)

plot(ts_For1$residuals)
mean(ts_For1$residuals)

#ts_Mod2 <- arima(train_data, order = c(1,1,0))
#print(ts_Mod2)

#valForcast <- forecast()
#length(valid_data)

#ts_For2 <- forecast(ts_Mod2, h= 50)
#plot(ts_For2)
#lines(ts_For2$fitted, lwd = 1, col = "blue")
#lines(valid_data)

ts_Mod4<- auto.arima(train_data, trace = TRUE)
ts_For4 <- forecast(ts_Mod4, h= 20)
plot(ts_For4)
lines(ts_For4$fitted, lwd = 1, col = "blue")
lines(valid_data)

plot(ts_For4$residuals)
mean(ts_For4$residuals)

accuracy(ts_For4, valid_data)
accuracy(ts_For1, valid_data)
# With Auto arima we get 0,1,0 as the hyperparameter. 
# But the results for our model (1,0,5) seem to perform better than Auto ARIMA


#predicting for 7 days with arima 1,0,5
ts_ModFinal <- arima(daywise_ts, order = c(1,0,5))
ts_ForFinal <- forecast(ts_ModFinal,h=7)
plot(ts_ForFinal)
lines(ts_ForFinal$fitted,, lwd = 1, col = "blue")
ts_ForFinal

plot(ts_ForFinal$residuals)



### Selecting optimal p,d,q based on minimum AIC value

##### REF :https://www.quantstart.com/articles/Autoregressive-Integrated-Moving-Average-ARIMA-p-d-q-Models-for-Time-Series-Analysis/

final.aic <- Inf
final.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
   current.aic <- AIC(arima(train_data, order=c(p, d, q)))
   if (current.aic < final.aic) {
     final.aic <- current.aic
     final.order <- c(p, d, q)
     final.arima <- arima(train_data, order=final.order)
   }
}


final.order

ts_Modf <- arima(train_data, order = c(3,0,4))
print(ts_Modf)

acf(resid(ts_Modf))
# No significant peaks for residuals

ts_Forf <- forecast(ts_Modf, h= length(valid_data))
plot(ts_Forf, ylim = c(1.1,1.2))
lines(ts_Forf$fitted,, lwd = 1, col = "blue")
lines(valid_data)

accuracy(ts_Forf, valid_data)
