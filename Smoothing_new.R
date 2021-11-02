library(tseries)
library(TSstudio)
library(zoo)
library(forecast)
library(dplyr)
setwd("/Users/Lily/Library/Mobile Documents/com~apple~CloudDocs/Trinity College Dublin/BU7143_BU7144 Business Data Mining and Forecasting/Group Assignment Forecasting/Github/Business-data-forecasting")
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
daywise1 <- daywise[1:350,]
daywise_ts2 = daywise1[,-1]
plot = plot.ts(daywise_ts2)
daywise_ts = ts(daywise1$`mean(BO)`)
View(daywise_ts)

#partioning the data
train_data = ts(head(daywise_ts$`mean(BO)`, n=250))
valid_data = ts(tail(daywise_ts$`mean(BO)`, n=100))

plot(train_data)
plot(valid_data)

decompose(daywise_ts)

###### Smoothing ######
#more sophisticated version of exponential smoothing, which can capture trend and/or seasonality
#we do have trend and seasonality in our time series, which means we use Holt–Winter’s Exponential Smoothing

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and additive seasonality.
exp_model_MAA <- ets(train_data, model = "MAN")
# create predictions
exp_pred_MAA <- forecast(exp_model_MAA, h=length(valid_data), level = 0)
# plot the series
plot(exp_pred_MAA,  ylab = "Mean Bid Price at the beginning of each hour per day", xlab = "Time",
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiplicative error, additive trend, and\n additive seasonality", flty = )
lines(exp_pred_MAA$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_MAA)
accuracy(exp_pred_MAA, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAM" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MAM <- ets(training, model = "MAM")
# create predictions
exp_pred_MAM <- forecast(exp_model_MAM, h=366, level = 0)
# plot the series
plot(exp_pred_MAM,  ylab = "Bid Price at beginning of hour", xlab = "Time",
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiplicative error, additive trend, and\n multiplicative seasonality")
lines(exp_pred_MAM$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_pred_MAM)
accuracy(exp_pred_MAM, testing)
