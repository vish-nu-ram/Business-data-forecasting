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
daywise1 <- daywise[1:350,]
daywise_ts2 = daywise1[,-1]
daywise_ts2 <- round(daywise_ts2,4)
plot = plot.ts(daywise_ts2)
daywise_ts = ts(round(daywise1$`mean(BO)`,4), frequency = 7)
View(daywise_ts)

#partioning the data
train_data = ts(head(daywise_ts, n=300))
valid_data = ts(tail(daywise_ts, n=50), start = 300, end = 350)

class(valid_data)
plot(train_data)
plot(valid_data)

dcomp <- stl(daywise_ts, s.window = "periodic")

plot(dcomp)
###### Smoothing ######
#more sophisticated version of exponential smoothing, which can capture trend and/or seasonality
#we do have trend and seasonality in our time series, which means we use Holt–Winter’s Exponential Smoothing

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAN" to fit Holt-Winter's exponential smoothing
exp_model_MAN <- ets(train_data, model = "MAN")
# create predictions
exp_pred_MAN <- forecast(exp_model_MAN, h=length(valid_data), level = 0)
# plot the series
plot(exp_pred_MAN,  ylab = "Mean Bid Price at the beginning\n of each hour per day", xlab = "Time",
     bty = "l", xaxt = "n", main = "multiplicative error, additive trend, and\n no seasonality")
lines(exp_pred_MAN$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_MAN, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MMN" to fit Holt-Winter's exponential smoothing
exp_model_MMN <- ets(train_data, model = "MMN")
# create predictions
exp_pred_MMN <- forecast(exp_model_MMN, h=length(valid_data), level = 0)
# plot the series
plot(exp_pred_MMN,  ylab = "Mean Bid Price per day", xlab = "Time",
     bty = "l", xaxt = "n", main = "multiplicative error, multiplicative trend, and\n no seasonality")
lines(exp_pred_MMN$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_MMN, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "AAN" to fit Holt-Winter's exponential smoothing
exp_model_AAN <- ets(train_data, model = "AAN")
# create predictions
exp_pred_AAN <- forecast(exp_model_AAN, h=length(valid_data), level = 0)
# plot the series
plot(exp_pred_AAN,  ylab = "Mean Bid Price at the beginning\n of each hour per day", xlab = "Time",
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nadditive error, additive trend, and\n no seasonality")
lines(exp_pred_AAN$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_AAN, valid_data)

# use ets() with option model = "MAN" to fit Holt-Winter's exponential smoothing
exp_model_ZZZ <- ets(train_data, model = "ZZZ")
# create predictions
exp_pred_ZZZ <- forecast(exp_model_ZZZ, h=length(valid_data), level = 0)
# plot the series
plot(exp_pred_ZZZ,  ylab = "Mean Bid Price at the beginning\n of each hour per day", xlab = "Time",
     bty = "l", xaxt = "n", main = "automatically selected")
lines(exp_pred_ZZZ$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_ZZZ)
accuracy(exp_pred_ZZZ, valid_data)

###### prediction
exp_model_pred <- ets(daywise_ts, model = "MAN")
exp_pred_7days <- forecast(exp_model_MAN, h=7, level = 0)
# plot the series
plot(exp_pred_7days,  ylab = "Mean Bid Price at the beginning\n of each hour per day", xlab = "Time",
     bty = "l", xaxt = "n", main = "multiplicative error, additive trend, and\n no seasonality")
lines(exp_pred_7days$fitted, lwd = 1, col = "blue")
