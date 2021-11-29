library(zoo)
library(tseries)
# install.packages("TSstudio")
library(TSstudio)
library(forecast)
library(dplyr)
library(ggplot2)

options(scipen = 999)

df <- read.csv(".../eurusd_hour.csv")
df <- eurusd_hour
View(df)
length(df)
range(df$BO)
head(df)
tail(df)
#the data frame
df50weeks <- tail(df, n = 24*7*50)
plot(df50weeks)

View(df50weeks)
head(df50weeks)
tail(df50weeks)

#aggregating the data so that we have single value for a single day
daywise <- df50weeks %>% group_by(Date) %>% summarise(mean(BO))
#daywise_ts = ts(daywise)
#plot(daywise_ts)

daywise1 <- daywise[1:80,] #select the part we want to analyse
head(daywise1)
tail(daywise1)
daywise_ts2 <- daywise1[,-1]
daywise_ts2

plot <- plot.ts(daywise_ts2)

#daywise$Date <- as.Date(daywise$Date)

daywise_ts <- ts(daywise_ts2$`mean(BO)`, start = 0, frequency = 7)
plot(daywise_ts)
plot(decompose(daywise_ts))
daywise_ts

#partioning the data
train_data <- head(daywise_ts, n=66, start = 0, end = 66)
valid_data <- tail(daywise_ts, n=14, start = 67, end = 80)

plot(train_data)
plot(valid_data)

###### Smoothing ######
#more sophisticated version of exponential smoothing, which can capture trend and/or seasonality
#we do have trend and seasonality in our time series, which means we use Holt–Winter’s Exponential Smoothing

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and additive seasonality.
exp_model_MAA <- ets(ts(train_data, start=0, frequency = 7), model = "MAA")
# create predictions
exp_pred_MAA <- forecast(exp_model_MAA, h=14, level = 0)
# plot the series
plot(exp_pred_MAA,  ylab = "Daily average Bid Open Price", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiplicative error, additive trend, and\n additive seasonality")
lines(exp_pred_MAA$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_MAA, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAM" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MAM <- ets(ts(train_data, start=0, frequency = 7), model ="MAM")
# create predictions
exp_pred_MAM <- forecast(exp_model_MAM, h=14, level = 0)
# plot the series
plot(exp_pred_MAM,  ylab = "Daily average Bid Open Price", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiplicative error, additive trend, and\n multiplicative seasonality")
lines(exp_pred_MAM$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_MAM, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MMM" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MMM <- ets(ts(train_data, start=0, frequency = 7), model ="MMM")
# create predictions
exp_pred_MMM <- forecast(exp_model_MMM, h=20, level = 0)
# plot the series
plot(exp_pred_MMM,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.15),
     bty = "l", xaxt = "n", main = "multiplicative error, multiplicative trend, and\n multiplicative seasonality", flty = )
lines(exp_pred_MMM$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_MMM, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "AAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_AAA <- ets(ts(train_data, start=0, frequency = 7), model ="AAA")
# create predictions
exp_pred_AAA <- forecast(exp_model_AAA, h=20, level=0)
# plot the series
plot(exp_pred_AAA,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nadditive error, additive trend, and\n additive seasonality", flty = )
lines(exp_pred_AAA$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_AAA, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MNN <- ets(ts(train_data, start=0, frequency = 7), model ="MNN")
# create predictions
exp_pred_MNN <- forecast(exp_model_MNN, h=20, level = 0)
# plot the series
plot(exp_pred_MNN,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error, no trend or seasonality", flty = )
lines(exp_pred_MNN$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_MNN, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MMN <- ets(ts(train_data, start=0, frequency = 7), model ="MMN")
# create predictions
exp_pred_MMN <- forecast(exp_model_MNN, h=20, level = 0)
# plot the series
plot(exp_pred_MMN,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error and trend and no seasonality", flty = )
lines(exp_pred_MMN$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_pred_MMN, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MNM <- ets(ts(train_data, start=0, frequency = 7), model ="MNM")
# create predictions
exp_pred_MNM <- forecast(exp_model_MNN, h=20, level = 0)
# plot the series
plot(exp_model_MNM,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error, no trend and multiple seasonality", flty = )
lines(exp_model_MNM$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_model_MNM, valid_data)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MNA <- ets(ts(train_data, start=0, frequency = 7), model ="MNA")
# create predictions
exp_pred_MNA <- forecast(exp_model_MNN, h=20, level = 0)
# plot the series
plot(exp_model_MNM,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error, no trend and additive seasonality", flty = )
lines(exp_model_MNM$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_model_MNA, valid_data)


# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_ZZZ <- ets(ts(train_data, start=0, frequency = 7), model ="ZZZ")
# create predictions
exp_pred_ZZZ <- forecast(exp_model_ZZZ, h=20, level = 0)
# plot the series
plot(exp_model_ZZZ,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error, no trend and additive seasonality", flty = )
lines(exp_model_ZZZ$fitted, lwd = 1, col = "blue")
lines(valid_data)
accuracy(exp_model_ZZZ, valid_data)
