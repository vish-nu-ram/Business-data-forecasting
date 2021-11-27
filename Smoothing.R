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

daywise_ts <- ts(daywise_ts2$`mean(BO)`, start = 1, frequency = 7)
plot(daywise_ts)
plot(decompose(daywise_ts))
daywise_ts

#partioning the data
train_data <- head(daywise_ts, n=60,start = 0, end = 60)
valid_data <- tail(daywise_ts, n=20, start = 60, end = 80)

plot(train_data)
plot(valid_data)

#run ts on train and valid
train_data_ts = ts(train_data, start = 0, frequency = 7)
valid_data_ts = ts(valid_data, start = 60, frequency = 7)

plot(train_data_ts)
plot(valid_data_ts)

length(train_data_ts) #60 days
length(valid_data_ts) #20 days

training <- train_data_ts
testing <- valid_data

###### Smoothing ######
#more sophisticated version of exponential smoothing, which can capture trend and/or seasonality
#we do have trend and seasonality in our time series, which means we use Holt–Winter’s Exponential Smoothing

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and additive seasonality.
exp_model_MAA <- ets(training, model = "MAA")
# create predictions
exp_pred_MAA <- forecast(exp_model_MAA, h=20, level = 0)
# plot the series
plot(exp_pred_MAA,  ylab = "Daily average Bid Open Price", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiplicative error, additive trend, and\n additive seasonality")
lines(exp_pred_MAA$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_pred_MAA)
accuracy(exp_pred_MAA, testing)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MAM" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MAM <- ets(training, model = "MAM")
# create predictions
exp_pred_MAM <- forecast(exp_model_MAM, h=20, level = 0)
# plot the series
plot(exp_pred_MAM,  ylab = "Daily average Bid Open Price", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiplicative error, additive trend, and\n multiplicative seasonality")
lines(exp_pred_MAM$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_pred_MAM)
accuracy(exp_pred_MAM, testing)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MMM" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MMM <- ets(training, model = "MMM")
# create predictions
exp_pred_MMM <- forecast(exp_model_MMM, h=20, level = 0)
# plot the series
plot(exp_pred_MMM,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.15),
     bty = "l", xaxt = "n", main = "multiplicative error, multiplicative trend, and\n multiplicative seasonality", flty = )
lines(exp_pred_MMM$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_pred_MMM)
accuracy(exp_pred_MMM, testing)

# run Holt-Winters exponential smoothing
# use ets() with option model = "AAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_AAA <- ets(training, model = "AAA")
# create predictions
exp_pred_AAA <- forecast(exp_model_AAA, h=20, prediction.interval = TRUE, level=0.95)
?forecast
# plot the series
plot(exp_pred_AAA,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nadditive error, additive trend, and\n additive seasonality", flty = )
lines(exp_pred_AAA$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_pred_AAA)
accuracy(exp_pred_AAA, testing)

acf(exp_model_AAA$residuals)
hist(exp_model_AAA$residuals)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MNN <- ets(training, model = "MNN")
# create predictions
exp_pred_MNN <- forecast(exp_model_MNN, h=20, level = 0)
# plot the series
plot(exp_pred_MNN,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error, no trend or seasonality", flty = )
lines(exp_pred_MNN$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_pred_MNN)
accuracy(exp_pred_MNN, testing)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MMN <- ets(training, model = "MMN")
# create predictions
exp_pred_MMN <- forecast(exp_model_MNN, h=20, level = 0)
# plot the series
plot(exp_pred_MMN,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error, multiple trend or seasonality", flty = )
lines(exp_pred_MMN$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_pred_MMN)
accuracy(exp_pred_MMN, testing)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MNM <- ets(training, model = "MNM")
# create predictions
exp_model_MNM <- forecast(exp_model_MNN, h=20, level = 0)
# plot the series
plot(exp_model_MNM,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error, no trend or multiple seasonality", flty = )
lines(exp_model_MNM$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_model_MNM)
accuracy(exp_model_MNM, testing)

# run Holt-Winters exponential smoothing
# use ets() with option model = "MNN" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MNM <- ets(training, model = "MNA")
# create predictions
exp_model_MNM <- forecast(exp_model_MNN, h=20, level = 0)
# plot the series
plot(exp_model_MNM,  ylab = "Bid Price at beginning of hour", xlab = "Time", ylim=c(1.12,1.155),
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiple error, no trend or multiple seasonality", flty = )
lines(exp_model_MNM$fitted, lwd = 1, col = "blue")
lines(testing)
accuracy(exp_model_MNM)
accuracy(exp_model_MNM, testing)


## comparison plot MAM & MMM models
plot(exp_pred_MAM,  ylab = "Bid Price at beginning of hour", xlab = "Time",
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nadditive error, additive trend, and\n additive seasonality", flty = )
plot(exp_pred_MMM,  ylab = "Bid Price at beginning of hour", xlab = "Time",
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nadditive error, additive trend, and\n additive seasonality", flty = )

autoplot(exp_pred_MAM)
lines(exp_pred_MAM$fitted, lwd = 1, col = "blue")
lines(testing)


lines(exp_pred_MAM$fitted, lwd = 1, col = "blue")
lines(exp_pred_MMM$fitted, lwd = 1, col = "red")
lines(testing)

