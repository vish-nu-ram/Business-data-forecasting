####### original: Train_Test_Split.R #######
### splitting ts to train and test 

library(tseries)
library(TSstudio)

df <- read.csv("Business-data-forecasting/eurusd_hour.csv")
df80weeks <- tail(df, n = 24*7*50)
head(df80weeks)
tail(df80weeks)

df30weeks<- head(df80weeks, n = 24*7*30)
ts30weeksBO <- ts(data = df30weeks$BO,frequency = 24)
plot.ts(ts30weeksBO)

split_ts <- ts_split(ts30weeksBO, sample.out = 24*7*2)
training <- split_ts$train
testing <- split_ts$test

length(training)
length(testing)

###### Smoothing ######
#more sophisticated version of exponential smoothing, which can capture trend and/or seasonality
#we do have trend and seasonality in our time series, which means we use Holt–Winter’s Exponential Smoothing

library(forecast)


# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and additive seasonality.
exp_model_MAA <- ets(training, model = "MAA")
# create predictions
exp_pred_MAA <- forecast(exp_model_MAA, h=366, level = 0)
# plot the series
plot(exp_pred_MAA,  ylab = "Bid Price at beginning of hour", xlab = "Time",
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiplicative error, additive trend, and\n additive seasonality", flty = )
lines(exp_pred_MAA$fitted, lwd = 1, col = "blue")
lines(testing)

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

# run Holt-Winters exponential smoothing
# use ets() with option model = "MMM" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_MMM <- ets(training, model = "MMM")
# create predictions
exp_pred_MMM <- forecast(exp_model_MMM, h=366, level = 0)
# plot the series
plot(exp_pred_MMM,  ylab = "Bid Price at beginning of hour", xlab = "Time",
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nmultiplicative error, multiplicative trend, and\n multiplicative seasonality", flty = )
lines(exp_pred_MMM$fitted, lwd = 1, col = "blue")
lines(testing)

# run Holt-Winters exponential smoothing
# use ets() with option model = "AAA" to fit Holt-Winter's exponential smoothing
# with multiplicative error, additive trend, and multiplicative seasonality.
exp_model_AAA <- ets(training, model = "AAA")
# create predictions
exp_pred_AAA <- forecast(exp_model_AAA, h=366, level = 0)
# plot the series
plot(exp_pred_AAA,  ylab = "Bid Price at beginning of hour", xlab = "Time",
     bty = "l", xaxt = "n", main = "Holt-Winter's Exponential Smoothing\nadditive error, additive trend, and\n additive seasonality", flty = )
lines(exp_pred_AAA$fitted, lwd = 1, col = "blue")
lines(testing)



