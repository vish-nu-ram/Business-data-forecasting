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
exp_model <- ets(training, model = "MAA")
# create predictions
exp_pred <- forecast(exp_model, level = 0)
# plot the series
plot(exp_pred,  ylab = "Bid Price at beginning of hour", xlab = "Time",
     bty = "l", xaxt = "n", main = "", flty = )
lines(exp_pred$fitted, lwd = 1, col = "blue")
lines(testing[4704:5040])
