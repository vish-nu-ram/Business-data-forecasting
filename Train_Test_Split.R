### splitting ts to train and test

library(tseries)

df <- read.csv("Business-data-forecasting/eurusd_hour.csv")
df80weeks <- tail(df, n = 24*7*50)
head(df6weeks)
tail(df6weeks)

df30weeks<- head(df80weeks, n = 24*7*30)
ts30weeksBO <- ts(data = df30weeks$BO)
plot.ts(ts30weeksBO)

split_ts <- ts_split(ts30weeksBO, sample.out = 24*7*2)
training <- split_ts$train
testing <- split_ts$test

length(training)
length(testing)

