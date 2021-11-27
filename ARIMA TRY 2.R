
df <- read.csv("Business-data-forecasting/eurusd_hour.csv")
install.packages("TSstudio")
# Selecting 30 week from the data and making it a TS object

df80weeks <- tail(df, n = 24*7*50)
head(df6weeks)
tail(df6weeks)

df30weeks<- head(df80weeks, n = 24*7*30)
ts30weeksBO <- ts(data = df30weeks$BO, frequency = 24)
plot.ts(ts30weeksBO)

#Splitting it to train and test
library(TSstudio)
split_ts <- ts_split(ts30weeksBO, sample.out = 24*7*2)
training <- split_ts$train
testing <- split_ts$test

length(training)
length(testing)

dcomp <- stl(ts30weeksBO, s.window = "periodic")

plot(dcomp)

## Test to see if data is stationary

adf.test(ts30weeksBO, k = 24)
class(ts30weeksBO)
##differencing

training_d1 <- diff(training, differences = 1)
adf.test(training_d1, k = 24)
plot.ts(training_d1)

# now this is stationary as p-value ios 0.01, so d = 1

pacf(training_d1)

# We can use P as 2?

acf(training_d1)

# Here we will choose q = 1 from the above graph.

# now we have p,d,q as (2,1,1), we use this to run arima
dcomp1 <- stl(training_d1, s.window = "periodic")
plot(dcomp1)

# Arima model with (2,1,1)
ts_Mod1 <- arima(training, order = c(2,1,1))
print(ts_Mod1)

# Forecast for next two weeks
autoplot(forecast(ts_Mod1, h= 24*7*2))
lines(ts_Mod1$fitted, lwd = 1, col = "blue")
lines(testing[4704:5040])


ts_Mod2 <- auto.arima(training, seasonal = FALSE)
autoplot(forecast(ts_Mod2, h= 24*7*2))

ts_Mod3 <- arima(training, order = c(0,1,1))
autoplot(forecast(ts_Mod3, h= 24*7*2))

