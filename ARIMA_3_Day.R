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
daywise_ts = ts(round(daywise1$`mean(BO)`,4))
View(daywise_ts)

#partioning the data
train_data = ts(head(daywise_ts, n=250))
valid_data = ts(tail(daywise_ts, n=100),start = 250, end = 350)

class(valid_data)
plot(train_data)
plot(valid_data)

dcomp <- stl(train_data, s.window = "periodic")

plot(dcomp)

adf.test(train_data)
training_d1 <- diff(train_data, differences = 1)
adf.test(training_d1)
plot.ts(training_d1)

pacf(training_d1)
# We can use p as 1 

acf(training_d1)
# we can use q as 0?

ts_Mod1 <- arima(train_data, order = c(1,1,0))
print(ts_Mod1)

#valForcast <- forecast()
length(valid_data)

ts_For1 <- forecast(ts_Mod1, h= 100)
plot(ts_For1)
lines(ts_For1$fitted,, lwd = 1, col = "blue")
lines(valid_data)



ts_Mod2 <- auto.arima(train_data, trace = TRUE)

autoplot(forecast(ts_Mod2, h= 20))

accuracy(ts_For1, valid_data)
# Even with Auto arima we get 1,1,0 as the hyperparameter. 
