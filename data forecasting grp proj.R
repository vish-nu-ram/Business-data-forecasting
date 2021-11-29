
library(zoo)
library(tseries)
# install.packages("TSstudio")
library(TSstudio)
library(forecast)
library(dplyr)


df <- read.csv("/Users/anushree/Downloads/eurusd_hour.csv")
length(df)
range(df$BO)

#the data frame
df50weeks <- tail(df, n = 24*7*50)
plot(df50weeks)

View(df50weeks)
head(df50weeks)
tail(df50weeks)

#aggregating the data so that we have single value for a single day

meanBO = mean(df50weeks$BO)

daywise = df50weeks %>% group_by(Date) %>% summarise(mean(BO))
#daywise_ts = ts(daywise)
#plot(daywise_ts)
daywise1 <- daywise[1:80,] #exclude the noisy part at the end from 360 onwards
plot.ts(daywise1)
daywise_ts2 = daywise1[,-1]
plot = plot.ts(daywise_ts2)

#daywise$Date <- as.Date(daywise$Date)

daywise_ts = ts(daywise_ts2$`mean(BO)`, start = 1, frequency = 7)
plot(daywise_ts)

decompose = decompose(daywise_ts)
plot(decompose)

#removing the seasonality
deseason = seasadj(decompose)
plot(deseason)
#removing the trend
ddeseason = diff(deseason)
plot.ts(ddeseason)

#partioning the data
train_data = head(ddeseason, n=66,start = 0, end = 66)
valid_data = tail(ddeseason, n=14, start = 66, end = 80)

plot(train_data)
plot(valid_data)


#Moving Average

# moving average on training
ma.trailing <- rollmean(train_data , k = 7, align = "right")
plot(ma.trailing)

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)


# create forecast based on last MA
ma.trailing.pred <- ts(rep(last.ma, length(valid_data)), start = 11,
                       end = 12, freq = 7)
plot(ma.trailing.pred)


plot(train_data, ylab = "mean BO", xlab = "Time",bty = "l",xaxt = "n", main = "")
axis(1, at = seq(0, 12, 1), labels = format(seq(0, 12, 1)))
lines(ma.trailing, lwd = 2, col = "blue")
lines(ma.trailing.pred, lwd = 2, col = "red", lty = 2)
lines(valid_data, lwd = 2)


ma.trailing.pred

accuracy(ma.trailing, train_data)
accuracy(ma.trailing, valid_data)



