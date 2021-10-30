######## ARIMA Model try 1

df <- read.csv("Business-data-forecasting/eurusd_hour.csv")
View(df)
head(df)
tail(df)
install.packages("TSstudio")

library(stats)
library(forecast)
library(tseries)
library(TSstudio)

#For now we are focusing on BO and trying to forecast the value for BO
z <- ts(data = df$BO,  freq= 24)

plot.ts(z[60000:100000])
split_BO <- ts_split(z)

z_train <- split_BO$train
z_test <- split_BO$test
# The data doesn't seem to be stationary, let us test it.

acf(z_train)
pacf(z_train)

adf.test(z_train)

## p -value is not less than 0.05 , which means the data is not stationary. 

boAutoArima <- auto.arima(z_train,ic="aic",trace = TRUE) 

#best model is (0,1,1)(0,0,1)[24]

acf(ts(boAutoArima$residuals))
pacf(ts(boAutoArima$residuals))
plot.ts(boAutoArima$residuals)

myBoForecast <- forecast(boAutoArima, h = 240)
plot(myBoForecast, xlim = c(2500,2750))

# Only picking last 2 years of data 
df2Years <- tail(df, n = 17520)
head(df2Years)
tail(df2Years)

ts2yearsBO <- ts(data = df2Years$BO,  freq= 24)
plot.ts(ts2yearsBO)


## Looking at BCh as it should not have any trend, 
#hopefully no seasonality as it is the change in value every hour

ts2yearsCh  <- ts(data = df2Years$BCh, freq = 24)
plot.ts(ts2yearsCh)

acf(ts2yearsCh)
pacf(ts2yearsCh)
adf.test(ts2yearsCh)

bchAutoArima <- auto.arima(ts2yearsCh,ic="aic",trace = TRUE) 

myBchForecast <- forecast(bchAutoArima, h = 240)
plot(myBchForecast, xlim = c(730,735))

###last 3 weeks data only

df3weeks <- tail(df, n = 504)
head(df3weeks)
tail(df3weeks)

ts3weeksBO <- ts(data = df3weeks$BO,  freq= 24)
plot.ts(ts3weeksBO)

acf(ts3weeksBO)
pacf(ts3weeksBO)
adf.test(ts3weeksBO)

boAutoArima <- auto.arima(ts3weeksBO,ic="aic",trace = TRUE) 

myBoForecast <- forecast(boAutoArima, h = 240)
plot(myBoForecast)
