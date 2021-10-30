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


