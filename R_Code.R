## Test file

df <- read.csv("Business-data-forecasting/eurusd_hour.csv")
View(df)
head(df)
tail(df)

library(stats)

z <- ts(data = df$BO,  freq= 24)
plot.ts(z[1:100000], ylab = "Bid Price - Opening of Day") 
#is there a way to change the x-axis back to the time format, e.g. in years or the dates?


