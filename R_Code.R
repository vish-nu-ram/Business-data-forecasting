## Test file

df <- read.csv("Business-data-forecasting/eurusd_hour.csv")
View(df)
head(df)
tail(df)

library(stats)

z <- ts(data = df$BO,  freq= 24)
plot.ts(z[1:100000])
