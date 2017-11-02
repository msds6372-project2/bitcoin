# Forecast of Bitcoin Closing Pricing 
myts <-ts(data=bitcoin$Log_Close, start=c(2013, 118), frequency=365)
fs <- forecast(myts, h=75, level=c(80,95))
plot(f, main = "Bitcoin Logged Closing Price Forecast", 
     ylab="Logged Closing Prices", xlab="Year")
lines(fitted(fs), col="red")

# Arima Modeling
library(forecast)
model <- auto.arima(bitcoin$Log_Close[1:1545])
fcst <- forecast(model, h=75)
accuracy(f=fcst, x=bitcoin$Log_Close[1546:1620])