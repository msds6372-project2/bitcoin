# Load lubridate package to convert dates from factor to date class
library(lubridate)
library(reshape2)
library(ggplot2)
library(forecast)
library(DescTools)

# File path of the data set
path <- "/Users/Jostein/Grad School/SMU/6372/project2/bitcoin/data/bitcoin_price.csv"

# Read in the CSV file of the data set
bitcoin <- read.csv(path, header = TRUE)

# Create new variable time via Lubridate, then order data set by ascending time
bitcoin$Time <- mdy(bitcoin$Date)
bitcoin <- bitcoin[order(bitcoin$Time),]

# Snapshot of the data set
head(bitcoin)
dim(bitcoin)
str(bitcoin)

# Line plot of daily closing price of bitcoin
plot(bitcoin$Close, type = "l", xlab = "Time", ylab = "Close Prices", main = "Daily Close Prices of Bitcoin")

# Let's check the diagnostic plots
fitClose <- lm(Close ~ Time, bitcoin)
par(mfrow = c(3, 2))
plot(fitClose, which = 1:6)


# Transformation appears to be needed
bitcoin$Log_Close <- log(bitcoin$Close)
bitcoin$Sqrt_Close <- sqrt(bitcoin$Close)

# How do we get to stationary?
# First differences for original and transformed data sets
diff1 <- diff(bitcoin$Close, lag = 1)
logDiff1 <- diff(bitcoin$Log_Close, lag = 1)
sqrtDiff1 <- diff(bitcoin$Sqrt_Close, lag = 1)

# Plot first differences for original and transformed data sets
dev.off()
par(mfrow = c(3, 1))
plot(diff1, type = "l", xlab = "Time", ylab = "Difference", main = "First Difference for Original Data")
plot(logDiff1, type = "l", xlab = "Time", ylab = "Difference", main = "First Difference for Logged Data")
plot(sqrtDiff1, type = "l", xlab = "Time", ylab = "Difference", main = "First Difference for Square-root Data")

# Diagnostic plots for first differences of square-root data
fitSqrtClose <- lm(Log_Close ~ Time, bitcoin)
par(mfrow = c(3, 2))
plot(fitLogClose, which = 1:6)
hist(bitcoin$Close)
# Full and partial auto-correlation plots for first differences of square-root data
par(mfrow = c(2, 1))
acf(sqrtDiff1, main = "Auto-correlation Function of the First Differences")
pacf(sqrtDiff1, main = "Partial Auto-correlation Function of the First Differences")

# Create second differences for square-root data
# Line plot and correlation plots for second differences
sqrtDiff2 <- diff(sqrtDiff1, lag = 1)
dev.off()
par(mfrow = c(3, 1))
plot(sqrtDiff2, type = "l", xlab = "Time", ylab = "Difference", main = "Second Difference for Square-root Data")
acf(sqrtDiff2, main = "Auto-correlation Function of the Second Differences")
pacf(sqrtDiff2, main = "Auto-correlation Function of the Second Differences")

# plot(model)
dev.off()
arima_fit <- auto.arima(bitcoin$Log_Close)
plot(forecast(arima_fit))
summary(arima_fit)

hist(logDiff1, prob = T, col = "red")
lines(density(logDiff1), lwd = 2)


plot(residuals(arima_fit))


# Arima Modeling
# Source: https://stats.stackexchange.com/questions/207473/how-to-set-the-prediction-range-of-arima-model-in-r
# Source: https://www.otexts.org/fpp/8/7
myts <-ts(data = bitcoin$Log_Close, start = c(2013, 118), frequency = 365)
model <- auto.arima(myts[1:1545])
summary(model)
# h parameter forecasts 75 days ahead
fcst <- forecast(model, h = 75)
plot(fcst, main = "Bitcoin Logged Closing Price Forecast", 
     ylab = "Logged Closing Price", xlab = "Year")
lines(fitted(fcst), col = "red")
accuracy(f = fcst, x = bitcoin$Log_Close[1546:1620])

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

# Use this for R markdown file
myts <-ts(data = bitcoin$Log_Close, start = c(2013, 118), frequency = 365)
arima_fit <- auto.arima(myts, ic = "aic", trace = TRUE)
summary(arima_fit)
forecast_arima_fit <- forecast(arima_fit, h = 75)
plot(forecast_arima_fit, xlab = "Time", ylab = "Logged Closing Price")
accuracy(f = forecast_arima_fit, x = myts[1546:1620])
#lines(fitted(forecast_arima_fit), col="red")
# plot(ts(data=bitcoin$Log_Close, start = c(2013, 118), frequency = 12))
# lines(ts(forecast_arima_fit, start = c(2013, 118), frequency = 12), col = "red")

model1 <- Arima(myts, c(0, 0, 0))
model2 <- Arima(myts, c(0, 1, 0))
model3 <- Arima(myts, c(1, 1, 0))
model4 <- Arima(myts, c(2, 1, 0))
model5 <- Arima(myts, c(2, 2, 0))

line1 <- c("Arima(0, 0, 0)", model1$sigma2, model1$aic, model1$bic)
line2 <- c("Arima(0, 1, 0)", model2$sigma2, model2$aic, model2$bic)
line3 <- c("Arima(1, 1, 0)", model3$sigma2, model3$aic, model3$bic)
line4 <- c("Arima(2, 1, 0)", model4$sigma2, model4$aic, model4$bic)
line5 <- c("Arima(2, 2, 0)", model5$sigma2, model5$aic, model5$bic)

# Source: http://www.cyclismo.org/tutorial/R/tables.html
# modelTable <- matrix(c("Arima(0, 0, 0)", model1$sigma2, model1$aic, model1$bic, 
#                        "Arima(0, 1, 0)", model2$sigma2, model2$aic, model2$bic,
#                        "Arima(1, 1, 0)", model3$sigma2, model3$aic, model3$bic,
#                        "Arima(2, 1, 0)", model4$sigma2, model4$aic, model4$bic,
#                        "Arima(2, 2, 0)", model5$sigma2, model5$aic, model5$bic,
#                        "Auto.Arima(0, 1, 0)", arima_fit$sigma2, arima_fit$aic, arima_fit$bic), ncol = 4, byrow = TRUE)
modelTable <- matrix(c(line1, line2, line3, line4, line5, 
                       "Auto.Arima(0, 1, 0)", arima_fit$sigma2, arima_fit$aic, arima_fit$bic), ncol = 4, byrow = TRUE)
colnames(modelTable) <- c("Model", "Variance", "AIC", "SBC")
modelTable

comparePrices <- matrix(c("2017-Oct-04", 4215.10, exp(forecast_arima_fit$mean[1]),
                          "2017-Oct-05", 4315.40, exp(forecast_arima_fit$mean[2]),
                          "2017-Oct-06", 4371.00, exp(forecast_arima_fit$mean[3]),
                          "2017-Oct-07", 4436.00, exp(forecast_arima_fit$mean[4]),
                          "2017-Oct-08", 4613.10, exp(forecast_arima_fit$mean[5]),
                          "2017-Oct-09", 4782.30, exp(forecast_arima_fit$mean[6]),
                          "2017-Oct-10", 4777.00, exp(forecast_arima_fit$mean[7])), ncol = 3, byrow = TRUE)
colnames(comparePrices) <- c("Date", "Actual", "Forecast")
comparison <- as.data.frame(comparePrices)
comparison$Date <- ymd(comparison$Date)
comparison$Actual <- as.numeric(comparison$Actual)
comparison$Forecast <- as.numeric(comparison$Forecast)
#class(comparison$Date)
class(comparison$Actual)
plot(x = comparison$Date, y = comparison$Actual, type = "l")
lines(x = comparison$Date, y = comparison$Forecast, type = "l", col = "red")

plot(x = comparePrices$Date, y = comparison$Actual, type = "l")
lines(x = comparison$Date, y = comparison$Forecast, type = "l", col = "red")


df <- read.csv("/Users/Jostein/Grad School/SMU/6372/project2/bitcoin/data/conclusion_comparison.csv", header = TRUE)
df$Date <- ymd(df$Date)
# Source: http://www.r-graph-gallery.com/119-add-a-legend-to-a-plot/
plot(y = df$Actual, x = df$Date, col=rgb(0.2,0.4,0.1,0.7), type = "b", bty="l", lwd=3, pch=17, xlab = "Time", ylab = "Close Prices", main = "Daily Close Prices of Bitcoin")
lines(y = df$Forecast, x = df$Date, col=rgb(0.8,0.4,0.1,0.7), lwd=3 , pch=19 , type="b")
legend("topleft", 
       legend = c("Actual", "Forecast"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))