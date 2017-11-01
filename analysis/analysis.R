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

# Possible take on project, combine close prices with google trends
# Leave for room for improvement to talk about in conclusion
# trends <- read.csv(file = "/Users/Jostein/Grad School/SMU/6372/project2/bitcoin/data/google_trends_bitcoin.csv", header = TRUE)
# newdata <- data.frame(diff1, trends)
# model <- lm(diff1 ~ trends, data = newdata)


# plot(model)
dev.off()
arima_fit <- auto.arima(bitcoin$Log_Close)
plot(forecast(arima_fit))
summary(arima_fit)

hist(logDiff1, prob = T, col = "red")
lines(density(logDiff1), lwd = 2)