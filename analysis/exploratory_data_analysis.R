# install.packages('lubridate')
# Load lubridate package to convert dates from factor to date class
library(lubridate)

# File path of the data set
path <- "/Users/Jostein/Grad School/SMU/6372/project2/bitcoin/data/bitcoin_price.csv"

# Read in the CSV file of the data set
bitcoin <- read.csv(path, header=TRUE)

# Create new variable time via Lubridate, then order data set by ascending time
attach(bitcoin)
bitcoin$Time <- mdy(bitcoin$Date)
bitcoin <- bitcoin[order(Time),]

# Snapshot of the data set
head(bitcoin)
dim(bitcoin)
str(bitcoin)

# Let's check the diagnostic plots
fitOpen <- lm(Open ~ Time, bitcoin)
fitHigh <- lm(High ~ Time, bitcoin)
fitLow <- lm(Low ~ Time, bitcoin)
fitClose <- lm(Close ~ Time, bitcoin)
par(mfrow=c(2, 2))
plot(fitOpen)
plot(fitHigh)
plot(fitLow)
plot(fitClose)

# Log transformation appears to be needed
# Log transform the various price variables
bitcoin$Log_Open <- log(bitcoin$Open)
bitcoin$Log_High <- log(bitcoin$High)
bitcoin$Log_Low <- log(bitcoin$Low)
bitcoin$Log_Close <- log(bitcoin$Close)

# Double check to see if new variables were created
str(bitcoin)

# Now let's check the diagnostic plots for the transformed data
fitLogOpen <- lm(Log_Open ~ Time, bitcoin)
fitLogHigh <- lm(Log_High ~ Time, bitcoin)
fitLogLow <- lm(Log_Low ~ Time, bitcoin)
fitLogClose <- lm(Log_Close ~ Time, bitcoin)
par(mfrow=c(3, 2))
plot(fitLogOpen, which=1:6)
plot(fitLogHigh, which=1:6)
plot(fitLogLow, which=1:6)
plot(fitLogClose, which=1:6)

# Summary stats
summary(fitLogOpen)
summary(fitLogHigh)
summary(fitLogLow)
summary(fitLogClose)



timeseries <- ts(data=bitcoin$Log_Close, start=c(2013, 118), frequency=365)
plot(timeseries)

plot(bitcoin$Open)
