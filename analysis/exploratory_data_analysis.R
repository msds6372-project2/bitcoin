# install.packages('lubridate')
# Load lubridate package to convert dates from factor to date class
library(lubridate)
library(reshape2)
library(ggplot2)
library(forecast)

# File path of the data set
path <- "/Users/Jostein/Grad School/SMU/6372/project2/bitcoin/data/bitcoin_price.csv"

# Read in the CSV file of the data set
bitcoin <- read.csv(path, header = TRUE)

# Create new variable time via Lubridate, then order data set by ascending time
detach(bitcoin)
attach(bitcoin)
bitcoin$Time <- mdy(bitcoin$Date)
bitcoin <- bitcoin[order(bitcoin$Time),]

# Convert factor variables into integers
# Source: https://stackoverflow.com/questions/4798343/convert-factor-to-integer
#bitcoin$Volume <- as.numeric(levels(Volume))[Volume]
#str(bitcoin)

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

# Correlation matrix heatmap
# Source: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# Source: https://stackoverflow.com/questions/3571909/calculate-correlation-cor-for-only-a-subset-of-columns
cormat <- round(cor(bitcoin[sapply(bitcoin, is.numeric)]), 2)
head(cormat)

# Get Lower triangle of the correlation matrix
get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Put helper functions to use
upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
head(melted_cormat)

# Build the heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
  coord_fixed()




timeseries <- ts(data=bitcoin$Log_Close, start=c(2013, 118), frequency=365)
plot(timeseries)

plot(bitcoin$Open)

arima_fit <- auto.arima(bitcoin$Close)
plot(forecast(arima_fit))