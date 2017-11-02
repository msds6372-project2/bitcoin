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
# Source: https://stackoverflow.com/questions/20077944/changing-dates-into-numeric-form-to-do-a-correlation
bitcoin$Time_Posix <- as.POSIXct(bitcoin$Time, format="%Y-%m-%d %H:%M:%S")
bitcoin$Time_Posix <- as.numeric(bitcoin$Time_Posix)
bitcoin$Volume_Numeric <- as.numeric(bitcoin$Volume)
bitcoin$Market.Cap_Numeric <- as.numeric(bitcoin$Market.Cap)
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
# upper_tri <- get_upper_tri(cormat)
# upper_tri

# Melt the correlation matrix
# melted_cormat <- melt(upper_tri, na.rm = TRUE)
# head(melted_cormat)

# Order the correlation matrix based
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Build the heatmap
# ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
#   geom_tile(color = "white")+
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
#   coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


timeseries <- ts(data=bitcoin$Close, start=c(2013, 118), frequency=365)
dev.off()
plot(timeseries)

plot(bitcoin$Open)

arima_fit <- auto.arima(bitcoin$Close)
plot(forecast(arima_fit))

# Possible take on project, combine close prices with google trends
# Leave for room for improvement to talk about in conclusion
# trends <- read.csv(file = "/Users/Jostein/Grad School/SMU/6372/project2/bitcoin/data/google_trends_bitcoin.csv", header = TRUE)
# newdata <- data.frame(diff1, trends)
# model <- lm(diff1 ~ trends, data = newdata)

