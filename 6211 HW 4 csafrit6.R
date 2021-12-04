library(forecast)
library(zoo)


dfCS <- read.csv('~/_DSBA_UNCC/DBSA_6211_Advanced_Business_and_Analytics/AustralianWines.csv', na.strings=c('NA',''))
str(dfCS)
head(dfCS)
View(dfCS)

xCS <- ts(dfCS$Red, start=c(80,1),frequency = 12)
xCS
plot(xCS)

# Model 1: Linear Trend Model
Wine.lmCS <- tslm(xCS~trend)
summary(Wine.lmCS)

# Data partition for time series data
# Use the last 48 months data as the training dataset
nValidCS <- 48
nTrainCS <- length(xCS)-nValidCS

train.tsCS <- window(xCS,start=c(80,1),end=c(80,nTrainCS))
valid.tsCS <- window(xCS,start=c(80,nTrainCS+1),end=c(80,nTrainCS+nValidCS))

train.lmCS <- tslm(train.tsCS~trend)
summary(train.lmCS)
train.lm.predCS <- forecast(train.lmCS,h=nValidCS,level=0)

# Visualize the linear trend model
par(mfrow = c(1, 1))
plot(train.lm.predCS, ylim = c(400, 2900),  ylab = "Red", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(80,94),main = "", flty = 2)
axis(1, at = seq(80, 94, 1), labels = format(seq(80, 94, 1)))
lines(train.lm.predCS$fitted, lwd = 2, col = "blue")
lines(valid.tsCS)

# Evaluate model performance
accuracy(train.lm.predCS,valid.tsCS)

# A model with seasonality
# In R, function tslm() uses ts() which automatically creates the categorical Season column (called season) and converts it into dummy variables.
train.lm.seasonCS <- tslm(train.tsCS ~ season)
summary(train.lm.seasonCS)
train.lm.season.predCS <- forecast(train.lm.seasonCS, h = nValidCS, level = 0)

# Visualize the seasonality model
par(mfrow = c(1, 1))
plot(train.lm.season.predCS, ylim = c(400, 2800),  ylab = "Red", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(80,94),main = "", flty = 2)
axis(1, at = seq(80, 94, 1), labels = format(seq(80, 94, 1)))
lines(train.lm.season.predCS$fitted, lwd = 2, col = "blue")
lines(valid.tsCS)
accuracy(train.lm.season.predCS,valid.tsCS)

#Both Linear Trend and seasonality
train.lm.trend.seasonCS <- tslm(train.tsCS~trend +season)
summary(train.lm.trend.seasonCS)

# Visualize the Linear Trend and seasonality model
plot(xCS,col = "red")
lines(train.lm.trend.season.predCS$fitted,lwd = 2,
      col = "blue")


train.lm.trend.season.predCS <- forecast(train.lm.trend.seasonCS,
                                              h=nValidCS,
                                              level = 0)
accuracy(train.lm.trend.season.predCS,valid.tsCS)

#Both polynomial Trend and seasonality
train.lm.poly.trend.seasonCS <- tslm(train.tsCS~trend + I(trend^2)+season)
summary(train.lm.poly.trend.seasonCS)

# Visualize the polynomial Trend and seasonality model
plot(xCS,col = "red")
lines(train.lm.poly.trend.season.predCS$fitted,lwd = 2,
      col = "blue")


train.lm.poly.trend.season.predCS <- forecast(train.lm.poly.trend.seasonCS,
                                            h=nValidCS,
                                            level = 0)
accuracy(train.lm.poly.trend.season.predCS,valid.tsCS)

# run simple exponential smoothing
# and alpha = 0.2 to fit simple exponential smoothing.
sesCS <- ses(train.tsCS, alpha = 0.2, h=36)
autoplot(sesCS)
accuracy(sesCS,valid.tsCS)

# Use ses function to estimate alpha
sesCS1 <- ses(train.tsCS, alpha = NULL, h=36)
summary(sesCS1)
accuracy(sesCS1,valid.tsCS)


