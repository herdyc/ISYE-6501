library(TTR)
library(ggplot2)
library(ggfortify)
library(fpp3)
library(forecast)
library(clipr)
library(xlsx)

rm(list = ls())
set.seed(1)

data <- read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_3_Homework-summer23/week 3 data-summer/data 7.2/temps.txt', header=TRUE)
head(data)
tail(data)
nrow(data)
ncol(data)
data[,2]
write_clip(data[,2])

# Turn the data into a time series object
# Frequency for number of rows in data
# Start = ths first year in temps file
data_ts <- ts(as.vector(unlist(data[,2:21])),start=1996,frequency=nrow(data))
plot(data_ts, xlab="year", ylab="temp")
length(data_ts)

# Sampling Testing for 1 year
data_ts <- ts(as.vector(unlist(data[,20:21])),start=2014,frequency=nrow(data))
plot(data_ts, xlab="year", ylab="temp")

# Simple Moving Average SMA Function
components_data_ts_SMA <- SMA(data_ts, n=8)
plot.ts(components_data_ts_SMA)

# Decompose the data
components_data_ts <- decompose(data_ts)
plot(components_data_ts)

# ggts with ACF and PACF
ggtsdisplay(data_ts)
pacf(data_ts)
acf(data_ts)

# Create HoltWinters Model
# alpha (HIGHER) = Puts more weight on recent observations/data
# beta (0) = Do Exponential Smoothing
# gamma (0) = Non-Seasonal Model is fitted

# Determine the optimal parameters of alpha, beta, and gamma
# Additive by default
m1a <- HoltWinters(data_ts, alpha=NULL, beta=NULL, gamma=NULL, seasonal='additive')
m1a
summary(m1a)
m1a$alpha
m1a$beta
m1a$gamma
plot(m1a)
fitted_m1a <- fitted(m1a)

# Seasonal factors
m1a_sf <- matrix(fitted_m1a[,4], nrow=123)
head(m1a_sf)

# Original Level
m1a_smoothed <- matrix(fitted_m1a[,2], nrow=123)
head(m1a_smoothed)

# xhat OR smoothed result
m1a_smoothed <- matrix(fitted_m1a[,1], nrow=123)
head(m1a_smoothed)

# Export to Excel
write.xlsx(m1a_smoothed, "C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_3_Homework-summer23/smoothed.xlsx")

# Testing Optimal Multiplicative
m1m <- HoltWinters(data_ts, alpha=NULL, beta=NULL, gamma=NULL , seasonal='multiplicative')
m1m
summary(m1m)
m1m$alpha
m1m$beta
m1m$gamma
fitted_m1m <- fitted(m1m)
plot(m1m)

# Seasonal factors
head(m1m$fitted)
tail(m1m$fitted)
m1m_sf <- matrix(m1m$fitted[,4], nrow=123)
head(m1m_sf)

# xhat OR smoothed result
m1m_smoothed <- matrix(m1m$fitted[,1], nrow=123)
head(m1m_smoothed)
m1m_smoothed[,19]
ncol(m1m_smoothed)

# Export to Excel
write.xlsx(m1m_smoothed, "C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_3_Homework-summer23/smoothed1.xlsx")


# Further Testing
# Double exponential smoothing
m2 <- HoltWinters(data_ts, gamma=F, seasonal='additive')
m2
plot(m2)

# Triple exponential smoothing (additive seasonality)
m3a <- HoltWinters(data_ts)
m3a

# Triple exponential smoothing (multiplicative seasonality)
m3m <- HoltWinters(data_ts, seasonal="multiplicative")
m3m
plot(m3m)

# Put the factors into a matrix
m <- matrix(m3m$fitted[,4],ncol=123)
m