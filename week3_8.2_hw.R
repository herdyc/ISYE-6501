library(ggplot2)
library(gridExtra)
library(caret)
library(AICcmodavg)
library(knitr) 

rm(list = ls())
set.seed(65)
data <- read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_3_Homework-summer23/week 3 data-summer/data 8.2/uscrime.txt', header=TRUE)
head(data)
tail(data)
nrow(data)
ncol(data)
range(data$Crime)

# Use lm function instead of glm
# The data should not have any relation and be independent
# , unlike time series data

# Std Error = Standard Deviation of an estimate
# t value for Hypothesis Test
# P really low = reject Hypothesis or small probability

# Linear Regression lm function
uscrime_lm <- lm(Crime~., data)
uscrime_lm
summary(uscrime_lm)
print(sprintf("RMSE = %0.2f", sigma(uscrime_lm)))

pred_model <- predict(uscrime_lm, test_point)

# Data frame from the Question
test_point <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640,  M.F = 94.0, Pop = 150, 
  NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0
)

# Predict with p-value of 0.05
data.frame(summary(uscrime_lm)$coef[summary(uscrime_lm)$coef[,4] <= .05, 4])

set.seed(1)
uscrime_lm_5 <- lm(Crime~M+Ed+Ineq+Prob, data)
uscrime_lm_5
summary(uscrime_lm_5)
print(sprintf("RMSE = %0.2f", sigma(uscrime_lm_5)))

pred_model_5 <- predict(uscrime_lm_5, test_point)

# Predict with codes (., *, **)
set.seed(1)
uscrime_lm_codes <- lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data)
uscrime_lm_codes
summary(uscrime_lm_codes)
print(sprintf("RMSE = %0.2f", sigma(uscrime_lm_codes)))

pred_model_codes <- predict(uscrime_lm_codes, test_point)

# Calculate AICc for all 3 models
set.seed(0)

models <- list(uscrime_lm, uscrime_lm_5, uscrime_lm_codes)
mod.names <- c('basic', '0.05', 'codes')
aictab(cand.set = models, modnames = mod.names, second.ord = TRUE)
bictab(cand.set = models, modnames = mod.names, second.ord = TRUE)


stitch("week3_8.2_hw.R")
