library(ggplot2)
library(gridExtra)
library(caret)

# Updated Dataset website
# http://archive.ics.uci.edu/dataset/144/statlog+german+credit+data

rm(list = ls())
set.seed(65)
GermanCredit <- read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_4_Homework-summer23/week 4 data-summer/data 10.3/germancredit.txt', header=F)

# Replace 1 and 2 to 0 and 1
GermanCredit$V21[GermanCredit$V21==1] <- 0
GermanCredit$V21[GermanCredit$V21==2] <- 1

table(GermanCredit$V21)
prop.table(table(GermanCredit$V21))

# Split into Train and Validation
creditSplit <- createDataPartition(GermanCredit$V21, times = 1, p = 0.7, list=FALSE)
creditTrain <- GermanCredit[creditSplit,] 
creditValid <- GermanCredit[-creditSplit,]

table(creditTrain$V21)
table(creditValid$V21)

# Use Logistic Regression for Training data and do 
creditGlm <- glm(V21~ ., data = creditTrain, family=binomial(link="logit"))
summary(creditGlm)

# Use Logistic Regression for the whole data after removing insignificant variables
creditGlmImproved <- glm(V21 ~ V1A14+V4A41+V4A43+V5+V8, data = creditTrain, family=binomial(link="logit"))
summary(creditGlmImproved)

# Prediction for Normal Training Data
creditGlmPredict <- predict(creditGlm, newdata=creditValid[,-21], type="response")
table(creditValid$V21, round(creditGlmPredict))

# Prediction for Improved Training Data
creditGlmPredict <- predict(creditGlmImproved, newdata=creditValid[,-21], type="response")
table(creditValid$V21, round(creditGlmPredict))

# Calculate Confusion Matrix with 0.7 threshold
threshold <- 0.7
creditPredictFinal <- predict(creditGlmImproved, newdata=creditValid[,-21], type="response")
t <- as.matrix(table(round(creditPredictFinal > threshold), creditValid$V21))
names(dimnames(t)) <- c("Predicted", "Observed")
t

# Calculate Accuracy 
accuracy <- (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
accuracy

# Calculate Sensitivity and Specifity
sensitivity(t)
