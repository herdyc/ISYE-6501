library(ggplot2)
library(gridExtra)
library(caret)
library(glmnet)

rm(list = ls())
set.seed(65)
uscrime <- read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_5_Homework-summer23/week 5 data-summer/data 11.1/uscrime.txt', header=TRUE)
head(uscrime)
tail(uscrime)

# Split into Train and Validation
splitIndex <- sample(seq_len(nrow(uscrime)), size=floor(0.7*nrow(uscrime)))
crimeTrain <- uscrime[splitIndex,] 
crimeTest <- uscrime[-splitIndex,]

table(crimeTrain$Crime)
table(crimeTest$Crime)

# RMSE and R squared function
rmse_rsq <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

# Basic Linear Regression
uscrime_lm <- lm(crimeTrain$Crime~., crimeTrain[1:15])
summary(uscrime_lm)

# Stepwise Regression for Training - Both
uscrime_step <- step(uscrime_lm, direction="both")
summary(uscrime_step)
anova_both <- uscrime_step$anova

# Calculate R-squared - Both
uscrime_lm_predict <- predict(uscrime_step, crimeTest[1:15])
rmse_rsq(crimeTest$Crime, uscrime_lm_predict, uscrime)

# Stepwise Regression for Training - Forward
uscrime_step <- step(uscrime_lm, direction="forward")
summary(uscrime_step)
anova_forward <- uscrime_step$anova

# Calculate R-squared - Forward
uscrime_lm_predict <- predict(uscrime_step, crimeTest[1:15])
rmse_rsq(crimeTest$Crime, uscrime_lm_predict, uscrime)

# Stepwise Regression for Training - Backward
uscrime_step <- step(uscrime_lm, direction="backward")
summary(uscrime_step)
anova_backward <- uscrime_step$anova

# Calculate R-squared - Backward
uscrime_lm_predict <- predict(uscrime_step, crimeTest[1:15])
rmse_rsq(crimeTest$Crime, uscrime_lm_predict, uscrime)

################################################################################

# Create Scaled data of US Crime and Split
set.seed(65)
xtrain<-scale(as.matrix(crimeTrain)[,-16], center = TRUE, scale = TRUE)
ytrain<-scale(as.matrix(crimeTrain)[,16], center = TRUE, scale = TRUE)
xtest<-scale(as.matrix(crimeTest)[,-16], center = TRUE, scale = TRUE)
ytest<-scale(as.matrix(crimeTest)[,16], center = TRUE, scale = TRUE)

# Lasso 
lasso_cv <- cv.glmnet(xtrain, ytrain, alpha=1,type.measure="mse", family="gaussian")
plot(lasso_cv)

# Model Prediction on Training
lasso_predict <- predict(lasso_cv, xtrain)
rmse_rsq(ytrain, lasso_predict, uscrime)

# Model Prediction on Testing
lasso_predict <- predict(lasso_cv, xtest)
rmse_rsq(ytest, lasso_predict, uscrime)

# Lasso - Improved
# Improve by using Lambda with minimum values
coef(lasso_cv, s=lasso_cv$lambda.min)
best_lambda <- lasso_cv$lambda.min
cat(best_lambda)

lasso_cv_improved <- glmnet(xtrain, ytrain, family="gaussian", alpha=1, lambda = best_lambda)
coef(lasso_cv_improved)

# Model Prediction on Training
lasso_predict <- predict(lasso_cv_improved, xtrain)
rmse_rsq(ytrain, lasso_predict, uscrime)

# Model Prediction on Testing
lasso_predict <- predict(lasso_cv_improved, xtest)
rmse_rsq(ytest, lasso_predict, uscrime)

################################################################################

# Elastic Net
# Model Building : Elastic Net Regression
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)

# Training ELastic Net Regression model
elasticNet <- train(Crime ~ .,
                       data = as.matrix(scale(crimeTrain)),
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 15,
                       trControl = control)

elasticNet

# Plot
plot(elasticNet, main = "Elastic Net Regression")

# Model Prediction on Training
elastic_predict_train <- predict(elasticNet, xtrain)
rmse_rsq(ytrain, elastic_predict_train, uscrime)

# Model Prediction on Testing
elastic_predict_test <- predict(elasticNet, xtest)
rmse_rsq(ytest, elastic_predict_train, uscrime)


################################################################################

# Data frame from the Question
test_point <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640,  M.F = 94.0, Pop = 150, 
  NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0
)
