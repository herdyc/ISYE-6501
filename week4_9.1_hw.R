library(ggplot2)
library(gridExtra)
library(AICcmodavg)

rm(list = ls())
set.seed(65)
US_Crime <- read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_4_Homework-summer23/week 4 data-summer/data 9.1/uscrime.txt', header=T)
head(US_Crime)
tail(US_Crime)
nrow(US_Crime)
ncol(US_Crime)
range(US_Crime$Crime)

pca <- prcomp(US_Crime[,1:15], scale. = T, center = T)
summary(pca)
plot(pca)
screeplot(pca, type="lines",col="red")
abline(h=1, col="black")

# Get Eigenvalues and plot it
pca_eigenvalues <- pca$sdev^2 
propvar_pca_eigenvalues <- pca_eigenvalues/sum(pca_eigenvalues)
plot(propvar_pca_eigenvalues, 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0,1), 
     type = "b")

# Choose 5 Principal Components from Kaiser Rule and combing with original US Crime data
k = 5
Pc_Crime <- cbind(pca$x[,1:k], US_Crime[,16])

# Create Linear Regression model
model_pca_lm <- lm(V6~.,data=as.data.frame(Pc_Crime))
summary(model_pca_lm)

# Extract the intercept
beta0 <- model_pca_lm$coefficients[1]
betas <- model_pca_lm$coefficients[2:(k+1)]
beta0
betas

# Transform the PC coefficients into coefficients for the original variables
alphas <- pca$rotation[,1:k] %*% betas

# Convert the coefficients back to original data
originalAlpha <- alphas/sapply(US_Crime[,1:15],sd)
originalBeta0 <- beta0 - sum(alphas*sapply(US_Crime[,1:15],mean)/sapply(US_Crime[,1:15],sd))

# Find estimates
estimates <- as.matrix(US_Crime[,1:15]) %*% originalAlpha + originalBeta0

# Calculate R-squared and Adjusted R-squared
SSE = sum((estimates - US_Crime[,16])^2)
SST = sum((US_Crime[,16] - mean(US_Crime[,16]))^2)
R2 <- 1 - SSE/SST
R2

R2_adjust <- R2 - (1-R2)*k/(nrow(US_Crime)-k-1)
R2_adjust

# Data frame from Question 8.2
test_point <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640,  M.F = 94.0, Pop = 150, 
  NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0
)

# Predict the model
pred_df <- data.frame(predict(pca, test_point))
pred <- predict(model_pca_lm, data.frame(predict(pca, test_point)))
pred 