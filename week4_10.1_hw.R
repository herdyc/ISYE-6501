library(ggplot2)
library(gridExtra)
library(tree) 
library(randomForest)

rm(list = ls())
set.seed(65)
US_Crime <- read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_4_Homework-summer23/week 4 data-summer/data 10.1/uscrime.txt', header=TRUE)
head(US_Crime)
tail(US_Crime)
nrow(US_Crime)
ncol(US_Crime)
range(US_Crime$Crime)

# Regression Tree model
US_Crime_Tree <- tree(Crime ~ ., data = US_Crime)
summary(US_Crime_Tree)
plot(US_Crime_Tree)
text(US_Crime_Tree)
title("US CRIME Regression Tree")
summary(US_Crime_Tree)

# Prune Regression Tree 5 nodes
US_Crime_Tree_Prune <- prune.tree(US_Crime_Tree, best = 5)
plot(US_Crime_Tree_Prune)
text(US_Crime_Tree_Prune)
title("Pruned Tree")
summary(US_Crime_Tree_Prune)

# Calculate R-Squared from RSS and TSS
US_Crime_Tree_Prune_Pred <- predict(US_Crime_Tree_Prune, data = US_Crime[,1:15])
RSS_5 <- sum((US_Crime_Tree_Prune_Pred - US_Crime[,16])^2)
TSS_5 <- sum((US_Crime[,16] - mean(US_Crime[,16]))^2)
R2_5 <- 1 - RSS_5/TSS_5
R2_5

# the Residual mean deviance increased so, it would better to shift to the orginal model before Pruning

# Prune Regression Tree with 7 nodes
US_Crime_Tree_Prune_7 <- prune.tree(US_Crime_Tree, best = 7)
plot(US_Crime_Tree_Prune_7)
text(US_Crime_Tree_Prune_7)
title("Pruned Tree")
summary(US_Crime_Tree_Prune_7)
US_Crime_Tree_Prune_7$where

# Calculate R-Squared from RSS and TSS
US_Crime_Tree_Prune_7_Pred <- predict(US_Crime_Tree_Prune_7, data = US_Crime[,1:15])
RSS <- sum((US_Crime_Tree_Prune_7_Pred - US_Crime[,16])^2)
TSS <- sum((US_Crime[,16] - mean(US_Crime[,16]))^2)
R2 <- 1 - RSS/TSS
R2

# Random Forest model ALL Variables
US_Crime_Forest <- randomForest(Crime ~ ., data=US_Crime, importance = TRUE, nodesize = 5, max_depth=5)
print(US_Crime_Forest)
randomForest::importance(US_Crime_Forest)
varImpPlot(US_Crime_Forest)

# Calculate R-Squared from RSS and TSS
US_Crime_Forest_Rf <- predict(US_Crime_Forest, data = US_Crime[,1:15])
RSS_Rf <- sum((US_Crime_Forest_Rf - US_Crime[,16])^2)
TSS_Rf <- sum((US_Crime[,16] - mean(US_Crime[,16]))^2)
R2_Rf <- 1 - RSS_Rf/TSS_Rf
R2_Rf

# Do loop to find max_depth
for (x in 1:10){
  US_Crime_Forest <- randomForest(Crime ~ ., data=US_Crime, importance = TRUE, nodesize = 5, max_depth=x)
  US_Crime_Forest_Rf <- predict(US_Crime_Forest, data = US_Crime[,1:15])
  RSS_Rf <- sum((US_Crime_Forest_Rf - US_Crime[,16])^2)
  TSS_Rf <- sum((US_Crime[,16] - mean(US_Crime[,16]))^2)
  R2_Rf <- 1 - RSS_Rf/TSS_Rf
  R2_Rf
  print(R2_Rf)
}

lm_Final <- lm(Crime~Po1+Po2+Prob, US_Crime)
summary(lm_Final)

