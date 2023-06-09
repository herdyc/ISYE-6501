library(kernlab) 
library(kknn)

setwd("C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_1_Homework-summer21/week 1 data-summer/data 2.2")
set.seed(65)
data = read.table("credit_card_data-headers.txt", stringsAsFactors = FALSE, header = TRUE) 
head(data)

# QN 2.2.1

# KSVM model with Scaling and Vanilladot as a Kernel.
model_ksvm <- ksvm(
  as.matrix(data[,1:10]),
  as.factor(data[,11]),
  type = "C-svc",
  kernel="vanilladot",
  C=100^100,
  scaled=TRUE)
model_ksvm
# calculate a1…am
a <- colSums(model_ksvm@xmatrix[[1]] * model_ksvm@coef[[1]])
a
# calculate a0
a0 <- model_ksvm@b*(-1)
a0
# see what the model predicts
pred <- predict(model_ksvm,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == data[,11]) / nrow(data)

# loop to find the optimal C
for(x in 1: 100){
  model <- ksvm(
    as.matrix(data[,1:10]),
    as.factor(data[,11]),
    type = "C-svc",
    kernel="vanilladot",
    C=x,
    scaled=TRUE)

  print(sprintf("When C is %i Accuracy = %s", x, sum(pred == data[,11]) / nrow(data)))
}

# KSVM model with NO Scaling and Vanilladot as a Kernel and C = 100.
model_ksvm_vanilla_100 <- ksvm(
  as.matrix(data[,1:10]),
  as.factor(data[,11]),
  type = "C-svc",
  kernel="vanilladot",
  C=100,
  scaled=FALSE)
model_ksvm_vanilla_100
# calculate a1…am
a <- colSums(model_ksvm_vanilla_100@xmatrix[[1]] * model_ksvm_vanilla_100@coef[[1]])
a
# calculate a0
a0 <- model_ksvm_vanilla_100@b*(-1)
a0
# see what the model predicts
pred <- predict(model_ksvm_vanilla_100,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == data[,11]) / nrow(data)

# KSVM model with NO Scaling and Vanilladot as a Kernel and C = 500.
model_ksvm_vanilla_500 <- ksvm(
  as.matrix(data[,1:10]),
  as.factor(data[,11]),
  type = "C-svc",
  kernel="vanilladot",
  C=500,
  scaled=FALSE)
model_ksvm_vanilla_500
# calculate a1…am
a <- colSums(model_ksvm_vanilla_500@xmatrix[[1]] * model_ksvm_vanilla_500@coef[[1]])
a
# calculate a0
a0 <- model_ksvm_vanilla_500@b*(-1)
a0
# see what the model predicts
pred <- predict(model_ksvm_vanilla_500,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == data[,11]) / nrow(data)

################################################################################

# QN 2.2.2

# KSVM model with Scaling and Rbfdot as a Kernel.
model_ksvm_rbf <- ksvm(
  as.matrix(data[,1:10]),
  as.factor(data[,11]),
  type = "C-svc",
  kernel="rbfdot",
  C=100,
  scaled=TRUE)
model_ksvm_rbf
# calculate a1…am
a <- colSums(model_ksvm_rbf@xmatrix[[1]] * model_ksvm_rbf@coef[[1]])
a
# calculate a0
a0 <- model_ksvm_rbf@b*(-1)
a0
# see what the model predicts
pred <- predict(model_ksvm_rbf,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == data[,11]) / nrow(data)

################################################################################

# QN 2.2.3

library(kknn)

set.seed(65)
data = read.table("credit_card_data-headers.txt", stringsAsFactors = FALSE, header = TRUE)

# Loop over neighbor k from 1 to 25
for (k in 1:25){
  for (i in 1:nrow(data)){
    train <- data[-i,]
    valid <- data[i,]
    model_knn <- kknn(
      R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15,
      train,
      valid,
      k=k,
      distance = 2,
      kernel = "rectangular",
      scale = TRUE)
    
    # Get the knn accuracy
    pred[i] <- as.integer(fitted(model_knn))
  }
  
  # Calculate the Average Accuracy of all points i
  avg_accuracy = sum(pred == data[,11]) / nrow(data)
  print(avg_accuracy)
}

################################################################################

# QN 3.1.a
rm(list = ls())

library(kknn)

set.seed(65)
data = read.table("credit_card_data-headers.txt", stringsAsFactors = FALSE, header = TRUE) 
data

# With train.kknn NORMAL
model_knn_train <- train.kknn(
  R1~.,
  data,
  kmax = 100,
  scaled=TRUE)

mean(fitted(model_knn_train)[[100]][1:nrow(data)])

# With train.kknn LOOP
kmax = 200
model_knn_train <- train.kknn(
  R1~.,
  data,
  kmax = kmax,
  scaled=TRUE)
model_knn_train
# Use loop to look through the fitted values and its accuracies
for (k in 150:kmax){
  mean_res <- print(mean(fitted(model_knn_train)[[k]][1:nrow(data)]))
}
  
# With cv.kknn NORMAL
set.seed(65)
model_knn_cv <- cv.kknn(
  R1~.,
  data,
  kcv = 10,,
  k=10,# 10-fold cross validation
  scale=TRUE
)
model_knn_cv
summary(model_knn_cv)
print(mean(model_knn_cv[[1]][,2]))

# With cv.kknn LOOP
set.seed(65)
# loop through k values
for (k in 1:25){
  model_knn_cv <- cv.kknn(
    R1~.,
    data,
    kcv = 10,,
    k=k,# 10-fold cross validation
    scale=TRUE
  )
  print(mean(model_knn_cv[[1]][,2]))
}

################################################################################

# QN 3.1.b

rm(list=)

library(kknn)
library(kernlab)

set.seed(65)
data = read.table("credit_card_data-headers.txt", stringsAsFactors = FALSE, header = TRUE) 

# Split into 60, 20, 20
sample <- sample(1:nrow(data), (0.6*nrow(data)))
train  <- data[sample, ]
valid_test   <- data[-sample, ]

sample2 <- sample(1:nrow(valid_test), (0.2*nrow(valid_test)))
valid <- valid_test[sample2, ]
test  <- valid_test[-sample2, ]

model_SVM <- ksvm(
  as.matrix(train[,1:10]),
  as.factor(train[,11]),
  type = "C-svc",
  kernel = "vanilladot",
  C = 5,
  scaled=TRUE)
model_SVM

# Use the model from Training set to test on Validation set
pred <- predict(model_SVM,valid[,1:10])
pred
sum(pred == data[,11]) / nrow(test)

model_SVM_2.fit <- kknn(
  as.factor(R1)~.,
  train,
  valid,
  k=i,
  kernel = "rbfdot",
  scale=TRUE
)
  
round(fitted(model_SVM_2)) == valid$R1

# Use the model from Training set to test on Validation set
pred <- predict(model_SVM_2.fit, type="prob")
pred

################################################################################
