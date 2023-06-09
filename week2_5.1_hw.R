library(outliers)

set.seed(65)
data = read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_2_Homework-summer23/week 2 data-summer/data 5.1/uscrime.txt', header=TRUE)
head(data)
nrow(data)

plot(data$Crime, ylim = c(0,3000))
text(data$Crime, labels = data$Crime, cex = 0.8, font = 2, pos = 3)

boxplot(data$Crime)

grubbs.test(data$Crime)

# Type = 10 for one max outlier
# Opposite = FALSE to check the value with largest difference from the mean 
grubbs.test(data$Crime, type = 10)

# Type = 11 for one max outlier
# Opposite = FALSE to check the value with largest difference from the mean 
grubbs.test(data$Crime, type = 11)
