library(ggplot2)

set.seed(65)
data = read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_2_Homework-summer23/week 2 data-summer/data 4.2/iris.txt')
head(data)

# Plot
ggplot(data, aes(Petal.Length, Petal.Width)) + geom_point(aes(col=Species), size=2)

# Use Elbow Diagram to plot the ideal number of clusters
# 10 randomized data sets with 15 iterations (Total = 150)
set.seed(65)
wss<- NULL # wss = With-in-sum-of-Squares
for (i in 1:10){
  fit = kmeans(iris[,3:4],i,nstart = 15,iter.max = 15)
  wss = c(wss, fit$tot.withinss)
}
plot(1:10, wss, type = "o", main = "Elbow Diagram for Iris", xlab = "Number of clusters(k)", ylab = "With-in-sum-of-Squares")

# Use 2 and 3 clusters since it has the most impact within the graph
cluster2 <- kmeans(iris[,3:4],2,nstart = 15)
table(cluster2$cluster,iris$Species)

cluster3 <- kmeans(iris[,3:4],3,nstart = 15)
table(cluster3$cluster,iris$Species)