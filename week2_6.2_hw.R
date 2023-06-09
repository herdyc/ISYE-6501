library(ggplot2)
library(gridExtra)

set.seed(65)
data = read.table('C:/Users/OMEN/OneDrive/Desktop/ISYE 6501/week_2_Homework-summer23/week 2 data-summer/data 6.2/temps.txt', header=TRUE)
head(data)
tail(data)
nrow(data)

# The hot season lasts for 3.8 months, from May 24 to September 17, 
# with an average daily high temperature above 81°F. 
# he hottest month of the year in Atlanta is July, with an average high of 88°F and low of 71°F.

# Thus, we are focusing on July, June, Aug and 3rd week of Sept which is roughly 80 days since 1st July.

# Plot the daily temp in 2015 and 1996
plotCombined <- 
  ggplot() +
  geom_line(data, mapping = aes(x=idu, y=X1996, group=1), color = "blue") +
  geom_line(data, mapping = aes(x=idu, y=X2015, group=1), color = "red") +
  xlab('idu') +
  ylab('Year 1996 and 2015')

# S value = Cusum
s <- rep(0,nrow(data))
s[[1]] <- 0
#use a C value of 25% of the standard deviation
c0 <- 0.25*sd(data$X1996)
# Mean value of 80 days since 1st July
mean_data <- mean(data$X1996[1:80])
#calculate the s values by day.
#Note that we are detecting the change in a decreasing time series trend.
for (i in 2:nrow(data)){
  s[[i]] <- max(0, s[[i-1]] + (mean_data - data$X1996[i] - c0))
}
s

# Plot S value with Standard Deviation Threshold
plot(s)
abline(h=5*sd(data$X1996), col="red") # 5% Threshold
min(which(s>=5*sd(data$X1996))) # First day when s reaches the Std Deviation

# New frame to store s value
cusumData <- data.frame(matrix(ncol = 21, nrow = 123))
colnames(cusumData) <- colnames(data)[1:21]
endOfSummerPerYear <- rep(0,20)

# Loop through all 20 years
for (i in 2:21){
  cusumData[,i][1] <- 0 # Set initial value to 0
  mean_data <- mean(data[,i][1:80])
  
  # Fill in Cusum value for each Year
  for (j in 2:123){
    cusumData[,i][j] <- max(0, cusumData[,i][j-1] + (mean_data - data[,i][j] - 0.25*sd(data[,i])))
  }
  
  # Assign the result
  endOfSummerPerYear[[i-1]] <- min(which(cusumData[,i]>=(5*sd(data[,i]))))
}

endOfSummerPerYear

# Pull out the Years
years <- sub('.', '', colnames(data)[2:21])
years

# Assign the Date result along with its respective Year
endOfSummerPerYear_df <- data.frame(cbind(years = years, days = as.numeric(endOfSummerPerYear)))
endOfSummerPerYear_df

# Plot the End of Summer
endOfSummerPerYearDates <- vector(length(endOfSummerPerYear), mode = "list")
for (i in 1:20){
  endOfSummerPerYearDates[[i]] <- data$DAY[endOfSummerPerYear[[i]]]
}

ggplot(data=endOfSummerPerYear_df, aes(x=years, y=as.numeric(days), group=1)) +
  geom_line(color="turquoise",lwd=2) +
  geom_point() +
  geom_text(
    label=endOfSummerPerYearDates, 
    nudge_x = 0.5, nudge_y = 1.5, 
    check_overlap = T
  ) +
  xlab('years') +
  ylab('days')

# Is Atlanta getting warmer? Yes

# Vector to store mean temp
avgSummerTemp <- rep(0,20)
for (i in 2:21){
  avgSummerTemp[[i-1]] <- mean(data[,i][1:endOfSummerPerYear[i-1]])  
}

avgSummerTemp

# Plot mean temp
plot(years, avgSummerTemp, main="Avg Summer Temperature 1996 - 2015", xlab="Year", ylab="Temperature (F)", xlim=c(1995,2016), ylim=c(75,100))
lines(years, avgSummerTemp, lwd=3, col="turquoise")
text(years, avgSummerTemp, round(avgSummerTemp,1), cex=0.6, pos=1, col="black")