#Load the data
unzip("activity.zip")
myData <- read.csv("activity.csv")

#Summarize the data by day
byDay <- tapply(myData$step,myData$date,sum, na.rm=TRUE)
hist(byDay, 
     main="Histogram of steps taken in a day", 
     xlab = "Steps taken", 
     ylab="Number of days", 
     col="pink",
     border="red",)

#Mean and median
meanByDay <- mean(byDay)
medianByDay <- median(byDay)

#Interval averaged
AvgByInt <- tapply(myData$step,as.factor(myData$interval),mean, na.rm=TRUE)
plot(names(AvgByInt), AvgByInt, 
     type="l",
     col="pink",
     lwd=3,
     main ="Average steps taken througout a day",
     xlab = "Time intervals",
     ylab = "Steps taken")