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

#Most steps
maxByInt <- AvgByInt[which.max(AvgByInt)]

#Missing values
missSteps <- sum(is.na(myData$steps))

#Replace missing values with interval average
myData2 <- myData
for (i in 1:nrow(myData2)) {
        if (is.na(myData2[i,1])) {
                myData2[i,1] <- as.numeric(AvgByInt[names(AvgByInt)==myData2[i,3]])
        }
}


#New histogram after missing values replaced
byDay2 <- tapply(myData2$step,myData2$date,sum, na.rm=TRUE)
hist(byDay2, 
     main="Histogram of steps taken in a day (NA replaced)", 
     xlab = "Steps taken", 
     ylab="Number of days", 
     col="lightcyan2")



