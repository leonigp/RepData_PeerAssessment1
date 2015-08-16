data <- read.csv("activity.csv", na.strings="NA")
summary(data)

#step 1
#totals <- xtabs(steps ~ date, data = data)
library(plyr)
totalSteps <- ddply(data, .(date), mutate, sum=sum(steps))
qplot(sum, data=totalSteps, geom="histogram", binwidth=500)

intervalFactor <- as.factor(data$interval)
data[,4] <- intervalFactor
#mean
meanSteps <- mean(totalSteps$sum, na.rm=TRUE)
meanSteps

#median
medianSteps <- median(totalSteps$sum, na.rm=TRUE)
medianSteps

#average
library(dplyr)
averageSteps <- ddply(data, .(intervalFactor), mutate, average=mean(steps, na.rm=TRUE))
averageSteps
qplot(interval, average, data = averageSteps, geom = "line")
intervalMax <- max(averageSteps$average)
intervalMax

#number of NAs
numberNA <- sum(is.na(data$steps))
indexNA <- which(is.na(data$steps))
indexNA

dataSansNA <- data
dataSansNA$steps[indexNA] <- averageSteps$average[indexNA]

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
totalStepsSansNA <- ddply(dataSansNA, .(date), mutate, sum=sum(steps))
hist(totalStepsSansNA$sum)
#mean
meanStepsSansNA <- mean(totalStepsSansNA$sum, na.rm=TRUE)
meanStepsSansNA

#median
medianStepsSansNA <- median(totalStepsSansNA$sum, na.rm=TRUE)
medianStepsSansNA

#weekdays
dataSansNA$dateNew <- as.Date(as.character(dataSansNA$date))
dataSansNA$dateNew[1:10]
days <- weekdays(dataSansNA$dateNew, abbreviate = FALSE)
days
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")
dataSansNA$daysOfWeek <- factor((weekdays(dataSansNA$dateNew) %in% weekdays), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
dataWeek <- ddply(dataSansNA, .(interval, daysOfWeek), mutate, averageWeek=mean(steps))
qplot(interval,  averageWeek,	data	=	dataWeek,	facets	=	daysOfWeek~., geom="line")	
