 activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "character"))
> activity$day <- factor(paste(substring(months(as.Date(activity$date)),0,3), 
+                       substring(as.Date  (activity$date), 9), sep=" ")) 
> tidyData <- na.omit(activity)
> head(tidyData)
    steps       date interval    day
289     0 0010-02-20        0 Feb 20
290     0 0010-02-20        5 Feb 20
291     0 0010-02-20       10 Feb 20
292     0 0010-02-20       15 Feb 20
293     0 0010-02-20       20 Feb 20
294     0 0010-02-20       25 Feb 20


MEAN AND MEDIAN
##mean and median steps per day
> tot_steps <- aggregate(tidyData$steps, list(tidyData$date), FUN="sum")
> names(tot_steps) <- c("date","total")
> tot_steps$date <- factor(paste(substring(months(as.Date(tot_steps$date)),0,3), 
+                       substring(as.Date  (tot_steps$date), 9), sep=" ")) 
> print(round(mean(tot_steps$total)))
[1] 10730
> print(median(tot_steps$total))
[1] 10890
> ##ANSWER: mean = 10730, meadian = 10890
> ##create histogram of number steps per day
> barplot(tot_steps$total, names.arg=tot_steps$date, xlab="Days", ylab="Total Steps", main="Total Steps Count Per Day", col="blue")
> histogram(tot_steps$total, names.arg=tot_steps$date, xlab="Days", ylab="Total Steps", main="Total Steps Count Per Day", col="blue")

AVERAGE DAILY ACTIVITY PATTERN
##average daily activity pattern
> ##1.Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
> activity_steps <- aggregate(tidyData$steps, list(as.numeric(tidyData$interval)), FUN="mean")
Warning messages:
1: "names.arg" is not a graphical parameter 
2: "names.arg" is not a graphical parameter 
3: "names.arg" is not a graphical parameter 
4: "names.arg" is not a graphical parameter 
> names(activity_steps) <- c("interval","mean")
> plot(activity_steps, type="l", xlab="Interval", ylab="Number of Steps", main="Daily Activity Pattern", col=3)
> 
> ##2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
> maxI <- activity_steps[which.max(activity_steps$mean),]
> print(maxI)
    interval     mean
104      835 296.4444
> ##ANSWER interval 835

IMPUTING MISSING VALUES
##imputing missing values
> ##1.Calculate and report the total number of missing values in the dataset 
> missingSteps <- which(is.na(activity))
> print(length(missingSteps))
[1] 12960
> ##ANSWER: 12960 missing values
> ##2.Devise a strategy for filling in all of the missing values in the dataset
> newData <- activity
> for(i in 1:length(missingSteps)){
+     newData[missingSteps[i], 1] <- activity_steps[activity_steps$interval ==
+                                    newData[missingSteps[i],]$interval,]$mean
+ }

 ##histogram new data
> new_steps <- aggregate(newData$steps, list(newData$date), FUN="sum")
> names(new_steps) <- c("date","total")
> new_steps$date <- factor(paste(substring(months(as.Date(new_steps$date)),0,3), 
+                       substring(as.Date  (new_steps$date), 9), sep=" ")) 
> barplot(new_steps$total, names.arg=new_steps$date, xlab="Days", ylab="Total Steps",
+         main="Total Steps Count Per Day (Without Missing Data)", col="steelblue")
> ##mean number steps per day
> print(round(mean(new_steps$total)))
[1] 10730
> ##median number steps per day
> print(median(new_steps$total))
[1] 10729.94
> ##diff between data sets
> print(mean(new_steps$total) - mean(tot_steps$total))
[1] 0
> ##ANSWER:  no difference
> print(median(new_steps$total) - median(tot_steps$total))
[1] -160.0556
> ##ANSWER:  difference in median


DIFFERENCE IN ACTIVITY WEEKDAYS AND WEEKENDS
 ##differences in activity patterns between weekdays and weekends
> ##1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
> head(newData)
      steps       date interval    day
1 4.5000000 0010-01-20        0 Jan 20
2 1.0000000 0010-01-20        5 Jan 20
3 0.3888889 0010-01-20       10 Jan 20
4 0.4444444 0010-01-20       15 Jan 20
5 0.0000000 0010-01-20       20 Jan 20
6 0.7222222 0010-01-20       25 Jan 20
> newData$daysOfWeek <- factor(weekdays(newData$date))
> levels(newData$daysOfWeek)
[1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
[7] "Wednesday"
> levels(newData$daysOfWeek) <- list(weekday = c("Monday", "Tuesday", "Wednesday", 
+                                                "Thursday", "Friday"),
+                                    weekend = c("Saturday", "Sunday"))
> table(newData$daysOfWeek)

weekday weekend 
   4608    2304 
> library(lattice)
> meanSteps <- aggregate(newData$steps, list(as.numeric(newData$interval),newData$daysOfWeek),        FUN = "mean")
> names(meanSteps) <- c("interval","weekDays", "avgSteps")
> xyplot(meanSteps$avgSteps ~ meanSteps$interval | meanSteps$weekDays, 
+        layout = c(1, 2), type = "l", 
+        xlab = "Interval", ylab = "Number of steps")






