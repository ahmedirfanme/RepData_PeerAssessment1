
# Reading the data
activity <- read.csv("~/DataScience/Rep Research/RepData_PeerAssessment1-master/activity.csv")

#adding total steps daily excluding NAs
dailysteps <- activity %>% 
                group_by(date) %>% 
                summarize(sumsteps = sum(steps, na.rm=TRUE))

#Histogram of the total number of steps taken each day
hist(dailysteps$sumsteps, col ="red", xlab = "Daily Steps", 
     main = "Histogram of total number of steps each day", ylim = c(0,31))

#Mean and median number of steps taken each day
meansteps <- mean(dailysteps$sumsteps)
mediansteps <- median(dailysteps$sumsteps)
paste("The mean is:", meansteps)
paste("The median is:", mediansteps)

#Time series plot of the average number of steps taken
stepsPerInterval <- activity %>%
    group_by(interval) %>%
    summarize(meansteps = mean(steps, na.rm = TRUE)) 
plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
    col="red", type="l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps",
    main = "Steps By Time Interval")

#The 5-minute interval that, on average, contains the maximum number of steps
paste("Interval where the maximum number of steps is: ", stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)])
paste("Average steps for that interval: ",round(max(stepsPerInterval$meansteps), digits = 2))
      
#Code to describe and show a strategy for imputing missing data
paste("Total number of NAs are: ", sum(is.na(activity$steps)))

activityNoNA <- activity  
for (i in 1:nrow(activity)){
    if(is.na(activity$steps[i])){
        activityNoNA$steps[i]<- stepsPerInterval$meansteps[activityNoNA$interval[i] == stepsPerInterval$interval]
    }
}

#Histogram of the total number of steps taken each day after missing values are imputed
dailystepsNoNA <- activityNoNA %>%
         group_by(date) %>%
         summarize(sumsteps = sum(steps, na.rm = TRUE))
      
hist(dailystepsNoNA$sumsteps, col ="red", xlab = "Daily Steps", main = "Histogram of total number of steps each day", ylim = c(0,31))

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)

activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
    group_by(interval) %>%
    summarize(steps = mean(steps)) 
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
    group_by(interval) %>%
    summarize(steps = mean(steps)) 
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)

g <- ggplot (wkdayWkend, aes (interval, steps))
g + geom_line() + facet_grid (day~.) + 
    theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
    labs(y = "Number of Steps") + labs(x = "Interval") + 
    ggtitle("Average Number of Steps - Weekday vs. Weekend") + 
    theme(plot.title = element_text(hjust = 0.5))

