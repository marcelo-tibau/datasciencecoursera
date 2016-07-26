### Codes to load and preprocess the data
directory <- ("activity")
file <- ("activity.csv")
activity <- read.csv(file.path(directory, file))
activity_na_omit <- na.omit(activity)
total_ac_na_omit <- aggregate(steps ~ date, activity_na_omit, sum)

### What is mean total number of steps taken per day?
## Code to make a histogram of the total number of steps taken each day
hist(
        total_ac_na_omit$steps, 
        main = "Total of steps per day", 
        xlab="Steps per day", 
        ylab="Frequency", 
        col = "green", 
        breaks = 20
        )

## Codes to calculate the mean and median total number of steps taken per day
steps_mean <- mean(total_ac_na_omit$steps)
steps_median <- median(total_ac_na_omit$steps)
print(steps_mean)
print(steps_median)

### What is the average daily activity pattern?
## Code to preprocess the data
total_ac_average <- aggregate(steps ~ interval, activity_na_omit, mean)

## Code to make a time series plot 
library(ggplot2)
par(mar = c(1.5, 1.5, 1, 1))
ggplot(total_ac_average, aes(interval, steps)) +
        geom_line() +
        ggtitle("Time series plot of steps by interval") +
        xlab("Interval") +
        ylab("Number of steps")

## Code to calculate the maximum number of steps
max_number_steps <- which.max(total_ac_average$steps)
print(total_ac_average[max_number_steps,])

### Imputing missing values
## Code to calculate the total number of missing values in the dataset.
number_na <- sum(is.na(activity$steps))
print(number_na)

## Code to fill in all of the missing values in the dataset. 
fill_na <- function(steps, interval) {
        fill_ac <- NA
        if (!is.na(steps))
                fill_ac <- c(steps)
        else
                fill_ac <- (total_ac_average[total_ac_average$interval==interval, "steps"])
        return(fill_ac)
}

## Codes to create a new dataset with the missing data filled in.
new_activity <- activity
new_activity$steps <- mapply(fill_na, new_activity$steps, new_activity$interval)

## Codes to make a histogram of the total number of steps taken each day and to calculate the mean and median total number of steps taken per day.
total_ac_steps <- aggregate(steps ~ date, new_activity, sum)
hist(
        total_ac_steps$steps, 
        main = "Total number of steps per day", 
        xlab="Steps per day", 
        ylab="Frequency", 
        col = "orange", 
        breaks = 20
)
total_ac_steps_mean <- mean(total_ac_steps$steps) 
total_ac_steps_median <- median(total_ac_steps$steps) 
print(total_ac_steps_mean)
print(total_ac_steps_median)

### Are there differences in activity patterns between weekdays and weekends?
## Code to create the new factor variable in the dataset with the levels "weekday" and "weekend".
new_activity$weekday <- weekdays(as.Date(new_activity$date, format = "%Y-%m-%d"))
#new_activity$weekday.type = factor(ifelse(new_activity$weekday == "Monday", "Tuesday", "Wednesday", "Thursday", "Friday" | new_activity$weekday == "Saturday", "Sunday", levels = c("weekday", "weekend")))
new_activity$weekday.type = factor(
        if (weekday == "Saturday")
                {weekday.type == "weekend"}
        if (weekday == "Sunday")
                {weekday.type == "weekend"}
        else {weekday.type == "weekday"}
        )

## Code to calculate the average number of steps per interval
new_activity_weekdays = new_activity[new_activity$weekday.type == "weekday", ]
new_activity_weekend = new_activity[new_activity$weekday.type == "weekend", ]
avg_weekdays = sapply(split(new_activity_weekdays$steps, new_activity_weekdays$interval), mean)
avg_weekend = sapply(split(new_activity_weekend$steps, new_activity_weekend$interval), mean)

ifelse()


































week_activity <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                stop("incorrect")
}
new_activity$date <- as.Date(new_activity$date)
new_activity$day <- sapply(new_activity$date, FUN = week_activity)

##
weekdays_steps <- function(data) {
        weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                                    FUN=mean, na.rm=T)
        weekdays_steps$interval <- 
                as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
        colnames(weekdays_steps) <- c("interval", "steps")
        weekdays_steps
}

data_by_weekdays <- function(data) {
        data$weekday <- 
                as.factor(weekdays(data$date)) 
        weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
        weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))
        
        weekend_steps <- weekdays_steps(weekend_data)
        weekday_steps <- weekdays_steps(weekday_data)
        
        weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
        weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))
        
        data_by_weekdays <- rbind(weekend_steps, weekday_steps)
        data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
        data_by_weekdays
}

data_weekdays <- aggregate(data_by_weekdays(new_activity))



## Code to make the time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
library(lattice)
xyplot(new_activity$weekday ~ interval | steps,
       geom_line(),
       layout = c(1,2),
       main = "Time series plot of steps by interval - weekday and weekend views",
       xlab = "Interval",
       ylab = "Number of steps")

## plot time series
library(lattice)
xyplot(
        mean_steps ~ interval | daytype,
        newinterval,
        type = "l",
        layout = c(1,2),
        main = "Time Series Plot of the 5-Minute Interval\nand the Average Number of Steps Taken,\nAveraged Across All Weekday Days or Weekend Days",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps Taken"
)