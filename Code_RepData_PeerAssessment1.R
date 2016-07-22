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
