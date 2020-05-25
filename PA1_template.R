data <- read.csv("activity.csv")

## What is mean total number of steps taken per day?

#1 Total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, data, sum)
steps_per_day

#2 Histogam of total number of teps taken per day
hist(steps_per_day$steps, main = paste("Total Steps Per Day"), col = "red", xlab = "Number of Steps")

#3 Mean & Median of total number of steps taken per day

Mean_steps <- mean(steps_per_day$steps)
Mean_steps

Median_steps <- median(steps_per_day$steps)
Median_steps


##What is the average daily activity pattern ?

#1 Time serise plot

steps_by_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per day by Interval", col="red")

#2 maximum number of steps in 5 min interval 

max_steps_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_steps_interval

#Imputing missing values

#1 Calculate and report the total number of missing values in the dataset

missing_values <- sum(!complete.cases(data))
missing_values

#2 Filling all the missing values in the dataset

AvgSteps <- aggregate(steps ~ interval, data = data, FUN = mean)

fill_missing <- numeric()
for (i in 1:nrow(data)) {
    obs <- data[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(AvgSteps, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fill_missing <- c(fill_missing, steps)
}

#3 Create a new dataset that is equal to the original dataset but with the missing data filled in

new_dataset <- data
new_dataset$steps <- fill_missing

#4 Histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

Total_steps <- aggregate(steps ~ date, data = new_dataset, sum, na.rm = TRUE)
hist(Total_steps$steps, main = paste("Total Steps Per Day"), col="blue", xlab="Number of Steps")

#Histogram to show difference. 
hist(steps_per_day$steps, main = paste("Total Steps Per Day"), col="green", xlab="Number of Steps", add=T)

legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)

#calculate Mean
mean_total <- mean(Total_steps$steps)
mean_total

#calculate Median
median_total <- median(Total_steps$steps)
median_total

##differencce from the estimates from the first part of the assignment?

mean_difference <- mean_total - Mean_steps 
mean_difference

median_difference <- median_total - Median_steps
median_difference

#Are there differences in activity patterns between weekdays and weekends?

#1 new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

new_dataset$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_dataset$date)),weekdays), "Weekday", "Weekend"))

Total_steps <- aggregate(steps ~ interval + dow, new_dataset, mean)

library(lattice)

xyplot(Total_steps$steps ~ Total_steps$interval | Total_steps$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
