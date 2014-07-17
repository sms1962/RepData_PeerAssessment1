setwd("D:/Statystyki-R/R/Coursera/Reproductible_Research/Assesment1a")
## Loading and preprocessing the data
actual.locale <- Sys.getlocale()
Sys.setlocale("LC_ALL","en_US")
activity <- read.csv("activity/activity.csv", header = TRUE, sep=",")
head(activity)
tail(activity)
str(activity)
activity$date <- as.Date(activity$date)
activity$month <- as.numeric(format(activity$date, "%M"))
activity$day <- as.numeric(format(activity$date, "%d"))
activity$weekday <- weekdays(activity$date)
# Create data set without NA
activity_no_rm <- activity[complete.cases(activity),]
str(activity)
summary(activity)
# dołóż kolumne dzień, miesiąc, dzień tygodnia


library("dplyr")
library("ggplot2")
# Group activity without NA per day for 
activity_per_day <- group_by(activity_no_rm, date)
steps_per_day <- summarise(activity_per_day,sumsteps = sum(steps))


# zrób histogram ale baz nazw na x
ggplot(steps_per_day, aes(x=sumsteps)) + geom_histogram()
hist(steps_per_day$sumsteps
     , main = "Histogram with breaks = 20"
     , xlab = "Total steps per day"
     , col = "blue"
     , breaks = 20)
activity.mean <- mean(krokow_per_day$krokow)
activity.median <- median(krokow_per_day$krokow)

# Average daily activity pattern
intervals_group <- group_by(activity_no_rm, interval)
steps_mean_per_interval <- summarise(intervals_group, meansteps=mean(steps))

g <- ggplot(steps_mean_per_interval
            , aes(x = interval, y = meansteps))
g + geom_line() +
        labs(y = "Average steps") +
        labs(x = " interval") +
        labs(title = "Average steps per interval")

# Which 5 min interval contains the maximum
# temp <- which(steps_per_mean_interval$meansteps==max(steps_per_mean_interval$meansteps))
max_interval<-steps_mean_per_interval[which(steps_mean_per_interval$meansteps==max(steps_per_mean_interval$meansteps)),]
max_interval


# Imputing missing values