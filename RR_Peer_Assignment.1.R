#Reproducible Research - Peer Assessment 1 - Filomena Ciccarelli

## Loading and preprocessing the data

setwd("C:/Users/CiccareF/Desktop/Data Science/Reproducible Research/data")
dir()
#[1] "activity.csv"

activity<-read.csv("activity.csv",header = TRUE)
library(plyr)
library(dplyr)
library(ggplot2)
str(activity)
#'data.frame':	17568 obs. of  3 variables:
#$ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
#$ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ interval: int  0 5 10 15 20 25 30 35 40 45 ...

###Convert the date variable from Factor to Date variable

activity$date<-as.Date(activity$date)

## What is mean total number of steps taken per day?

### Total number of steps taken per day
tot.steps.day<-group_by(activity,date)%>%summarise(total.steps=sum(steps))
head(tot.steps.day)

### Plot the data on histogram Q1.1
hist(tot.steps.day$total.steps,
     breaks=50,
     main="Total Number of steps per day",
     xlab="Total step per day",
     ylab="count",
     col="blue")

### Mean and Median total number of steps per day Q1.2
steps.avg.day<-mean(tot.steps.day$total.steps,na.rm = TRUE)
steps.median.day<-median(tot.steps.day$total.steps,na.rm = TRUE)
steps.avg.day
steps.median.day
## What is the average daily activity pattern
avg.steps.interval<-group_by(activity,interval)%>%summarise(average.steps=mean(steps,na.rm=TRUE))

### Plot the data Q2.1
plot(avg.steps.interval$interval,avg.steps.interval$average.steps,
     type="l",
     xlab="Interval",
     ylab="Average Number of step",col="dark green")

### Interval with max number of steps Q2.2
max.steps.int<-which.max(avg.steps.interval$average.steps)
interval.max.steps<-avg.steps.interval[max.steps.int,1]
interval.max.steps
## Imputing missing values

### Total number of missing values in the dataset
table(complete.cases(activity))

### Create a new data frame and impute missing values using interval averages
imput.activity<-activity
imput.activity$steps<-ifelse(is.na(imput.activity$steps),
                             avg.steps.interval$average.steps[match(imput.activity$interval,avg.steps.interval$interval)],
                             imput.activity$steps)
table(complete.cases(imput.activity))

### Total number of steps taken per day with imputed missing values
imput.tot.steps.day<-group_by(imput.activity,date)%>%summarise(total.steps=sum(steps))
head(imput.tot.steps.day)

### Plot the data on histogram Q3.4
hist(imput.tot.steps.day$total.steps,
     breaks=50,
     main="Total Number of steps per day (NAs imputed)",
     xlab="Total step per day",
     ylab="count",
     col="red")
imput.steps.avg.day<-mean(imput.tot.steps.day$total.steps)
imput.steps.median.day<-median(imput.tot.steps.day$total.steps)

### The average and median steps by day are not affected by the adopted imputation of the NA values approach
### The distribution of the total nr of steps per day is a closer approximation of a Gaussian distribution

## Are there differences in activity patterns between weekdays and weekends?

### Identify the day of the week varaible first in the imput.activity data frame and then create a weekday or weekend variable

imput.activity$day<-weekdays(imput.activity$date)
w.day<-c("Monday","Tuesdya","Wednesday","Thursday","Friday")
w.end<-c("Saturday","Sunday")
imput.activity$weekpart<-ifelse(imput.activity$day %in% w.day,"weekday","weekend")
head(imput.activity)
table(imput.activity$weekpart)

avg.steps.wday.wend<-group_by(imput.activity,interval,weekpart)%>%summarise(average.steps=mean(steps))
head(avg.steps.wday.wend)

### Plot weekday and weekend average steps per intervals

avg.steps.wday.wend.plot<-ggplot(avg.steps.wday.wend,aes(interval,average.steps))
avg.steps.wday.wend.plot<-avg.steps.wday.wend.plot+geom_line()+facet_grid(weekpart~.)
print(avg.steps.wday.wend.plot)