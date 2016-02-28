# Reproducible Research: Peer Assessment 1

```r
knitr::opts_chunk$set(echo=TRUE,warning=FALSE, message=FALSE)
```
## Loading library and preprocessing the data


```r
library(plyr)
library(dplyr)
library(ggplot2)
activity<-read.csv("activity.csv",header = TRUE)
```


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Convert the date variable from Factor to Date variable

```r
activity$date<-as.Date(activity$date)
```


## What is mean total number of steps taken per day?
Compute the data needed for the plot

```r
tot.steps.day<-group_by(activity,date)%>%summarise(total.steps=sum(steps))
head(tot.steps.day)
```

```
## Source: local data frame [6 x 2]
## 
##         date total.steps
##       (date)       (int)
## 1 2012-10-01          NA
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```
Plot the data on histogram Q1.1

```r
hist(tot.steps.day$total.steps,
     breaks=50,
     main="Total Number of steps per day",
     xlab="Total steps per day",
     ylab="count",
     col="blue")
```

![](https://github.com/FilomenaC/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/Histogram.Total.Number.of.steps.per.day-1.png)
\
Calculate the mean and median of total number of steps per day, Q1.2

```r
steps.avg.day<-mean(tot.steps.day$total.steps,na.rm = TRUE)
steps.median.day<-median(tot.steps.day$total.steps,na.rm = TRUE)
```

```r
steps.avg.day
```

```
## [1] 10766.19
```

```r
steps.median.day
```

```
## [1] 10765
```
## What is the average daily activity pattern?
Compute the data needed for the plot

```r
avg.steps.interval<-group_by(activity,interval)%>%summarise(average.steps=mean(steps,na.rm=TRUE))
```
Plot the requested data, Q2.1

```r
plot(avg.steps.interval$interval,avg.steps.interval$average.steps,
     type="l",
     xlab="Intervals",
     ylab="Average number of steps",
     main="Average Number of Steps per Intervals",
     col="dark green")
```

![](PA1_template_files/figure-html/Time series plot Average Number of steps per interval-1.png)\
Interval with max number of steps, Q2.2

```r
max.steps.int<-which.max(avg.steps.interval$average.steps)
interval.max.steps<-avg.steps.interval[max.steps.int,1]
```


```r
interval.max.steps
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```
8:35 is the 5-minute interval on average with the maximum number of steps

## Imputing missing values
Total number of missing values in the dataset

```r
table(complete.cases(activity))
```

```
## 
## FALSE  TRUE 
##  2304 15264
```
Create a new data frame and impute missing values using average steps per 5 min-interval 

```r
imput.activity<-activity
imput.activity$steps<-ifelse(is.na(imput.activity$steps),
                             avg.steps.interval$average.steps[match(imput.activity$interval,
                                avg.steps.interval$interval)],
                                imput.activity$steps)
```

```r
table(complete.cases(imput.activity))
```

```
## 
##  TRUE 
## 17568
```
Compute total number of steps per day with the imputed missing values

```r
imput.tot.steps.day<-group_by(imput.activity,date)%>%summarise(total.steps=sum(steps))
```


```r
head(imput.tot.steps.day)
```

```
## Source: local data frame [6 x 2]
## 
##         date total.steps
##       (date)       (dbl)
## 1 2012-10-01    10766.19
## 2 2012-10-02      126.00
## 3 2012-10-03    11352.00
## 4 2012-10-04    12116.00
## 5 2012-10-05    13294.00
## 6 2012-10-06    15420.00
```
Plot the data on histogram, Q3.4

```r
hist(imput.tot.steps.day$total.steps,
     breaks=50,
     main="Total number of steps per day (NAs imputed)",
     xlab="Total steps per day",
     ylab="count",
     col="red")
```

![](PA1_template_files/figure-html/Total number of steps per day (NAs imputed)-1.png)\
Report Mean and Median with the imputed missing values

```r
imput.steps.avg.day<-mean(imput.tot.steps.day$total.steps)
imput.steps.median.day<-median(imput.tot.steps.day$total.steps)
```


```r
imput.steps.avg.day
```

```
## [1] 10766.19
```


```r
imput.steps.median.day
```

```
## [1] 10766.19
```
The average and median steps by day are not affected by the adopted imputation of the NA values approach.
The distribution of the total nr of steps per day becomes a closer approximation of a Gaussian distribution.

## Are there differences in activity patterns between weekdays and weekends?
Identify the day of the week variable from the imput.activity data frame and then create a weekday or weekend variable

```r
imput.activity$day<-weekdays(imput.activity$date)
w.day<-c("Monday","Tuesdya","Wednesday","Thursday","Friday")
w.end<-c("Saturday","Sunday")
imput.activity$weekpart<-ifelse(imput.activity$day %in% w.day,"weekday","weekend")
```


```r
head(imput.activity)
```

```
##       steps       date interval    day weekpart
## 1 1.7169811 2012-10-01        0 Monday  weekday
## 2 0.3396226 2012-10-01        5 Monday  weekday
## 3 0.1320755 2012-10-01       10 Monday  weekday
## 4 0.1509434 2012-10-01       15 Monday  weekday
## 5 0.0754717 2012-10-01       20 Monday  weekday
## 6 2.0943396 2012-10-01       25 Monday  weekday
```

```r
table(imput.activity$weekpart)
```

```
## 
## weekday weekend 
##   10368    7200
```
Group_by data by interval and type of weekday/end 

```r
avg.steps.wday.wend<-group_by(imput.activity,interval,weekpart)%>%summarise(average.steps=mean(steps))
```


```r
head(avg.steps.wday.wend)
```

```
## Source: local data frame [6 x 3]
## Groups: interval [3]
## 
##   interval weekpart average.steps
##      (int)    (chr)         (dbl)
## 1        0  weekday    2.81394130
## 2        0  weekend    0.13735849
## 3        5  weekday    0.55660377
## 4        5  weekend    0.02716981
## 5       10  weekday    0.21645702
## 6       10  weekend    0.01056604
```

Plotting the  weekday and weekend average steps per intervals

```r
avg.steps.wday.wend.plot<-ggplot(avg.steps.wday.wend,aes(interval,average.steps))
avg.steps.wday.wend.plot<-avg.steps.wday.wend.plot+geom_line()+facet_grid(weekpart~.)
```
Average activity steps are higher on weekday days than weekend days

```r
print(avg.steps.wday.wend.plot)
```

![](PA1_template_files/figure-html/Plot average number of steps weekday and weekend-1.png)\
