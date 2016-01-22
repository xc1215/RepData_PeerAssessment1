---
title: Reproducible Research Course Project 1 - Analysis of personal movement data
output: html_document
---

##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Loading and preprocessing the data

1.Load the data (i.e. read.csv())


```r
x<-read.csv("activity.csv",header=TRUE,sep=",")
```


2.Process/transform the data (if necessary) into a format suitable for your analysis


```r
x$date <- as.Date(x$date,"%Y-%m-%d")
```

##What is mean total number of steps taken per day?

For this part of the assignment, missing value (NA in steps) is ignored.

1.Calculate the total number of steps taken per day


```r
x_small <- na.omit(x)

library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.2.3
```

```r
Steps_Per_Day <- ddply(x_small[,1:2],"date",numcolwise(sum))
head(Steps_Per_Day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2.Make a histogram of the total number of steps taken each day


```r
hist(Steps_Per_Day$steps,main="Histogram of Total Steps Each Day",xlab="Total Steps Each Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

3.Calculate and report the mean and median of the total number of steps taken per day

### mean:

```r
mean(Steps_Per_Day$steps)
```

```
## [1] 10766.19
```
### median:

```r
median(Steps_Per_Day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
Steps_Per_Interval <- ddply(x_small,"interval",numcolwise(mean))
plot(Steps_Per_Interval$interval,Steps_Per_Interval$steps,type="l",main="Average Steps on 5-minute interval",xlab="interval",ylab="Average Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Steps_Per_Interval_sorted <- Steps_Per_Interval[with(Steps_Per_Interval,order(-steps)),]
Steps_Per_Interval_sorted[1,1]
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(x))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Answer: We will use average steps on that particular interval to fill in missing values in the dataset

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
mergex <- merge(x,Steps_Per_Interval,by.x="interval",by.y="interval",all=TRUE)
mergex$New_Steps[which(is.na(mergex$steps.x))] <- mergex$steps.y[which(is.na(mergex$steps.x))]
mergex$New_Steps[which(!is.na(mergex$steps.x))] <- mergex$steps.x[which(!is.na(mergex$steps.x))]
x_new <- mergex[,c("interval","date","New_Steps")]
head(x_new)
```

```
##   interval       date New_Steps
## 1        0 2012-10-01  1.716981
## 2        0 2012-11-23  0.000000
## 3        0 2012-10-28  0.000000
## 4        0 2012-11-06  0.000000
## 5        0 2012-11-24  0.000000
## 6        0 2012-11-15  0.000000
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Steps_Per_Day_New <- ddply(x_new[,c("date","New_Steps")],"date",numcolwise(sum))
head(Steps_Per_Day_New)
```

```
##         date New_Steps
## 1 2012-10-01  10766.19
## 2 2012-10-02    126.00
## 3 2012-10-03  11352.00
## 4 2012-10-04  12116.00
## 5 2012-10-05  13294.00
## 6 2012-10-06  15420.00
```

```r
hist(Steps_Per_Day_New$New_Steps,main="Histogram of Total Steps Each Day (NA filled)",xlab="Total Steps Each Day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

### mean:

```r
mean(Steps_Per_Day_New$New_Steps)
```

```
## [1] 10766.19
```
### median:

```r
median(Steps_Per_Day_New$New_Steps)
```

```
## [1] 10766.19
```

Mean and Median are changed because of additional NA values filled in.  If we choose a different method, we could get different result.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
x_new$weekday <- weekdays(x_new$date)
x_new$weekday[x_new$weekday == "Saturday" | x_new$weekday == "Sunday"] <- "weekend"
x_new$weekday[!(x_new$weekday == "weekend")] <- "weekday"
x_new$weekday <- as.factor(x_new$weekday)
head(x_new)
```

```
##   interval       date New_Steps weekday
## 1        0 2012-10-01  1.716981 weekday
## 2        0 2012-11-23  0.000000 weekday
## 3        0 2012-10-28  0.000000 weekend
## 4        0 2012-11-06  0.000000 weekday
## 5        0 2012-11-24  0.000000 weekend
## 6        0 2012-11-15  0.000000 weekday
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
Steps_Per_Interval_new <- ddply(x_new,c("weekday","interval"),numcolwise(mean))
Time_plot <- qplot(interval,New_Steps,data=Steps_Per_Interval_new,facets = weekday~., geom = "line")
Time_plot+labs(title="Steps per interval: weekend vs. weekday")+labs(x="interval", y="steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
