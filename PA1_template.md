title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
### Load the data

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipFile <- "./activity.zip"
csvFile <- "./activity.csv"
if(!file.exists(csvFile)){
  download.file(url,zipFile)
  unzip(zipFile, overwrite = TRUE, exdir = ".")
}
fullData <- read.csv(csvFile,header = TRUE, na.strings = "NA" )
```
### Process the data
#### Remove NAN

```r
validData <- fullData[!is.na(fullData$steps),]
```
#### Convert date to Date class

```r
validData$date <- as.Date(validData$date,"%Y-%m-%d")
```


## What is mean total number of steps taken per day?
### Calculate the total number of steps taken per day
Plotting the total number of steps per day:

```r
sumData <- aggregate(data=validData,steps~date,sum)
names(sumData) <- c("Date","TotalSteps")
plot(sumData$Date, sumData$TotalSteps, type = "l", lty = 1,lwd=3,
     xlab="Date", ylab="Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Create a barplot of total nr of steps per day

```r
barplot(TotalSteps~Date, data=sumData,
        main="Barplot of total nr of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Create a histogram of total nr of steps per day

```r
hist(sumData$TotalSteps,xlab = "Total nr of steps each day",
     main="Histogram of total nr of steps each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Calculate and report the mean and median of the total number of steps per day

```r
meanSt <- mean(sumData$TotalSteps)
medianSt <- median(sumData$TotalSteps)
```
The mean is 10766.2 and the median is 10765.

## What is the average daily activity pattern?
Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
averaged across all days (y-axis)

```r
data_Interval <- sapply(split(validData$steps,validData$interval),mean)
plot(names(data_Interval),data_Interval,type = "l",lwd=3,xlab="5min interval",ylab="ave steps",
     main="Ave steps per 5 min interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
whichInt <- data_Interval[data_Interval == max(data_Interval)]
```
It is 835.


## Imputing missing values
### Calculate and report the total number of missing values in the dataset


```r
misValues <- dim(fullData[is.na(fullData$steps),])[1]
```

Total number of missing values is 2304.

### Fill in the missing values from 5min interval 
Based on the mean for the specific 5min interval (rounded).


```r
library(dplyr)
library(tidyr)
data_Interval_mean <- cbind(interval=as.numeric(names(data_Interval)),average=data_Interval)
merged <- merge(fullData,data_Interval_mean,by = c("interval","interval"))
replacedData <- merged %>% mutate(steps = coalesce(steps,round(average, digits = 0)))
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newDataset <- replacedData[,1:3]
```

### Make a histogram of the total number of steps taken each day 

```r
sumDataNew <- aggregate(data=newDataset,steps~date,sum)
names(sumDataNew) <- c("Date","TotalSteps")
par(mfrow=c(1,2))
hist(sumDataNew$TotalSteps,xlab = "Total nr of steps each day",
     main="After replacing NA values",ylim=c(0,40))
hist(sumData$TotalSteps,xlab = "Total nr of steps each day",
     main="Without NA values",ylim=c(0,40))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

### Calculate and report the mean and median total number of steps taken per day. 

```r
meanStNew <- mean(sumDataNew$TotalSteps)
medianStNew <- median(sumDataNew$TotalSteps)
```
The new mean is 10765.6 (old mean was 10766.2) and the new median is 10762 (old median was 10765). The effect is very small, but it is towards reducing the mean and median values.

## Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
replacedData$date <- as.Date(replacedData$date,"%Y-%m-%d")
weekend <- c("Saturday","Sunday")
replacedData$dayType <- factor((weekdays(replacedData$date) %in% weekend),
                         levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))
replacedData <- replacedData %>% arrange(date,interval)
```

### Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
data_weekend <- replacedData[replacedData$dayType=="weekend",]
data_weekday <- replacedData[replacedData$dayType=="weekday",]
data_weekend_ave <- sapply(split(data_weekend$steps,data_weekend$interval),mean)
data_weekday_ave <- sapply(split(data_weekday$steps,data_weekday$interval),mean)
par(mfrow = c(2, 1))
plot(names(data_weekday_ave),data_weekday_ave,type = "l",lwd=3,xlab="5min interval",
     ylab="ave steps", col = "blue", ylim = c(0,250), main="Ave steps per 5 min interval on weekday")
plot(names(data_weekend_ave),data_weekend_ave,type = "l",lwd=3,xlab="5min interval",
     ylab="ave steps", col = "blue", ylim = c(0,250), main="Ave steps per 5 min interval on weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

- There is more activity within the day and in the late hours during the weekends. 
- There is more activity in the mornings during weekdays.
