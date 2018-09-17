    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Loading and preprocessing the data
----------------------------------

### 1. Load the data

    setwd("~/Desktop/Coursera/5- Reproducible Research")

    activity<- read.csv("./data/activity.csv")
    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    summary(activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

### 2. Process/transform the data (if necessary) into a format suitable for your analysis

    activity$date<- as.Date(activity$date, format = "%Y-%m-%d")
    activity2<- activity[!is.na(activity$steps),]
    head(activity2)

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25

    summary(activity2)

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-29   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0

What is mean total number of steps taken per day?
-------------------------------------------------

Calculate the total number of steps taken per day:

    activity3<- group_by(activity2, date)
    act3<-data.frame(summarise(activity3, sum(steps)))
    head(act3)

    ##         date sum.steps.
    ## 1 2012-10-02        126
    ## 2 2012-10-03      11352
    ## 3 2012-10-04      12116
    ## 4 2012-10-05      13294
    ## 5 2012-10-06      15420
    ## 6 2012-10-07      11015

### 1. Make a histogram of the total number of steps taken each day

    ggplot(act3, (aes(x = date, y = sum.steps.))) + 
            geom_bar(stat="identity") +
            labs(x = "Date", 
                 y = "Number of steps",
                 title = "Total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

### 2. Calculate and report the mean and median total number of steps taken per day

    actMean<- mean(act3$sum.steps.)
    actMean

    ## [1] 10766.19

    actMedian<- median(act3$sum.steps.)
    actMedian

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    activity4<- data.frame(activity2$interval, activity2$steps)
    interval<- group_by(activity4, activity2.interval)
    act_interval<- data.frame(summarise(interval, mean(activity2.steps)))
    head(act_interval)

    ##   activity2.interval mean.activity2.steps.
    ## 1                  0             1.7169811
    ## 2                  5             0.3396226
    ## 3                 10             0.1320755
    ## 4                 15             0.1509434
    ## 5                 20             0.0754717
    ## 6                 25             2.0943396

    plot(act_interval$activity2.interval, act_interval$mean.activity2.steps, type ="l", main= "Time series plot", xlab= "Intervals", ylab= "Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    max_interval<- which.max(act_interval$mean.activity2.steps)
    max_interval

    ## [1] 104

    act_interval[104,1]

    ## [1] 835

On average across all the days in the dataset, the maximum number of
steps is in the 8:35 5-minute interval, with a mean of 206.17 steps.

Imputing missing values
-----------------------

### 1. Calculate and report the total number of missing values in the dataset

    sum(is.na(activity))

    ## [1] 2304

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We are going to use the mean for that 5-minute interval.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in

    fillNA<- activity
    for (i in 1:nrow(fillNA)){
            if (is.na(fillNA$steps[i])){
                    fillNA$steps [i]<- act_interval[which(fillNA$interval [i] == act_interval$activity2.interval), ]$mean.activity2.steps.
            }
    }

    sum(is.na(fillNA))

    ## [1] 0

    head(fillNA)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Total number of steps taken each day:

    activity_fill<- group_by(fillNA, date)
    act_fill<-data.frame(summarise(activity_fill, sum(steps)))
    head(act_fill)

    ##         date sum.steps.
    ## 1 2012-10-01   10766.19
    ## 2 2012-10-02     126.00
    ## 3 2012-10-03   11352.00
    ## 4 2012-10-04   12116.00
    ## 5 2012-10-05   13294.00
    ## 6 2012-10-06   15420.00

Histogram:

    ggplot(act_fill, (aes(x = date, y = sum.steps.))) + 
            geom_bar(stat="identity") +
            labs(x = "Date", 
                 y = "Number of steps",
                 title = "New total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-16-1.png)

Calculate and report the mean and median total number of steps taken per
day:

    actnewMean<- mean(act_fill$sum.steps.)
    actnewMean

    ## [1] 10766.19

    actnewMedian<- median(act_fill$sum.steps.)
    actnewMedian

    ## [1] 10766.19

These values do not differ from the estimates from the first part of the
assignment (data without NA), so there is no impact of imputing missing
data on the estimates of the total daily number of steps.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    fillNA$days<- factor(format(as.Date(fillNA$date),"%A"))
    levels(fillNA$days)

    ## [1] "domingo"   "jueves"    "lunes"     "martes"    "miércoles" "sábado"   
    ## [7] "viernes"

    levels(fillNA$days)<- list(weekday = c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            weekend = c("sábado", "domingo"))
    levels(fillNA$days)

    ## [1] "weekday" "weekend"

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

    avg_step_days<- aggregate(steps ~ interval + days, data=fillNA, FUN = "mean")
    head(avg_step_days)

    ##   interval    days      steps
    ## 1        0 weekday 2.25115304
    ## 2        5 weekday 0.44528302
    ## 3       10 weekday 0.17316562
    ## 4       15 weekday 0.19790356
    ## 5       20 weekday 0.09895178
    ## 6       25 weekday 1.59035639

    xyplot(avg_step_days$steps ~ avg_step_days$interval | avg_step_days$days, 
           layout = c(1, 2), type = "l", 
           xlab = "5-minute interval", ylab = "Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-22-1.png)
