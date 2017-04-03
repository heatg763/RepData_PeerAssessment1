# Peer Assignment 1
Heather Grebe

# Load Relevant Packages


```r
library(magrittr)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(xtable)
```

# Load and Preprocess Data


```r
unzip("./RepData_PeerAssessment1/activity.zip", exdir = "./RepData_PeerAssessment1")
filePath <- "./RepData_PeerAssessment1/activity.csv"
activity <- read.csv(filePath)
```


```r
activity$date <- as.Date(activity$date)
tbl_df(activity)
```

```
## # A tibble: 17,568 × 3
##    steps       date interval
##    <int>     <date>    <int>
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## # ... with 17,558 more rows
```

# What is the mean total number of steps taken per day?

Below is a representation of the total number of steps taken each day.

```r
tot <- activity %>% select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total = sum(steps, na.rm = TRUE))

g <- ggplot(as.data.frame(tot), aes(date, total)) + geom_histogram(stat = "identity")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
g + ggtitle("Total Steps per Day") + xlab("Date") + ylab("Total Steps") 
```

![plot of chunk sumperday](figure/sumperday-1.png)

From these counts we can compute the mean and median number of steps taken each day.

```r
activity %>% select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total = sum(steps, na.rm = TRUE), 
                  mean = mean(steps, na.rm = TRUE),
                  median = median(steps, na.rm = TRUE))
```

```
## # A tibble: 61 × 4
##          date total     mean median
##        <date> <int>    <dbl>  <dbl>
## 1  2012-10-01     0      NaN     NA
## 2  2012-10-02   126  0.43750      0
## 3  2012-10-03 11352 39.41667      0
## 4  2012-10-04 12116 42.06944      0
## 5  2012-10-05 13294 46.15972      0
## 6  2012-10-06 15420 53.54167      0
## 7  2012-10-07 11015 38.24653      0
## 8  2012-10-08     0      NaN     NA
## 9  2012-10-09 12811 44.48264      0
## 10 2012-10-10  9900 34.37500      0
## # ... with 51 more rows
```

# What is the average daily activity pattern?

Below is a time series plot showing the average number of steps during a given 5-minute interval averaged across all days.

```r
dact <- activity %>% group_by(interval) %>%
        summarize(aveact = mean(steps, na.rm = TRUE))
d <- ggplot(as.data.frame(dact), aes(interval, aveact))
d + geom_line() + ggtitle("Average Steps per Interval") + xlab("5-Minute Interval") + ylab("Average Steps Across All Days")
```

![plot of chunk timeseries](figure/timeseries-1.png)



```r
maxstep <- dact %>% arrange(desc(aveact))
maxint <- maxstep[[1]][1]
maxval <- round(maxstep[[2]][1],0)
```
From this time series plot we can easily see that the interval with the maximum number of steps on average is the interval beginning at 835 minutes. This interval contains, on average, 206 steps. 

# Imputing missing values


```r
totna <- sum(is.na(activity$steps))
perna <- round(totna/count(activity)*100, 1)
```

This dataset contains 2304 missing values which account for approximately 13.1% of the data.

Imputation is a common method for dealing with these NA's in a dataset. There are many imputation strategies but for this dataset I will be using the simple method of using the mean for the 5-minute interval since some days do not have a mean and most days have a median of 0 given the large number of 0 step entries. 


```r
newna <- activity %>% select(steps, date, interval) %>%
        group_by(interval) %>%
        mutate(aveint = mean(steps, na.rm = TRUE)) %>%
        filter(is.na(steps)) %>%
        mutate(steps = aveint)
newnotna <- activity %>% select(steps, date, interval) %>%
        group_by(interval) %>%
        mutate(aveint = mean(steps, na.rm = TRUE)) %>%
        filter(!is.na(steps))
finalnew <- rbind(newna, newnotna) %>% select(steps, date, interval) %>%
        group_by(date)
```

From this new dataset we can again look at the total number of steps each day and the mean and median of those days respectively.

```r
totnew <- finalnew %>% select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total = sum(steps, na.rm = TRUE))

g <- ggplot(as.data.frame(totnew), aes(date, total)) + geom_histogram(stat = "identity")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
g + ggtitle("Total Steps per Day Without NA's") + xlab("Date") + ylab("Total Steps") 
```

![plot of chunk sumperdaynew](figure/sumperdaynew-1.png)


```r
finalnew %>% select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total = sum(steps, na.rm = TRUE), 
                  mean = mean(steps, na.rm = TRUE),
                  median = median(steps, na.rm = TRUE))
```

```
## # A tibble: 61 × 4
##          date    total     mean   median
##        <date>    <dbl>    <dbl>    <dbl>
## 1  2012-10-01 10766.19 37.38260 34.11321
## 2  2012-10-02   126.00  0.43750  0.00000
## 3  2012-10-03 11352.00 39.41667  0.00000
## 4  2012-10-04 12116.00 42.06944  0.00000
## 5  2012-10-05 13294.00 46.15972  0.00000
## 6  2012-10-06 15420.00 53.54167  0.00000
## 7  2012-10-07 11015.00 38.24653  0.00000
## 8  2012-10-08 10766.19 37.38260 34.11321
## 9  2012-10-09 12811.00 44.48264  0.00000
## 10 2012-10-10  9900.00 34.37500  0.00000
## # ... with 51 more rows
```

Using this method of imputation, we being to see median values emerge that are now equal to zero. This is due to values being put in place for each interval of days that were previously populated only with NA's. These medians are not representative of the actual data and suggest that perhaps a different method of impuation would be more appropriate. This impuation also has the effect of increasing the total daily number of steps on any day that previously has missing values populated. 

# Are there differences in activity patterns between weekdays and weekends?

First we must differentiate between weekdays and weekends within the dataset by creating a new factor variable. Then it is necessary to find the average number of steps per interval across both weekdays and weekend days.  

```r
finalnewday <- finalnew %>% select(date, steps, interval) %>%
        mutate(weekday = weekdays(date)) %>%
        filter(!(weekday %in% c("Saturday", "Sunday"))) %>%
        mutate(endorday = "weekday")

finalnewend <- finalnew %>% select(date, steps, interval) %>%
        mutate(weekday = weekdays(date)) %>%
        filter(weekday %in% c("Saturday", "Sunday")) %>%
        mutate(endorday = "weekend")

dayorend <- rbind(finalnewday, finalnewend) %>% 
        select(steps, date, interval, endorday) %>%
        group_by(interval, endorday) %>%
        summarize(aveact = mean(steps))
```

Finally, with this information at hand we can create a time series plot to visualize the difference in the average number of steps in a particular interval segmented by weekdays and weekend days.

```r
doe <- ggplot(as.data.frame(dayorend), aes(interval, aveact))
doe + geom_line() + facet_grid(endorday~.) + ggtitle("Average Steps per Interval") + 
        xlab("5-Minute Interval") + ylab("Average Steps Across Selected Days")
```

![plot of chunk dayorendhist](figure/dayorendhist-1.png)


















