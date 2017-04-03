############### COURSE PROJECT 1 ###############
# Load relevant packages
library(magrittr)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(xtable)

# Read in data
unzip("./RepData_PeerAssessment1/activity.zip", exdir = "./RepData_PeerAssessment1")
filePath <- "./RepData_PeerAssessment1/activity.csv"
activity <- read.csv(filePath)
str(activity)

# Convert date column to date format
activity$date <- as.Date(activity$date)
str(activity)
tbl_df(activity)

# What is the mean and median total number of steps take per day? Compute total 
# count per day first and plot as a histogram.
tot <- activity %>% select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total = sum(steps, rm.na = TRUE))

g <- ggplot(as.data.frame(tot), aes(date, total)) + geom_histogram(stat = "identity")
g + ggtitle("Total Steps per Day") + xlab("Date") + ylab("Total Steps") 

activity %>% select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total = sum(steps, rm.na = TRUE), mean = mean(steps, rm.na = TRUE),
                  median = median(steps, na.rm = TRUE))

# What is the average daily activity pattern? Create a time series plot
# that shows the average number of steps taken during that 5-minute interval 
# averaged across all days. Which interval contains the max number of steps?
dact <- activity %>% group_by(interval) %>%
        summarize(aveact = mean(steps, na.rm = TRUE))
d <- ggplot(as.data.frame(dact), aes(interval, aveact))
d + geom_line() + ggtitle("Average Steps per Interval") + 
        xlab("5-Minute Interval") + ylab("Average Steps Across All Days")

maxstep <- dact %>% arrange(desc(aveact))
maxstep

# Imputing missing values. Calculate and report the total number to missing values
# then determine a method of imputing those missing values. Afterwards, create
# a new dataset with these missing values filled in by your chosen method. Finally
# make a historam of the tota numnber of sgeps taken each day and calculate and 
# report the mean and median number of steps taken each day.

totna <- sum(is.na(activity$steps))
perna <- round(totna/count(activity)*100, 1)

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
totnew <- finalnew %>% select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total = sum(steps, na.rm = TRUE))

g <- ggplot(as.data.frame(totnew), aes(date, total)) + 
        geom_histogram(stat = "identity")
g + ggtitle("Total Steps per Day Without NA's") + xlab("Date") +
        ylab("Total Steps") 

finalnew %>% select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total = sum(steps, na.rm = TRUE), 
                  mean = mean(steps, na.rm = TRUE),
                  median = median(steps, na.rm = TRUE))

# Are there differences in activity patterns between weekdays and weekends?

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

doe <- ggplot(as.data.frame(dayorend), aes(interval, aveact))
doe + geom_line() + facet_grid(endorday~.) + ggtitle("Average Steps per Interval") + 
        xlab("5-Minute Interval") + ylab("Average Steps Across Selected Days")

