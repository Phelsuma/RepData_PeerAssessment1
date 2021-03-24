---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
 


```r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.0.5     ✓ dplyr   1.0.3
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such
## file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
head(activity)
```

```
## Error in head(activity): object 'activity' not found
```

```r
summary(activity)
```

```
## Error in summary(activity): object 'activity' not found
```
## What is mean total number of steps taken per day?

```r
dailysteps <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
```

```
## Error in tapply(activity$steps, activity$date, sum, na.rm = TRUE): object 'activity' not found
```
Calculating mean number of steps

```r
meanstepsdaily <- mean(dailysteps, na.rm = TRUE)
```

```
## Error in mean(dailysteps, na.rm = TRUE): object 'dailysteps' not found
```

```r
meanstepsdaily
```

```
## Error in eval(expr, envir, enclos): object 'meanstepsdaily' not found
```
Calculating median number of steps

```r
medstepsdaily <- median(dailysteps, na.rm = TRUE)
```

```
## Error in median(dailysteps, na.rm = TRUE): object 'dailysteps' not found
```

```r
medstepsdaily
```

```
## Error in eval(expr, envir, enclos): object 'medstepsdaily' not found
```

### Histogram for total number of steps taken each day ###

```r
hist(dailysteps, breaks = 10, xlab = "daily step count", main = "Histogram of daily steps", col = "darkorchid")
```

```
## Error in hist(dailysteps, breaks = 10, xlab = "daily step count", main = "Histogram of daily steps", : object 'dailysteps' not found
```

## What is the average daily activity pattern?
Calculate number of steps each day

```r
activity$date <- as.character(activity$date)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
meanstepsinterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
```

```
## Error in tapply(activity$steps, activity$interval, mean, na.rm = TRUE): object 'activity' not found
```

### Time-series plot of mean steps per 5-minute interval ###

```r
plot(row.names(meanstepsinterval), meanstepsinterval, type = "l", xlab = "5 minute intervals (12AM - 11:59PM)", ylab="mean number of steps", main="Mean steps per interval", col="darkorchid", cex=1, cex.axis=0.75, cex.lab=0.75, cex.main=0.95, font.lab=2, font=2, lwd = 2)
```

```
## Error in row.names(meanstepsinterval): object 'meanstepsinterval' not found
```
Which interval averages the most steps?

```r
maxstepsinterval <- which.max(meanstepsinterval)
```

```
## Error in which.max(meanstepsinterval): object 'meanstepsinterval' not found
```

```r
print(maxstepsinterval)
```

```
## Error in print(maxstepsinterval): object 'maxstepsinterval' not found
```

```r
names(maxstepsinterval)
```

```
## Error in eval(expr, envir, enclos): object 'maxstepsinterval' not found
```
## Imputing missing values

```r
sum(is.na(activity))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```
*Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

The mean number of steps for each interval is used to impute the missing values (NAs) in the data.

```r
imputed <- activity
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
meansteps <- mean(activity$steps, na.rm=TRUE)
```

```
## Error in mean(activity$steps, na.rm = TRUE): object 'activity' not found
```

```r
imputed$steps[is.na(imputed$steps)] <- meansteps
```

```
## Error in eval(expr, envir, enclos): object 'meansteps' not found
```
Are there any NAs remaining in the dataset?

```r
sum(is.na(imputed))
```

```
## Error in eval(expr, envir, enclos): object 'imputed' not found
```
New table with NAs imputed from interval means

```r
dailystepsimputed <- tapply(imputed$steps, imputed$date, sum)
```

```
## Error in tapply(imputed$steps, imputed$date, sum): object 'imputed' not found
```

```r
head(dailystepsimputed)
```

```
## Error in head(dailystepsimputed): object 'dailystepsimputed' not found
```

```r
summary(imputed)
```

```
## Error in summary(imputed): object 'imputed' not found
```
### Histogram for total number of steps taken each day with NA values imputed from interval means ###

```r
hist(dailystepsimputed, breaks = 10, xlab = "daily step count", main = "Histogram of daily steps (imputed means)", col = "orchid2")
```

```
## Error in hist(dailystepsimputed, breaks = 10, xlab = "daily step count", : object 'dailystepsimputed' not found
```
Calculate new mean number of steps

```r
imputedmeanstepsdaily <- mean(dailystepsimputed)
```

```
## Error in mean(dailystepsimputed): object 'dailystepsimputed' not found
```

```r
imputedmeanstepsdaily
```

```
## Error in eval(expr, envir, enclos): object 'imputedmeanstepsdaily' not found
```
Calculate new median number of steps

```r
imputedmedstepsdaily <- median(dailystepsimputed)
```

```
## Error in median(dailystepsimputed): object 'dailystepsimputed' not found
```

```r
imputedmedstepsdaily
```

```
## Error in eval(expr, envir, enclos): object 'imputedmedstepsdaily' not found
```
Change in steps with imputed NA values?

```r
summary(dailysteps)
```

```
## Error in summary(dailysteps): object 'dailysteps' not found
```


```r
summary(dailystepsimputed)
```

```
## Error in summary(dailystepsimputed): object 'dailystepsimputed' not found
```
How did the imputed data differ from the original data?

```r
meandifference <- mean(dailystepsimputed) - mean(dailysteps)
```

```
## Error in mean(dailystepsimputed): object 'dailystepsimputed' not found
```

```r
meandifference
```

```
## Error in eval(expr, envir, enclos): object 'meandifference' not found
```


```r
mediandifference <- median(dailystepsimputed) - median(dailysteps)
```

```
## Error in median(dailystepsimputed): object 'dailystepsimputed' not found
```

```r
mediandifference
```

```
## Error in eval(expr, envir, enclos): object 'mediandifference' not found
```
## Are there differences in activity patterns between weekdays and weekends?


```r
imputed$daysweek <- weekdays(as.Date(imputed$date))
```

```
## Error in as.Date(imputed$date): object 'imputed' not found
```

```r
imputed$days[(imputed$daysweek == "Saturday" | imputed$daysweek == "Sunday")] <- "weekend"
```

```
## Error in imputed$days[(imputed$daysweek == "Saturday" | imputed$daysweek == : object 'imputed' not found
```

```r
imputed$days[!(imputed$daysweek == "Saturday" | imputed$daysweek == "Sunday")] <- "weekday"
```

```
## Error in imputed$days[!(imputed$daysweek == "Saturday" | imputed$daysweek == : object 'imputed' not found
```

```r
head(imputed)
```

```
## Error in head(imputed): object 'imputed' not found
```

```r
imputed$daysweek <- as.factor(imputed$daysweek)
```

```
## Error in is.factor(x): object 'imputed' not found
```

```r
imputed$days <- as.factor(imputed$days)
```

```
## Error in is.factor(x): object 'imputed' not found
```

### Time-series panel plot for mean number of steps taken between weekdays & weekends ###

```r
library(lattice)
imputedweekdaymeans <- aggregate(steps ~ interval + days, data = imputed, mean)
```

```
## Error in eval(m$data, parent.frame()): object 'imputed' not found
```

```r
xyplot(steps ~ interval | days, data = imputedweekdaymeans, type = "l", xlab = "interval", 
       ylab = "mean number of steps", main = "Mean number of step per 5 minute interval", col = "orchid", lwd = 2, layout = c(1,2))
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'imputedweekdaymeans' not found
```

