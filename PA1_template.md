---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

===================

```r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.0.5     ✓ dplyr   1.0.3
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```
## What is mean total number of steps taken per day?

```r
dailysteps <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
```
Calculating mean number of steps

```r
meanstepsdaily <- mean(dailysteps, na.rm = TRUE)
meanstepsdaily
```

```
## [1] 9354.23
```
Calculating median number of steps

```r
medstepsdaily <- median(dailysteps, na.rm = TRUE)
medstepsdaily
```

```
## [1] 10395
```

### Histogram for total number of steps taken each day ###

```r
hist(dailysteps, breaks = 10, xlab = "daily step count", main = "Histogram of daily steps", col = "darkorchid")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## What is the average daily activity pattern?
Calculate number of steps each day

```r
activity$date <- as.character(activity$date)
meanstepsinterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
```

### Time-series plot of mean steps per 5-minute interval ###

```r
plot(row.names(meanstepsinterval), meanstepsinterval, type = "l", xlab = "5 minute intervals (12AM - 11:59PM)", ylab="mean number of steps", main="Mean Steps per Interval", col="darkorchid", cex=1, cex.axis=0.75, cex.lab=0.75, cex.main=0.95, font.lab=2, font=2, lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
Which interval averages the most steps?

```r
maxstepsinterval <- which.max(meanstepsinterval)
print(maxstepsinterval)
```

```
## 835 
## 104
```

```r
names(maxstepsinterval)
```

```
## [1] "835"
```
## Imputing missing values

```r
sum(is.na(activity))
```

```
## [1] 2304
```
*Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

The mean number of steps for each interval is used to impute the missing values (NAs) in the data.

```r
imputed <- activity
meansteps <- mean(activity$steps, na.rm=TRUE)
imputed$steps[is.na(imputed$steps)] <- meansteps
```
Are there any NAs remaining in the dataset?

```r
sum(is.na(imputed))
```

```
## [1] 0
```
New table with NAs imputed from interval means

```r
dailystepsimputed <- tapply(imputed$steps, imputed$date, sum)
head(dailystepsimputed)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00
```

```r
summary(imputed)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 37.38                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0
```
### Histogram for total number of steps taken each day with NA values imputed from interval means ###

```r
hist(dailystepsimputed, breaks = 10, xlab = "daily step count", main = "Histogram of daily steps (imputed means)", col = "orchid2")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
Calculate new mean number of steps

```r
imputedmeanstepsdaily <- mean(dailystepsimputed)
imputedmeanstepsdaily
```

```
## [1] 10766.19
```
Calculate new median number of steps

```r
imputedmedstepsdaily <- median(dailystepsimputed)
imputedmedstepsdaily
```

```
## [1] 10766.19
```
Change in steps with imputed NA values?

```r
summary(dailysteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```


```r
summary(dailystepsimputed)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```
How did the imputed data differ from the original data?

```r
meandifference <- mean(dailystepsimputed) - mean(dailysteps)
meandifference
```

```
## [1] 1411.959
```


```r
mediandifference <- median(dailystepsimputed) - median(dailysteps)
mediandifference
```

```
## [1] 371.1887
```
## Are there differences in activity patterns between weekdays and weekends?


```r
imputed$daysweek <- weekdays(as.Date(imputed$date))
imputed$days[(imputed$daysweek == "Saturday" | imputed$daysweek == "Sunday")] <- "weekend"
imputed$days[!(imputed$daysweek == "Saturday" | imputed$daysweek == "Sunday")] <- "weekday"
head(imputed)
```

```
##     steps       date interval daysweek    days
## 1 37.3826 2012-10-01        0   Monday weekday
## 2 37.3826 2012-10-01        5   Monday weekday
## 3 37.3826 2012-10-01       10   Monday weekday
## 4 37.3826 2012-10-01       15   Monday weekday
## 5 37.3826 2012-10-01       20   Monday weekday
## 6 37.3826 2012-10-01       25   Monday weekday
```

```r
imputed$daysweek <- as.factor(imputed$daysweek)
imputed$days <- as.factor(imputed$days)
```

### Time-series panel plot for mean number of steps taken between weekdays & weekends ###

```r
library(lattice)
imputedweekdaymeans <- aggregate(steps ~ interval + days, data = imputed, mean)
xyplot(steps ~ interval | days, data = imputedweekdaymeans, type = "l", xlab = "interval", 
       ylab = "mean number of steps", main = "Mean number of step per 5 minute interval", col = "orchid", lwd = 2, layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

