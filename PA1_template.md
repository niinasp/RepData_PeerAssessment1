
## Loading and preprocessing the data

```r
activity <- read.csv("/Users/niinasiitonen/Dropbox/courses/reproducibleResearch2014/peerAssessment/activity.csv", 
    header = TRUE)
```


## What is mean total number of steps taken per day?

```r
a <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
hist(a$x, xlab = "Number of steps", main = "Total number of steps per day", 
    ylab = "Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(a$x, na.rm = TRUE)
```

```
## [1] 9354
```

```r
median(a$x, na.rm = TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r

# transform interval into factor format
activity$int <- factor(activity$interval)
# calculate average no of steps per interval across all days
b <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
plot(b$Group.1, b$x, type = "l", main = "Mean steps per interval", xlab = "interval", 
    ylab = "Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# Find interval with max value of average steps
b[which.max(b$x), 1]  # 835
```

```
## [1] 835
```



## Imputing missing values

```r
table(is.na(activity$steps))  # 2304 NAs
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
# filling in all of the missing values b$x is the average steps per interval
b$interval <- b$Group.1
c <- merge(b, activity, by = "interval", all = TRUE)
c$steps2 <- ifelse(is.na(c$steps), c$x, c$steps)

d <- aggregate(c$steps2, by = list(c$date), FUN = sum, na.rm = TRUE)
hist(d$x, xlab = "Number of steps", main = "Total number of steps per day (imputed data)", 
    ylab = "Frequency")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(d$x, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(d$x, na.rm = TRUE)
```

```
## [1] 10766
```



## Are there differences in activity patterns between weekdays and weekends?


```r
head(c)
```

```
##   interval Group.1     x steps       date int steps2
## 1        0       0 1.717    NA 2012-10-01   0  1.717
## 2        0       0 1.717     0 2012-11-23   0  0.000
## 3        0       0 1.717     0 2012-10-28   0  0.000
## 4        0       0 1.717     0 2012-11-06   0  0.000
## 5        0       0 1.717     0 2012-11-24   0  0.000
## 6        0       0 1.717     0 2012-11-15   0  0.000
```

```r
# date to factor format
c$date2 <- as.Date(c$date)
# weekday
c$wday <- weekdays(c$date2)
c$arki <- ifelse(c(c$wday == "Lauantai" | c$wday == "Sunnuntai"), 0, 1)
# subset to 2 datasets
week <- c[c$arki == 1, ]
weekend <- c[c$arki == 0, ]
head(week)
```

```
##   interval Group.1     x steps       date int steps2      date2
## 1        0       0 1.717    NA 2012-10-01   0  1.717 2012-10-01
## 2        0       0 1.717     0 2012-11-23   0  0.000 2012-11-23
## 4        0       0 1.717     0 2012-11-06   0  0.000 2012-11-06
## 6        0       0 1.717     0 2012-11-15   0  0.000 2012-11-15
## 8        0       0 1.717     0 2012-11-16   0  0.000 2012-11-16
## 9        0       0 1.717     0 2012-11-07   0  0.000 2012-11-07
##          wday arki
## 1   Maanantai    1
## 2   Perjantai    1
## 4     Tiistai    1
## 6     Torstai    1
## 8   Perjantai    1
## 9 Keskiviikko    1
```

```r
# plots
par(mfrow = c(2, 1))
# steps per interval weekdays
week1 <- aggregate(week$steps2, by = list(week$interval), FUN = sum, na.rm = TRUE)
# head(week1)

plot(week1$Group.1, week1$x, type = "l", main = "Mean steps per interval (Mon-Fri)", 
    xlab = "interval", ylab = "Steps")

weekend1 <- aggregate(weekend$steps2, by = list(weekend$interval), FUN = sum, 
    na.rm = TRUE)

plot(weekend1$Group.1, weekend1$x, type = "l", main = "Mean steps per interval (Sat-Sun)", 
    xlab = "interval", ylab = "Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


