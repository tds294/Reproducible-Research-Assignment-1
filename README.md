---
title: "Analyzing FitBit Data"
author: "Tejaswi Durga Sudhakar"
date: "April 17, 2016"
---

##Synopsis
The purpose of this project was to practice:

* loading and preprocessing data
* imputing missing values
* interpreting data to answer research questions

## Data
The data for this assignment was downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

```{r init, include=FALSE, results='hide', echo=FALSE}
library(sqldf)
library(tcltk)
library(lattice)
library(ggplot2)
```

## Loading and preprocessing the data

Download, unzip and load data into data frame `data`. 
```{r loadingData, results='hide'}
Sys.setlocale(category = "LC_TIME", locale = "C")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileName <- "activity.zip"
imagesFolder <- "figure"

if(!file.exists(fileName)){
  download.file(fileUrl,fileName, mode = "wb", method="curl")
}

unzip(fileName)

datasetFileName <- "activity.csv"
dataset <- read.table(datasetFileName, sep = ",", header = TRUE, stringsAsFactors= F)
dataset$date <- as.Date(dataset$date, format = "%Y-%m-%d")
dataset$steps <- as.numeric(dataset$steps)
dataset$interval <- as.numeric(dataset$interval)
```



## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day
```{r} 
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
```

The `mean` is `r rmean` and the `median` is `r rmedian`.

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
5. 
```{r}
steps_by_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```
Mean of the total number of steps taken per day

```{r meanOriginal}
mean(totalStepsByDay$steps, na.rm = TRUE)
```

Median of the total number of steps taken per day

```{r medianOriginal}
median(totalStepsByDay$steps, na.rm = TRUE)
```
## What is the average daily activity pattern?

```{r avgStepsPerInterval, results='hide'}
avgStepsPerInterval <- sqldf("select interval, avg(steps) steps from dataset where steps is not null group by interval order by interval")
png(paste(imagesFolder, "avgStepsPerInterval.png", sep = "/"))
plot(avgStepsPerInterval$interval, avgStepsPerInterval$steps, type="l", xlab = "Interval", ylab="Average steps count")
dev.off()

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is `r max_interval`.

```{r maxStepsInterval}
avgStepsPerInterval$interval[avgStepsPerInterval$steps == max(avgStepsPerInterval$steps)]
```

## Imputing missing values
Total count of NA steps values
```{r naStepsCount}
sqldf("select count(*) count from dataset where steps is null")[, "count"]
```

Replacing all NA steps values with the average value in the according interval
```{r replacingNa}
datasetWithoutNA <- sqldf(c("update dataset set steps = (select steps from avgStepsPerInterval where avgStepsPerInterval.interval = dataset.interval) where steps is null", "select * from main.dataset"))
```


### Updated steps spreading histogram
```{r removedNaDatasetHist, results='hide'}
totalStepsByDay <- sqldf("select date, sum(steps) steps from datasetWithoutNA where steps is not null group by date order by date")
png(paste(imagesFolder, "totalStepsByDay.png", sep = "/"))
hist(totalStepsByDay$steps, col="red", main = "Number of steps taken each day", xlab = "Steps")
dev.off()



## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
Your plot will look different from the one above because you will be using the activity monitor data. Note that the above plot was made using the lattice system but you can make the same version of the plot using any plotting system you choose. 
Mean of the total number of steps taken per day

```{r meamRemovedNa}
mean(totalStepsByDay$steps, na.rm = TRUE)
```

Median of the total number of steps taken per day

```{r medianRemovedNa}
median(totalStepsByDay$steps, na.rm = TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?

### Difference between weekday and weekend sensor data
```{r diffByWeekday, results='hide'}
datasetWithoutNA$"day" <- weekdays(datasetWithoutNA$"date")

datasetWithoutNA <- sqldf(c("update datasetWithoutNA set day = 'weekend' where day in ('Saturday', 'Sunday')", "select * from main.datasetWithoutNA"))
datasetWithoutNA <- sqldf(c("update datasetWithoutNA set day = 'weekday' where day <> 'weekend'", "select * from main.datasetWithoutNA"))

groupedByWeekday <- aggregate(steps ~ interval + day, data = datasetWithoutNA, mean)

png("figure/weekendsPlot.png")
xyplot(steps ~ interval | day, data = groupedByWeekday, layout=c(1,2), type = "l")
dev.off()
```
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

```
