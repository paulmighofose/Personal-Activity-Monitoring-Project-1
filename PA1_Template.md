---
title: 'Project 1: Reproducible Research'
author: "Paul Ighofose"
date: "2/26/2021"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
  pdf_document: default
  word_document: default
--- 

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
```

## Personal Activity Monitoring

This data set utilizes personal activity data obtained from an anonymous individual. Its observations (i.e. step count) were recorded via five minute intervals during the months of October and November of 2012. It is my intention to read and process the data to discover a daily mean and median step count, its distribution, and the wholesomeness of the data and its potential affects on any analytically output generated. All data and views represented within this study are not implications on the population as a whole, yet is only a fragmented representation of how personal activity data could, potentially, be utilized to further understand an individuals daily patterns and possibly its affects on health.

## Methods

Utilizing R, the personal activity monitoring data set was read into the IDE and processed to provide a data frame consisting of three columns(i.e. steps,date, & interval) with 17,768 observations. 

```{r}
###Installing necessary R packages:
library(tidyverse)

###Reading Data into R:
activity <- read.csv("Activity Monitoring.csv")
summary(activity)
```

Of those observations, 2,304 or approximately 13.1% of the steps were noted as "Na's" and indicated a need for removal. An identifier (Id), was applied to the data set to ensure consistency and correlation between step,date, and interval  observations. 



```{r}
Id <- 1:17568
activity.Id <- cbind(Id, activity)
head(activity.Id,3)
Steps <- activity.Id %>% select(Id, steps)
interval <- activity.Id %>% select(Id,interval)
dAte <- as.Date(activity$date, "%m/%d/%y")
id <- activity.Id$Id
Data <- data.frame(Id = id, steps = activity.Id$steps, date = dAte, interval = activity.Id$interval)
Movement <- na.omit(Data)
tibble(Movement)
```


To further understand the total number of steps taken each day, a loop was applied to summarize the steps observations diurnally. From the matrix provided, the date column was subset and integrated with the sum total step count to provide a more robust data frame. More so, the same process was replicated to determine, both, the mean and median step counts per day. 
```{r}
daily.sum <- tapply(Movement$steps,Movement$date,sum)
head(daily.sum,3)
step.sum <- data.frame(sum = (daily.sum))
Dates <- data.frame(date = c("2012/10/02", "2012/10/03","2012/10/04","2012/10/05","2012/10/06","2012/10/07","2012/10/09","2012/10/10","2012/10/11","2012/10/12","2012/10/13","2012/10/14","2012/10/15","2012/10/16","2012/10/17","2012/10/18","2012/10/19","2012/10/20","2012/10/21","2012/10/22","2012/10/23","2012/10/24","2012/10/25","2012/10/26","2012/10/27","2012/10/28","2012/10/29","2012/10/30","2012/10/31","2012/11/02","2012/1103","2012/1105","2012/11/06","2012/11/07","2012/11/08","2012/11/11","2012/11/12","2012/11/13","2012/11/15","2012/11/16","2012/11/17","2012/11/18","2012/11/19","2012/11/20","2012/11/21","2012/11/22","2012/11/23","2012/11/24","2012/11/25","2012/11/26","2012/11/27","2012/11/28","2012/11/29"))
dates <- data.frame(date = as.Date(Dates$date, "%Y/%m/%d"))
movement.sum <- cbind(dates,step.sum) 
head(movement.sum,3)
```

```{r movement.sum$sum, echo=TRUE}
hist(movement.sum$sum, xlab = "Steps", main = "Total Steps per Day", col = "dark blue")
```

```{r}
daily.mean <- tapply(Movement$steps,Movement$date,mean)
head(daily.mean,)
step.mean <- data.frame(mean = (daily.mean))
Dates <- data.frame(date = c("2012/10/02", "2012/10/03","2012/10/04","2012/10/05","2012/10/06","2012/10/07","2012/10/09","2012/10/10",  "2012/10/11", "2012/10/12", "2012/10/13", "2012/10/14", "2012/10/15", "2012/10/16", "2012/10/17", "2012/10/18", "2012/10/19", "2012/10/20","2012/10/21","2012/10/22","2012/10/23","2012/10/24","2012/10/25","2012/10/26","2012/10/27","2012/10/28","2012/10/29","2012/10/30","2012/10/31","2012/11/02","2012/11/03","2012/11/05","2012/11/06","2012/11/07","2012/11/08","2012/11/11","2012/11/12","2012/11/13","2012/11/15","2012/11/16","2012/11/17","2012/11/18","2012/11/19","2012/11/20","2012/11/21","2012/11/22","2012/11/23","2012/11/24","2012/11/25","2012/11/26","2012/11/27","2012/11/28","2012/11/29"))
dates <- data.frame(date = as.Date(Dates$date, "%Y/%m/%d"))
movement.mean <- cbind(dates,step.mean)
head(movement.mean,3)
```

```{r,movement.mean$mean, echo=TRUE}
hist(movement.mean$mean, xlab = "Steps", main = "Average Steps per Day", col = "dark green")
```

```{r}
###Step Median: 
daily.median <- tapply(Movement$steps,Movement$date,median)
head(daily.median,3)
step.median <- data.frame(median = (daily.median))
Dates <- data.frame(date = c("2012/10/02", "2012/10/03","2012/10/04","2012/10/05","2012/10/06","2012/10/07","2012/10/09","2012/10/10",  "2012/10/11", "2012/10/12", "2012/10/13", "2012/10/14", "2012/10/15", "2012/10/16", "2012/10/17", "2012/10/18", "2012/10/19", "2012/10/20","2012/10/21","2012/10/22","2012/10/23","2012/10/24","2012/10/25","2012/10/26","2012/10/27","2012/10/28","2012/10/29","2012/10/30","2012/10/31","2012/11/02","2012/11/03","2012/11/05","2012/11/06","2012/11/07","2012/11/08","2012/11/11","2012/11/12","2012/11/13","2012/11/15","2012/11/16","2012/11/17","2012/11/18","2012/11/19","2012/11/20","2012/11/21","2012/11/22","2012/11/23","2012/11/24","2012/11/25","2012/11/26","2012/11/27","2012/11/28","2012/11/29"))
dates <- data.frame(date = as.Date(Dates$date, "%Y/%m/%d"))
movement.median <- cbind(dates,step.median)
head(movement.median,3)
```

```{r, hist(movement.median$median), echo= TRUE}
setwd("/users/paulighofose/Desktop/Reproducible Research")
png(filename = "Median Steps per Day.png", width = 480, height = 480)
hist(movement.median$median,col = "dark red", xlab = "Steps", main = "Median Steps per Day")
```

Yet, when considering the data along a continuum such as a time series, the information is limited. Thus, to measure the activity (i.e. step count) across an acculmination of 5 minute intervals, a time series plot was utilized. 

```{r,}
steps.total <- as.table(tapply(Movement$steps, Movement$interval, sum ))
head(steps.total,3)
max(steps.total)
```
```{r,steps.total, echo=TRUE}
plot(steps.total,type  = "l", col = "dark blue", xlab = "Interval (5 Minute)", ylab = "Steps (Total)", main = "Steps Taken per Interval")
```

Examination of the time series indicates an increased number of steps taken between intervals 800 and 1000,  and more specifically a maximum of 10,927 steps were taken within that period.

Although simplistic in its' representation, this maximum value also indicates an extreme within the step count observation. For if, one were to include the complete data set, thereby replacing all "NA" values with that of the population's mean, the average step count figure would have become skewed and inconsiderable as a accurate representation of that data. Thus, when re-examining the data and replacing those observations with a median value of "0.00", the data more accurately depicts the distribution of a complete data set. 

```{r}
head(activity,3)
summary(activity)
median <- 0
activity[is.na(activity)] = median

###Creating a new dataset of data with input values:
activity2 <- activity
tibble(activity2)
```

And just as before, the new inclusive data set was looped and a total step count per day was derived.

```{r}
stepsperday <- tapply(activity2$steps,activity2$date, sum)

###Renaming column:
stepsperday.with <- data.frame(steps = (stepsperday))
head(stepsperday.with,3)
as.array(stepsperday)
###Dates are copied and a new data.frame is created with Dates and steps
Dates <- data.frame(dates = c("10/1/12", "10/10/12", "10/11/12", "10/12/12", "10/13/12", "10/14/12", "10/15/12", "10/16/12", "10/17/12", "10/18/12", "10/19/12", "10/2/12", "10/20/12", "10/21/12", "10/22/12", "10/23/12", "10/24/12", "10/25/12", "10/26/12", "10/27/12", "10/28/12", "10/29/12","10/3/12", "10/30/12", "10/31/12",  "10/4/12",  "10/5/12",  "10/6/12",  "10/7/12",  "10/8/12",  "10/9/12",  "11/1/12", "11/10/12", "11/11/12", "11/12/12", "11/13/12", "11/14/12", "11/15/12", "11/16/12", "11/17/12", "11/18/12", "11/19/12",  "11/2/12", "11/20/12","11/21/12", "11/22/12", "11/23/12", "11/24/12", "11/25/12", "11/26/12", "11/27/12", "11/28/12", "11/29/12",  "11/3/12", "11/30/12", "11/4/12",  "11/5/12",  "11/6/12",  "11/7/12",  "11/8/12",  "11/9/12"))
Date <- data.frame(date = as.Date(Dates$dates, "%m/%d/%y"))
head(Date,3)
activity4 <- cbind(Date,steps = stepsperday.with$steps)
head(activity4,)
```
```{r, echo=TRUE}
par(mfrow = c(1,2))
hist(movement.sum$sum, xlab = "Steps", main = "Total Steps per Day", sub = "(without NA's)", col = "dark blue")
hist(activity4$steps,xlab = "Steps",col = "dark red", main = "Total Steps per Day", sub = "(with inputed median values)")
```

The distribution of the data is slightly different, with the imputed data set skewing slightly left. This indicates an increase in steps   below 15,000 in comparison to the data set without "NA" observations. Thus, one must consider if the data had been complete (i.e without any missing values) then the individual's activity monitor would have indicated an increase in total steps per day. The only question to now consider is whether the individuals weekday and weekend activities are similar or not. 


To do so, the character vector "date" was converted to a date vector and filtered by day. Weekdays were combined to provide one collective, as weekend observations were combined to provide another. And, just as before the two subsets were individually looped and a mean step count calculated. 
```{r}
date.2 <- activity2$date
date.3 <- data.frame(date = as.Date(date.2, "%m/%d/%y"))
activity.data <- cbind(steps = activity2$steps,date = date.3,interval = activity2$interval)
head(activity.data)
activity.data.1 <- mutate(activity.data,weekday = weekdays(activity.data$date))
tibble(activity.data.1)

###Sorting Days:
activity6m <- activity.data.1 %>% filter(weekday == "Monday")
activity6t <- activity.data.1 %>% filter(weekday == "Tuesday")
activity6w <- activity.data.1 %>% filter(weekday == "Wednesday")
activity6th <- activity.data.1 %>% filter(weekday == "Thursday")
activity6f <- activity.data.1 %>% filter(weekday == "Friday")
activity6sat <- activity.data.1 %>% filter(weekday == "Saturday")
activity6sun <- activity.data.1 %>% filter(weekday == "Sunday")

###Combining weekdays and weekend days:
weekday <- rbind(activity6m, activity6t, activity6w, activity6th, activity6f)
weekend <- rbind(activity6sat, activity6sun)


###Creating time series of weekday and weekend activity:
steps.weekday <- as.table(tapply(weekday$steps, weekday$interval, mean ))
steps.weekend <- as.table(tapply(weekend$steps, weekend$interval,mean ))
```

```{r, steps.weekday, echo=TRUE}
par(mfrow = c(2,1))
plot(steps.weekday,type  = "l", col = "dark blue", xlab = "Interval (5 Minute)", ylab = "Steps (Total)", main = "Average Weekday Steps")
plot(steps.weekend,type  = "l", col = "dark red", xlab = "Interval (5 Minute)", ylab = "Steps (Total)", main = "Average Weekend Steps")
```


In comparison the average weekday step count appears greater, however if one were to remove the weekday maximum, one would notice that the weekend step count maximum and occurrence are greater and more frequent. Indicating greater mobility within that time period. 



