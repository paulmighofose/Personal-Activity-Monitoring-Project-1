### 1). READING IN DATASET & TIDYING DATA FOR PROCESSING:

###Installing necessary R packages:
library(tidyverse)

###Reading Data into R:
activity <- read.csv("Activity Monitoring.csv")

###Evaluating Data set for consistency and usability: 
tibble(activity)
summary(activity)

Id <- 1:17568
activity.Id <- cbind(Id, activity)
tibble(activity.Id)

Steps <- activity.Id %>% select(Id, steps)
tibble(Steps)

interval <- activity.Id %>% select(Id,interval)
tibble(interval)
dAte <- as.Date(activity$date, "%m/%d/%y")
tibble(dAte)


id <- activity.Id$Id
Data <- data.frame(Id = id, steps = activity.Id$steps, date = dAte, interval = activity.Id$interval)
tibble(Data)

Movement <- na.omit(Data)
tibble(Movement)

### 2). CREATING HISTOGRAM OF TOTAL NUMBER OF STEPS TAKEN PER DAY:
daily.sum <- tapply(Movement$steps,Movement$date,sum)
tibble(daily.sum)
step.sum <- data.frame(sum = (daily.sum))
Dates <- data.frame(date = c("2012/10/02", "2012/10/03","2012/10/04","2012/10/05","2012/10/06","2012/10/07","2012/10/09","2012/10/10","2012/10/11","2012/10/12","2012/10/13","2012/10/14","2012/10/15","2012/10/16","2012/10/17","2012/10/18","2012/10/19","2012/10/20","2012/10/21","2012/10/22","2012/10/23","2012/10/24","2012/10/25","2012/10/26","2012/10/27","2012/10/28","2012/10/29","2012/10/30","2012/10/31","2012/11/02","2012/1103","2012/1105","2012/11/06","2012/11/07","2012/11/08","2012/11/11","2012/11/12","2012/11/13","2012/11/15","2012/11/16","2012/11/17","2012/11/18","2012/11/19","2012/11/20","2012/11/21","2012/11/22","2012/11/23","2012/11/24","2012/11/25","2012/11/26","2012/11/27","2012/11/28","2012/11/29"))
dates <- data.frame(date = as.Date(Dates$date, "%Y/%m/%d"))
movement.sum <- cbind(dates,step.sum)
tibble(movement.sum)

hist(movement.sum$sum, xlab = "Steps", main = "Total Steps per Day", col = "dark blue")

### 3). CREATING DATAFRAME AND HISTOGRAMS OF MEAN & MEDIAN NUMBER OF STEPS TAKEN PER DAY:
###step mean:
daily.mean <- tapply(Movement$steps,Movement$date,mean)
tibble(daily.mean)
step.mean <- data.frame(mean = (daily.mean))
Dates <- data.frame(date = c("2012/10/02", "2012/10/03","2012/10/04","2012/10/05","2012/10/06","2012/10/07","2012/10/09","2012/10/10",  "2012/10/11", "2012/10/12", "2012/10/13", "2012/10/14", "2012/10/15", "2012/10/16", "2012/10/17", "2012/10/18", "2012/10/19", "2012/10/20","2012/10/21","2012/10/22","2012/10/23","2012/10/24","2012/10/25","2012/10/26","2012/10/27","2012/10/28","2012/10/29","2012/10/30","2012/10/31","2012/11/02","2012/11/03","2012/11/05","2012/11/06","2012/11/07","2012/11/08","2012/11/11","2012/11/12","2012/11/13","2012/11/15","2012/11/16","2012/11/17","2012/11/18","2012/11/19","2012/11/20","2012/11/21","2012/11/22","2012/11/23","2012/11/24","2012/11/25","2012/11/26","2012/11/27","2012/11/28","2012/11/29"))
dates <- data.frame(date = as.Date(Dates$date, "%Y/%m/%d"))
movement.mean <- cbind(dates,step.mean)
tibble(movement.mean)

hist(movement.mean$mean, xlab = "Steps", main = "Average Steps per Day", col = "dark green")

###Step Median: 
daily.median <- tapply(Movement$steps,Movement$date,median)
tibble(daily.median)
step.median <- data.frame(median = (daily.median))
Dates <- data.frame(date = c("2012/10/02", "2012/10/03","2012/10/04","2012/10/05","2012/10/06","2012/10/07","2012/10/09","2012/10/10",  "2012/10/11", "2012/10/12", "2012/10/13", "2012/10/14", "2012/10/15", "2012/10/16", "2012/10/17", "2012/10/18", "2012/10/19", "2012/10/20","2012/10/21","2012/10/22","2012/10/23","2012/10/24","2012/10/25","2012/10/26","2012/10/27","2012/10/28","2012/10/29","2012/10/30","2012/10/31","2012/11/02","2012/11/03","2012/11/05","2012/11/06","2012/11/07","2012/11/08","2012/11/11","2012/11/12","2012/11/13","2012/11/15","2012/11/16","2012/11/17","2012/11/18","2012/11/19","2012/11/20","2012/11/21","2012/11/22","2012/11/23","2012/11/24","2012/11/25","2012/11/26","2012/11/27","2012/11/28","2012/11/29"))
dates <- data.frame(date = as.Date(Dates$date, "%Y/%m/%d"))
movement.median <- cbind(dates,step.median)
tibble(movement.median)

hist(movement.median$median, xlab = "Steps", main = "Median Steps per Day")

#### 4). CREATING TIME SERIES OF AVERAGE NUMBER OF STEPS TAKEN:
###Time series:
steps.total <- as.table(tapply(Movement$steps, Movement$interval, sum ))
steps.mean <- as.table(tapply(Movement$steps, Movement$interval,mean ))
steps.median <- as.table(tapply(Movement$steps, Movement$interval,median))

###Plot Time Series: 
plot(steps.total,type  = "l", col = "dark blue", xlab = "Interval (5 Minute)", ylab = "Steps (Total)", main = "Steps Taken per Interval")
plot(steps.mean,type = "l", col = " dark red", xlab = " Interval (5 Minute)", ylab = "Steps (Mean)", main = "Steps Taken per Interval")
plot(steps.median,type = "l", xlab = "Interval (5 Minute)", ylab = "Steps (Median)", main = "Steps Taken per Interval")

### 5). DETERMINING THE 5 MINUTE INTERVAL CONTAINING THE MAXIMUM NUMBER OF STEPS TAKEN:


### 6). STRATEGY FOR INPUTTING MISSING VARIABLES:
tibble(activity)
summary(activity)
median <- 0
activity[is.na(activity)] = median

###Creating a new dataset of data with input values:
activity2 <- activity
tibble(activity2)

### 7). HISTOGRAM OF THE TOTAL NUMBER OF STEPS TAKEN EACH DAY AFTER MISSING VALUES ARE INPUTED:	
hist(activity4$steps,xlab = "Steps", main = "Distribution of Total Steps Taken")

### 8). CREATING PANEL PLOT COMPARING THE NUMBER OF STEPS TAKEN PER 5 MINUTE INTERVAL ON WEEKDAYS VS. WEEKENDS:

 ###Determining weekdays and weekends:
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

###Creating Panel plot: 
par(mfrow = c(2,1))
plot(steps.weekday,type  = "l", col = "dark blue", xlab = "Interval (5 Minute)", ylab = "Steps (Total)", main = "Average Weekday Steps")
plot(steps.weekend,type  = "l", col = "dark red", xlab = "Interval (5 Minute)", ylab = "Steps (Total)", main = "Average Weekend Steps")