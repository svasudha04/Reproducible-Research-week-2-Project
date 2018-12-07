---
title: "Reproducible Research week-2 Project"
author: "Vasudha Singh"
date: "December 7, 2018"
output: html_document
---

##1.Loading and preprocessing the data  
Load the data and transform the data into a format suitable for analysis  
```{r}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="actvitiy.zip", method="auto")
unzip("actvitiy.zip")
actdata<- read.csv("activity.csv")
str(actdata)
head(actdata)
summary(actdata)
```  
##Load libraries
```{r}
library(dplyr)
library(ggplot2)
```  

##2. What is mean total number of steps taken per day?  
For this part of the assignment, you can ignore the missing values in the dataset.  
```{r}
act.complete<- na.omit(actdata)
```
###(a) Calculate the total number of steps taken per day  

```{r}
steps_per_day<- aggregate(act.complete$steps, by=list(act.complete$date), FUN=sum) 
colnames(steps_per_day)<-c("Date", "TotalSteps")
```  
###(b) Make a histogram of the total number of steps taken each day

```{r}
ggplot(steps_per_day, aes(x=TotalSteps))+geom_histogram(fill="red",binwidth = 1000)+labs(x="Steps", Y="Frequency", title = "Total Number Of Steps Taken Per Day")
```  
###(c)Calculate and report the mean and median of the total number of steps taken per day  

```{r}
mean(steps_per_day$TotalSteps)
median(steps_per_day$TotalSteps)
```  

##What is the average daily activity pattern?
###(a) Make a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```{r}
steps_per_int<- aggregate(act.complete$steps, by=list(act.complete$interval), FUN=mean) 
colnames(steps_per_int)<-c("interval", "avg_Steps")

ggplot(steps_per_int,aes(x=interval, y=avg_Steps))+geom_line()+ labs(x="Interval", ylab=" avg_Steps", title="Time series plot of 5- minute interval")
```    


###(b) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```{r}
steps_per_int[which(steps_per_int$avg_Steps==max(steps_per_int$avg_Steps)),]
```  

##Imputing missing values  

###(a)Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) 
```{r}
sapply(X=actdata, FUN=function(x) sum(is.na(x)))
```  

###(b)Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

```{r}
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm=TRUE))
meanday <- actdata%>% group_by(interval)  %>% mutate(steps= replacewithmean(steps))
head(meanday)
```  

###(c)Create a new dataset that is equal to the original dataset but with the missing data filled in  
```{r}
new_dataset <-as.data.frame(meanday)
head(meanday)

summary(new_dataset)
```  
 
###(d)Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```{r}
tot_data_perday<-aggregate(new_dataset$steps, by=list(new_dataset$date), FUN=sum)
colnames(tot_data_perday)<- c("date","totalsteps")
head(tot_data_perday)

ggplot(tot_data_perday, aes(x=totalsteps))+ geom_histogram(fill="blue", binwidth=1000)+ labs(x="steps", y="Frequency",title="Total Steps Per Day")

mean(tot_data_perday$totalsteps)
median(tot_data_perday$totalsteps)
```   
####Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?   
  - Yes, the median value is differ from the estimates the first part of the assignment. The impact of imputing  missing data of the total daily no. of steps is that it shifted towards the mean.  
  
##Are there differences in activity patterns between weekdays and weekends?  
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part    
  
###(a)Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  

```{r}
new_dataset$weekend_day <-ifelse(weekdays(as.Date(new_dataset$date)) %in% c("Monday", "Teusday", "Wednesday","Thursday", "Friday"), "Weekday", "Weekend")
head(new_dataset)
```  

###(b)Make a panel plot containing a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  

```{r}
meanWeek_end_day<- aggregate(new_dataset$steps, by=list(new_dataset$weekend_day, new_dataset$interval), mean )
colnames(meanWeek_end_day)<- c("weekend_day","interval","steps")

ggplot(meanWeek_end_day, aes(x= interval, y=steps, color= weekend_day))+ geom_line()+ facet_grid(weekend_day ~ .) + labs(x="Interval", y="Steps", title = "Comparison of Avg Number of steps in each Interval")
```


