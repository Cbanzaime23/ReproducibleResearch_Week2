---
title: "Reproducible Research: Peer Assessment 1"
author: "Christian Ibanez"
date: "2023-07-22"
output: html_document
keep_md: true
---

## Load the libraries

```{r, echo = TRUE,message = FALSE,warning=FALSE}

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)
```
## Loading and preprocessing the data
```{r, echo = TRUE}
Activity_Data <- read.csv("./repdata_data_activity/activity.csv")
summary(Activity_Data)
```

## 1. What is mean total number of steps taken per day?
```{r, echo = TRUE}
Activity_SumNoSteps <- Activity_Data %>%
                      group_by(date) %>%
                      summarise(Number_Steps = sum(steps)) %>%
                     mutate(Number_Steps = ifelse(is.na(Number_Steps), 0, Number_Steps))
Mean_NoSteps <- round(mean(Activity_SumNoSteps$Number_Steps), 2)
Median_NoSteps <- median(Activity_SumNoSteps$Number_Steps)
Plot1 <- ggplot(data=Activity_SumNoSteps, aes(x=Number_Steps)) +
        geom_histogram(bins = 20) + 
        geom_vline(aes(xintercept = Mean_NoSteps),col='red',size=2) + 
        geom_vline(aes(xintercept = Median_NoSteps),col='blue',size=2) +
        ggtitle( paste("Mean No. of Steps(RED) =", Mean_NoSteps,'\n',"Median No. of Steps(BLUE) =", Median_NoSteps ) ) +
        xlab("No. of Steps") +
        ylab("Count")
```
<div style="text-align: center;">
```{r, echo = TRUE}
Plot1
```

## 2. What is the average daily activity pattern?

#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo = TRUE}

Activity_MeanNoSteps <- Activity_Data %>%
  group_by(interval) %>%
  summarise(Mean_NumberSteps = mean(steps, na.rm = TRUE)) %>%
  mutate(Mean_NumberSteps = ifelse(is.na(Mean_NumberSteps), 0, Mean_NumberSteps))

Mean_NumberSteps_MAX <- max(Activity_MeanNoSteps$Mean_NumberSteps)
Mean_NumberSteps_MAX_Interval <- Activity_MeanNoSteps[Activity_MeanNoSteps$Mean_NumberSteps == Mean_NumberSteps_MAX,]$interval

Plot2 <- ggplot(data = Activity_MeanNoSteps, aes(x = interval, y = Mean_NumberSteps)) +
        geom_line(color = "black", size = 1) +
        geom_point(size = 0.5) +
        ggtitle( paste("The Interval that has the Maximum Mean No. of steps is ",Mean_NumberSteps_MAX_Interval,'\n'  ,"(Mean No. of Steps =", round(Mean_NumberSteps_MAX,2), ")",'\n' ) ) +
        xlab("5-Minute Interval throughout the day") +
        ylab("Average No. of Steps")
```
<div style="text-align: center;">
```{r, echo = TRUE}
Plot2
```


## 3. Imputing missing values

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r, echo = TRUE}
sum(is.na(Activity_Data$steps))
```
#### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r, echo = TRUE}
mean(Activity_Data$steps, na.rm = T)
```
#### Fill NA by Mean

```{r, echo = TRUE}
Activity_Data_ImputeNAs <- Activity_Data
Activity_Data_ImputeNAs$steps[is.na(Activity_Data_ImputeNAs$steps)] <- mean(Activity_Data_ImputeNAs$steps, na.rm = T)
head(Activity_Data_ImputeNAs)
```
#### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r, echo = TRUE}
sum(is.na(Activity_Data_ImputeNAs$steps))
```
#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
*Please view the plot to see the values of the Mean and Median when we impute mean for NAs*

#### Do these values differ from the estimates from the first part of the assignment?  
*Yes!, the values of Mean and Median after imputation are now equal compare when we haven't perform the imputation*

#### What is the impact of imputing missing data on the estimates of the total daily number of steps? 
*The effect of Imputing NAs values using mean is it transform the initial Histogram into a more normally distributed Histogram *

```{r, echo = TRUE}
Activity_SumNoSteps_ImputeNAs <- Activity_Data_ImputeNAs %>%
                      group_by(date) %>%
                      summarise(Number_Steps = sum(steps)) 

Mean_NoSteps <- round(mean(Activity_SumNoSteps_ImputeNAs$Number_Steps), 2)
Median_NoSteps <- median(Activity_SumNoSteps_ImputeNAs$Number_Steps)

Plot3 <- ggplot(data=Activity_SumNoSteps_ImputeNAs, aes(x=Number_Steps)) +
        geom_histogram(bins = 20) + 
        geom_vline(aes(xintercept = Mean_NoSteps),col='red',size=2) + 
        geom_vline(aes(xintercept = Median_NoSteps),col='blue',size=2) +
        ggtitle( paste("Mean No. of Steps(RED) =", Mean_NoSteps,'\n',"Median No. of Steps(BLUE) =", Median_NoSteps ) )+
        xlab("No. of Steps") +
        ylab("Count")
```
<div style="text-align: center;">
```{r, echo = TRUE}
Plot3
```

## 4. Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
```{r, echo = TRUE}
Activity_Data_ImputeNAs <- Activity_Data_ImputeNAs %>%
  mutate(weekend = ifelse(wday(date) %in% c(1,7), "weekend", "weekday"))
Activity_Data_ImputeNAs$date <- as.Date(Activity_Data_ImputeNAs$date)
```


#### For WEEKEND
```{r, echo = TRUE}
Activity_Data_ImputeNAs_WEND <- Activity_Data_ImputeNAs[Activity_Data_ImputeNAs["weekend"] == "weekend",]

Activity_Data_ImputeNAs_WEND_Interval <- Activity_Data_ImputeNAs_WEND %>%
  group_by(interval) %>%
  summarise(Mean_NumberSteps = mean(steps, na.rm = TRUE)) %>%
  mutate(Mean_NumberSteps = ifelse(is.na(Mean_NumberSteps), 0, Mean_NumberSteps))


WEEKEND_plot <- ggplot(data = Activity_Data_ImputeNAs_WEND_Interval, aes(x = interval, y = Mean_NumberSteps)) +
                    geom_line(color = "black", size = 1) +
                    scale_y_continuous(limits = c(0, 210)) +
                    geom_point(size = 0.5) +
                    ggtitle(paste("The distribution of Ave. No. of Steps in Weekend is very different from \n the one for Weekday ")) +
                    theme(plot.title = element_text(face = "bold")) +
                    xlab("WEEKEND: 5-Minute Interval throughout the day") +
                    ylab("Average No. of Steps")
```

#### For WEEKDAY
```{r, echo = TRUE}
Activity_Data_ImputeNAs_WDAY <- Activity_Data_ImputeNAs[Activity_Data_ImputeNAs["weekend"] == "weekday",]

Activity_Data_ImputeNAs_WDAY_Interval <- Activity_Data_ImputeNAs_WDAY %>%
  group_by(interval) %>%
  summarise(Mean_NumberSteps = mean(steps, na.rm = TRUE)) %>%
  mutate(Mean_NumberSteps = ifelse(is.na(Mean_NumberSteps), 0, Mean_NumberSteps))

WEEKDAY_plot <- ggplot(data = Activity_Data_ImputeNAs_WDAY_Interval, aes(x = interval, y = Mean_NumberSteps)) +
                    geom_line(color = "black", size = 1) +
                    scale_y_continuous(limits = c(0, 210)) +
                    geom_point(size = 0.5) +
                    ggtitle(paste("But for the Weekday's distribution, it is very close to the overall Ave. \n No. of Steps Distribution")) +
                    theme(plot.title = element_text(face = "bold")) +
                    xlab("WEEKDAY: 5-Minute Interval throughout the day") +
                    ylab("Average No. of Steps")
```
<div style="text-align: center;">
```{r, echo = TRUE}
# Arrange the two plots side by side
Plot4 <- grid.arrange(WEEKEND_plot, WEEKDAY_plot,ncol = 1, nrow = 2, heights = c(4, 4))
```

