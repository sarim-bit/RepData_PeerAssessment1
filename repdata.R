
library(lattice)
library(dplyr)
library(lubridate)

#Uncomment the next line if data not available
# unzip("activity.zip")

# 1. Loading the data
activity <- read.csv("activity.csv")
act_na <- na.omit(activity)

# 2. Histogram for total steps per day
act_day <- group_by(act_na,date) %>%
           summarise(steps=sum(steps)) %>%
           select(date,steps)
hist(act_day$steps,xlab = "Total steps per day", main = "Total number of steps taken each day")

# 3. Mean and median of steps taken in a day
act_day_mean <- mean(act_day$steps)
act_day_mean

act_day_median <- median(act_day$steps)
act_day_median

# 4. Time series plot on average number of steps taken
act_time <- group_by(act_na,interval) %>%
            summarise(steps=mean(steps)) %>%
            select(interval,steps)
plot(act_time,type="l",col="blue",lwd=2,xlab = "5-min Intervals",ylab = "Average steps taken",
     main = "Average steps taken in 5-minute intervals")

# 5. Maximum nuber of average steps in an interval
max_steps <- act_time[act_time$steps== max(act_time$steps),]$interval
max_steps

# 6. Imputing Missing Data
na_count <- nrow(activity[is.na(activity$steps),])
na_count

act_im <- activity %>%
          group_by(interval) %>%
          mutate(daily_mean = mean(steps,na.rm = TRUE))

# Rounding off the mean as number of steps will be positive integers
act_im$steps <- ifelse(is.na(act_im$steps),round(act_im$daily_mean),act_im$steps)

# 7. Histogram for total steps per day after imputing data
act_im_day <- group_by(act_im,date) %>%
              summarise(steps=sum(steps)) %>%
              select(date,steps)

hist(act_im_day$steps,xlab = "Total steps per day", main = "Total number of steps taken each day")

# Mean and Median
act_im_day_mean <- mean(act_im_day$steps)
act_im_day_mean

act_im_day_median <- median(act_im_day$steps)
act_im_day_median

# 8. Average number of steps taken per 5-minute interval across weekdays and weekends
act_week <- act_im
act_week$date <- weekdays(as.Date(act_im$date))
weekday <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
act_week$day <- factor(ifelse(act_week$date %in% weekday,"weekday","weekend"))
act_week<-act_week %>% group_by(interval,day) %>% summarise(steps=mean(steps))
xyplot(steps~interval | day,data=act_week,layout= c(1,2),index.cond=list(c(2,1)),type="l",lwd=2,
       ylab="Average Steps taken",xlab= "5-min Intervals")



