#url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

#if (!file.exists("data")) dir.create("data")
#if (!file.exists("data/data.zip")) download.file(url, destfile = "data/data.zip", mode="wb")
#if (!file.exists("data/activity.csv")) unzip("data/data.zip", exdir="data")

activity <- read.csv("./data/activity.csv")

class(activity)

head(activity)

summary(activity)


## What is mean total number of steps taken per day?

# 1. The total number of steps taken per day
daily_steps <- with(activity, tapply(steps, date, sum))

# 2. Histogram of number of steps taken per day
hist(daily_steps, n=10)

# 3. Mean and median of number of steps taken per day
mean(daily_steps, na.rm = TRUE)
median(daily_steps, na.rm = TRUE)





## What is the average daily activity pattern?

# 2. time series plot of pattern 
pattern_steps <- with(activity, tapply(steps, interval, mean, na.rm = TRUE))
int5 <- with(activity, tapply(interval, interval, mean, na.rm = TRUE))


#time <- .POSIXct(character(dim(int5)))
#c <- 0
#for (i in int5){
#      c <- c+1
#      if (i<1000 & i>99){
#            time[c] <- strptime(paste0("0",toString(i)), format = "%H%M")
#      }
#      if (i<100 & i>9){
#            time[c] <- strptime(paste0("00",toString(i)), format = "%H%M")
#      }
#      if (i<10){
#            time[c] <- strptime(paste0("000",toString(i)), format = "%H%M")
#      }
#}
#plot(time, pattern_steps, type = "l")
#axis.POSIXct(1, at=time, labels=format(time, "%H/%M"))

plot(int5,pattern_steps, type = "l")

# 1. Max value

max_value <- max(pattern_steps)
max_interval <- int5[pattern_steps == max_value]

## What is the average daily activity pattern?
# 1. Number of missing values
sum(is.na(activity$steps))

# 2. Filling missing values

activity_full <- activity
check_na <- is.na(activity$steps)

for (i in seq(1, length(check_na), by = 1)){
      if (check_na[i] == TRUE){
            activity_full$steps[i] <- pattern_steps[activity$interval[i] == int5]            
      }
}

daily_steps_full <- with(activity_full, tapply(steps, date, sum))

hist(daily_steps_full, n = 10)

day <- weekdays(as.POSIXlt(activity$date), abbreviate = TRUE)
wkday <- factor((day=="Sun" | day == "Sat"), labels = c("Weekday", "Weekend"))
activity["day_flag"] <- wkday

daily_steps <- with(activity, tapply(steps, list(date, wkday), sum))

qplot(interval, steps, data = activity, facets = .~day_flag, color = type, geom = "line")+ ggtitle("test")


pattern_steps <- as.data.frame(with(activity, tapply(steps, list(interval, wkday), mean, na.rm = TRUE)))

steps <- c(pattern_steps[,1], pattern_steps[,2])
day_flag <- c(rep("Weekday", times = length(steps)/2), rep("Weekend", times = length(steps)/2))
interval <- c(int5,int5)
pattern_steps_new <- data.frame(interval,steps,day_flag)


qplot(interval, steps, data = pattern_steps_new, facets = .~day_flag, color = day_flag, geom = "line")+ ggtitle("Test")

