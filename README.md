Data

It is now possible to collect a large amount of DATA about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these DATA remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and SOFTWARE for processing and interpreting the data.

In this report we make use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. Data was provided for the course students for this particular assignment, avaliable to DOWNLOAD via this link (avaliable at 2014-05-25).

In the report, we try to answer the following questions:

What is mean total number of steps taken per day?
What is the average daily activity pattern?
Are there differences in activity patterns between weekdays and weekends?
Loading and preprocessing the data

In the first step, we load the data into R environment from the CSV file in the working directory.

# Read Activity monitoring data ('amd') data set from the working directory
amd <- read.csv("./activity/activity.csv", stringsAsFactors = FALSE)
The data is in an appropriate form for further analysis and thus it needs no more transformation at this point.

What is mean total number of steps taken per day?

We are INTERESTED in what is mean total number of steps taken per day. We begin with plotting a histogram of the total number of steps taken each day. Please note that for this part of the analysis, we ignore the missing values in the dataset.

# Group observations by date and sum steps taken per each day
library(dplyr)
total.steps.per.day <- amd %.% group_by(date) %.% summarise(total = sum(steps, 
    na.rm = T))

# Define total mean and total median of steps sums for each day
total.mean <- mean(total.steps.per.day$total)
print(total.mean)
## [1] 9354
total.median <- median(total.steps.per.day$total)
print(total.median)
## [1] 10395

library(ggplot2)
# Set default font size for all ggplot2 plots
theme_set(theme_gray(base_size = 14))

# The histogram display a general distribution of numbers which are total
# steps for each day in our DATA.
ggplot(total.steps.per.day, aes(x = total)) + geom_histogram(fill = "yellow", 
    colour = "black") + geom_vline(xintercept = total.mean, color = "brown", 
    linetype = "dashed", size = 1) + geom_vline(xintercept = total.median, color = "red", 
    linetype = "dashed", size = 1) + labs(title = "Histogram of total number of steps \n taken for each day") + 
    labs(x = "", y = "")
plot of chunk unnamed-chunk-3

The mean and median total number of steps taken per day

On the plot there ale also two vertical lines. They represents:

brown line - the mean total number of steps taken per day, which equals approximately 9354,
red line - the median total number of steps taken per day, which equals approximately 1.0395 × 104.
Sum steps for each particular day

Additionally, we may take a look at the plot of sum of steps for each partivular day. (Once again, brown line stands for the mean total number of steps taken per day, red line - the median total number of steps taken per day.)

#' Sum steps for each day and display it in a bar plot 
#' in the way that every x-axis element corresponds to 
#' a particular day and mapped y-axis element is a number 
#' of total steps that day. 
ggplot(total.steps.per.day, aes(date, total)) + geom_bar(fill = "chartreuse", 
    colour = "black") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    geom_hline(yintercept = total.mean, color = "brown", linetype = "dashed", 
        size = 1) + geom_hline(yintercept = total.median, color = "red", linetype = "dashed", 
    size = 1) + labs(title = "Total number of steps for each particular day") + 
    labs(x = "Date", y = "Total number of steps")
plot of chunk unnamed-chunk-4

What is the average daily activity pattern?

We are also intrested in the daily activity pattern. We investigate it by observing the average number of steps taken for subsequent day 5-minute intervals, averaged across all days. This pattern is presented on the plot below.

# Create DATA frame with number of steps taken, averaged across all days
avg.intvl.steps <- amd %.% group_by(interval) %.% summarise(AVG.intvl = mean(steps, 
    na.rm = T))

# Find out which 5-minute interval contains the maximum number of steps
max.num.of.steps.interv.ind <- which(avg.intvl.steps$avg.intvl == max(avg.intvl.steps$avg.intvl))
max.num.of.steps.interv <- avg.intvl.steps[max.num.of.steps.interv.ind, 1]

qplot(interval, avg.intvl, DATA = avg.intvl.steps) + geom_line() + geom_vline(xintercept = max.num.of.steps.interv, 
    color = "red", linetype = "dashed", size = 1) + labs(title = "Time series of the 5-minute interval and the average number of steps taken, \n averaged across all days") + 
    labs(x = "5-minute interval signature", y = "number of steps ")
plot of chunk unnamed-chunk-5

5-minute interval with the maximum number of steps

On the plot there is a red line representing the maximum of averaged number of steps per each 5-minute time interval. The maximum is found in the interwal with signature: 835.

Signature 835 represents time of some early part of a day. It seems sensible as we realize that during early day hours people generaly tend to get up; they are supposed to be in a rush waking up and going to school / work etc., making plenty of meters / kilometers by foot.

Imputing missing values

The presence of missing days may introduce bias into some CALCULATIONS or summaries of the data. In this section of the report we deal with these missing values, that unfortunately do occur in our data.

Total number of missing values in the dataset

# CALCULATE and report the total number of missing values in the dataset
# (the total number of rows with NAs)
na.rows.num <- nrow(amd) - sum(COMPLETE.cases(amd))
Total number of missing values in the dataset equals 2304. This is the number of rows with so called NA values.

A strategy for filling in all of the missing values in the dataset

To fill in all of the missing values in our data set, we use value of the mean number of steps for the 5-minute interval that each of NA value belongs to.

We create a new dataset that is equal to the original dataset but with the missing data filled in, with the use of the startegy descripted above.

#' We use `AVG.intvl.steps` data frame from previous section, 
#' which contains average steps number for every interval.
#' 
#' We substitute NA values with a steps mean value of the interval 
#' that this NA value belonges to. 

# Create copy of a base data frame
amd.na.imputed <- amd

for (row.num in 1:nrow(amd.na.imputed)) {

    if (is.na(amd.na.imputed[row.num, "steps"])) {

        interval.sgn <- amd.na.imputed[row.num, "interval"]
        interval.sgn.ind <- which(avg.intvl.steps[, "interval"] == interval.sgn)
        interval.steps.mean <- avg.intvl.steps[interval.sgn.ind, "avg.intvl"]
        amd.na.imputed[row.num, "steps"] <- interval.steps.mean
        message(paste0("NA value in row num. ", row.num, " imputed!"))
    }
}

# CHECK if number of COMPLETE cases equals number of rows
(nrow(amd.na.imputed) - sum(complete.cases(amd.na.imputed))) == 0
## [1] TRUE
Dataset with imputed NA values - histogram of the total number of steps taken each day

Now we can plot histogram of the total number of steps taken each day, but this time we use DATA set with imputed NA values.

total.steps.per.day.imputed <- amd.na.imputed %.% group_by(date) %.% summarise(total = sum(steps, 
    na.rm = T))

total.mean.imputed <- mean(total.steps.per.day.imputed$total)
print(total.mean.imputed)
## [1] 10766
total.median.imputed <- median(total.steps.per.day.imputed$total)
print(total.median.imputed)
## [1] 10766

#' The histogram display a general distribution of numbers
#' which are total steps for each day in our DATA. 

ggplot(total.steps.per.day.imputed, aes(x = total)) + geom_histogram(fill = "yellow", 
    colour = "black") + geom_vline(xintercept = total.mean.imputed, color = "brown", 
    linetype = "dashed", size = 1) + geom_vline(xintercept = total.median.imputed, 
    color = "red", linetype = "dashed", size = 1) + labs(title = "Histogram of total number of steps taken for each day \n in the data set with imputed NA values") + 
    labs(x = "", y = "")
plot of chunk unnamed-chunk-8

Dataset with imputed NA values - the mean and median total number of steps taken per day

We can observe that imputing NA values does have an inpact on total number of steps taken per day values:

both mean and median are higher than the same metrics taken from the raw data,
suprisingly, mean and median have exactly the same values this time; they both equal 1.0766 × 104 (this value is marked on the histogram with the use of a red vertical line).
Are there differences in activity patterns between weekdays and weekends?

Eventually, an interesting thing to CHECK is whether or not are there differences in activity patterns between weekdays and weekends for our individual. To do so, we add a new column to our DATA set, which represents the information about day type (“weekend” or “weekday”).

A panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days, is presented below.

# Set language to English (to correlcty display weekday abbrevations)
Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "English")
## [1] "English_United States.1252"

#' Create a new factor variable in the dataset with two levels 
#' – “weekday” and “weekend” indicating whether a given date is 
#' a weekday or weekend day
weekdays.sgn <- weekdays(as.Date(amd.na.imputed$date))
amd.na.imputed$day.type <- sapply(weekdays.sgn, function(sgn) {
    if (sgn %in% c("Saturday", "Sunday")) {
        return("weekend")
    } else {
        return("weekday")
    }
})

# Group DATA by interval and day type and get average steps number gor each
# group
day.type.interv.steps <- amd.na.imputed %.% group_by(interval, day.type) %.% 
    summarise(AVG.steps = mean(steps))

#' Panel plot containing a time series plot 
#' of the 5-minute interval and the average 
#' number of steps taken, averaged across all 
#' weekday days or weekend days.
library(lattice)
xyplot(avg.steps ~ interval | day.type, data = day.type.interv.steps, type = "b", 
    layout = c(1, 2))
plot of chunk unnamed-chunk-9
