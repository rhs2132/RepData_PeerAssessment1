
library(tidyverse)

activityData <- read_csv("activity/activity.csv", col_types = list(steps = col_integer(), date = col_date(format = "%Y-%m-%d"), interval = col_integer()))



dailySteps <- activityData %>% group_by(date) %>% summarize(totalSteps = sum(steps, na.rm = TRUE), meanSteps = mean(steps, na.rm = TRUE), medianSteps = median(steps, na.rm = TRUE))

dailyStepsHist <- 
    ggplot(dailySteps, aes(x = totalSteps), inherit.aes = FALSE) + 
    geom_histogram(bins = 30, color = "black", fill = "gray", na.rm = TRUE) +
    labs(title = "Histogram of Daily Total Steps", x = "Total Steps", y = "Number of Occurrences") + 
    scale_y_continuous(breaks = seq(0,10,2))
dailySteps %>% summarize(meanTotalSteps = mean(totalSteps, na.rm = TRUE), medianTotalSteps = median(totalSteps, na.rm = TRUE))



intervalSteps <- activityData %>% group_by(interval) %>% summarize(totalSteps = sum(steps, na.rm = TRUE), meanSteps = mean(steps, na.rm = TRUE), medianSteps = median(steps, na.rm = TRUE))

intervalStepsPlot <- 
    ggplot(intervalSteps, aes(x = interval, y = meanSteps), inherit.aes = FALSE) +
    geom_line(na.rm = TRUE) +
    labs(title = "Mean Steps Over 5 Minute Increments", x = "Time of Day (Minutes Since Midnight)", y = "Mean Steps")
intervalSteps %>% slice_max(meanSteps) %>% select(interval, meanSteps)



missingRows <- nrow(activityData) - nrow(activityData %>% drop_na())

imputedData <- activityData %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

dailyImpSteps <- imputedData %>% group_by(date) %>% summarize(totalSteps = sum(steps), meanSteps = mean(steps), medianSteps = median(steps))

dailyImpStepsHist <- 
    ggplot(dailyImpSteps, aes(x = totalSteps), inherit.aes = FALSE) + 
    geom_histogram(bins = 30, color = "black", fill = "gray") +
    labs(title = "Histogram of Daily Total Steps Using Imputed Data", x = "Total Steps", y = "Number of Occurrences") + 
    scale_y_continuous(breaks = seq(0,12,2))
dailyImpSteps %>% summarize(meanTotalSteps = mean(totalSteps), medianTotalSteps = median(totalSteps))


imputedData$dayType <- factor((weekdays(imputedData$date) %in% c("Saturday", "Sunday")), levels = c(FALSE, TRUE), labels = c("weekday", "weekend"))
weekendSteps <- imputedData %>% group_by(dayType, interval) %>% summarize(meanSteps = mean(steps))

intervalStepsPlot <- 
    ggplot(weekendSteps, aes(x = interval, y = meanSteps), inherit.aes = FALSE) +
    geom_line(na.rm = TRUE) +
    facet_wrap(~dayType, ncol = 1) +
    labs(title = "Mean Steps Over 5 Minute Increments", x = "Time of Day (Minutes Since Midnight)", y = "Mean Steps")

averageSteps <- imputedData %>% group_by(dayType) %>% summarize(meanSteps = mean(steps))
averageSteps$meanSteps <- 24*60/5*averageSteps$meanSteps


