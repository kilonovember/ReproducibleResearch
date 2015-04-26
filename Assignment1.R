loadData <- function(filepath){
  activity<-read.csv(filepath)
  activity
}

mean.steps <-function(df){
  # Compute mean steps per day, eliminating rows with NA value for steps

  # Collect total number of steps per day
  spd <- aggregate(df$steps, list(df$date), sum, na.rm=TRUE)

  #Compute mean
  mean(spd$x)
}

median.steps <-function(df){
  # Compute median steps per day, eliminating rows with NA value for steps

  # Collect total number of steps per day
  spd <- aggregate(df$steps, list(df$date), sum, na.rm=TRUE)

  #Compute median
  median(spd$x)
}

histo.steps <- function(df){
  agg <- aggregate(df$steps, list(df$date), sum, na.rm=TRUE)
  ggplot(agg, aes(x=x)) + geom_histogram(data=agg$steps, binwidth = 1000, color="black", fill="white") +  scale_y_continuous(breaks=seq(0, 20, 1)) + ylab("Count of Days") + xlab("Steps per Day")
}

histo.steps.synth <- function(df){
  synth.obs <- synthesize.missing.data(df)
  actual.obs <- subset.complete.cases(df)
  combined <- merge.datasets(synth.obs, actual.obs)
  histo.steps(combined)
}

steps.grouped.by.interval <- function(df){
  # Compute the mean number of steps per five-minute interval over the period of the data set
   aggregate(df$steps, list(df$interval), mean, na.rm=TRUE)
}

ts.plot <- function(df){
  spi <- aggregate(df$steps, list(df$interval), mean, na.rm=TRUE)
  ggplot(spi, aes(x=Group.1, y=x)) + geom_line() + ylab("Steps") + xlab("5-minute Interval") 
}

weekday.vs.weekend.plot <- function(df){  
  spi <- aggregate(df$steps, list(df$interval, df$weekday), mean, na.rm=TRUE)
  ggplot(spi, aes(x=Group.1, y=x)) + geom_line() + ylab("Steps") + xlab("5-minute Interval") + facet_wrap(~ Group.2)
}

count.NAs <- function(df){
  length(which(is.na(df)))
}

subset.NAs <- function(df){
  missing <- subset(df, is.na(steps))
  missing
}

subset.complete.cases <- function(df){
  complete <- subset(df, !is.na(steps))
  complete
}

median.steps.per.interval <- function(df){
  spi <- aggregate(df$steps, list(df$interval), median, na.rm=TRUE)
  spi
}

mean.steps.per.interval <- function(df){
  spi <- aggregate(df$steps, list(df$interval), mean, na.rm=TRUE)
  spi
}

synthesize.missing.data <- function(df){
  incomplete.cases <- subset.NAs(df)
  mean.steps.pi <- mean.steps.per.interval(df)

  # Loop through the incomplete cases, and select from the mean.steps
  # data frame the calculated mean value for the matching 5-minute period
  # and assign it to the corresponding NA value

  for(i in 1:nrow(incomplete.cases)){
    interval.number <- incomplete.cases[i,3]
    incomplete.cases[i,1] <- mean.steps.pi[mean.steps.pi$Group.1 == interval.number,2]
  }
  
  # The formerly incomplete rows in incomplete.cases now have 
  # been made artifically complete by inserting into the "steps" column, 
  # the value of the average number of steps taken during the 
  # corresponding interval in the days where data was recorded
  
  incomplete.cases
}

merge.datasets <- function(synth.obs, actual.obs){
  # synth.obs are those rows with the synthesized
  # value for the "steps" column.
  # actual.obs are those rows with recorded values
  # for the "steps" column
  
  rbind(synth.obs,actual.obs)
}

add.weekday.factor<- function(df){
  complete <- subset.complete.cases(df)
  synth <- synthesize.missing.data(df)
  merged <- merge.datasets(synth, complete)
  merged$weekday <- as.factor(weekdays(as.Date(merged$date)) %in% c("Saturday", "Sunday"))
  levels(merged$weekday)[levels(merged$weekday) == FALSE] <- "Weekend"
  levels(merged$weekday)[levels(merged$weekday) == TRUE] <- "Weekday"
  merged
}
