setwd("C:/Users/Gerrit/repdata_assignment_1")

#read in the data:
dat <- read.csv("activity.csv", header = T, colClasses=c("numeric", "character", "numeric"))

#format the date:
dat$date <- as.Date(dat$date,format = "%Y-%m-%d")

#calculate the total number of steps per day:
sums <- tapply(dat$steps, dat$date, sum, na.rm=T)

#plot a histogram of sums
hist(sums, breaks = 20, col="grey")

#get median and mean:
summary(sums)



# calculate the average number of step for each interval
av <- aggregate(dat$steps, by=list(interval=dat$interval), FUN=mean, na.rm=TRUE)

#time series plot for steps per interval:
plot(av$interval, av$x, type="l", xlab="interval", ylab="average number of steps", 
     main="average number of steps per interval")

#find the max:
av[which.max(av$x),]





#estimate number of NAs:
table(is.na(dat$steps))

#fill NAs with interval means 
dat.filled <- dat
for (i in 1:length(dat$steps)){
    if (is.na(dat.filled$steps[i])){
        dat.filled$steps[i] <- av$x[which(av$interval == dat.filled$interval[i])]
    }
}

#calculate the total number of steps per day:
sums <- tapply(dat.filled$steps, dat.filled$date, sum)

#plot a histogram of sums
hist(sums, breaks = 20, col="grey")

#get median and mean:
summary(sums)






#get weekdays:
dat.filled$weekday <- weekdays(dat.filled$date)
#replace weekdays with weekday/weekend factor
for (i in 1:length(dat.filled$weekday)){
    if (dat.filled$weekday[i] == "Sonntag" | dat.filled$weekday[i] == "Samstag"){
        dat.filled$weekday[i] <- "weekend"
    }else{
        dat.filled$weekday[i] <- "weekday"
    }
}
dat.filled$weekday <- as.factor(dat.filled$weekday)

#calculate the interval means for 
av <- aggregate(steps ~ interval + weekday, data=dat.filled, mean)
#plot the result:
par(mfrow=c(2,1), mar=c(3,4,2,1))
plot(subset(av, av$weekday=="weekday")$interval , subset(av, av$weekday=="weekday")$steps, 
     type="l", xlab="interval", ylab="average number of steps", 
     main="average number of steps per interval on weekdays")
plot(subset(av, av$weekday=="weekend")$interval , subset(av, av$weekday=="weekend")$steps, 
     type="l", xlab="interval", ylab="average number of steps", 
     main="average number of steps per interval on weekends")


