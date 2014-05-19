Programming Assignment 1
========================================================

##This assignment deals with the data about movement activity during day, the data were collected between 1st of October 2012 and 30th of November 2012.



**Loading and preprocessing the data**

##A working directory is set and the data are loaded:

setwd("C:/Users/Lubomír Štěpánek/Documents/Data Science Specialization/Reproducible Research/Week 2")

data<-read.csv("activity.csv",header=TRUE)

##Structure of data is chekced:

str(data)
head(data)




**What is mean total number of steps taken per day?**

##Identificator variable for number of day in scoped interval (1st to 61th) is set:

id<-NULL
for(i in 1:61){
id<-c(id,rep(i,288))
}
data<-cbind(data,id)

##Steps per day are counted:

steps_per_day<-NULL
for(i in 1:61){
steps_per_day<-c(steps_per_day,sum(na.omit(data[,1][data[,4]==i])))
}
steps_per_day
## The average value of steps per day is:
mean(steps_per_day)
## The median of steps per day is:
median(steps_per_day)

hist(steps_per_day,main="Steps per day",xlab="steps per day")




**What is the average daily activity pattern?**

##The next identificator variable an order of 5 min interval is set: 

interval<-rep(c(1:288),61)

data<-cbind(data,interval)

steps_per_interval<-NULL
for(i in 1:288){
steps_per_interval<-c(steps_per_interval,mean(na.omit(data[,1][data[,5]==i])))
}
plot(steps_per_interval~interval,data=cbind(interval,steps_per_interval),type="l",main="Steps in 5 min interval during a day",xlab="# of interval",ylab="average value of steps per day")




**Imputing missing values**

## Missing values are filled in, each of missing value is replaced by an average value of the variable
## (Steps per day, Date or Interval):

data.wo.na<-data
for(i in 1:3){
data.wo.na[,i][which(is.na(data.wo.na[,i]))]<-mean(na.omit(data.wo.na[,i]))
}

which(is.na(data.wo.na[,1]))
which(is.na(data.wo.na[,2]))
which(is.na(data.wo.na[,3]))

data.wo.na<-cbind(data.wo.na,id)

steps_per_day<-NULL
for(i in 1:61){
steps_per_day<-c(steps_per_day,sum(na.omit(data.wo.na[,1][data.wo.na[,4]==i])))
}

# The average value of steps per day (w/o NAs) is:
mean(steps_per_day)
# The median of steps per day (w/o NAs) is:
median(steps_per_day)

hist(steps_per_day,main="Steps per day (w/o NAs)",xlab="steps per day")




**Are there differences in activity patterns between weekdays and weekends?**

##1st of October of 2012 was Monday (via Wolfram Alpha). Weekday and weekends are detected:

##weekdays
weekday<-NULL
row<-NULL
weekday<-as.data.frame(weekday)
for(i in 1:dim(data)[1]){
if(data[i,4]%%7<6&data[i,4]%%7>0){
row<-data[i,]
weekday<-rbind(weekday,row)
row<-NULL
}
}

steps_per_interval_wd<-NULL
for(i in 1:288){
steps_per_interval_wd<-c(steps_per_interval_wd,mean(na.omit(weekday[,1][weekday[,5]==i])))
}

##weekends
weekend<-NULL
row<-NULL
weekend<-as.data.frame(weekend)
for(i in 1:dim(data)[1]){
if(data[i,4]%%7==6|data[i,4]%%7==0){
row<-data[i,]
weekend<-rbind(weekend,row)
row<-NULL
}
}

steps_per_interval_we<-NULL
for(i in 1:288){
steps_per_interval_we<-c(steps_per_interval_we,mean(na.omit(weekend[,1][weekend[,5]==i])))
}

par(mfrow=c(2,1))
plot(steps_per_interval_wd~interval,data=cbind(interval,steps_per_interval_wd),type="l",main="Steps in 5 min interval during a weekday",xlab="# of interval",ylab="average value of steps per day")
plot(steps_per_interval_we~interval,data=cbind(interval,steps_per_interval_we),type="l",main="Steps in 5 min interval during a weekend",xlab="# of interval",ylab="average value of steps per day")


