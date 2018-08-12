 setwd("C://Users//jntrcs//Downloads")
dat<-read.csv("fuelup.csv")
#remove partial fill ups
dat<-dat[dat$partialFuelup==0,]

dat$fuelupDate<-as.Date(dat$fuelupDate)
#calculate days between fill ups
#I will use this as a rough approx. of my ratio of city to freeway driving
dat$DaysBetween<-c(NA,dat$fuelupDate[order(dat$fuelupDate)[2:nrow(dat)]]-dat$fuelupDate[order(dat$fuelupDate)[1:(nrow(dat)-1)]])
hist(dat$DaysBetween)

#calculate mpg
dat$mpg<-dat$milesLastFuelup/dat$amount
hist(dat$mpg)

#remove fill ups when we don't know days between or MPG
dat<-dat[!is.na(dat$mpg) & !is.na(dat$DaysBetween),]


plot(dat$mpg~dat$DaysBetween)
cor(dat$mpg, dat$DaysBetween)

table(dat$fuelBrandID)
mod<-lm(mpg~ fuelBrandID+odometer+DaysBetween, data=dat)
summary(mod)
