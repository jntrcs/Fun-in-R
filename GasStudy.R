 setwd("C://Users//jntrcs//Downloads")
dat<-read.csv("Export.csv")
dat<-dat[-1,]
dat<-dat[dat$Type=="Gas",]
dat<-dat[dat$Filled.Up=="Full",]
dat$Cost.Gallon<- as.numeric(sub('\\$','',as.character(dat$Cost.Gallon)))
dat$Date<-as.Date(as.character(dat$Date))
dat$Odometer<-as.numeric(sub(',','',dat$Odometer))
dat$RoadTrip<-dat$Location %in% c("Colorado", "Blanding", "Laramie")
dat$RoadTrip[13]<-T
dat$RoadTrip[37]<-T
plot(dat$Odometer~dat$Date)
dat$DaysSinceLastFillUp<-c(dat$Date[-length(dat$Date)]-dat$Date[-1], NA)

model<-lm(dat$MPG~dat$Gas.Brand+dat$DaysSinceLastFillUp+dat$RoadTrip)
plot(model$residuals, col=dat$Gas.Brand)
summary(model)


dat$MPG
aggregate(dat$MPG~dat$Gas.Brand, FUN=function(i)c(mean(i), length(i)))
hist(dat$MPG[dat$Gas.Brand=="Maverick"])
