token <- "e9b0d794dfdee1e2f86314ccab0f0d07adbdc382"
library(httr)
library(rjson)
library(leaflet)
library(dplyr)
options(stringsAsFactors = F)

# Functions 
get.coord.df.from.stream <- function (stream.obj) {
  data.frame(lat = sapply(stream.obj[[1]]$data, USE.NAMES = F, FUN = function (x) x[[1]]),
             lon = sapply(stream.obj[[1]]$data, USE.NAMES = F, FUN = function (x) x[[2]]))
}

get.stream.from.activity <- function (act.id, token) {
  stream <- GET("https://www.strava.com/",
                path = paste0("api/v3/activities/", act.id, "/streams/latlng"),
                query = list(access_token = token))
  content(stream)
}

get.activities2 <- function (token) {
  activities <- GET("https://www.strava.com/", path = "api/v3/activities",
                    query = list(access_token = token, per_page = 200))
  activities <- content(activities, "text")
  activities <- fromJSON(activities)
  res.df <- data.frame()
  for (a in activities) {
    values <- sapply(c("name", "distance", "moving_time", "elapsed_time", "total_elevation_gain",
                       "type", "id", "start_date_local",
                       "location_country", "average_speed", "max_speed", "has_heartrate", "elev_high",
                       "elev_low", "average_heartrate", "max_heartrate"), FUN = function (x) {
                         if (is.null(a[[x]])) {
                           NA } else { a[[x]] }
                       })
    res.df <- rbind(res.df, values)
  }
  names(res.df) <- c("name", "distance", "moving_time", "elapsed_time", "total_elevation_gain",
                     "type", "id", "start_date_local",
                     "location_country", "average_speed", "max_speed", "has_heartrate", "elev_high",
                     "elev_low", "average_heartrate", "max_heartrate")
  res.df
}

get.multiple.streams <- function (act.ids, token) {
  res.list <- list()
  for (act.id.i in 1:length(act.ids)) {
    if (act.id.i %% 5 == 0) cat("Actitivy no.", act.id.i, "of", length(act.ids), "\n")
    stream <- get.stream.from.activity(act.ids[act.id.i], token)
    coord.df <- get.coord.df.from.stream(stream)
    res.list[[length(res.list) + 1]] <- list(act.id = act.ids[act.id.i],
                                             coords = coord.df)
  }
  res.list
}

activities <- get.activities2(token)

activities$date<-as.Date(activities$start_date_local)
plot(activities$distance~activities$date)
plot(activities$average_speed~activities$date)
activities$distance<-as.numeric(activities$distance)/1609.4
activities$moving_time<-as.numeric(activities$moving_time)/60
activities$total_elevation_gain<-as.numeric(activities$total_elevation_gain)
activities$speed<-activities$moving_time/activities$distance
##Throw out the obvious outlier
activities<-activities[activities$speed<20,]

#Will also throw out mine and Ryan's hike, because it's a huge outlier
activities<-activities[activities$total_elevation_gain<800,]
activities$recentTrainingDays<-0
for (i in 1:nrow(activities)){
  activities$recentTrainingDays[i]<-sum(activities$day[i]-activities$day<30&activities$day[i]-activities$day>0)
}

plot(activities$speed~activities$average_speed)
hist(activities$total_elevation_gain)
activities$log_elevation_gain<-log(activities$total_elevation_gain)
activities$log_elevation_gain[activities$log_elevation_gain<0]=0
hist(activities$log_elevation_gain)
activities$day<- as.numeric(activities$date)
activities$day<-activities$day-min(activities$day)
plot(activities$day)

require(splines)
mod<-lm(speed~distance+total_elevation_gain+poly(day, 3)+
          distance:poly(day,3), data=activities)
summary(mod)
plot(activities$speed~activities$day, ylab="Minutes per mile", xlab="Last three years", main="Speed over time", ylim=c(6,12))
predFrame=data.frame(day=1:860, distance=mean(activities$distance), 
                     recentTrainingDays= mean(activities$recentTrainingDays),
                     total_elevation_gain=mean(activities$total_elevation_gain))
predFrame$date<-predFrame$day+min(activities$date)
predFrame$preds<-predict(mod, newdata=predFrame)
days<-1:860
lines(predFrame$preds~days, col="blue")

pdf("RunningFitness.pdf")
ggplot(data=activities)+geom_point(aes(x=date, y=speed, size=distance))+
  geom_line(data=predFrame, aes(x=date, y=preds, color="Fitness Level"), size=2)+
  ylab("Minutes per mile") + xlab("Last 3 years")+
  scale_x_date(date_labels = "%b %Y")+
  scale_size_continuous(name="Miles Ran")+
  scale_color_discrete(name=NULL)+
  ggtitle("Running Fitness over Time", subtitle = "Personal Strava data")
dev.off()


plot(mod)
acf(mod$residuals)
plot(mod$residuals)
hist(rstudent(mod))
plot(hatvalues(mod))

red.mod<-lm(speed~distance+poly(day, 3), data=activities)
mean(mod$residuals^2)
mean(red.mod$residuals^2)
anova(red.mod, mod)

require(car)
avPlots(mod) ##elevation gain is completely anchored by a handful or big climbs
