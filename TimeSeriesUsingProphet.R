#I wanted to test out FB's Prophet package,
#So I used data about how many minutes my phone screen is on each day
#Unfortunately I only have 8 months of data, so estimating seasonal effects is not possible
#But we can estimate days of the week effects
require(prophet)
require(lubridate)

data=read.csv("UnlockCounter.csv")
data$Date<-mdy_hm(data$Date)
df<-data
df$Unlocks<-NULL
names(df)<-c("ds", "y")
df$cap<-24*60
head(df)
#The change point entered is when I moved to Indianapolis
#after moving I spent a lot more time on my phone staying in touch with friends and family
mod<-prophet(df, yearly.seasonality = F, daily.seasonality = F, changepoints = mdy(c("5-20-2018")))

future <- make_future_dataframe(mod, periods = 365)
forecast<-predict(mod, future)
head(forecast)

plot(mod, forecast)
prophet_plot_components(mod, forecast)
