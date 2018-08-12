setwd("C:/Users/jntrcs/Google\ Drive/Data\ Sets")
lds<-read.csv("ldsChurch.csv", stringsAsFactors = F)
names(lds)[1]<-"Year"
lds$Membership<-as.numeric(gsub(",","",lds$Membership))
plot(y=lds$Membership, x=lds$Year)
cor(lds$Membership, lds$Year)
mod<-lm(lds$Membership~lds$Year)
abline(mod)
plot(mod$residuals)

world<-read.csv("worldPop.csv")
world$Population<-as.numeric(gsub(",","",world$Population))

world95<-world[world$Year>=1995 & world$Year<=2016,]
world95<-world95[order(world95$Year),]
plot(world95$Population~world95$Year)

ratio<-(lds$Membership/world95$Population)*100
plot(ratio~world95$Year, main="Ratio of Mormons among World Population", xlab="Year", ylab="% of Global Population", type="l")

#world$NetChange<-as.numeric(gsub(",","",world$NetChange))
#plot(world$NetChange~world$Year)
lds$Stakes<-as.numeric(gsub(",","",lds$Stakes))
lds$Wards.and.Branches<-as.numeric(gsub(",","",lds$Wards.and.Branches))
lds$Full.Time.Missionaries<-as.numeric(gsub(",","",lds$Full.Time.Missionaries))
lds$Converts.Baptized<-as.numeric(gsub(",","",lds$Converts.Baptized))
lds$New.Children.of.Record<-as.numeric(gsub(",","",lds$New.Children.of.Record))
lds$Children.per.member<-lds$New.Children.of.Record/lds$Membership
lds$Temples.per.member<-lds$Temples.in.Operation/lds$Membership

 plot(lds$Membership/lds$Stakes)
 plot(lds$Membership/lds$Wards.and.Branches)
plot(lds$Converts.Baptized~lds$Full.Time.Missionaries)
plot(lds$Children.per.member~lds$Year)
plot(lds$Temples.per.member~lds$Year)
