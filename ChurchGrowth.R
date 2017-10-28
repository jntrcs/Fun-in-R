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
