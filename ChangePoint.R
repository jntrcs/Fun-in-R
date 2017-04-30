scrabble<-read.csv("R Git Stuff/Fun-in-R/scrabble.csv")
#scrabble<-scrabble[seq(1,1542000, by=20),]
scrabble$date<-as.Date(scrabble$date)
#plot(scrabble$loserscore~scrabble$date)
scrabble<-scrabble[scrabble$winnerscore>=1,]
scrabble<-scrabble[scrabble$loserscore>=1,]
scrabble<-scrabble[order(scrabble$date),]
require("xts")
require("changepoint")
#ts<-xts(scrabble$winnerscore,scrabble$date)
mvalue = cpt.mean(scrabble$loserscore, method='AMOC')
pt<-cpts(mvalue)
plot(mvalue)
scrabble[cpts(mvalue),]
 mean(scrabble$winnerscore[1:pt])
 mean(scrabble$winnerscore[pt:nrow(scrabble)])
 pt<-which(scrabble$date==as.Date("2006-06-01"))
#changepoint package was a failure
 
#BCP
require("bcp")
?bcp 
bayesmod<-bcp(scrabble$winnerscore)
scrabble[which(bayesmod$posterior.prob>=1),]

mo <- strftime(scrabble$date, "%m")
yr <- strftime(scrabble$date, "%Y")
dd <- data.frame(mo, yr, scrabble$winnerscore)
dd.agg <- aggregate(amt ~ mo + yr, dd, FUN = mean)

plot(density(scrabble$winnerscore))
lines(density(scrabble$loserscore))
ts(scrabble$winnerscore)


mo <- strftime(scrabble$date, "%m")
yr <- strftime(scrabble$date, "%Y")
dd <- data.frame(mo, yr, score=scrabble$loserscore+scrabble$winnerscore)

dd.agg <- aggregate(score ~ mo + yr, dd, FUN = function(dat)c(mean(dat), length(dat)))
dd.agg[,3:4]<-cbind(dd.agg[,3][,1], dd.agg[,3][,2])
names(dd.agg)<-c("Month", "Year", "score", "numGames")
plot(dd.agg$score[255:315])
abline(v=30)
dd.agg[275:295,]
plot(dd.agg$score)

#lm
score<-scrabble$winnerscore+scrabble$loserscore
mod<-lm(score~scrabble$date)
plot(sample(mod$residuals, 10000))

cutoff<-which(scrabble$date==as.Date("2006-03-04"))[1]
beforeMod<-lm(score[1:cutoff]~scrabble$date[1:cutoff])
afterMod<-lm(score[cutoff:length(score)]~scrabble$date[cutoff:length(score)])
beforeMod
afterMod
plot(score~scrabble$date)
abline(beforeMod, col="blue")
abline(afterMod, "green")
