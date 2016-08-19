data<-c(23,
        130,
        220,
        185,
        471,
        30,
        25,
        19,
        248)
lambda<-length(data)/sum(data)
lambda
mean(data)
1/lambda
hist(data, ylim=c(0,))
lines(1:500, dexp(1:500, lambda)*sum(data), type='l')


#Bootstrap that thing
computeStat<-function(data)
{
  stat <-c()
  for (i in 2:length(data))
  {
    stat<-c(stat,min(c(data[i],data[i-1])/max(c(data[i],data[i-1]))))
  }
  mean(stat)
}

originalStat<-computeStat(data)
originalStat
#make distribution
dist<-sapply(1:10000, FUN=function(n){computeStat(sample(data, length(data)))})
hist(dist)
abline(v=originalStat, col='RED',lwd=5)
pval = sum(dist>originalStat)/length(dist)
pval
min(dist)
max(dist)
computeStat(big)
head(dist,50)

#A simulation where you use the last draw as your mean of your next draw
correlateddata<-rexp(1,lambda)
for (i in 1:150)
{
correlateddata<-c(correlateddata, rexp(1, 1/correlateddata[i]))
}
correlateddata
originalStat<-computeStat(correlateddata)
originalStat
dist<-sapply(1:5000, FUN=function(n){computeStat(sample(correlateddata, length(data)))})
hist(dist)
abline(v=originalStat, col='RED',lwd=5)
pval = sum(dist>originalStat)/length(dist)
pval
min(dist)
max(dist)
