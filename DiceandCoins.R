##Coins, dice, and board games
hit<-vector()
for (i in 1:100000)
{
  space<-0
  while (space<6)
  {
    space<-space+sample(1:6, size=1)
    hit<-c(hit,space)
  }
}
hit<-hit[hit<7]
hist(hit)
b<-table(hit)/100000
all<-expand.grid(1:6, 1:6, 1:6, 1:6, 1:6, 1:6)
all<-as.matrix(all)

p1<-1/6
p2<-1/6+1/36
p3<-1/6+2/36+1/6^3
p4<-1/6+1/6*sum(p1,p2,p3)
p5<-1/6+1/6*sum(p1,p2,p3,p4)
p6<-1/6+1/6*sum(p1,p2,p3,p4,p5)
probsHit<-c(p1,p2,p3,p4,p5,p6)
for (i in 7:10000)
{
  probsHit[i]<-1/6*sum(probsHit[(i-6):(i-1)])
}
order(probsHit, decreasing = TRUE)[1:50]
sum(probsHit[c(6,12,10)])
head(probsHit, 12)
