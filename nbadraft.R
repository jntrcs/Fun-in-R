teams<-data.frame(rep("not me", 30), 1:30, rep(1, 30))
names(teams)<-c("Team", "wins", "Group")
teams$Team<-as.character(teams$Team)
teams$Team[teams$wins==10]<-"me"

d<-vector(length=100000)
for (i in 1:100000)
{
  d[i]<-0
  teams$Group<-sample(c(1,2), 30, replace=TRUE)
  mynum<-teams$Group[teams$Team=="me"]
  mygroup<-teams[teams$Group==mynum,]
  if (mynum==2)
    d[i]<-30-nrow(mygroup)
  d[i]=d[i]+which(mygroup$Team=="me")
}
mean(d)
max(d)
hist(d)
table(d)
