oneOfEach<-function()
{
  monsters<-0
  gems<-c(0,0,0)
  while(0%in%gems)
  {
    whichGem<-sample(c(1,2,3), 1, prob=c(3,2,1))
    gems[whichGem]<-gems[whichGem]+1
  }
  gems[1]
}

simul<-sapply(1:100000, FUN=function(i) oneOfEach())
hist(simul)
mean(simul)
table(simul)/length(simul)
max(simul)
sum(simul[simul==1])/10000

#Baseball
bsimul<-sapply(1:50000, FUN = function(i) max(rbinom(5,162,.5)))
hist(bsimul)
max(bsimul)
min(bsimul)
mean(bsimul)
