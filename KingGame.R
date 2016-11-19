peasants<-function(pop)
{
everyone<-1:pop
counter<-0
while (length(everyone)>1)
{
  counter<-counter+1
  a<-sample(everyone, length(everyone), replace=TRUE)
  while(any(a==which(a>0)))
        {a[a==which(a>0)]<-sample(everyone, sum(a==which(a>0)), replace=TRUE)}
        
  everyone<-everyone[!everyone%in%a]
}
length(everyone)==1
}
c<-sapply(1:10000, FUN=function(i){peasants(56000)})
d<-sapply((1:100), FUN=function(n){sum(sapply(1:1000, FUN=function(i){peasants(n)}))})
plot(d)
