amoeba<-function(reproduce=.5)
{
  die = 1-reproduce
  lived<-rep(F, 10000)
  for (i in 1:10000)
  {
    n=1
    for (j in 1:19)
    {
      day<-sample(c(T,F), n, prob=c(reproduce, die))
      n = 2* sum(day==T)
      if (n==0)
        {
        lived[i]<-F
        break
        }
    }
    if (n>0)
    {lived[i]=T}
  }
  mean(lived)
}

analytic<-function(p)
{
  if (p<=.5)
{    return(1)
}  
  if (.5<p&p<=1)
{    return ((1-p)/p)
}  
  else
{    
  return('Not a valid probability')
}
}

num<-numeric(10000)
num[1:100]<-0
startnum<-500
for (i in 1:10000)
{
    startnum<-rbinom(1,startnum,.51)*2
    num[i]<-startnum
    if (num[i]==0)
      break
}
plot(num[1:10000])
head(num, 20)
length(num[num>0])
max(num[1200])
