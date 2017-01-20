bill<-function()
{
  who<-0
  rand<-2
  while (rand!=0)
  {
  rand<-sample(c(-1,1,0),1)
  who<-who+rand
  }
  who%%6
}

dat<-sapply(1:100000, FUN=function(n) bill())
table(dat)/100000
