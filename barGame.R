simulateOneGame<-function(x,y)
{
  position<-0
  count<-0
  while(position!=x&position!=y)
  {
    flip<-sample(c(-1,1),1)
    position=position+flip
    count=count+1
  }
  count
}

simulateOneGame(1,-1)

results<-matrix(nrow=10, ncol=10)
for (x in 1:10)
{
  for (y in 1:10)
  {
    results[x,y]<-mean(sapply(1:1000, FUN=function(n){simulateOneGame(-x,y)}))
  }
    }
results
?sapply

test<-sapply(1:100, FUN=function(n){simulateOneGame(-100,100)})
hist(test)
mean(test)
table(test)
sum(test>1)

error<-matrix(nrow=10, ncol=10)
for (x in 1:10)
{
  for (y in 1:10)
  {
    error[x,y]<-results[x,y]-x*y
  }
}
error
which.max(abs(error))
