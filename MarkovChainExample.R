nRep<-100000
dist<-numeric(nRep)
for (i in 1:nRep)
{
  balls<-1:4
  count<-0
  while(!all(balls==balls[1])){
    count<-count+1
    draws<-sample(1:4,2)
    balls[draws[2]]<-balls[draws[1]]
  }
  dist[i]<-count
}
hist(dist)
mean(dist)

##We can solve this problem using Markov Chain models
#The Transition matrix
P<-matrix(c(0,0,0,0,0,1,1/2,0,0,0,0,1/6,1/3,1/4,0,0,1/3
            ,2/3,1/2,0,0,0,0,1/4,1), nrow=5)
Q<-P[-5,-5]
I<-diag(4)
Expected<-solve(I-Q)
sum(Expected[1,]) #Expected number of steps to go from state 1 to absorbing state 5
