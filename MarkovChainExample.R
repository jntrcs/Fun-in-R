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
N<-solve(I-Q)
sum(N[1,]) #Expected number of steps to go from state 1 to absorbing state 5
P
R<-P[1:4,5] #Canonical Form
N%*%R #probability that it is absorbed into state 5 from state i

P5<-matrix(c(0,1,0,0,0,0,0,0,.4,.3,.3,0,0,0,0,0,
             .4,.4,.2,0,0,0,0,.3,.3,.1,.3,0,0,0,0,0,.7,.3,
             0,0,0,0,0,.2,.6,.2,0,0,0,0,0,0,1), 
           byrow = T, nrow=7)
N5<-solve(diag(6)-P5[-7,-7])

P3<-matrix(c(0,1,0,0,2/3,1/3,0,0,1), byrow=T, nrow=3)
N3<-solve(diag(2)-P3[-3,-3])
N3
