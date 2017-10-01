##Doctored coin
#"You have two identical coins. One flips heads 60% of the time and the other is fair. If you can flip both coins
#at the same time, how many flips do you need to observed to have a 95% chance of correctly identifying the doctored coin

plot(dbinom(0:134,134,.6))
points(dbinom(0:134, 134,.5), col="red")

weighted_coin<-function(p=.6, conf=.95){
  if (p>1 | p<.5){
    print("p must be between .5 and 1")
    return(NaN)
  }
  if (conf>1|conf<0)
  {
    print("conf must be between 0 and 1")
    return(NaN)
  }
#Your strategy: always guess the coin with higher heads. If tied just choose one
flips<-0
probs<-0
while(probs<conf){
probs<-0
flips<-flips+1
for (i in 0:flips){
  probs<-probs + .5*dbinom(i, flips, p)*dbinom(i, flips, .5) #add in probability of guessing right given that they're tied
  probs<-probs+dbinom(i, flips, .5)*pbinom(i, flips, p, lower.tail = F) #probability that weighted coin is higher than unweighted
  
}
print(paste(flips, probs))
}

}
