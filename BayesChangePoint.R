library(boot)
data(coal)
y <- tabulate(floor(coal[[1]]))
y <- y[1851:length(y)]
y<-c(rpois(100,5), rpois(100,10))
barplot(y,xlab="years", ylab="frequency of disasters")
# initialization
n <- length(y) # number of data elements to process
m <- 1000 # target length of the chain
# now set up blank 1000 element arrays for mu, lambda, and k
mu <- lambda <- k <- numeric(m)
L <- numeric(n) # likelihood fxn has one slot per year
k[1] <- sample(1:n,1) # pick 1 random year to start at
mu[1] <- 1
lambda[1] <- 1
b1 <- 1
b2 <- 1

# start at 2, so you can use initialization values as seeds
# and go through this process once for each of your m iterations
for (i in 2:m) {
  kt <- k[i-1] # start w/random year from initialization
  # set your shape parameter to pick mu from, based on the characteristics
  # of the early ("before") chunk of your data
  r <- .5 + sum(y[1:kt]) 
  # now use it to pick mu
  mu[i] <- rgamma(1,shape=r,rate=kt+b1) 
  # if you're at the end of the time periods, set your shape parameter
  # to 0.5 + the sum of all the frequencies, otherwise, just set the shape
  # parameter that you will use to pick lambda based on the later ("after")
  # chunk of your data
  if (kt+1 > n) r <- 0.5 + sum(y) else r <- 0.5 + sum(y[(kt+1):n])
  lambda[i] <- rgamma(1,shape=r,rate=n-kt+b2)
  # now use the mu and lambda values that you got to set b1 and b2 for next iteration
  b1 <- rgamma(1,shape=.5,rate=mu[i]+1)
  b2 <- rgamma(1,shape=.5,rate=lambda[i]+1)
  # for each year, find value of LIKELIHOOD function which you will 
  # then use to determine what year to hop to next
  for (j in 1:n) {
   # L[j] <- exp((lambda[i]-mu[i])*j) * (mu[i]/lambda[i])^sum(y[1:j])
  L[j]<-lambda[i]-mu[i]*j+sum(y[1:j])*log(mu[i]/lambda[i])
  }
  L <- exp(L)/sum(exp(L))
  # determine which year to hop to next
  k[i] <- sample(1:n,prob=L,size=1)
}
plot(k)
quantile(k[5:1000])
