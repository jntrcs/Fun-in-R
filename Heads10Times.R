#10 heads first

heads<-.5^2
?dgeom
first<-dgeom(seq(0,700000, by=2), prob=heads)
sum(first)
second<-dgeom(seq(1, 7000001, by=2), prob=heads)
tots<-sum(second)
totf<-sum(first)
tots+totf
totf-tots
curve(dgeom(x, prob=heads), xlim=c(0,10000))
dgeom(0, prob=heads)
