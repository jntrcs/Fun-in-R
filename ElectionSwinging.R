##Can you swing the election

a<-sapply(1:1000*2, FUN = function(n){dbinom(n/2, n, .5)})
plot(a)
plot(1/a^2)
          