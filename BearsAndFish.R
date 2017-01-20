withtake<-function(fish)
{
  total<-fish[1]
  if (fish[2]>fish[1])
  {
    total<-total+fish[2]
    if (fish[3]>fish[2])
    {
      total<-total+fish[3]
    }
  }
  total
}

withouttake<-function(fish)
{
  total<-fish[2]
  if (fish[3]>fish[2])
  {
    total<-total+fish[3]
  }
  total
}

mapply(0:10, FUN = function(n){
  c(n/100, mean(sapply(1, FUN<-function(i){
    withtake(c(n/100,runif(2)))
  })))
})

sapply(1:10, FUN<-function(i){
  withtake(c(3/100,runif(2)))
})
