cantstop<-function(runners)
{
  all<-rep(FALSE, 10000)
  for (i in 1:10000)
  {
    success<-FALSE
    dice<-sample(1:6, 4, replace=TRUE)
    possibilities<-c(dice[1]+dice[2], dice[1]+dice[3], dice[1]+dice[4],dice[2]+dice[3], dice[2]+dice[4], dice[3]+dice[4])
    for (b in 1:length(runners))
      {
      if (runners[b] %in% possibilities)
      {success<-TRUE}
      all[i]<-success
    }
  
  }
  mean(all)
}
