stairs<-function(n)
{
  g<-c(1,2,4)
  if (n>3)
  for (i in 4:n)
  {
    g[i]<-sum(g[(i-3):(i-1)])
  }
  g[n]
}

