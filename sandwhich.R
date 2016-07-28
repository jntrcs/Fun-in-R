square<-c()
for (i in 0:128)
{
  for (j in 0:128)
  {
    square<-rbind(square, c(i/16, j/16))
  }
}


square<-cbind(square, abs(8-square[,1]), abs(8-square[,2]))
dist<-c()
distfromcenter<-c()
for (x in 1:nrow(square))
{
  dist<-c(dist,min(square[x,]))
  distfromcenter<-c(distfromcenter,sqrt((square[x,1]-4)^2+(square[x,2]-4)^2))
}
square<-cbind(square, dist, distfromcenter)

plot(square, col=(square[,5]>=square[,6])+10, pch=4)
sum(square[,5]>=square[,6])/nrow(square)

