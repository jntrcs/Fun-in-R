onePoint<-function()
{
  anchor<-runif(1,-1,1)
  posNeg<-sample(c(-1,1),1)
  yval<-posNeg*sqrt(1-anchor^2)
  c(anchor, yval)
}

oneLine<-function()
{
  answer<-matrix(nrow=2, ncol=2)
  answer[1,]<-onePoint()
  answer[2,]<-onePoint()
  answer
}

test<-oneLine()
test
calcYInt(test)
calcSlope(test)

calcSlope<-function(line)
{
  m<-(line[2,2]-line[1,2])/(line[2,1]-line[1,1])
  m
}

calcYInt<-function(line)
{
  x<-line[1,1]
  y<-line[1,2]
  m<-calcSlope(line)
  y-m*x
}

makeChord<-function()
{
  line<-oneLine()
  slope<-calcSlope(line)
  b<-calcYInt(line)
  c(slope, b)
}
makeChord()

findIntercept<-function(chord1, chord2)
{
  m1<-chord1[1]
  b1<-chord1[2]
  m2<-chord2[1]
  b2<-chord2[2]
  x<-(b2-b1)/(m1-m2)
  y<-m1*x+b1
  c(x, y)
}
findIntercept(makeChord(), makeChord())

inUnitCircle<-function(point)
{
  point[1]^2+point[2]^2<=1
}

##The actual simulation to find probability of two chords intersecting
nreps<-500000
simul<-rep(FALSE, nreps)
for (i in 1:nreps)
{
  simul[i]<-inUnitCircle(findIntercept(makeChord(), makeChord()))
}
sum(simul)/nreps
##two random chords have a 1/3 chance of crossing!!

nreps<-100000
answer<-rep(0, nreps)
for (i in 1:nreps)
{
  answer[i]<-3+sample(c(0,0,1),1)+sample(c(0,0,1),1)+sample(c(0,0,1),1)
}
mean(answer)
