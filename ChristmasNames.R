drawYouself<-function(n)
{ 
  #factorial(n) events;
  #1-1/2!+1/3!-1/4!+....(-1)^(n+1)/n!
  prob = 1;
  if (n==1)return(1)
  for (i in 2:n)
  {
    prob=prob+(-1)^(i+1)/factorial(i)
  }
  1-prob
}

#1-1/1
#2--1/2
#3-2/6
#4-9/24
drawYouself(1)
drawYouself(2)
drawYouself(3)
drawYouself(4)

4/6*2/4

18/24*12/18