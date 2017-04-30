 setwd("C://Users//jntrcs//Downloads")
dat<-read.csv("fuelup.csv")
#remove partial fill ups
dat<-dat[dat$partialFuelup==0,]

dat$fuelupDate<-as.Date(dat$fuelupDate)
#calculate days between fill ups
#I will use this as a rough approx. of my ratio of city to freeway driving
dat$DaysBetween<-c(NA,dat$fuelupDate[2:nrow(dat)]-dat$fuelupDate[1:nrow(dat)-1])
#remove fill ups when we don't know days between or MPG
dat<-dat[!is.na(dat$mpg) & !is.na(dat$DaysBetween),]

a<-lm(dat$mpg~dat$DaysBetween)
abline(a)
plot(a$residuals)
plot(density(dat$mpg))

library(R2jags)
library(coda)
modelJAGS <- "
model {
for(i in 1:40){
mpg[i] ~ dnorm(mu[i], sigma);
mu[i] <- beta1*Days[i]+ w[type[i]];
}
for (j in 1:10)
{
w[j]~dnorm(a, b);
}
beta0 ~ dnorm(25,10);
beta1 ~ dnorm(0,10);
a~dgamma(4,.25);
b~dgamma(1,1);
sigma~dgamma(1,1);
}"
 
 writeLines(modelJAGS,"GasData.txt")
 
 mpg <- dat$mpg
Days<-dat$DaysBetween
type<-dat$fuelBrand

 data.jags <- c('mpg','Days', 'type')
 parms <- c('beta1','w', "sigma", 'a', 'b')
 #inits=NULL takes a random deviate from the prior as your starting values
 logisticReg.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,model.file="GasData.txt",n.iter=52000,n.burnin=2000,n.chains=1,n.thin=1) 
 sims <- as.mcmc(logisticReg.sim)
 chains <- as.matrix(sims)
 head(chains)
 a<-chains[,1]
 b<-chains[,2]
 beta1<-chains[,3]
 plot(b[1:6000])
 plot(density(b))
means<-apply(chains,2,mean)
postGasBrand<-rnorm(50000,a,b)
plot(density(postGasBrand))
max(postGasBrand)
abline(v=means[7:17])
plot(beta1[15000:20000])
plot(density(beta1))
