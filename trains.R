Year=seq(2003,1975,by=-1)
KM=c(518,516,508,503,505,487,463,437,423,415,425,430,439,431,436,443,397,414,418,
     389,401,372,417,430,426,430,425,426,436)
Train=c(0,1,0,1,1,0,1,2,1,2,0,1,2,1,4,2,1,2,0,5,2,2,2,2,3,2,1,2,5)
TrRd=c(3,3,4,3,2,4,1,2,2,4,4,4,6,2,4,4,6,13,5,3,7,3,2,2,3,4,8,12,2)

traincollisions=data.frame(Year=Year,KM=KM,Train=Train,TrRd=TrRd)
traincollisions
trains.log=glm(TrRd~I(Year-1975),offset=log(KM),family=poisson(link=log),
		data=traincollisions)
summary(trains.log)

sum(resid(trains.log,type="pearson")^2)

attach(traincollisions)
plot(Year,1000*TrRd/KM,ylim=c(0,1000*max(TrRd/KM)),
		ylab="Collisions per Billion Train-Kilometers")
curve(1000*exp(-4.21-0.0329*(x-1975)),add=TRUE)
detach(traincollisions)

### Section on Overdispersion

sum(resid(trains.log,type="pearson")^2)
sum(resid(trains.log,type="pearson")^2)/trains.log$df.residual

# Negative Binomial
library(MASS)
trains.nb=glm.nb(TrRd ~ I(Year-1975) + offset(log(KM)),data=traincollisions)
summary(trains.nb)
