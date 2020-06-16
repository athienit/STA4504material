set.seed(723462)
n=30
x=sort(round(runif(n,0,100),1))

y=rep(0,n)
y[x>50]=1

cbind(x,y)
plot(x,y)
fit=glm(y~x,family=binomial)
summary(fit)
curve(plogis(fit$coefficients[1]+fit$coefficients[2]*x), from=0,to=100,lwd=1,add=T)

library(ProfileLikelihood)
dataf=data.frame(x,y)
xx=profilelike.glm(y~1,data=dataf,profile.theta="x",family=binomial,length=20,round=2)
profilelike.plot(theta=xx$theta, profile.lik.norm=xx$profile.lik.norm, round=2)