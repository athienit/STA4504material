Defendant=rep(c("White","Black"),each=2)
Victim=rep(c("White","Black"),2)
Yes=c(53,0,11,4)
No=c(414,16,37,139)
dpwide=data.frame(Defendant=Defendant, Victim=Victim, Yes=Yes, No=No)
dpwide

dp.fit1=glm(cbind(Yes,No)~Defendant+Victim,family=binomial,data=dpwide)
summary(dp.fit1)

exp(dp.fit1$coefficients[2])
exp(dp.fit1$coefficients[2]+c(-1,1)*1.96*sqrt(vcov(dp.fit1)[2,2]))

drop1(dp.fit1,test="LRT")
