Gender=rep(c("Female","Male"),each=2)
Party=rep(c("Democrat","Republican"),2)
VLib=c(44,18,36,12)
SLib=c(47,28,34,18)
Mod=c(118,86,53,62)
SCon=c(23,39,18,45)
VCon=c(32,48,23,51)
ideow=data.frame(Gender,Party,VLib,SLib,Mod,SCon,VCon)

ideow

library(VGAM)
ideo.cl1=vglm(cbind(VLib,SLib,Mod,SCon,VCon) ~ Gender + Party,
       family=cumulative(parallel=TRUE), data=ideow)
summary(ideo.cl1)
confint(ideo.cl1)

ideo.cl2=vglm(cbind(VLib,SLib,Mod,SCon,VCon) ~ Gender*Party,
              family=cumulative(parallel=TRUE), data=ideow)
summary(ideo.cl2)

1-pchisq(3.993,2)