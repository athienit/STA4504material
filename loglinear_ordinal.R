VD=c(2,2,0,0)
LD=c(4,6,1,3)
MS=c(13,22,15,13)
VS=c(3,4,8,8)

Freq=c(VD,LD,MS,VS)
income=rep(c(3,10,20,30),times=4)
satis=rep(1:4,each=4)

dat_job=data.frame(Freq,income,satis)
m=glm(Freq~factor(income)+factor(satis)+income:satis,family=poisson,data=dat_job)
summary(m)
