data(UCBAdmissions)

Dept=rep(rep(c("A","B","C","D","E","F"),each=2),2)
Gender=rep(rep(c("Male","Female"),6),2)
Count=matrix(UCBAdmissions,ncol=2,byrow=TRUE,dimnames=list(NULL,c("Admit","Reject")))
Admit=rep(c("Yes","No"),each=12)
Freq=c(Count[,1],Count[,2])
berk2=data.frame(Dept,Gender,Admit,Freq)
head(berk2)

UCB.loglin=glm(Freq~Admit*Gender+Admit*Dept+Gender*Dept,family=poisson,data=berk2)
summary(UCB.loglin)
