library(VGAM)
NTH=c(189,53,46)
PH=c(908,311,335)
VH=c(382,180,294)
Religion=1:3
happiness=data.frame(Religion,NTH,PH,VH)

happy.df=vglm(cbind(NTH,PH,VH)~Religion,
		family=cumulative(parallel=TRUE),data=happiness)
summary(happy.df)