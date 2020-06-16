crossover=matrix(c(12,49,10,15),2,2,byrow=TRUE,dimnames=list(Drug=c("S","F"),Placebo=c("S","F")))
crossover

#### PART A
# McNemar's Test
mcnemar.test(crossover,correct=FALSE)

# AN exact version that tests the odds ratio
require(exact2x2)
mcnemar.exact(crossover)

#### PART B
# Logistic 
library(gee)

Subject=factor(rep(1:86,each=2))
Treat=rep(c("Drug","Placebo"),86)
Resp=c(rep(c(1,1),12),rep(c(0,1),10),rep(c(1,0),49),rep(c(0,0),15))

crossm1=data.frame(Subject,Treat,Resp)
crossm1[1:12,]

crossm1=transform(crossm1, Treat=relevel(Treat, "Placebo"))
cross.gee1=gee(Resp ~ Treat, id=Subject, data=crossm1,family=binomial,
corstr="exchangeable")
summary(cross.gee1)

cross.gee2=gee(Resp ~ Treat, id=Subject, data=crossm1,family=binomial,
corstr="independence")
summary(cross.gee2)

