library(gee)

depression=read.table("http://users.stat.ufl.edu/~aa/cat/data/Depression.dat",header=TRUE)
depression$severity=factor(depression$severity,levels=c(0,1),labels=c("mild","severe"))
depression$drug=factor(depression$drug,levels=c(0,1),labels=c("std","new"))
depression$response=factor(depression$outcome,levels=c(0,1),labels=c("abnormal","normal"))

head(depression)
dep.gee1=gee((response == "normal") ~ severity + drug*time,
		id=case, data=depression, family=binomial, corstr="exchangeable")
summary(dep.gee1)

dep.gee2=gee((response == "normal") ~ severity + drug*time,
		id=case, data=depression, family=binomial)
summary(dep.gee2)