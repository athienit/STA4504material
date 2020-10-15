depression=read.table("http://users.stat.ufl.edu/~aa/cat/data/Depression.dat",header=TRUE)
depression$severity=factor(depression$severity,levels=c(0,1),labels=c("mild","severe"))
depression$drug=factor(depression$drug,levels=c(0,1),labels=c("std","new"))
depression$response=factor(depression$outcome,levels=c(0,1),labels=c("abnormal","normal"))

# Mixed Model
library(lme4)
dep.lme=glmer((response == "normal") ~ severity + drug*time + (1|case),
		data=depression, family=binomial)
summary(dep.lme)
