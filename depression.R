library(gee)

depression=read.table("http://www.stat.ufl.edu/~athienit/STA4504/depression.txt",
				header=TRUE)
head(depression)
dep.gee1=gee((response == "normal") ~ severity + drug*time,
		id=subject, data=depression, family=binomial, corstr="exchangeable",
		contrasts=list(drug=contr.treatment(2,base=2,contrasts=TRUE)))
summary(dep.gee1)

dep.gee2=gee((response == "normal") ~ severity + drug*time,
		id=subject, data=depression, family=binomial,
		contrasts=list(drug=contr.treatment(2,base=2,contrasts=TRUE)))
summary(dep.gee2)