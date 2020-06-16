depression=read.table("http://www.stat.ufl.edu/~athienit/STA4504/depression.txt",header=TRUE)
# Mixed Model
library(lme4)
dep.lme=glmer((response == "normal") ~ severity + drug*time + (1|subject),
		data=depression, family=binomial,
		contrasts=list(drug=contr.treatment(2,base=2,contrasts=TRUE)))
summary(dep.lme)
