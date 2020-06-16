homicide=data.frame(nvics=rep(c(0:6),2),
			race=rep(c("Black","White"),each=7),
			Freq=c(119,16,12,7,3,2,0,1070,60,14,4,0,0,1))

# Data entered in shorter format
xtabs(Freq~race+nvics,data=homicide)

homicide=transform(homicide,race=relevel(race,"White"))
#options(contrasts=c("contr.SAS","contr.poly"))

hom.poi=glm(nvics~race,family=poisson(link="log"),weights=Freq,data=homicide)
            #,contrasts=list(race=contr.treatment(2,base=2,contrasts=TRUE)))
summary(hom.poi)

sum(resid(hom.poi,type="pearson")^2)/(sum(homicide$Freq)-length(hom.poi$coefficients))

library(MASS)
hom.nb=glm.nb(nvics~race,weights=Freq,data=homicide)
summary(hom.nb)

sum(resid(hom.nb,type="pearson")^2)/(sum(homicide$Freq)-length(hom.nb$coefficients))

# Wald CI on e^beta
exp(confint.default(hom.nb))

# Likelihood CI
exp(confint(hom.nb))

# Entering data in form to obtain correct df
homicide2=homicide[rep(1:14,homicide$Freq),]
homicide2[1:20,]
hom.poi2=glm(nvics~race,family=poisson(link="log"),data=homicide2)
summary(hom.poi2)


