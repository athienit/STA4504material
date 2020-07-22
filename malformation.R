Alcohol=c(0,0.5,1.5,4,7)
Absent=c(17066,14464,788,126,37)
Present=c(48,38,5,1,1)

cbind(Alcohol,Absent,Present)
y=NULL;alc=NULL
for(i in 1:5){
	y=c(y,c(rep(0,Absent[i]),rep(1,Present[i])))
	alc=c(alc,rep(Alcohol[i],Absent[i]+Present[i]))
}
plot(jitter(y,factor=0.6)~jitter(alc),type="p",ylab="Malformation",xlab="alcohol score")
# Linear model
malform.lin=glm(cbind(Present,Absent)~Alcohol,family=binomial(make.link("identity")))
summary(malform.lin)

curve(malform.lin$coefficients[1]+malform.lin$coefficients[2]*x,
	 from=0,to=7,lwd=2,ylab="probability",xlab="alcohol score",add=TRUE)

sum((resid(malform.lin,type="pearson"))^2)

# Logit model
malform.logit=glm(cbind(Present,Absent)~Alcohol,family=binomial(link=logit))
summary(malform.logit)
confint(malform.logit)

curve(exp(malform.logit$coefficients[1]+malform.logit$coefficients[2]*x)/
	(1+exp(malform.logit$coefficients[1]+malform.logit$coefficients[2]*x)),	
	from=0,to=7,col=2,lty=2,lwd=2,add=TRUE)
title("Model for Binomial data")
legend(5,0.8,c("Identity","Logit"),col=1:2,lty=1:2,lwd=2,bg="light gray")

sum((resid(malform.logit,type="pearson"))^2)

predict.glm(malform.logit,newdata=data.frame(Alcohol=1.5),type="response")

# Probit model
malform.probit=glm(cbind(Present,Absent)~Alcohol,family=binomial(link=probit))
summary(malform.probit)

curve(pnorm(malform.probit$coefficients[1]+malform.probit$coefficients[2]*x),	
      from=0,to=7,col="blue",lty=4,lwd=2,add=TRUE)
title("Model for Binomial data")
legend(5,0.8,c("Identity","Logit","Probit"),col=c(1,2,"blue"),lty=c(1,2,4),lwd=2,bg="light gray")

sum((resid(malform.probit,type="pearson"))^2)

# Residuals on logit
residuals(malform.logit,type="pearson")
residuals(malform.logit,type="pearson")/sqrt(1 - hatvalues(malform.logit))
residuals(malform.logit,type="deviance")
residuals(malform.logit,type="deviance")/sqrt(1 - hatvalues(malform.logit))
rstandard(malform.logit)