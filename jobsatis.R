income=c(3,10,20,30)
jobsat=c("VD","LD","MS","VS")
table.sat=expand.grid(income=income,jobsat=jobsat)
counts=c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8)
table.sat=cbind(table.sat,count=counts)
temp=xtabs(count~income+jobsat,table.sat)
temp



###--- Multinomial Baseline-Logit ---###
########################################
library(VGAM)
# May read data as individual trials (see table.sat1) but the deviance is 205 on df=306
# as the saturated model is "different"
#table.sat1=structure(.Data=table.sat[rep(1:nrow(table.sat),temp),],row.names=1:104)[,1:2]
#fit.blogit=vglm(jobsat~income,multinomial,data=table.sat1)
#summary(fit.blogit)

# Read data in easiest form to get correct goodness of fit
income=c(3,10,20,30)
VD=c(2,2,0,0)
LD=c(4,6,1,3)
MS=c(13,22,15,13)
VS=c(3,4,8,8)
dat=data.frame(income,VD,LD,MS,VS)

fit.blogit=vglm(cbind(VD,LD,MS,VS)~income,family=multinomial,data=dat)
summary(fit.blogit)
sum(resid(fit.blogit,type="pearson")^2)

vglm(cbind(VD,LD,MS,VS)~1,family=multinomial,data=dat)

###--- Multinomial Cumulative Logit ---###
##########################################

# Assuming parallel lines
fit.clogit1=vglm(cbind(VD,LD,MS,VS)~income,family=cumulative(parallel=TRUE),data=dat)
summary(fit.clogit1)
1-pchisq(deviance(fit.clogit1),df.residual(fit.clogit1))

vglm(cbind(VD,LD,MS,VS)~1,family=cumulative(parallel=TRUE),data=dat)

# Plot of P(Y<=j|x)
cols=rainbow(3)
coef=coefficients(fit.clogit1)
curve(exp(coef[1]+coef[4]*x)/(1+exp(coef[1]+coef[4]*x)),from=3,to=30,lwd=2,ylim=c(0,1),col=cols[1],ylab="probability",xlab="income",main="Color as Categories, probability")
for(i in 2:3)
	curve(exp(coef[i]+coef[4]*x)/(1+exp(coef[i]+coef[4]*x)), from=3,to=30,lwd=2,col=cols[i],add=TRUE)
legend(20,1,col=c(cols,"black"),lwd=2,legend=c(expression(P(Y<=1)),expression(P(Y<=2)),expression(P(Y<=3))),bg = "light gray")


# Assuming NON-parallel lines
fit.clogit2=vglm(cbind(VD,LD,MS,VS)~income,family=cumulative(parallel=FALSE),data=dat)
summary(fit.clogit2)
1-pchisq(deviance(fit.clogit2),df.residual(fit.clogit2))

# Plot of P(Y<=j|x)
coef2=coefficients(fit.clogit2)
curve(exp(coef2[1]+coef2[4]*x)/(1+exp(coef2[1]+coef2[4]*x)),from=3,to=30,lwd=2,ylim=c(0,1),col=cols[1],ylab="probability",xlab="income",main="Color as Categories, probability")
for(i in 2:3)
	curve(exp(coef2[i]+coef2[i+3]*x)/(1+exp(coef2[i]+coef2[i+3]*x)), from=3,to=30,lwd=2,col=cols[i],add=TRUE)
legend(20,1,col=c(cols,"black"),lwd=2,legend=c(expression(P(Y<=1)),expression(P(Y<=2)),expression(P(Y<=3))),bg = "light gray")
