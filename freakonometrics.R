### From https://freakonometrics.hypotheses.org/8210

n=250
set.seed(1)
X1=rnorm(n)
X2=rnorm(n)
score=X1^ 2 +X2-1
proba=exp(score)/(1+exp(score))
Y=rbinom(n,1 ,proba)

reg=glm(Y~X1+X2,family=binomial)
plot(predict(reg),residuals(reg),col=c("blue","red")[1+Y])
abline(h=0,lty=2,col="grey")
lines(lowess(predict(reg),residuals(reg)),col="black",lwd=2)

library(splines)
rl=lm(residuals(reg)~bs(predict(reg),8))
#rl=loess(residuals(reg)~predict(reg))
y=predict(rl,se=TRUE)
segments(predict(reg),y$fit+2*y$se.fit,predict(reg),y$fit-2*y$se.fit,col="green")

# Now plot residuals vs each predictor

plot(X2,residuals(reg),col=c("blue","red")[1+Y])
lines(lowess(X2,residuals(reg)),col="black",lwd=2)
lines(lowess(X2[Y==0],residuals(reg)[Y==0]),col="blue")
lines(lowess(X2[Y==1],residuals(reg)[Y==1]),col="red")
abline(h=0,lty=2,col="grey")

plot(X1,residuals(reg),col=c("blue","red")[1+Y])
lines(lowess(X1,residuals(reg)),col="black",lwd=2)
lines(lowess(X1[Y==0],residuals(reg)[Y==0]),col="blue")
lines(lowess(X1[Y==1],residuals(reg)[Y==1]),col="red")
abline(h=0,lty=2,col="grey")

# Model including this quadratic effect

reg=glm(Y~X1+I(X1^2)+X2,family=binomial)
plot(predict(reg),residuals(reg),col=c("blue","red")[1+Y])
lines(lowess(predict(reg)[Y==0],residuals(reg)[Y==0]),col="blue")
lines(lowess(predict(reg)[Y==1],residuals(reg)[Y==1]),col="red")
lines(lowess(predict(reg),residuals(reg)),col="black",lwd=2)
abline(h=0,lty=2,col="grey")