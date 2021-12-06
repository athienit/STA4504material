crabs=read.table("http://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=TRUE)
attach(crabs)

############## Part (I) 1 variable

fit=glm(y ~ weight, family=binomial(link=logit),data=crabs)
summary(fit)

# Can create Wald CI for beta by 
beta_h=coef(fit)[2]
se.beta_h=sqrt(vcov(fit)[2,2])
beta_h+c(-1.96,1.96)*se.beta_h
# Can create Likelihood Ratio CI for beta by
confint(fit,"weight")

# Consequenyly, CI for the odds ratio for x increasing by 0.1kg is
exp(0.1*(beta_h+c(-1.96,1.96)*se.beta_h)) # using Wald CI
# or
exp(0.1*confint(fit,"weight")) # using LR

# Predict probability of success for female with weight 2.4kg
eta=predict(fit,newdata=data.frame(weight=2.4),type="link",se.fit=TRUE)
eta
# Note that se.fit is 
sqrt(vcov(fit)[1,1]+2.4^2*vcov(fit)[2,2]+2*2.4*vcov(fit)[1,2])

eta.ci=eta$fit+c(-1,1)*qnorm(0.975)*eta$se.fit
eta.ci # This is (l,u) interval
plogis(eta.ci) # This is (exp(l)/(1+exp(l)),exp(u)/(1+exp(u)))

plot(c(1,4),c(0,1),type="n",xlab="Weight(kg)",ylab="Prop. having satellites")
ind=order(weight)
lines(weight[ind], fit$fitted.values[ind],type="l",lty=1)
lines(y ~ weight,type="p")

############### Part (II) variables (WEIGHT, COLOR)
 
###### A
crabs$color=factor(crabs$color, labels=c("ML","M","MD","D"))  #  treat color as a factor
crabs$color=relevel(crabs$color,4) 
fit2=glm(y ~ weight + color, family=binomial(link=logit),data=crabs)
summary(fit2)
vc=vcov(fit2);vc

# CI on pi for crab weight 2.4 color=dark
logit_CI=sum(coefficients(fit2)*c(1,2.4,0,1,0))+c(-1,1)*1.96*sqrt(vc[1,1]+2.4^2*vc[2,2]+vc[4,4]+2*2.4*vc[1,2]+2*vc[1,4]+2*2.4*vc[2,4])
plogis(logit_CI) #exp^(logit_CI)/(1+e^(logit_CI))
logit_CI1=predict(fit2,newdata=data.frame(weight=2.4,color="M"),type="link",se.fit=T)
plogis(logit_CI1$fit+c(-1,1)*1.96*logit_CI1$se.fit)

## Plot of probabilities
cols=rainbow(3)
curve(plogis(fit2$coefficients[1]+fit2$coefficients[2]*x), from=1,to=5.5,lwd=2,ylab="probability",xlab="weight",main="Color as Categories, probability")
for (i in 1:3)
	curve(plogis(fit2$coefficients[1]+fit2$coefficients[2+i]+fit2$coefficients[2]*x), from=1,to=5.5,lwd=2,col=cols[i],add=TRUE)
legend(3.5,.6,col=c(cols,"black"),lwd=2,legend=c("Medium Light", "Medium", "Medium Dark","Dark"),bg = "light gray")

# TEST WHETHER COLOR IS SIGNIFICANT
1-pchisq(fit$deviance-fit2$deviance,fit$df.residual-fit2$df.residual)
# or use 
anova(fit2,fit,test="LRT")

###### B
crabs$dark=ifelse(unclass(crabs$color)==4,1,0)
fit2.2=glm(y ~ weight + dark, family=binomial(link=logit),data=crabs)
summary(fit2.2)

## Plot of probability
curve(exp(fit2.2$coefficients[1]+fit2.2$coefficients[2]*x)/(1+exp(fit2.2$coefficients[1]+fit2.2$coefficients[2]*x)), from=1,to=5.5,ylim=c(0,1),lwd=2,col=c("red"),ylab="probability",main="Color as Binary Variable, probability")
curve(exp(fit2.2$coefficients[1]+fit2.2$coefficients[2]*x+fit2.2$coefficients[3])/(1+exp(fit2.2$coefficients[1]+fit2.2$coefficients[2]*x+fit2.2$coefficients[3])), from=1,to=5.5,lwd=2,col=c("black"),add=TRUE)
legend(3.5,.5,col=c("red","black"),lwd=2,legend=c("Not Dark","Dark"),bg = "light gray")

1-pchisq(fit$deviance-fit2.2$deviance,fit$df.residual-fit2.2$df.residual)

###### C
linear=unclass(color)  #  convert back to integer levels
fit2.3=glm(y ~ weight + linear, family=binomial(link=logit),data=crabs)
summary(fit2.3)

# TEST WHETHER COLOR IS SIGNIFICANT
1-pchisq(fit$deviance-fit2.3$deviance,fit$df.residual-fit2.3$df.residual)

## Plot of probabilities
par(mfrow=c(1,2))
cols=rainbow(3)
curve(plogis(fit2$coefficients[1]+fit2$coefficients[2]*x), from=1,to=5.5,lwd=2,ylab="probability",xlab="weight",main="Color as Categories, probability")
for (i in 1:3)
  curve(plogis(fit2$coefficients[1]+fit2$coefficients[2+i]+fit2$coefficients[2]*x), from=1,to=5.5,lwd=2,col=cols[i],add=TRUE)
legend(2.5,.3,col=c(cols,"black"),lwd=2,legend=c("Medium Light", "Medium", "Medium Dark","Dark"),bg = "light gray")


curve(exp(fit2.3$coefficients[1]+fit2.3$coefficients[2]*x+fit2.3$coefficients[3]*4)/(1+exp(fit2.3$coefficients[1]+fit2.3$coefficients[2]*x+fit2.3$coefficients[3]*4)), from=1,to=5.5,lwd=2,ylab="probability",xlab="weight",main="Color as Linear Variable, probability")
for (i in 1:3)
	curve(exp(fit2.3$coefficients[1]+fit2.3$coefficients[2]*x+fit2.3$coefficients[3]*i)/(1+exp(fit2.3$coefficients[1]+fit2.3$coefficients[2]*x+fit2.3$coefficients[3]*i)), from=1,to=5.5,lwd=2,col=cols[i],add=TRUE)
legend(2.5,.3,col=c(cols,"black"),lwd=2,legend=c("Medium Light", "Medium", "Medium Dark","Dark"),bg = "light gray")


###### D
# Interaction term
fit2.4=glm(y ~ weight*dark, family=binomial,data=crabs)
summary(fit2.4)
anova(fit2.4,fit2,test="LRT")

############### Part (III) Predictive Power
cor(y,fitted(fit)) #weight
cor(y,fitted(fit2)) #weight and color
cor(y,fitted(fit2.2)) #weight and dark
cor(y,fitted(fit2.3)) #weight and linear color

pihat <- vector(length=173)
for (i in 1:173){
  pihat[i] <-  predict.glm(update(fit2, subset=-i),
                           newdata=crabs[i,], type="response")
}
yhat <- as.numeric(pihat > 0.50)
#y <- as.numeric(horseshoecrabs$Satellites > 0)
confusion <- table(y, yhat)
confusion

############### Part (IV) ROC Curves
library(epiDisplay)
par(mfrow=c(1,1))
# concordance index
lroc(fit,grid=FALSE,graph=TRUE,line.col=2)$auc
title("ROC Curves")
lroc(fit2,grid=FALSE,graph=TRUE,add=TRUE,line.col=3)$auc
lroc(fit2.2,grid=FALSE,graph=TRUE,add=TRUE,line.col=4)$auc
lroc(fit2.3,grid=FALSE,graph=TRUE,add=TRUE,line.col=5)$auc
legend(0.6,0.5,col=c(2,3,4,5),lwd=1,
       legend=c("Weight","Weight+Color","Weight+Dark","Weight+Linear"),bg="light gray")

############### Part (V) variable selection
attach(crabs)
color=factor(color)
spine=factor(spine)

fit.wewi=glm(y ~ weight+width, family=binomial(link=logit))
vif(fit.wewi)

fitnone=glm(y~1,family=binomial)
fit.w=glm(y ~ width, family=binomial(link=logit))
fit.wcs=glm(y ~ width*color*spine, family=binomial(link=logit))
summary(fit.wcs)

fit.wc_ws_cs=update(fit.wcs,.~.-width:color:spine) #remove 3 way interaction since too complicated
fit.w_c_s=update(fit.wc_ws_cs,.~.-width:color-width:spine-color:spine)

#LRT for additive model vs model with all 2 way interactions
anova(fit.w_c_s,fit.wc_ws_cs)

library(MASS)
#backwards
drop1(fit.wc_ws_cs,test="LRT") #iterate this one, keep dropping
stepAIC(fit.wc_ws_cs,scope=list(upper=~width*color+width*spine+color*spine,lower=~1),direction="backward")

#forward
add1(fit.w,scope=~width*color*spine,test="LRT") #iterate this one, keep adding
stepAIC(fit.w,scope=list(upper=~width*color+width*spine+color*spine,lower=~1),direction="forward")

#both
stepAIC(fit.w,scope=list(upper=~width*color+width*spine+color*spine,lower=~1),direction="both")
        