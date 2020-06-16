odds.ratio=function(x,addtocounts=0){
x=x+addtocounts
(x[1,1]*x[2,2])/(x[1,2]*x[2,1])
}

data(UCBAdmissions)
dimnames(UCBAdmissions)
ftable(UCBAdmissions,row.vars="Dept",col.vars=c("Gender","Admit"))
#---------------------------------------------------#
########## Start with exploratory analysis ##########
#---------------------------------------------------#

ftable(round(prop.table(UCBAdmissions,c(2,3)),2),row.vars="Dept",col.vars=c("Gender","Admit"))
round(apply(UCBAdmissions,3,odds.ratio),2) # individual departmental odds ratio

UCBGbyA=margin.table(UCBAdmissions,c(2,1))
UCBGbyA
round(prop.table(UCBGbyA,1),2)
odds.ratio(UCBGbyA) # Marginal odds ratio. 
# Note that Simpson's paradox is present, will be illutrated better when GLM is fitted and we obtain
# conditional odds ratio e^beta

# Test for independence between admission status and gender
chisq.test(UCBGbyA,correct=FALSE)

round(prop.table(margin.table(UCBAdmissions,c(2,3)),1),2)
round(prop.table(margin.table(UCBAdmissions,c(1,3)),2),2)

# Most males apply to Dept A and B where acceptance has a higher rate while more females apply to
# Dept C,D,E,F where acceptance is lower

#----------------------------------------#
########## Perform GLM analysis ##########
#----------------------------------------#

Dept=rep(c("A","B","C","D","E","F"),each=2)
Gender=rep(c("Male","Female"),6)
Counts=matrix(UCBAdmissions,ncol=2,byrow=TRUE,dimnames=list(NULL,c("Admit","Reject")))
berk=data.frame(Dept,Gender,Counts);berk

#------ Model 1
berk=transform(berk,Dept=relevel(Dept,"F"))
UCB.logit=glm(cbind(Admit,Reject)~Gender+Dept,family=binomial(link=logit),data=berk)
summary(UCB.logit)

# Note that conditional odds ratio of acceptance with gender conditional on dept is exp(-0.999)=0.9
# compared to the marginal 1.84 earlier

# Also the G^2 statistic with df=5 (for Ho: Model Holds) indicates a lack of fit
1-pchisq(UCB.logit$deviance,UCB.logit$df.residual)

## Residual analysis ##

p.residuals=residuals(UCB.logit,type="pearson");round(p.residuals,2)
round(rstandard(UCB.logit,type="pearson"),2) # standardized pearson
# round(p.residuals/sqrt(1-lm.influence(UCB.logit)$hat),2) 
round(rstandard(UCB.logit),2) # standardized deviance

#------ Model 2
# Remove Gender

UCBnoG.logit=update(UCB.logit,.~.-Gender)
summary(UCBnoG.logit)

# Wald test indicated removal of gender predictor but so does likelihood ratio
1-pchisq(UCBnoG.logit$deviance-UCB.logit$deviance,UCBnoG.logit$df.residual-UCB.logit$df.residual)
#anova(UCBnoG.logit,UCB.logit,test="Chisq")

# Also the G^2 statistic with df=6 (for Ho: Model Holds) indicates a lack of fit
1-pchisq(UCBnoG.logit$deviance,UCBnoG.logit$df.residual)

## Residual analysis ##

round(rstandard(UCBnoG.logit,type="pearson"),2) # standardized pearson
round(rstandard(UCBnoG.logit),2) # standardized deviance

#------ Model 3
# From the residuals we see that Dept A does not fit well, so let's see what
# happens when we remove it

UCBnoGA.logit=glm(cbind(Admit,Reject)~Dept,family=binomial(link=logit),data=berk,subset=(Dept!="A"))
summary(UCBnoGA.logit)

# Notice the goodness of fit G^2 is 2.6815 now and also the residuals are much better

round(rstandard(UCBnoGA.logit,type="pearson"),2)# standardized pearson
round(rstandard(UCBnoGA.logit),2) # standardized deviance
