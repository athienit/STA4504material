Race=rep(c("White","Black","Other"),each=2)
Belief=rep(c("Yes","No"),3)
count=c(1339,300,260,55,88,22)
after=data.frame(Race,Belief,count)
xtabs(count~.,data=after)

after=transform(after,Race=relevel(Race,"Other"))
after
B_R=glm(count~Belief+Race,family=poisson(link=log),data=after)
summary(B_R)

# saturated model
BR=glm(count~Belief*Race,family=poisson(link=log),data=after)
summary(BR)

anova(B_R,BR,test="Chisq")

# Dissimilarity Index
sum(abs(after$count-fitted(B_R)))/(2*sum(after$count))