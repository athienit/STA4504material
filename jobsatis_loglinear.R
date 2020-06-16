income=c(3,10,20,30)
jobsat=c("VD","LD","MS","VS")
table.sat=expand.grid(income=income,jobsat=jobsat)
counts=c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8)
table.sat=cbind(table.sat,count=counts)
temp=xtabs(count~income+jobsat,table.sat)
temp

jobsat.ind=glm(count~factor(income)+jobsat,family=poisson(link=log),data=table.sat)
summary(jobsat.ind)

jobsat.sat=update(jobsat.ind,.~.+factor(income)*jobsat)
  glm(count~factor(income)*jobsat,family=poisson(link=log),data=table.sat)
#summary(jobsat.sat)

anova(jobsat.ind,jobsat.sat,test="Chisq")
