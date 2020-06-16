A=c(8,7,6,6,3,4,7,2,3,4)
B=c(9,9,8,14,8,13,11,5,7,6)
trt=factor(rep(c("A","B"),each=10))
wafers=data.frame(trt=trt,defects=c(A,B))
print(wafers)

wafers.lin=glm(defects~trt,family=poisson(link="identity"),data=wafers)
summary(wafers.lin)

wafers.log=glm(defects~trt,family=poisson(link="log"),data=wafers)
summary(wafers.log)
confint(wafers.log)

wafers_alt=transform(wafers,trt=relevel(trt,"B"))
wafers.log_alt=glm(defects~trt,family=poisson(link="log"),data=wafers_alt)
summary(wafers.log_alt)

##
sum((resid(wafers.log,type="pearson"))^2)
