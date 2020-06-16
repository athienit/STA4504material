Input=("
Flight Temp Failure
1 66 0 
2 70 1 
3 69 0 
4 68 0 
5 67 0 
6 72 0 
7 73 0 
8 70 0 
9 57 1 
10 63 1 
11 70 1 
12 78 0 
13 67 0 
14 53 1 
15 67 0 
16 75 0
17 70 0
18 81 0
19 76 0
20 79 0
21 75 1
22 76 0
23 58 1
")

preC=read.table(textConnection(Input),header=TRUE)
plot(Failure~Temp,data=preC,xlim=c(35,85))

# Logit model
preC.logit=glm(Failure~Temp,family=binomial(link=logit),data=preC)
summary(preC.logit)
confint(preC.logit)
curve(exp(preC.logit$coefficients[1]+preC.logit$coefficients[2]*x)/
        (1+exp(preC.logit$coefficients[1]+preC.logit$coefficients[2]*x)),	
      from=36,to=85,col=2,lty=2,lwd=2,add=TRUE)
axis(1,36,col='red',labels=c("36"),cex.axis=1,font=3)
points(36,0.9987521,pch=2)
text(36,0.97,label=c("0.9987"),cex=0.8)


predict.glm(preC.logit,newdata=data.frame(Temp=36),type="response")
