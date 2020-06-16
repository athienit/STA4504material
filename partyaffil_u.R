### Readind data
dat1=matrix(c(762, 327, 468, 484, 239, 477),2,3, byrow = T,
		dimnames=list(c("F","M"),c("DEM","IND","REP")))
dat1

gender=rep(c("F","M"),each=3)
party=rep(c("DEM","IND","REP"),2)
count=c(762, 327, 468, 484, 239, 477)
dat2=data.frame(gender,party,count)
dat2
xtabs(count~gender+party,data=dat2)

### Marginal Totals
margin.table(dat1,1)
apply(dat1,1,sum)

margin.table(dat1,2)
addmargins(dat1)

### Proportions
# Joint
prop.table(dat1)
# Row props
prop.table(dat1,1)
# Column props
prop.table(dat1,2)

### Test for Independence
# X^2 test
dat1.chisq=chisq.test(dat1);dat1.chisq
dat1.chisq$expected
dat1.chisq$residuals
dat1.chisq$stdres

# G^2 test
G2=2*sum(dat1.chisq$observed*log(dat1.chisq$observed/dat1.chisq$expected))
G2
1-pchisq(G2,2)

##### Partinioning chi-squared #####
dat1.1=chisq.test(dat1[,c("DEM","IND")],correct=FALSE);dat1.1
G2.1=2*sum(dat1.1$observed*log(dat1.1$observed/dat1.1$expected));G2.1

dat1.2=chisq.test(cbind(dat1[,"DEM"]+dat1[,"IND"],dat1[,"REP"]),correct=FALSE)
dat1.2
G2.2=2*sum(dat1.2$observed*log(dat1.2$observed/dat1.2$expected));G2.2

dat1.1$statistic+dat1.2$statistic
G2.1+G2.2


