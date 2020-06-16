Input=c("
  N  ND   HB GRP
10  1  4.1   1
11  4  3.2   1
12  9  4.7   1
 4  4  3.5   1
10 10  3.2   1
11  9  5.9   1
 9  9  4.7   1
11 11  4.7   1
10 10  3.5   1
10  7  4.8   1
12 12  4.3   1
10  9  4.1   1
8  8  3.2   1
11  9  6.3   1
6  4  4.3   1
9  7  3.1   1
14 14  3.6   1
12  7  4.1   1
11  9  4.8   1
13  8  4.7   1
14  5  4.8   1
10 10  6.7   1
12 10  5.2   1
13  8  4.3   1
10 10  3.9   1
14  3  6.3   1
13 13  4.4   1
4  3  5.2   1
8  8  3.9   1
13  5  7.7   1
12 12  5.0   1
10  1  8.6   2
3  1 11.1   2
13  1  7.2   2
12  0  8.8   2
14  4  9.3   2
9  2  9.3   2
13  2  8.5   2
16  1  9.4   2
11  0  6.9   2
4  0  8.9   2
1  0 11.1   2
12  0  9.0   2
8  0 11.2   3
11  1 11.5   3
14  0 12.6   3
14  1  9.5   3
11  0  9.8   3
3  0 16.6   4
13  0 14.5   4
9  2 15.4   4
17  2 14.5   4
15  0 14.6   4
2  0 16.5   4
14  1 14.8   4
8  0 13.6   4
6  0 14.5   4
17  0 12.4   4
")

## Data also include HB = mother's hemoglobin level

terat=read.table(textConnection(Input),header=TRUE)
terat$GRP=factor(terat$GRP)
terat$Litter=factor(1:58)
terat.binom=glm(cbind(ND,N-ND)~GRP, family=binomial, data=terat)
summary(terat.binom)
1-pchisq(173.45,df.residual(terat.binom)) # Gooodness of fit via LRT

X2=sum(resid(terat.binom,type="pearson")^2);X2
1-pchisq(X2,df.residual(terat.binom)) # Gooodness of fit via Pearson

X2/df.residual(terat.binom) # Evidence of overdispersion

#######################

######## Need data in ungrouped (binary) format for GEE (???):
Fail=terat$N-terat$ND
Succ=terat$ND

temp1=terat[,-c(1,2)];temp2=temp1
temp1=cbind(temp1,NA);temp2=cbind(temp2,NA)
colnames(temp1)[4]="Resp";colnames(temp2)[4]="Resp"

temp1$Resp="Dead"
temp2$Resp="Alive"

library(splitstackshape)
t1=expandRows(temp1,count=Fail,count.is.col = F)
t2=expandRows(temp2,count=Succ,count.is.col = F)

teratbnry=rbind(t1,t2)
teratbnry$Litter=factor(teratbnry$Litter)
remove(t1,t2,temp1,temp2,Fail,Succ)

library(gee)
terat.gee <- gee((Resp == "Dead") ~ GRP, id = Litter,
      data = teratbnry, family = binomial,
      corstr = "exchangeable")

coef(summary(terat.gee))[,c("Estimate","Robust S.E.")]

# Big working correlation matrix (17 x 17), but
# all correlations equal with exchangeable struc:
terat.gee$working.correlation[1,2]


###### GLMM
# glmer can use grouped or ungrouped data.
library(lme4)
# Using grouped data
terat.glmm <- glmer(cbind(ND, N-ND) ~ GRP + (1|Litter),
        data = terat, family = binomial)
# Using ungrouped binary data
terat.glmm <-  glmer((Resp == "Dead") ~ GRP + (1|Litter),
        data = teratbnry, family = binomial)
summary(terat.glmm)
