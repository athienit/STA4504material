teens <-
  array(c(911,44,3,2, 538,456,43,279),
        dim = c(2,2,2),
        dimnames = list(cigs=c("yes","no"),
                        alc=c("yes","no"), mj=c("yes","no")))
 ## Next line just for Table 7.4. Not required.
teens <- aperm(teens, c(3,1,2))
teens <- as.table(teens)
ftable(teens, row.vars=c("alc","cigs"))

teens.df=as.data.frame(teens)
# teens.df

teens.df=transform(teens.df,
            cigs = relevel(cigs, "no"),
            alc = relevel(alc, "no"),
            mj = relevel(mj, "no"))

teens.AC.AM.CM <-
  glm(Freq ~ alc*cigs + alc*mj + cigs*mj,
      family=poisson, data=teens.df)
### Another way:
## teens.AC.AM.CM <-
## glm(Freq ~ alc*cigs*mj - alc:cigs:mj,
## family=poisson, data=teens.df)
summary(teens.AC.AM.CM)

# Goodness of fit seems...good
df.residual(teens.AC.AM.CM)
deviance(teens.AC.AM.CM)
X2=sum(residuals(teens.AC.AM.CM,type="pearson")^2);X2
1-pchisq(X2,1)

## test 3 way interaction, i.e vs saturated model
teens.ACM <- update(teens.AC.AM.CM, . ~ alc*cigs*mj)
anova(teens.AC.AM.CM, teens.ACM, test="Chisq")

## Next we check if any 2-way interactions can be removed
drop1(teens.AC.AM.CM, test="Chisq")

## test for conditional independence of A and C given M:
teens.AM.CM <- update(teens.AC.AM.CM, . ~ alc*mj + cigs*mj)
anova(teens.AM.CM, teens.AC.AM.CM, test="Chisq")

## fitted values for several different models fit to these data.
teens.AM.CM <-
  update(teens.AC.AM.CM, . ~ alc*mj + cigs*mj)
teens.AC.M <-
  update(teens.AC.AM.CM, . ~ alc*cigs + mj)
teens.A.C.M <-
  update(teens.AC.AM.CM, . ~ alc + cigs + mj)
teens.ACM <-
  update(teens.AC.AM.CM, . ~ alc*cigs* mj)
table.7.4 <-
  data.frame(predict(teens.A.C.M, type="response"))
table.7.4 <-
  cbind(table.7.4, predict(teens.AC.M, type="response"))
table.7.4 <-
  cbind(table.7.4, predict(teens.AM.CM, type="response"))
table.7.4 <-
  cbind(table.7.4, predict(teens.AC.AM.CM, type="response"))
table.7.4 <-
  cbind(table.7.4, predict(teens.ACM, type="response"))

table.7.4=signif(table.7.4, 3)
table.7.4=cbind(teens.df[,c("alc","cigs","mj")],table.7.4)
names(table.7.4) <-
  c("alc","cigs","mj",
    "(A,C,M)","(AC,M)","(AM,CM)","(AC,AM,CM)","(ACM)")
table.7.4

rstandard(teens.AC.AM.CM)


## Residual checking
# library(vcdExtra)
# standardized deviance residuals
# mosaic(teens.AC.AM.CM,~mj+cigs+alc,residuals_type = "rstandard")

# standardized deviance residuals
# mosaic(teens.AC.AM.CM,~mj+cigs+alc,residuals=rstandard(teens.AC.AM.CM,type="pearson"))


######################## PART II
exp(coef(teens.AM.CM)[5])

AM.CM.fitted <- teens
AM.CM.fitted[,,] <- predict(teens.AM.CM, type="response")
AM.CM.fitted[,"yes",]
AM.CM.fitted[,"no",]
AM.CM.fitted[,"yes",] + AM.CM.fitted[,"no",]

AM.CM.fitted["yes",,]
AM.CM.fitted["no",,]
AM.CM.fitted["yes",,] + AM.CM.fitted["no",,]