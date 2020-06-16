library(icda)
data(teens)

ftable(R + G + M ~ A + C, data = teens)

teens.df <- as.data.frame(teens)
ACM <- margin.table(teens, 1:3)
ACM.df <- as.data.frame(ACM)

teens.m6 <-
  glm(Freq ~ A*C + A*M + C*M + A*G + A*R + G*M + G*R,
      family = poisson, data = teens.df)
AC.AM.CM <- glm(Freq ~ A*C + A*M + C*M,
                  family = poisson, data = ACM.df)
coef(teens.m6)
coef(AC.AM.CM)
