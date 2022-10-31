movie=matrix(c(24,8,10,8,13,9,13,11,64),3,3)
dimnames(movie)=list(c("Con","Mixed","Pro"),c("Con","Mixed","Pro"))
print(movie)

library(psych)
cohen.kappa(movie)
sqrt(cohen.kappa(movie)$var.kappa)
sqrt(cohen.kappa(movie)$var.weighted)

cohen.kappa(movie)$weight

# Data in long format to be used in other packages

library(splitstackshape)
library(reshape2)
movie_l=melt(movie)
movie_l=expandRows(movie_l,"value")
dimnames(movie_l)[[2]]=c("Reviewer1","Reviewer2")

library(irr)
kappa2(movie_l)