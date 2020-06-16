library(VGAM)
library(icda)
library(reshape2)
data(alligators2)

Fish=c(23,7,5,13,5,8,16,17)
Invertebrate=c(4,0,11,8,11,7,19,1)
Reptile=c(2,1,1,6,2,6,1,0)
Bird=c(2,3,0,1,1,3,2,1)
Other=c(8,5,3,0,5,5,3,3)

Lake=rep(c("Hancock","Oklawaha","Trafford","George"),each=2)
Size=rep(c("S","L"),times=4)

mod=vglm(cbind(Fish,Invertebrate,Reptile,Bird,Other)~Lake+Size,family=multinomial)
summary(mod)
