job=matrix(c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8),4,4,dimnames=list(c("<5","5k-15k","15k-25k",">25k"),c("Dissat","Little","Moderate","Very")))
addmargins(job)
job_test=chisq.test(job)
job_test$expected

library(DescTools)
GTest(job)

round(job_test$stdres,4)

fisher.test(job)