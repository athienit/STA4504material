library(DescTools)


# Revised BreslowDayTest() (arguments same as original function)
bdt <- function(data, OR = NA, correct = FALSE) {

  #set up empty table to match format of inputted data
  new_table <- xtabs(freq ~ .,
                      cbind(expand.grid(x=unlist(dimnames(data)[1]),
                                        y=unlist(dimnames(data)[2]),
                                        z=unlist(dimnames(data)[3])),
                            freq=c(0, 0, 0, 0, 0, 0, 0, 0)))
  
  #calculate expected cell counts
  for (k in 1:dim(data)[3]) {
    for (i in 1:dim(data)[1]) {
      for(j in 1:dim(data)[2]) {
        new_table[i,j,k] <- sum(data[,j,k])*sum(data[i,,k])/sum(data[,,])
      }
    }
  }

  #set up potential warning
  if(sum(new_table[,,]>5)/(dim(data)[1]*dim(data)[2]*dim(data)[3])
     < 0.8) {
    print(BreslowDayTest(data, OR, correct))
    warning("less than 80% of expected cell counts greater than 5")
  } else {
    BreslowDayTest(data, OR, correct)
  }

}


#Examples

# example of warning
migraine <- xtabs(freq ~ .,
                  cbind(expand.grid(treatment=c("active", "placebo"),
                                    response =c("better", "same"),
                                    gender   =c("female", "male")),
                        freq=c(16, 5, 11, 20, 12, 7, 16, 19))
                  )
print(migraine)

bdt(migraine)

# example of no warning
tuberculosis <- xtabs(freq ~ .,
                  cbind(expand.grid(treatment=c("active", "placebo"),
                                    response =c("better", "same"),
                                    gender   =c("female", "male")),
                        freq=c(20, 13, 16, 25, 20, 18, 20, 21))
                  )

print(tuberculosis)
bdt(tuberculosis)
