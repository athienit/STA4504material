# --- Coke example ---
dbinom(0,3,0.6)

dbinom(0:3,3,0.6)

cbind(0:3,dbinom(0:3,3,0.6))

# Plot of the binomial distribution
plot(0:3,dbinom(0:3,3,0.6),type="h",xlab="y",ylab="p(y)")


# --- How a binomial distribution with large n, looks like a normal.
plot(0:20,dbinom(0:20,20,0.6),type="h",xlab="y",ylab="p(y)")


# Plot of likelihood function for coke example
curve(dbinom(1,3,x),xlim=c(0,1))