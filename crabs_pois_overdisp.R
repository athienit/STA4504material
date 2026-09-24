crabs=read.table("https://raw.githubusercontent.com/alanagresti/categorical-data/master/Crabs.dat", header=TRUE)

############# L13 Overdispersion
fit.pois = glm(sat ~ weight, family=poisson, data=crabs)
sum(residuals(fit.pois,type="pearson")^2)/171

fit.quasi = glm(sat ~ weight, family=quasipoisson, data=crabs)
summary(fit.quasi)
sum(residuals(fit.quasi,type="pearson")^2)/171