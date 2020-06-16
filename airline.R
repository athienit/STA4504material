# Sources: 
#  MIT Airline Data Project
#  Wikipedia
ASM=c(829581, 862621, 884192, 898359, 945245, 980769, 953875, 914901, 922277, 998868, 1028621, 1027553, 1060116, 1040840, 975307, 991934, 1012597, 1012261, 1025616, 1048107, 1090198, 1131694, 1168055)
Fatalities=c(1828, 2796, 1768, 1721, 1150, 1586, 1539, 1418, 1233, 767, 1463, 1298, 981, 952, 1108, 1130, 828, 800, 459, 1328, 898, 629, 399)
Year=1995:2017

air_deaths=data.frame(Fatalities,ASM,Year)

air.poisson=glm(Fatalities~I(Year-1995),family=poisson,data=air_deaths,offset=log(ASM))
summary(air.poisson)

air.nb=glm.nb(Fatalities~I(Year-1995)+offset(log(ASM)),data=air_deaths)
summary(air.nb)
