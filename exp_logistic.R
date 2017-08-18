library(SDSFoundations)
world <- WorldBankData


table(world$Country)

gbr2000 <- subset(world,world$Country=="United Kingdom" & world$year>=2000 & world$year<2010)

head(gbr2000)

time <- gbr2000$year - min(gbr2000$year)

mv <- gbr2000$motor.vehicles
length(mv)
length(time)

# Logistic Fit
logisticFit(time,mv,xlab = "Time",ylab = "Motor Vehicles")

#Exponential Fit
expFit(time,mv,xlab = "Time",ylab = "Motor Vehicles")

#Fit three models at once using tripleFit
tripleFit(time, mv,xlab = "Time",ylab = "Motor Vehicles")

#Predict outcome using expFitPred
expFitPred(time,mv,12)

#Predict outcome using logisticFitPred
logisticFitPred(time,mv,12)

##########################################

us <- world[world$Country=="United States",]
us_select <- us[us$year>=1990,]

us_select$internet.mil <- us_select$internet.users / 1000000
us_select$time <- us_select$year - min(us_select$year)
us_select_10 <- us_select[us_select$time<10,]

expFit(us_select_10$time, us_select_10$internet.mil)

logisticFit(us_select_10$time, us_select_10$internet.mil)

e <- expFitPred(us_select_10$time, us_select_10$internet.mil, 16)

l <- logisticFitPred(us_select_10$time, us_select_10$internet.mil, 16)

View(us_select)

################################

world$proportion <- world$internet.users/world$population

world_1990 <- world[world$year>=1990,]
world_1990$years_since_1990 <- world_1990$year - 1990 
denmark <- world_1990[world_1990$Country == "Denmark",]

expFit(denmark$years_since_1990,denmark$proportion)
logisticFit(denmark$years_since_1990,denmark$proportion)

################################

brazil <- world[world$Country=="Brazil",]
brazil_1995 <- brazil[brazil$year>=1995,]
brazil_1995$years_since_1995 <- brazil_1995$year - 1995
brazil_1995$mobile.users_mill <- brazil_1995$mobile.users / 1000000
brazil_1995[brazil_1995$years_since_1995==5,]
head(brazil_1995[brazil_1995$mobile.users_mill>100,],1)

plot(brazil_1995$years_since_1995,brazil_1995$mobile.users_mill,pch=8)
tripleFit(brazil_1995$years_since_1995,brazil_1995$mobile.users_mill)

l <- logisticFitPred(brazil_1995$years_since_1995,brazil_1995$mobile.users_mill,35)
l
