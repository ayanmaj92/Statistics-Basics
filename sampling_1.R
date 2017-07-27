library(SDSFoundations)
survey <- StudentSurvey
mean(survey$age)
sd(survey$age)
hist(survey$age)

sample(survey$age,size = 30)


myxbar <- rep(NA,1000)

for (i in 1:1000) {
  mysamp <- sample(survey$age, size = 30)
  myxbar[i] <- mean(mysamp)
}

hist(myxbar)
mean(myxbar)
sd(myxbar)
