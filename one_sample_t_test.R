library(SDSFoundations)
bull <- BullRiders

age <- 2012 - bull$YearBorn

#See if the age distribution is approximately normal
hist(age)

#two-sided hypothesis test
t.test(age,mu = 30)

#one-sided hypothesis test
t.test(age,mu = 30, alternative = 'less')

######################################

colnames(bull)

usa <- bull[bull$Country=="USA",]

mean(usa$Weight)
sd(usa$Weight)

hist(usa$Weight, main='Histogram of US Bull Rider Weights',xlab='Weight (lbs)')

t.test(usa$Weight,mu = 190)

#######################################

sample_1 <- bull[bull$Events14>=5,]
mean(sample_1$RidePer14)
sd(sample_1$RidePer14)

hist(sample_1$RidePer14)

t.test(sample_1$RidePer14,mu = 0.5)

#######################################

ev_12 <- bull[bull$Events12>0,]
earnings_per <- ev_12$Earnings12/ev_12$Events12
mean(earnings_per)
sd(earnings_per)
hist(earnings_per)
#The earnings_per is way too skewed to apply hypothesis t-test.
#Let us take log(earnings) to transform this to a normalized distributions.
lg_earn12 <- log(earnings_per)
mean(lg_earn12)
sd(lg_earn12)
hist(lg_earn12)

t.test(lg_earn12,mu = 12)


# One Sample t-test
# 
# data:  lg_earn12
# t = -23.557, df = 28, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 12
# 95 percent confidence interval:
#   8.572169 9.120605
# sample estimates:
#   mean of x 
# 8.846387 
# 
# > 1.96*(sd(lg_earn12)/sqrt(28))
# [1] 0.2670274
# > mean(lg_earn12)
# [1] 8.846387
# > 8.846387 - 0.2670274
# [1] 8.57936
# > 8.846387 + 0.2670274
# [1] 9.113414
# > exp(8.572169)
# [1] 5282.575
# > exp(9.12)
# [1] 9136.202
# > exp(9.120605)
# [1] 9141.731

########################################

var1 <- c(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2)
mean(var1)
sd(var1)

#Find the t-critical value using qt() function. Arg-1 is the alpha(critical)
#Remember to divide by 2 if it is a two-tailed. Arg-2 is num of df.
qt(0.025,7)
#Use pt() to do opposite... Given a t-val, find probability/proportion.
#VERY MUCH LIKE pnorm() and qnorm()...
#pt(2.365,7)

t.test(var1,mu = 28.5)

########################################

(93.6-91)/(7.8/sqrt(25))

qt(0.95,24)

# N.B.
# The type of test to be performed (right-tail or left-tail)
# is determined by the alternative hypothesis...
# So if my test is to say that chemical level is > 91, it is right tailed.

#########################################

qt(0.05,11) #1.795885

#Lower and upper bounds of confidence = Mean +- margin_error
#margin_error = t_critical * standard_error
#standard_error = standard_deviation_sample/sqrt(sample_size)

se <- 5.3/sqrt(12)
qt(0.05,11) # considering two-tail test and 90% confidence interval
1.795885*se + 42.6
42.6 - 1.795885*se

qt(0.025,11)
