library(SDSFoundations)
post <- PostSurvey
#Trying 2-sample paired t-test
hist(post$exclusive)

hist(post$post_exclusive)

# None of these are individually normal. But our requirement is the 
# difference has an approx normal distribution.
diff_exclusive <- post$exclusive - post$post_exclusive
#Looks more normal!!!
hist(diff_exclusive)

#Paired t-test
t.test(post$exclusive,post$post_exclusive,paired = TRUE)
mean(diff_exclusive)

# To do a one sided hyp test, do t.test(post$exclusive,post$post_exclusive,paired=T,alternative='less') OR greater
##########################################3
#Trying 2-sample independent t-test
fsleep <- post$sleep_Tues[post$gender=="Female"]
msleep <- post$sleep_Tues[post$gender=="Male"]

#Check normality of both populations
hist(fsleep)
hist(msleep)

#Here also, we can give alternative="greater" or less option for one-sided
#alternative test.
t.test(fsleep,msleep)

###########################################
NROW(post)
post$classification[post$gender=="Male"]
first_ten <- post$live_campus[1:10]
head(post)
first_ten
table(first_ten)
table(post$classification)
colnames(post)

############################################
#**FOR INDEPENDENT**
underclass_happy <- post$happy[post$classification=="Freshman"|post$classification=="Sophomore"]
upperclass_happy <- post$happy[post$classification=="Junior"|post$classification=="Senior"]

hist(underclass_happy)
hist(upperclass_happy)

t.test(underclass_happy,upperclass_happy)

#**FOR PAIRED**
post$diff_happy <- post$happy - post$post_happy
hist(post$diff_happy)
t.test(post$happy,post$post_happy,paired = T)
length(post$diff_happy)

##########################################33

diff_hw <- post$hw_hours_college - post$hw_hours_HS
hist(diff_hw)
mean(diff_hw)

#Performing 2 sample independent t-test that is one-tailed (right)
t.test(post$hw_hours_college,post$hw_hours_HS,paired = T, alternative = "greater")

greek_sleep <- post$sleep_Sat[post$greek=="yes"]
non_greek_sleep <- post$sleep_Sat[post$greek=="no"]
hist(greek_sleep)
hist(non_greek_sleep)
t.test(greek_sleep,non_greek_sleep,alternative = "less")


#########################################

post$diff_hw <- post$hw_hours_college - post$hw_hours_HS
hw_bio <- post$diff_hw[post$major=="Biology"]
hw_nur <- post$diff_hw[post$major=="Nursing"]

hist(hw_bio)
hist(hw_nur)

t.test(hw_bio,hw_nur)
