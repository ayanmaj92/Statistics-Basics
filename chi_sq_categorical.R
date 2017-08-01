###GOODNESS OF FIT###
library(SDSFoundations)
acl <- AustinCityLimits

gtab <- table(acl$Grammy)

# Claim is 2/3 didnt win Grammy, 1/3 did
claimp <- c(2/3,1/3)

chisq.test(gtab,p = claimp)

#Shows expected values depending on claimed proportions
chisq.test(gtab,p = claimp)$expected

###TEST OF INDEPENDENCE###
grammyage <- table(acl$Grammy,acl$Age.Group)
grammyage
#No p= required as we are just testing independence...
chisq.test(grammyage)$expected

#correct= is to say DO NOT APPLY ANY CORRECTIONS IF WE DON'T MEET ANY ASSUMPTIONS
chisq.test(grammyage,correct = F)

##################################################

acl[acl$Artist=="Allen Toussaint",]

#Test of best-fit
gender_tab <- table(acl$Gender)
exp_gender <- c(0.5,0.5)

chisq.test(gender_tab,p = exp_gender)$expected

chisq.test(gender_tab,p = exp_gender)

barplot(gender_tab,beside = T,legend = T)

#Test of Independence

gender_top10 <- table(acl$Gender,acl$BB.wk.top10)
gender_top10

barplot(gender_top10,beside = T, legend = T)

# See the expected proportions
chisq.test(gender_top10, correct = F)$expected

# Test for independence
chisq.test(gender_top10,correct = F)

########################################

genre_tab <- table(acl$Genre)
genre_tab
exp_prop <- c(1/4,1/4,1/4,1/4)

chisq.test(genre_tab,p=exp_prop)$expected
chisq.test(genre_tab,p=exp_prop)

genre_twitter <- table(acl$Genre,acl$Twitter.100k)
genre_twitter
prop.table(genre_twitter,1)

chisq.test(genre_twitter,correct = F)

########################################

acl$Recent[acl$Year < 2012] <- 0
acl$Recent[acl$Year >= 2012] <- 1

table(acl$Gender[acl$Recent==1])
