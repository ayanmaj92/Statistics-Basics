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
