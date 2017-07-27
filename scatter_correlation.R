bull <- BullRiders
plot(bull$YearsPro,bull$BuckOuts12,xlab = "Years Pro", ylab = "Buck Outs 12", main = "Years Pro and Buck Outs")

# Use abline to plot the linear relationship
# lm() is used to generate linear model, and even do regression linear. The ~ basically means function of...
abline(lm(bull$BuckOuts12~bull$YearsPro))

#Let us check number of buck outs relationship with another variable
plot(bull$Events12,bull$BuckOuts12,xlab = "Events Participated 12", ylab = "Buck Outs 12", main = "Events participated and Buck outs")

abline(lm(bull$BuckOuts12~bull$Events12))

cor(bull$YearsPro,bull$BuckOuts12)

cor(bull$Events12,bull$BuckOuts12)

myvars <- c('YearsPro','Events12','BuckOuts12')

# Building a correlation matrix
cor(bull[,myvars])


######################

first_ten_riders <- bull[1:10,]
bull[bull$YearsPro>=10,]
NROW(bull[bull$YearsPro>=10,])

#Extract Top 15 Riders of 2015
top_15 <- subset(bull,bull$Rank15>=1 & bull$Rank15<=15)
View(top_15)
sort(top_15$BuckOuts14)
top_15[which(top_15$BuckOuts14==1),]


##########################

#Subset for riders that participated in at least one event in 2013
new_bull <- bull[bull$Events13  > 0 ,]

# Visualize and describe the first variable of interest
hist(new_bull$Rides13)
fivenum(new_bull$Rides13)
mean(new_bull$Rides13)
sd(new_bull$Rides13)

# Visualize and describe the second variable of interest
hist(new_bull$Top10_13)
fivenum(new_bull$Top10_13)
mean(new_bull$Top10_13)
sd(new_bull$Top10_13)

# Create a scatterplot
plot(new_bull$Rides13,new_bull$Top10_13)

# Add line of best fit
abline(lm(new_bull$Top10_13~new_bull$Rides13))

# Calculate the correlation coefficient
cor(new_bull$Rides13,new_bull$Top10_13)

# Create a correlation matrix 
vars <- c("Top10_13", "Rides13")
cor(new_bull[,vars])

which(new_bull$Top10_13==2 & new_bull$Rides13==22)

new_bull[4,]

#######################################

new_bull12 <- bull[bull$Events12>0,]
hist(new_bull12$Earnings12)
fivenum(new_bull12$Earnings12)
sd(new_bull12$Earnings12)
mean(new_bull12$Earnings12)
vars <- c("Earnings12","RidePer12","CupPoints12")
cor(new_bull12[,vars])

plot(new_bull12$RidePer12,new_bull12$Earnings12)

plot(new_bull12$CupPoints12,new_bull12$Earnings12)

# identify specific case
which(new_bull12$Earnings12 == max(new_bull12$Earnings12))

new_bull12[4,"Rank12"]

#Subset the data
nooutlier <- new_bull12[new_bull12$Earnings12 < 1000000 ,]

cor(nooutlier[,vars])

plot(nooutlier$RidePer12,nooutlier$Earnings12)

plot(nooutlier$CupPoints12,nooutlier$Earnings12)

#############################################

new_bull <- bull[bull$Rides14 > 0,]

RidesPerEvent14 <- new_bull$Rides14/new_bull$Events14
hist(RidesPerEvent14)
fivenum(RidesPerEvent14)

plot(RidesPerEvent14,new_bull$Rank14)
abline(lm(new_bull$Rank14~RidesPerEvent14))

cor(RidesPerEvent14,new_bull$Rank14)


#############################################

min_spent <- c(30,45,180,95,130,140,30,80,60,110,0,80)
exam_grade <- c(74,68,87,90,94,84,92,88,82,93,65,90)

prod1 <- min_spent * exam_grade
s_prod <- sum(prod1)
s_mspent <- sum(min_spent)
s_grade <- sum(exam_grade)
sq_min_spent <- min_spent^2
sq_exam_grade <- exam_grade^2

s_sq_mspent <- sum(sq_min_spent)
s_sq_grade <- sum(sq_exam_grade)
n <- length(min_spent)

num <- ((n*s_prod) - (s_mspent*s_grade))
denom <- sqrt((n*s_sq_mspent)-s_mspent^2) * sqrt((n*s_sq_grade)-s_grade^2)

num/denom

cor(min_spent, exam_grade)



min_spent_nooutlier <- c(30,45,180,95,130,140,80,60,110,0,80)
exam_grade_nooutlier <- c(74,68,87,90,94,84,88,82,93,65,90)

cor(min_spent_nooutlier,exam_grade_nooutlier)
