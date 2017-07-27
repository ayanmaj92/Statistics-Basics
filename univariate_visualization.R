# Pre-determined function to see data instead of opening spreadsheet view
head(animaldata)

# See count of categorical data
table(animaldata$Sex)

# Plot a bar plot
plot(animaldata$Sex,main="Bar Chart of Animal Genders",xlab="Sex",ylab="Frequency")

# Plotting histogram

hist(animaldata$Age.Intake,main="Histogram of Animal Intake Ages",xlab = "Age")

# Extrace the female and male ages
female_age <- animaldata$Age.Intake[animaldata$Sex=="Female"]
male_age <- animaldata$Age.Intake[animaldata$Sex=="Male"]

hist(female_age,main = "Histogram of Female Animals",xlab = "Female Animal Age")

# break option gives us the option to adjust number of bins in histogram
hist(male_age,main = "Histogram of Male Animals",xlab = "Male Animal Age",breaks = 15)

max(male_age)
max(female_age)

#Tells about record number for particular criteria
which(animaldata$Age.Intake==17)

animaldata[415,]

mean(animaldata$Age.Intake)
median(animaldata$Age.Intake)
#Standard Deviation
sd(animaldata$Age.Intake)

#Gives min, Quartile 1, Median (Quartile 2), Quartile 3, max
fivenum(animaldata$Age.Intake)

#Find the number of animals that were adopted
table(animaldata$Outcome.Type)

#Pull out only adopted animals
adopted <- animaldata[animaldata$Outcome.Type=="Adoption",]

#Pull out just the days in shelter for the adopted animals
daystoadopt <- adopted$Days.Shelter

#Visualize and describe this variable
hist(daystoadopt)
fivenum(daystoadopt)
mean(daystoadopt)
sd(daystoadopt)
which(animaldata$Days.Shelter==max(daystoadopt))

### LAB STUFF ###

adultanimals <- animaldata[animaldata$Age.Intake>=1,]
adultdogs <- adultanimals[adultanimals$Animal.Type=="Dog",]
adultcats <- adultanimals[adultanimals$Animal.Type=="Cat",]

table(adultanimals$Animal.Type)

