acl <- AustinCityLimits

grammy_tab <- table(acl$Grammy)

# Proportion Table... Takes as argument a table object and gives corresponding proportions
prop.table(grammy_tab)

#Generate contingency table... Just give multiple attributes to table()
gtab2 <- table(acl$Grammy,acl$Gender)
gtab2

# The proportion over whole total population (Total Male + Total Female) or (Total Grammy Winners + Total Non-Winners) both are same here
prop.table(gtab2)

# Across a row
prop.table(gtab2,margin = 1)
#Across a column
prop.table(gtab2,margin = 2)

barplot(grammy_tab,main = "ACL Grammy Winners", ylab = "Frequency", xlab = "Grammy Winners")

barplot(gtab2, legend = T, main = "Gender wise Grammy Winners", xlab = "Gender", ylab = "Counts", beside = T)

# Now to generate a mosaic plot (stacked bar plot) to show proportion of male and female grammy winners

barplot(prop.table(gtab2,margin = 2))

######################################

NROW(acl)
#first_ten <- acl[1:10,]
table(acl[1:10,]$Grammy)

# What genre was played by the first female artist in the dataset who was over 60 years of age?

female_artists <- acl[which(acl$Gender=="F"),]
a <- which(acl[which(acl$Gender=="F"),]$Age>60)

female_artists[a[1],"Genre"]

#####################################

older <- acl[acl$Age >= 30,]
# Create tables for marginal distributions
genre <- table(older$Genre)
gender <- table(older$Gender)

# Create the two-way contingency table
twoway <- table(acl$Gender,acl$Genre)
twoway

#Visualize the counts
barplot(twoway,legend=T,beside = T)

#Proportions for genre P(A)
prop.table(genre)

#Proportion for P(A|B) where B is gender (so we need row-wise)
prop.table(twoway,1)


#####################################

male_artists <- acl[acl$Gender=="M",]
table(male_artists$Grammy)

m_grammy <- table(male_artists$Grammy)
m_genre <- table(male_artists$Genre)

table(male_artists$Grammy,male_artists$Genre)
twoway_1 <- table(male_artists$Grammy,male_artists$Genre)

prop.table(twoway_1,2)
barplot(prop.table(twoway_1,2),legend=T)

######################################

popular <- acl[acl$Facebook.100k==1,]
table(popular$Age.Group)
prop.table(table(popular$Age.Group))

twoway_2 <- table(acl$Facebook.100k,acl$Age.Group)
twoway_2
prop.table(twoway_2,2)
