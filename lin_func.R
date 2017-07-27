library(SDSFoundations)
wr <- WorldRecords
mens800 <- wr[wr$Event=="Mens 800m",]
# Linear fit... linFit(x,y)
linFit(mens800$Year,mens800$Record)

############################
table(wr$Event)
wr[wr$Athlete=="Usain Bolt",]

# Use subset to use multiple filters on dataframe
subset(wr,wr$Event=="Womens Mile" & wr$Record<260)
############################
menshot <- wr[wr$Event=="Mens Shotput",]
womenshot <- wr[wr$Event=="Womens Shotput",]

plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

linFit(menshot$Year,menshot$Record)
linFit(womenshot$Year,womenshot$Record)

#############################
menmile <- wr[wr$Event=="Mens Mile",]
womenmile <- wr[wr$Event=="Womens Mile",]

plot(menmile$Year,menmile$Record,main='Mens Mile Records',xlab = 'Year',ylab = 'Time',pch=16)
plot(womenmile$Year,womenmile$Record,main='Womens Mile Records',xlab = 'Year',ylab = 'Time',pch=16)

linFit(menmile$Year,menmile$Record)
linFit(womenmile$Year,womenmile$Record)

#############################
menpolevault <- subset(wr,wr$Event=="Mens Polevault" & wr$Year>=1970)

sort(menpolevault$Record)

menpolevault[menpolevault$Record>6,]

plot(menpolevault$Year,menpolevault$Record,main='Mens Pole Vault Records',xlab = 'Year',ylab = 'Height',pch=16)

linFit(menpolevault$Year,menpolevault$Record)
