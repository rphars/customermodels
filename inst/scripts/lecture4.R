# Customer Models - lecture 4: censoring, Tobit type 1 and type 2
# Loading customermodels loads all course packages (here: AER, sampleSelection, censReg) and the data.
library(customermodels)

######VIDEO 1#######

#read datasets
tickets_data <- cm_data("movie_ticket_data")
donation_data <- cm_data("donation_data")


#view tickets data
if (interactive()) View(tickets_data)

#some histograms
hist(tickets_data$ticket_price,col="blue")
#there's a small bug in the lecture slides. The barplot for weekday was created by hist(tickets_data$weekday,col="blue",breaks=7)
#but that does _not_ work for discrete variables. Dropping breaks=7 leads to an ugly plot. One option to solve it is below
counts <- table(tickets_data$weekday)
barplot(counts, col="blue", border="white",
        main="Weekday Frequencies", xlab="Day", ylab="Frequency")

#hist for popularity
hist(tickets_data$popularity,col="blue",breaks=13)

#estimate OLS
m0<-lm(tickets_sold~ticket_price+popularity+as.factor(weekday),data=tickets_data)
#interpret results
summary(m0)


#see histogram of DV
hist(tickets_data$tickets_sold,main="",xlab="Ticket sales",col="blue")

#first shiny app (only in an interactive session)
if (interactive()) runGitHub('rphars/cmviz',subdir='censoring')

###########Video 2: Tobit type 1 model###############
#shiny app (only in an interactive session)
if (interactive()) runGitHub('rphars/cmviz',subdir='lvcensoring')

#estimate a tobit type 1 model. Note the right=500. For censoring from below, use left=
m1<-tobit(tickets_sold~ticket_price+popularity+as.factor(weekday),data=tickets_data,right=500)

#see results, and compare to ols (OLS is m0)
#ols
summary(m0)
#tobit
summary(m1)

######VIDEO 3: Donation behavior#############
if (interactive()) View(donation_data)
hist(donation_data$GIFT,col="blue")

#make dummy for donated yes/no.
donation_data$donated<-ifelse(donation_data$GIFT>0,1,0)


#two part model. Part 1 is for amount (notice this is only for the people who donated)
amount_reg<-lm(log(GIFT)~GIFTLASTRESP+AVERAGEGIFT+MAILSPERYEAR,data=donation_data,subset=donated==1)
#part 2 is a logit.
donated_reg<-glm(donated~MAILSPERYEAR,data=donation_data,family=binomial(link="logit"))

#results on the slides. Note the rather high R2, probably because we're predicting future gifts
#with previous gifts, which may be a very good predictor for donation amount..
summary(amount_reg)
summary(donated_reg)


#tobit type 2
#note: you might get different results than on the slides.
#I apparently used a subset..
m3<-selection(donated~RESPLASTMAIL+WEEKSLASTRESP+MAILSPERYEAR+PROPRESPONSE+AVERAGEGIFT,
              GIFT~RESPLASTMAIL+WEEKSLASTRESP+MAILSPERYEAR+PROPRESPONSE+AVERAGEGIFT,data=donation_data)

summary(m3)
