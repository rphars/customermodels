# Customer Models - lecture 5: count models (Poisson, NB, truncated, zero-inflated, hurdle) with VGAM
# Loading customermodels loads all course packages (here: MASS, AER, VGAM, Hmisc) and the data.
library(customermodels)

flyer <- cm_data("cm_lecture5_flyer")
contests <- cm_data("cm_lecture5_contest")

#fix column name of first column
colnames(flyer)[1]<-"Customer_ID"
flyer$House.f<-as.factor(flyer$House)

#view data
if (interactive()) View(flyer)

#run OLS to show it can lead to negative predictions..
OLS<-lm(NumbItems~Flyer+House.f+Gender+Age,data=flyer)
#create new observation to predict
newdat<-data.frame(House.f='1',Age=16,Gender=0,Flyer=0)
#negative!
predict(OLS,newdata=newdat)


#shiny app: poisson model (only in an interactive session)
if (interactive()) runGitHub('rphars/cmviz',subdir='poisson')

#estimate a poisson regression
m1<-glm(NumbItems~Flyer+House.f+Gender+Age,data=flyer,family='poisson')

#predict for the same customer (not discussed in the slides)
##positive!
predict(m1,newdata=newdat,type="response")

#view model coefficients
summary(m1)
#exponentiate coefs
exp(coef(m1))

#calculate null model
nullmodel<-glm(NumbItems~1,data=flyer,family="poisson")
#LR test
anova(nullmodel,m1,test="Chisq")
#AIC and BIC
AIC(nullmodel,m1)
BIC(nullmodel,m1)

##########VIDEO 2#################

colnames(contests)[1]="id"
#more descriptive names
contests$type=factor(contests$type,labels=c("Logo","Website","Design(print,product)","Other"))

#obtain some descriptives
mean(contests$num_creatives)
var(contests$num_creatives)

mean(contests$num_star_rating_5)
var(contests$num_star_rating_5)

mean(contests$num_entries)
var(contests$num_entries)
#create some histograms
hist(contests$num_creatives,main="Number of Solvers",xlab="Number of solvers",breaks=40)
hist(contests$num_star_rating_5,main="Number of five star ratings",xlab="Number of five star ratings",breaks=40)


# creates dummy variables for the categorical variable innovation type
contests$type.f <- factor(contests$type)

#estimate poisson model
cm1<-glm(num_creatives~type.f+num_awards+award_assured+
           award_amount_USD,data=contests,family='poisson')

#interpret coefficients
summary(cm1)
exp(coef(cm1))

#run dispersion test
dispersiontest(cm1,trafo=2)


#estimate negative binomial model
cm2<-glm.nb(num_creatives~type.f+num_awards+award_assured+
           award_amount_USD,data=contests)
#see results
summary(cm2)
#note that theta=1/alpha (and vice versa! alpha=1/theta), so:
alpha=1/cm2$theta
alpha
#compare fit of both models to data
AIC(cm1,cm2)
BIC(cm1,cm2)

##truncated model
#create truncated variable to show application (our dataset didn't have one by default!)
#i.e. don't do this to your actual data.
contestspos <- subset(contests,num_star_rating_5>0)

#estimated truncated negbin model. May take a few seconds.
cm3_truncnb<-vglm(num_star_rating_5~type.f+num_awards+award_assured+
                    award_amount_USD,data=contestspos,family=posnegbinomial())

#view results (may -surprisingly- again take a few seconds)
summary(cm3_truncnb)
cm3_truncnb

#estimate models for data with 'too many' zeroes
#zero inflated poisson, note the 'family' part
ZIPoissonModel_5star <- vglm(num_star_rating_5 ~ factor(type) +num_awards
                             + award_assured + award_amount_USD,
                             family = zipoisson(), data = contests)
summary(ZIPoissonModel_5star)
#zero inflated NB, takes a while to run

ZINBModel_5star <- vglm(num_star_rating_5 ~ factor(type) +num_awards
                        + award_assured + award_amount_USD,
                        family = zinegbinomial(), data = contests)
summary(ZINBModel_5star)
#zero-altered poisson

ZAPoissonModel_5star <- vglm(num_star_rating_5 ~ factor(type) +num_awards
                             + award_assured + award_amount_USD,
                             family = zapoisson(), data = contests)
#zero-altered NB
ZANBModel_5star <- vglm(num_star_rating_5 ~ factor(type) +num_awards
                        + award_assured + award_amount_USD,
                        family = zanegbinomial(), data = contests)

#also estimate 'regular' poisson and nb models
poissonmodel_5star<-glm(num_star_rating_5 ~ factor(type) +num_awards
                         + award_assured + award_amount_USD,
                        data = contests,family="poisson")

nbmodel_5star<-glm.nb(num_star_rating_5 ~ factor(type) +num_awards
                        + award_assured + award_amount_USD,
                         data = contests)

#compare models based on BIC
BIC(ZIPoissonModel_5star)
BIC(ZINBModel_5star)
BIC(ZAPoissonModel_5star)
BIC(ZANBModel_5star)
BIC(poissonmodel_5star)
BIC(nbmodel_5star)

#interpret ZANB model.
#Careful about the DV in the logistic part (P(observing a 0))! The 'reversed' directions between the logit and count coefficients are a giveaway here..
summary(ZANBModel_5star)
exp(coef(summary(ZANBModel_5star)))
