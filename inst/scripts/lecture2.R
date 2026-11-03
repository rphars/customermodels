# Customer Models - lecture 2: logit and probit
# Loading customermodels loads all course packages (here: mfx) and the data.
library(customermodels)

#load data
data <- cm_data("telco_churn")

#view data
if (interactive()) View(data)

#estimate a logit
model1<-glm(churn~day.mins,family=binomial(link="logit"),data=data)

#view parameter estimates (original scale)
summary(model1)
#view parameter estimates on odds (ratio) scale
exp(coef(model1))


#calculating baseline churn probability and p(churn)..
#...try it yourself manually!


##have r calculate p(churn) for you
data$pchurn<-predict(model1,type="response")

#calculate marginal effects

#estimate a slightly more complex model:
model2<-glm(churn~day.mins+night.charge,family=binomial(link="logit"),data=data)

#calculate marginal effects:
logitmfx(churn~day.mins+night.charge,data=data)

##different model, including dichotomous variable
model3<-glm(churn~day.mins+intl.calls+voice.plan,
            family=binomial(link="logit"),data=data)
#coefs
summary(model3)
#marginal effects
logitmfx(churn~day.mins+intl.calls+voice.plan,data=data)


###PROBIT
model4<-glm(churn~day.mins+intl.calls+voice.plan,family=binomial(link='probit'),data=data)
#coefs
summary(model4)

#baseline pchurn
pnorm(-1.9001703)

#pchurn for customer with 200 day mins, no int calls, w/ voice plan
pnorm(-1.9001+0.0056*200-0.034*0-0.433*1)
pnorm(-1.9001+0.0056*50-0.034*10-0.433*0)

#enable the following option if you want to see model coefs without scientific notation
#options(scipen=999)

####MARGINAL EFFECTS COMPARISON#####
#marginal effects: probit
probitmfx(churn~day.mins+intl.calls+voice.plan,data=data)
#marginal effects: logit for comparison
logitmfx(churn~day.mins+intl.calls+voice.plan,data=data)


###MODEL VALIDATION
#Hit rate
#calculate predicted p_churn for model
data$pred_churn<-predict(model3,type="response")

#dichotomize
data$predchoice<-ifelse(data$pred_churn>0.5,1,0)

table(data$churn,data$predchoice)
#hitrate: sum the 'diagonal' elements, divide over nobs
sum(diag(table(data$churn,data$predchoice)))/nrow(data)

#LR test

#full model, with k parameters, here: 4
##no need to re-estimate since we've already estimated it previously
summary(model3)
#nullmodel
nullmodel<-glm(churn~1,family=binomial(link="logit"),data=data)

#Akaike information criterion
AIC(model3)
AIC(model1)

#LRtest
anova(nullmodel,model3,test="Chisq")


#this week's shiny apps (only run in an interactive session)
if (interactive()) {
  #visualizing log odds vs odds vs probability
  runGitHub('rphars/cmviz',subdir='logoddsprob')
  #visualizing marginal effects at mean
  runGitHub('rphars/cmviz',subdir='mem')
}
