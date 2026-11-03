# Customer Models - lecture 6: survival analysis (Kaplan-Meier, Cox)
# Loading customermodels loads all course packages (here: survival, survminer, Hmisc, ggplot2) and the data.
library(customermodels)

# load csv file
df <- cm_data("churn_data")
describe(df)
summary(df)

# create factors
for(i in 6:length(colnames(df))){
  df[,i] <- as.factor(df[,i])
}

# Overview of week of churn, 52 weeks, right-censored
ggplot(df, aes(x=week)) + geom_bar(fill = '#0073C2FF')

# Estimation of Survival function using Kaplan-Meier method
KMSurvivalCurve.1 <- survfit(Surv(week, churn) ~1, data = df)
ggsurvplot(KMSurvivalCurve.1, data=df)
summary(KMSurvivalCurve.1)

# Estimation of Survival function using Kaplan-Meier method with strata
KM_SurvivalCurve.2 <- survfit(Surv(week, churn) ~ mails, data = df)
ggsurvplot(KM_SurvivalCurve.2, data = df, pval = T)
survdiff(Surv(week, churn) ~ mails, data = df, rho = 0)


# Estimation of cox model
CoxModel.1 <- coxph(Surv(week, churn) ~ mails + internet + age +
                      educ, method = 'efron', data = df)
summary(CoxModel.1)

# Plot confidence levels
# plot(survfit(CoxModel.1), ylim = c(.7,1), xlab = 'Weeks',
#      ylab = 'Proportion not churned')
# legend('bottomleft', c('lower 95%', 'Survival estimate', 'Upper 95%'),
#        lty=c(2,1,2))

# Testing proportionality assumption
proptest <- cox.zph(CoxModel.1, global = T)
print(proptest)
plot(proptest)

# Plot of Schoenfeld residuals over time
ggcoxzph(proptest)

#solving non-proportionality

# Solution 2: Different strata
agecat <- cut(df$age, breaks=c(0, 20, 25, Inf),
              labels=c('-20', '20-25', '25+'))
summary(agecat)

# Estimation of Cox proportional Hazard model, with baseline based on age (three categories)
CoxModel.3  <- coxph(Surv(week, churn) ~ mails + internet + educ +
                       strata(agecat), method = 'efron', data = df)
summary(CoxModel.3)
proptest3 <- cox.zph(CoxModel.3, global = T)
print(proptest3)

# Solution 3: Estimation of Cox proportional Hazard model with interactions

##### NOT like this, this doesnt work.#####
CoxModel.4 <- coxph(Surv(week, churn) ~ mails + internet + age*log(week) +
                      educ - log(week), method = 'efron', data = df)
summary(CoxModel.4)

# CORRECT way, interactions of time-dependent coefficients with tt()
CoxModel.5 <- coxph(Surv(week, churn) ~ mails + internet + age +
                      tt(age) + educ, method = 'efron',
                    tt= function(x, t, ...) x * log(t), data = df)
summary(CoxModel.5)
