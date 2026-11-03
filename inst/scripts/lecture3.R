# Customer Models - lecture 3: multinomial logit
# Loading customermodels loads all course packages (here: mlogit, lmtest) and the data.
library(customermodels)

####VIDEO 1#######

#read data
data <- cm_data("cm_lecture3")

#view data
if (interactive()) View(data)

#make sure programmename is a factor.
data$programmename<-as.factor(data$programmename)


###########VIDEO 3#############

#convert to format required for mlogit
data_mlogit<-mlogit.data(data,choice='programmename',shape='wide')

#estimate model
m1=mlogit(programmename~0|marketing+math+leadership+reading.writing,
          data=data_mlogit, reflevel = "history_pr")
#model estimates
summary(m1)

#odds (ratios) again
exp(coef(m1))

#marginal effects at mean for math
effects(m1,covariate="math")

########VIDEO 4: Prediction/validation#######

#Code below is used to obtain predicted probabilities for specific levels of dv.
#lecture asks you to calculate some by hand, but in case you want to do this in R, this might be useful.
#Code is somewhat clunky due to fact that mlogit needs one row per level of your dv and is rather picky about the data.
#easiest thing to do: draw a sample of three obs (since we need three rows) of data that we know works:
d2<-data_mlogit[1:3,]
#replace values by values we're interested in
d2$math=0
d2$marketing=0
d2$reading.writing=0
d2$leadership=0
#predict!
predict(m1,newdata=d2)

##market shares; not that useful in our specific context, but might be useful for simulations.
m1$fitted<-apply(fitted(m1,type="probabilities"),2,mean)
rbind(m1$fitted,m1$freq/sum(m1$freq))

#####likelihood ratio test
#estimate a smaller model to run the LRtest against
m2<-mlogit(programmename~0|math,
          data=data_mlogit,reflevel = "history_pr")

#LR test results shown in slides.
lrtest(m2,m1)

# Hit rate
#there's a bug in the lecture slide: the hit rate is 61%, not 30%.
#There was an error in an earlier version of the code, likely due to incorrectly ordering of rows.

#obtain predicted class
data$predclass<-max.col(m1$probabilities)
#check where predicted=observed, if so TRUE, if not FALSE. sum trues and divide by NROWS to get hitrate
sum(data$predclass==data$programme)/NROW(data)


###extra code for nested logit shiny visualization (only in an interactive session)
if (interactive()) {
  library(DiagrammeR)
  #visualization on nested logit from lecture
  runGitHub('rphars/cmviz',subdir='nestedviz')
}
