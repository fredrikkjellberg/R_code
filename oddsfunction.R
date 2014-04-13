 
odds <- function(rate, outof){
   p = (rate/(outof-rate))
   return(p)
 }

Male:

odds(0.0086,1000)
odds(0.0452,1000)



odds(20.8,100000) # 0.0002080433
odds(119,100000) # 0.001191418
odds(1760,100000) # 0.01791531

female:
odds(17.2,100000) # 0.0001720296
odds(252,100000) # 0.002526366
odds(1270,100000) # 0.01286336

Temperature <- 50
prob.mle <- exp(-15.0429+.2322*Temperature)/(1+exp(-15.0429+.2322*Temperature))
prob.mle # = 0.03128293

Temperature <- 70
prob.mle <- exp(-15.0429+.2322*Temperature)/(1+exp(-15.0429+.2322*Temperature))
prob.mle # = 0.7704935


whickitywhack<-fetchWhickham()
View(whickitywhack)

logisticwhick<-glm(outcome~smoker+age,data=whickitywhack,family=binomial)
summary(logisticwhick)

glm(formula = outcome ~ smoker + age, family = binomial, data = whickitywhack)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-3.2795  -0.4381   0.2228   0.5458   1.9581  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  7.599221   0.441231  17.223   <2e-16 ***
  smokerTRUE  -0.204699   0.168422  -1.215    0.224    
age         -0.123683   0.007177 -17.233   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

LowerBeta<- -0.204699-1.96*0.168422
UpperBeta<- -0.204699+1.96*0.168422
BetaCI<-c(LowerBeta,UpperBeta)
BetaCI

LowerBeta<- 0.2322+-1.96*0.1082
UpperBeta<- 0.2322+1.96*0.1082
BetaCI<-c(LowerBeta,UpperBeta)
BetaCI




############


# ------------------------------------------------
# ESOPH DATASET:
# ------------------------------------------------

  25-34  35-44	45-54	55-64	65-74	75+
0.0086	0.0452	0.216	0.314	0.3416	0.2955

# Similar to the Whick dataset, using the optional to understand function
# fetchEsoph() should grab and format our data for us correctly here.
ES<-fetchEsoph()
View(ES)

# The "esoph" data comes from a case-control study of esophageal cancer.
# The "cancer" column is the number of people in each age group with cancer,
# while the "healthy" column represents the number of people in each age group
# who do not have cancer.

# This data is an example of the "rate" situation discussed in the slides.
# With a little bit of work, we can use data in this tabular form to
# get the cancer rates by age:


25-34  35-44	45-54	55-64	65-74	75+
  0.0086	0.0452	0.216	0.314	0.3416	0.2955


cancerrate<-with(ES,cancer/(cancer+healthy))
plot(cancerrate~with(ES,age))
lm(cancerrate~age,data=ES)

cancerrate

# Suppose we fit a linear regression line through these cancer rates:
linearES<-lm(cancerrate~age,data=ES)
lines(predict(linearES,type="response")~age,data=ES,col="blue")

# Why wouldn't this be a good model for predicting cancer rate?

# Suppose we tried to fit this model:
logoddsES<-lm(logit(cancerrate)~age,data=ES)
logoddsES

# Why does this not give us errors like before?

# Now we can fit our logistic regression:
logisticES<-glm(cbind(cancer,healthy)~age,data=ES,family=binomial)
# Note that we are passing the glm command a matrix here, rather than
# an actual rate value. The syntax is sort of weird for this command,
# but this is what is necessary to perform a logistic regression on 
# this kind of data. No need to worry too much about it, but the
# cbind command just makes a matrix with cancer and healthy in the
# columns:
with(ES,cbind(cancer,healthy))

logisticES

# ====================================
# ACTIVITY
# ====================================
# Given these models based on the ES data, attempt to do the following:

# 1. Plot the predictions of the models linearES, logoddsES, and logisticES
#    on the same plot using the "lines" command. This should be similar 
#    to what was done in the whick data case.
#    How do the model predictions of the cancer rates compare?

# 2. For the logisticES model, do a Wald Test and likelihood ratio test for age,
#    and find the Wald confidence interval for age.
