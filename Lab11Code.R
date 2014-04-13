library(mosaic)

# ------------------------------------------------
# FUNCTIONS TO GET DATA. OPTIONAL TO UNDERSTAND.
# ------------------------------------------------
fetchEsoph <- function() {
  data(esoph)
  age <- c(30,40,50,60,70,75)
  cancer <- sum(ncases ~ agegp, data=esoph)
  healthy <- sum(ncontrols ~ agegp, data=esoph)
  data.frame(age, cancer, healthy)
}

fetchWhickham <- function() {
  library(mosaic)
  data(Whickham)
  out <- Whickham[with(Whickham, order(age)),]
  rownames(out) <- 1:nrow(out)
  out$outcome <- out$outcome == "Alive"
  out$smoker <- out$smoker == "Yes"
  return(out)
}
# ------------------------------------------------
# END OF OPTIONAL TO UNDERSTAND CODE
# ------------------------------------------------

# ================================================
# LOGISTIC REGRESSION:
# ================================================

# ------------------------------------------------
# WHICK DATASET:
# ------------------------------------------------
# We'll start by doing an example with the whick data.
# To get this data, just use the "fetchWhickham" function
# from the optional code above:
whickitywhack<-fetchWhickham()
View(whickitywhack)

# Here we are in the "binary outcome" setting described in the slides.
# Data are organized in a long list with an outcome which is
# binary, e.g. either true or false in this case.
# Here, the outcome represents whether or not the smoker has
# cancer. Here "TRUE" means "still alive 20 years after initial 
# measurement."

# Suppose we want to predict survival probabilities 20 years out.
# We can start crudely by running a regular least squares model
# to predict the outcome using the age variable:
linearwhick<-lm(outcome~smoker+age,data=whickitywhack)
summary(linearwhick)

# This gives us some idea about how to predict the outcome.
# The model tells us that smoker status is not statistically
# significant for predicting the outcome, but age is.
# What are the obvious problems with using this model for such a
# prediction?

# Now we'll try a different method of predicting things about the
# outcome. First, recall the logit() and ilogit() functions from lecture.
# These are just a convenient way of calculating the logit and the inverse
# logit of some number. So these do the same thing:
logit(1/4)
log(1/4/(1-1/4))

# Similarly, the inverse logit is the same as ilogit. So these do the same thing:
exp(1/4)/(1+exp(1/4))
ilogit(1/4)

# And ilogit cancels out logit, and vice versa:
ilogit(logit(1/2))
logit(ilogit(1/2))

# From lecture, recall that logistic regression is "like" performing a linear
# regression on the log odds.
# With this in mind, we could also try to fit a model that looks like this:
lm(logit(outcome)~smoker+age,data=whickitywhack)

# Why does this give us an error message? Why doesn't this model work?
# ** logit(1) = inf, logit(0) = -inf

# Since this gives us problems, we will use the command introduced in lecture
# for doing logistic regression:
logisticwhick<-glm(outcome~smoker+age,data=whickitywhack,family=binomial)
logisticwhick

# If we fit this model, smoker is still not statistically significant. However,
# it starts to look in some sense "less insignificant" than it did before.

# WALD TESTS, WALD CONFIDENCE INTERVALS, LIKELIHOOD RATIO TESTS
# -------------------------------------------------------------

# Wald Tests:
# -----------
# Wald tests are probably the easiest test to do here.
# The test output by default in the summary table for each coefficient
# are Wald tests by default:
summary(logisticwhick)

# Wald Confidence Intervals:
# --------------------------
# First, consider the odds ratios. 
# How would we get a confidence interval for the odds ratio of smoker here?
# FIRST, try getting a confidence interval for the coefficient for smoker
# using the standard error and the properties of the normal distribution:
LowerBeta<- -0.204699-1.96*0.168422
UpperBeta<- -0.204699+1.96*0.168422
BetaCI<-c(LowerBeta,UpperBeta)
BetaCI

# Now recall from lecture how we would go from the beta coefficients here
# to the odds ratio associated with smoker:
exp(-0.204699)

# So if we want to take our confidence interval from beta to the odds ratio,
# we could do the following:
exp(BetaCI)

# Suppose we were trying to figure out whether the odds ratio showed that smoker
# made statistically significant difference. With a normal confidence interval,
# e.g. for a Beta in our regression model, we generally can check to see
# whether the confidence interval contains zero. Does the same thing work here?

# Likelihood Ratio Test:
# ----------------------
# Here, I want to run another kind of test of the same thing. The Likelihood Ratio Test
# is potentially better for small sample sizes than the Wald test. For large samples,
# you should usually find similar results though. 

# Suppose we care about testing whether smoker has an effect.
# One way you could attempt to run the test is as follows:
anova(logisticwhick,test="LRT")

# Why is this test NOT what we want to do?

# Instead, we should run this test:
anova(logisticwhick, glm(outcome~age,data=whickitywhack,family=binomial),test="LRT")

# What is the difference in what is being tested between these two?

# Plotting this data:
# -------------------
# Sometimes it's easier to interpret what is going on by plotting the
# predictions from the data set.

# this first command just plots the outcomes (with TRUE as 1 and FALSE as 0)
# across age on the x axis:
plot(outcome~age,whickitywhack)

# We want to add lines representing our model predictions to this plot.
# In order to do this, we have to get our predictions "out" of our model
# somehow. There are two ways we will discuss doing this:

# Way 1:
# -------
predict(logisticwhick,type="response")

# Way 2:
# ------
fitted(logisticwhick)

# Ultimately these two commands will do the same thing.
# So let's store our model predictions for the logistic model
# and plot them:
pred<-predict(logisticwhick,type="response")

# We can use the "lines" command to draw a line here corresponding
# to the prediction across ages:
lines(pred~age,data=whickitywhack,col="purple")

# Why does this line look weird?

# What if we tried:
lines(pred[whickitywhack$smoker]~age[whickitywhack$smoker],col="blue",data=whickitywhack)
lines(pred[!whickitywhack$smoker]~age[!whickitywhack$smoker],col="red",data=whickitywhack)

# At every starting age, smokers were slightly more likely to have died 20 years out.
# The jig-jags in the line are because of pooling the data together between smokers and
# nonsmokers.

# We can also include the predictions from our regression line in the graph
# in order to compare it to our logistic models:
lines(predict(linearwhick,type="response")~age,data=whickitywhack,col="darkgreen")

# This line is a little jaggedy, for the same reason the logistic line is.
# It includes predictions from both smokers and non-smokers.

# ------------------------------------------------
# ESOPH DATASET:
# ------------------------------------------------

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
cancerrate<-with(ES,cancer/(cancer+healthy))
plot(cancerrate~with(ES,age))

# Suppose we fit a linear regression line through these cancer rates:
linearES<-lm(cancerrate~age,data=ES)
lines(predict(linearES,type="response")~age,data=ES,col="blue")

# Why wouldn't this be a good model for predicting cancer rate?

# Suppose we tried to fit this model:
logoddsES<-lm(logit(cancerrate)~age,data=ES)

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
