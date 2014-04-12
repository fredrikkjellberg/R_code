
####################################################################################################################
As part of an epidemiological study, data are collected on exposure to radon (R) and cancer status (C). 
Assume the study is properly conducted for a study of its type.

Here "Pr(A|B)" is the probability of A given B, "Odds(A|B)" is the odds of A relative to B, and "OR" is the 
ratio of the odds of cancer if exposed to the odds of cancer if not exposed.
#####################################################################################################################
# 1. Assume the study design is cross sectional. Which of the following quantities are estimable from the data? 
# (You may select multiple answers if needed.)
#####################################################################################################################

(You do not need an explanation, unless you feel your answer calls for one.)

A. 	P(C|R)
B. 	P(R|C)
C. 	Odds(C|R)
D. 	Odds(R|C)
E. 	OR

in a cross sectional study information is collected simulatnisouy on both variables of the study. 
this often occurse when all the data are collected from one sample and then placed into a classificaiton.
the row totals and colum totals are not known prior to the data collection. 
the total sample N, may or may not be known before the data is collected.

example: donner party analysis of death...




#####################################################################################################################
# 2. Assume the study design is a cohort study. Which of the following quantities are estimable from the data? 
# (You may select multiple answers if needed.)
#####################################################################################################################

(You do not need an explanation, unless you feel your answer calls for one.)
A.   P(C|R)
B. 	P(R|C)
C. 	Odds(C|R)
D. 	Odds(R|C)
E. 	OR

in a cohort study, indivudals (or units) who differ with respect to a a sertain explanaory variable are selected (or assigned to groups)
and then a respons variable is measured. these pre-detremained groups are called are called cohors, and if the repsons variable is measured over time the design is called prosepective deisgn. 
in a cohort study, the totals conrresponding to the explanatory variable are known beofre the resons are collected.

#####################################################################################################################
# 3. Assume the study design is a case-control study. Which of the following quantities are estimable from the data? 
# (You may select multiple answers if needed.)
#####################################################################################################################

(You do not need an explanation, unless you feel your answer calls for one.)
A.   P(C|R)
B.   P(R|C)
C. 	Odds(C|R)
D. 	Odds(R|C)
E. 	OR

all see slide 16/17 on march 24th
#####################################################################################################################
# 4. Assume the study design is a convenience sample. Which of the following quantities are estimable from the data? 
# (You may select multiple answers if needed.)
#####################################################################################################################

(You do not need an explanation, unless you feel your answer calls for one.)
A.   P(C|R)
B.   P(R|C)
C.   Odds(C|R)
D. 	Odds(R|C)
E. 	OR

#####################################################################################################################
# 5. Refer to the "Research Project" at the end of chapter 6, and to related exercises in Lab 9 (March 27 or 28). 
#    Re-do the permutation test with the same adjustments for “clustering” but with a different test statistic 
#    than "LTE" or "LT".

#    Design your test statistic in such a way that a low status handlers interacting with a high status infant 
#    makes a stronger contribution against the null hypothesis than does the event of a medium status handler 
#    interacting with a high status infant. The specific difference in what these contributions should be is 
#    up to you, as is whether your test addresses RH1 (p. 206) or RH2 (p.208).

1) Briefly explain your test statistic, including which hypothesis your test addressees (RH1 or RH2).
2) Present a concise program to perform your test.
3) Report your p-value.

#   (Your program can use functions from the "usefulRfunctions.R" and "baboonfunctions.R" scripts provided for 
#   Lab 9 via Ctools. You do not need to include any code from these files, unless you are modifying them.)
#####################################################################################################################

1) Briefly explain your test statistic, including which hypothesis your test addressees (RH1 or RH2).

I have decided to set up the test to test whether rank is lower than OR EQUAL TO the handler rank
 in the search problem this is refered to as RH1



# Look again at the table:
babtab

# Intuitively, the cells in the top corner cells represent situations in which
# mother's rank was higher than handler's rank. These cells would contradict our
# hypothesis if they were large enough. By contrast, the lower cells and the
# diagonal are in line with our hypothesis. H0 and the negative are not.

# if the goal is to increase the effect of handler.rank.low/infant.rank.high vs hadler.rank.medium/infant.rank.high
# we could instead of just mutiplying it with -1 we can mutipliy it with -2 increaseing the wieght of that extrem case.

# We do the same weighted sum that will assigns a negative value to observations
# that contradict our hypothesis, and a positive value to cells that match it.
# the difference is that handler.rank.low/infant.rank.high is weight highter (-2) vs (-1) for hadler.rank.medium/infant.rank.high

# the old matric would look like this:
LTE<- matrix(c( 1,-1,-1,
                1, 1,-1,
                1, 1, 1),nrow=3,byrow=TRUE)

# the new matrix will look like this:
newWeight<- matrix(c( 1,-1,-2,
                1, 1,-1,
                1, 1, 1),nrow=3,byrow=TRUE)

# The point of this matrix is that there will be a "1" in all the cells where
# the research hypothesis is true, and a "-1" in all the cells where the research hypothesis
# is contradicted. and a -2 in the case of handler.rank.low/infant.rank.high.
#old:
babtab*LTE

Hi Mid Low
Hi    5  -5  -3
Mid  97  83 -95
Low  68 138 184

#New:
babtab*newWeight

  Hi Mid Low
Hi    5  -5  -6
Mid  97  83 -95
Low  68 138 184

# And then take the sum. This should just add all the cells in the matrix together.
obsLTE<-sum(babtab*LTE)
[1] 472
obs_newWeight<-sum(babtab*newWeight)
[1] 469
# we use this number (469) as our new test stats = "test statistic". 
# As we can see it is smaller thant our old case but will behave in the same way: 
# if there are lots of cases where the research hypothesis looks true,
# and smaller if there are lots of cases where the research hypothesis looks false.

babsim<-function(babdat,mask)
{
  sbtab<-with(baboon.shuffle(babdat),table(Mother.Rank,Handler.Rank))
  sum(mask*sbtab)
}

simoutput<-do(1000)*babsim(baboon,LTE)
simoutput_new<-do(1000)*babsim(baboon,newWeight)

# We look at whether the test statistic output by the simulation is
# bigger than or equal to the observed test statistic, e.g. we count
# the number of times we see a statistic by chance in our shuffled data 
# that is as or more supportive of the hypothesis than what we observed.
# Then we divide this by the number of simulations we did, in order to
# get an estimate of our p-value.
sum(simoutput>=obsLTE)/1000
[1] 0.026
sum(simoutput_new>=obs_newWeight)/1000
[1] 0.015


#####################################################################################################################

Model the relationship between age and accidents in the adult US population using "drivedata.txt" in Resources/Data Sets.

Some notes about the data: Numbers of drivers are listed in thousands. They pertain to 2009, and come from Table 
1114 of the U.S. Census Bureau’s Statistical Abstract of the United States: 2012. Variable age is a midpoint, 
so '30' is really all individuals from 25-34, while '40' is individuals 35-44.

# 6. 
#You're interested primarily in capturing patterns affecting drivers between 18 and 80. 
#Before you begin fitting models, decide whether this means you should exclude any of the data 
#given in the table from your analysis. You can either make your decision based on inspection 
#of a graphical display of the data, on a priori considerations, or a combination of the two. 
#Briefly explain your decision.

Note: Youll have the opportunity to start this part in lab this week.
#####################################################################################################################
include(driverdata.csv)
View(drivedata)

# assumptions:

1. i am first assuming that since there is no note on scale of accidents but that the drivers variable is in 1000, I also asume
that accidents will also be in 

https://www.census.gov/compendia/statab/2012/tables/12s1114.pdf

# This data is an example of the "rate" situation discussed in both the slides and in lab. However, the data is not
# formated so that rate of accidents is a variable for each age group. With a little bit of work, we can use data 
# in this tabular form to get the accident rates by age:
accident_rate<-with(drivedata,accidents/(drivers))
plot(accident_rate~with(drivedata,age))

accident_rate2<-with(drivedata,accidents/(drivers))
plot(accident_rate2~with(drivedata,age))

if we know that the upper and lower range 18 and 80 actually means 18+/-5 and 80+/-5, and we are only intrested 
in age between 18 and 80 we are technically including too young people and too old people if we dont take out some data
however, since that is not very easy, i am going to assum that there over 80 and bellow 18 are not going to effect he accident rate a lot. 

liner_driver<-lm(accident_rate~age,data=drivedata)
summary(liner_driver)


#####################################################################################################################

7. 
Use linear regression with rates as the response variable, 

linear regression with logits of rates as the response, 

and logistic regression to model the relationship between age and accident rates. 


Create a single visual display to 
compare the fitted values from these three models to the actual observed accident rates. With reference to your figure, 
explain which of three models you prefer for explaining this relationship.

#####################################################################################################################

assuming that rate = accident rate per number of drivers (not per 100 licensed drivers)
assuming that both accidents and number of drivers is in 1000

accident_rate<-with(drivedata,accidents/(drivers))
plot(accident_rate~with(drivedata,age))

liner_driver<-lm(accident_rate~age,data=drivedata)
summary(liner_driver)

plot(accident_rate~with(drivedata,age))

# Suppose we fit a linear regression line through these accidents rates:
linear_accident_rate<-lm(accident_rate~age,data=drivedata)
# prints the line on the graph!
lines(predict(linear_accident_rate,type="response")~age,data=drivedata,col="red")

# Suppose we tried to fit this model (logits ):
logodds_accident_rate<-lm(logit(accident_rate)~age,data=drivedata)
# prints the line on the graph!
lines(ilogit(predict(logodds_accident_rate,type="response"))~age,data=drivedata,col="green")

# Now we can fit our logistic regression:
logistic_accident_rate<-glm(cbind(accidents,(drivers-accidents))~age,data=drivedata,family=binomial)
# prints the line on the graph!
lines(predict(logistic_accident_rate,type="response")~age,data=drivedata,col="blue")

probaly prefer the logistic since it is following the orginal prety well, it is also the line  that does the least over estimations in terms
of the accident rate.

#####################################################################################################################

Fan, Heckman and Wand analyzed third-degree burn data from the universtiy of Souther California General Hospital Burn 
Center. In the Burns data set, 435 patients (adults ages 18-85) were grouped according to the size of the third-degree
burns on their body. The explanatory variable is listed as the midpoint of set intervals: ln(area in square centimeters + 1). 
The response in this data set is whether or not the patient survived (1 represents a survival).

8. Create a logistic regression model using area to estimate the probability of survival.

Report both regression coefficients.

#####################################################################################################################

LogBurn<-glm(survive~area.1,data=Burns,family=binomial)
summary(LogBurn)


Call:
  glm(formula = survive ~ area.1, family = binomial, data = Burns)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.7345  -0.6585   0.2194   0.5139   2.3876  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  10.2338     0.9690   10.56   <2e-16 ***
  area.1       -1.3724     0.1354  -10.14   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 525.39  on 434  degrees of freedom
Residual deviance: 332.66  on 433  degrees of freedom
AIC: 336.66

Number of Fisher Scoring iterations: 5

LogBurn

Call:  glm(formula = survive ~ area.1, family = binomial, data = Burns)

Coefficients:
  (Intercept)       area.1  
10.234       -1.372  

Degrees of Freedom: 434 Total (i.e. Null);  433 Residual
Null Deviance:      525.4 
Residual Deviance: 332.7 	AIC: 336.7

#####################################################################################################################

9. Note: This question is slightly modified from the one in the book.

Produce a plot which compares the observed values versus the fitted logistic regression line. 
(E.g. Figure 7.3 on page 216, but without the green simple linear model line.)

Optionally, include your R code for partial credit.

Hint: An easy way to plot involves two calls, the first to plot, plotting a formula of the dependent and 
indepedent variables, and the second to lines, with a formula of the predicted values and independent variables.

#####################################################################################################################

pred <- predict(LogBurn, type='response')
plot(survive ~ area.1, data=Burns) 
lines(pred ~ area.1, data=Burns)


#####################################################################################################################

10. Interpret the model in terms of the odds ratio. Use the Wald statistic to create a 95% confidence 
interval for the odds ratio.

#####################################################################################################################

# Wald Test:
# ----------------------
summary(LogBurn)
# The age column of the table contains the test output.

# Wald Confidence Interval:
# -------------------------
Lower<- (-1.3724-1.96*0.1354)
Upper<- (-1.3724+1.96*0.1354)
logCI<-c(Lower,Upper)
logCI
-1.637784 -1.107016
WaldCI<-exp(logCI)
WaldCI

0.1944104 0.3305438

when the area of burns go up 1 unit odds success would be mutiplied - 0.2534978
exp(-1.3724)
#####################################################################################################################

11. Test H0: ß1 = 0 vs Ha: ß1 not = 0 using both Walds test and the likelihood ratio test. State your conclusion based 
on these tests.

(For the likelihood ratio test, try R code of the form anova(mylogisticmodel, test='LRT').)

#####################################################################################################################

the likelyhood ratio model is derived by calculating the difference between the adequatcy of the full and restricted log-likelyhood models.






# OR
anova(logisticES,glm(cbind(cancer,healthy)~1,data=ES,family=binomial),test='LRT')

# In this case, there is only one predictor variable, so both forms of this command
# do the same test. This would NOT be the case if there were two predictors,
# as in the whick example!








validity vs relability..

#####################################################################################################################

12. Suppose a survey of UM undergraduates day-to-day economic circumstances is being planned, and investigators 
are deciding between a measure of income based solely on students’ wages, scholarships and loan disbursements, 
and an alternate measure combining all of those with allowances or other transfers from parents. Wages, 
scholarships and loans will be abstracted from tax and ﬁnancial aid documents, whereas investigators will 
have to rely on respondents’ self-reports of cash transfers from relatives.



Which of these two measures will be more reliable? Briefly explain.

A. 	a measure of income based solely on students wages, scholarships and loan disbursements

	B. 	an alternate measure combining all of those with allowances or other transfers from parents



#####################################################################################################################
A will be more reliable since B needs to bring in self reported data. for example if this was a case of a scale, and we (the investigator)
weighted all the subject we would get a pretty accurate picture of everyones weight, defiantly reliable. However, if we let each individual
self report we might get a similar answers but the that is not as reliable since it is self reported. Since Wages, scholarships and loans 
will be abstracted from tax and financial aid documents in the same way every time this data will have high reliability, where as 
the self reported data will have low reliability since the chance of lying- miss interpretation on a form here is greater....

#####################################################################################################################
13. 

Which of the two measures will have greater validity? Justify your answer in a sentence or two.

  A. 	a measure of income based solely on students' wages, scholarships and loan disbursements

B. 	an alternate measure combining all of those with allowances or other transfers from parents

#####################################################################################################################

even if A would give use high reliability over ti# Wald Test:
summary(LogBurn)
# The age column of the table contains the test output.
-10.14   <2e-16 ***
  
  reject the null

# Likelihood Ratio Test:
# ----------------------
# Either
anova(LogBurn,test='LRT')

Analysis of Deviance Table

Model: binomial, link: logit

Response: survive

Terms added sequentially (first to last)


Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
NULL                     434     525.39              
area.1  1   192.73       433     332.66 < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1me (if we took multiple samples over time, reading the tax documents would be consistent). however, since we are trying to infer something about the general college population not including finical support from parents, does not create a very good-true sample. 
so including only A in the study, where a lot of students get their money from parents, would not be a vary valid study since it is not a good representation of the populations that they are trying to infer something about. at the same time only looking at allowances or other transfers from parents
is not very valid either since we are missing out on what each students is making themselves in terms of scholarships and extra jobs. 

the best validity would be to combind both A and B. but since this questions in only asking to compare, i would say B have higher validity since i am assuming that most college students spending money does not come from scholarships or loans(that gos to paying for school) but day-to-day money is prol. more likely to come for the parents or other transfers.

