#homework 9

#     Members of the Donner party attempted a new route between Fort Bridger, Wyoming, and the Humboldt River, Nevada. This new route 
#     took much longer than expected, and the entire party was trapped in the Sierra Nevada in the winter of 1846-1847. By the time they 
#     were rescued in the spring, many members had died. Table 6.11 on page 198 lists the gender and survival status of all adults who 
#     were trapped during this trip.

#     One way to generate the data follows.

gender <- rep(c("male", "female"), times=c(53, 34))
status <- rep(c("survived", "died", "survived", "died"), times=c(23,30,25,9))
donner <- table(gender, status)
donner2 <- table(status, gender)

########################################################################################################################################
# 1. Plot the data with a segmented bar graph. Describe the overall patterns seen in the data. (You do not need to include the actual plot.)
#    Hint: There is a file, usefulRfunctions.R, found in the Resources tab, which has a function called stackedbarchart. 
#    This function uses the built-in barplot function, with several additional arguments to make the output easier to interpret.
#    You may use this function if you'd like.

#    Hint2: To swap the variables in the bar chart, an easy way is to change the order of arguments in table, e.g. 
#    from table(a, b) to table(b, a), then re-plot the new table.
########################################################################################################################################
Observations:

If we again look at a segmented bar graph: We see clearly
that there was more survivors than people who died
(not a huge gap but enough), we can also see that
ration of people who died was predominantly male, where as
if e look at ration of survivors, it was almost equal with
a slight skewed towards more female survivors (25>23). 


data:
  
stackedbarchart(donner)
donner
expect.count(donner)
  
status
gender   died survived
female    9       25
male     30       23

total male = 30 + 23 = 53
total femal = 9 + 25 = 34
total doner pary dide = 30 + 9 = 39 
total surviors = 25 + 23 = 47

femal male ration in the donner party = 34/(53+34) = 0.3908046
donner total survival rate = 47/(47+39) = 0.5465116
donner total death rate = 39/(47+39) = 0.4534884
female survivale rate = 25/(25+9) = 0.7352941
female death rate = 9/(25+9) = 0.2647059
male survivale rate = 23/(23+30) = 0.4339623
male death rate = 30/(23+30) 0.5660377
########################################################################################################################################
# 2. Is this an example of an experiment or an observational study?
########################################################################################################################################

A.   Experiment.
[B. 	Observational study.]

########################################################################################################################################
# 3. Is this a cross-classification, cohort, or case-control study?
########################################################################################################################################

A. 	Cross-classification study.
[B. 	Cohort study.]
C. 	Case-control study.

cross-classification - classification according to more than one attribute at the same time; 
"the cross-classification of cases was done by age and sex"

A cohort is a group of people who share a common characteristic or experience within a defined period (e.g., are born, are exposed 
to a drug or vaccine or pollutant, or undergo a certain medical procedure). Thus a group of people who were born on a day or in 
a particular period, say 1948, form a birth cohort. The comparison group may be the general population from which the cohort 
is drawn, or it may be another cohort of persons thought to have had little or no exposure to the substance under investigation, 
but otherwise similar. Alternatively, subgroups within the cohort may be compared with each other.

The case-control is a type of epidemiological observational study. An observational study is a study in which 
subjects are not randomized to the exposed or unexposed groups, rather the subjects are observed in order to 
determine both their exposure and their outcome status and the exposure status is thus not determined by the researcher.

Cohort, cross sectional, and case-control studies are collectively referred to as observational studies. 
Often these studies are the only practicable method of studying various problems, for example, studies of aetiology, 
instances where a randomised controlled trial might be unethical, or if the condition to be studied is rare. 

Cohort studies are used to study incidence, causes, and prognosis. Because they measure events in chronological 
order they can be used to distinguish between cause and effect. 

Cross sectional studies are used to determine prevalence. 
They are relatively quick and easy but do not permit distinction between cause and effect. 

Case controlled studies compare groups retrospectively. They seek to identify possible 
predictors of outcome and are useful for studying rare diseases or outcomes. They are often used to generate hypotheses 
that can then be studied via prospective cohort or other studies. 

########################################################################################################################################
# 4. Perform either a simulation study or a Fisher's exact test to test the one-sided hypothesis that females were more likely to survive. 
#    The p-value from your test was . Below, clearly indicate which test your performed, and state your conclusion.

#    You need not justify your choice of procedures. Optionally, include your R code below.

#    Hint: If you've set up your data as described above, prop would function similarly to mean if these were numerical instead of words.
########################################################################################################################################

##  The null hypothesis is that there is no association between the sex and survival rate, 
##  the alternative that there is a positive association between sex and survival rate (that the odds ratio is greater than 1) [if you have female, you are more likely to survive].

fisher.test(donner, alternative = "less")
#p-value = 0.00518

Fisher's Exact Test for Count Data

data:  donner
p-value = 0.00518
alternative hypothesis: true odds ratio is less than 1
95 percent confidence interval:
 0.000000 0.665633
sample estimates:
odds ratio 
 0.2802454

########################################################################################################################################
# 5. D. K. Grayson stated, "The differential fate of the members of the Donner Party lends strong support to the argument that females 
#   are better able than males to withstand conditions marked by famine and extreme cold." Does the small p-value lead us to 
#   conclude that females have stronger survival abilities than males? Can you suggest any other reasonable explanation?
########################################################################################################################################

help woman and children first is normal a concet that is used in emergency situations, 
so perhaps one of the resons why woman survided better than men is becouse they reviced more food than men since they where concidered to be
weaker. So, as a result of trying to feed woman and chldren first more men dided than woman, perhaps due to nutrituion levels rather than woman being
stronger in the cold.


########################################################################################################################################
# Answer the following questions for the data displayed in Table 6.5 on page 190.

# One way to generate the data follows.


########################################################################################################################################

lungcancer
smoker      no yes
nonsmoker 32  19
smoker    28  41


########################################################################################################################################
#  6. Which of the following was fixed before the study was conducted?

A.   Explanatory (row) variable.
[B.   Response (column) variable.]

########################################################################################################################################

Response variable: lung cancer is fixed, the reassrchers looked at lungcancer vs no lung cancer these number where fixed going into the study, 
and then asked the patient and the control group if they smoked or not.

########################################################################################################################################
#  7. Is this an example of an experiment or an observational study?

A. 	Experiment.
[B. 	Observational study.]

########################################################################################################################################



########################################################################################################################################
# 8. Is this a cross-classification, cohort or case-control study?

A. 	Cross-classification study.
B. 	Cohort study.
[C. 	Case-control study.]

########################################################################################################################################

the control group was people without cancer
the case group people with cancer
the outcome was smoker or non smoker

If we separately query lung cancer patients and others about past behavior, what patterns of smoking do we see?

########################################################################################################################################
# 9. Create a segmented bar chart for the data. Describe the overall patterns seen in the data. (You do not need to include the actual plot.)
#    Hint: There is a file, usefulRfunctions.R, found in the Resources tab, which has a function called stackedbarchart. 
#    This function uses the built-in barplot function, with several additional arguments to make the output easier to interpret. 
#    You may use this function if you'd like.

#    Hint2: To swap the variables in the bar chart, the easiest way is to likely change the order of arguments in table, e.g. from table(a, b) to table(b, a).
########################################################################################################################################

the group of people who did have cancer, have a lot higher ration of smoker to non-smokers
the goup of people who did not have cancer where about 50/50 split on smoking, non smoking, 
with a slight skew towards non smokers

smoking
stackedbarchart(smoking)
expect.count(smoking)

lungcancer
smoker      no yes
nonsmoker 32  19
smoker    28  41

lungcancer
smoker        no  yes
nonsmoker 25.5 25.5
smoker    34.5 34.5

########################################################################################################################################
# 10. Perform either a simulation study or a Fisher's exact test to test the one-sided hypothesis that smokers are more likely to 
#     have lung cancer. The p-value from your test was . Below, clearly indicate which test your performed, and state your conclusion.

#     You need not justify your choice of procedures. Optionally, include your R code below.
#     Hint: For categorical instead of numeric data, prop functions similarly to mean.
########################################################################################################################################

smoker <- rep(c("smoker", "nonsmoker"), times=c(69, 51))
lungcancer <- rep(c("yes", "no", "yes", "no"), times=c(41,28,19,32))
smoking <- table(smoker, lungcancer)
smoking2 <-table(lungcancer,smoker)

# I perform a Fisher's exact test to test the one-sided hypothesis that females were more likely to survive.

##  The null hypothesis is that there is no association between the smoking and lung cancer, 
##  the alternative that there is a positive
##  association (that the odds ratio is greater than 1) [if you have lung cancer, you are likely a smoker].
fisher.test(smoking2, alternative = "greater")
# => p-value = 0.01312 which means that we can see an association at a 95% confidence level

# Fisher's Exact Test for Count Data
data:  smoking
p-value = 0.01312
alternative hypothesis: true odds ratio is greater than 1
95 percent confidence interval:
  1.236336      Inf
sample estimates:
  odds ratio 
2.447101 

########################################################################################################################################
# 11. [Classwork from 3/26, Lecture 19. As the paper handout could not be distributed on that day, please type in your answers to (i)--(iii) below.]

#     (i) Translate P(S|C) = P(S|!C) into a equation involving a,b,c, and/or d. The algebraic expressions should involve no division. (slide 3/16)

#     (ii) Translate P(C|S) = P(C|!S) into an equation involving a,b,c, and/or d. The algebraic expressions should involve no division. (slide 3/16)

#     (iii) In table 3b of the CDC report on testing trends in the US (Resources/lectures/lec19-handout-b.pdf), 
#     footnote (c) serves to dispel an apparent contradiction between columns labeled "sample size", "No. persons tested", and "% tested".
#     Identify a specific (sample size, No. persons tested, % tested) triple in the table that exemplifies the contradiction,  
#     and briefly explain the apparent contradiction and its resolution.
########################################################################################################################################

(i)
P(S|C)= P(S|!C) 
a/(a+b) = c/(c+d) 
a*(c+d) = (a+b)*c 
ac + ad = ac + bc 
ad=bc 

(ii)
P(C|S) = P(C|!S) 
a/(a+c)= b/(b+d) 
(a+c)*b=a*(b+d) 
ab+ bc= ab + ad 
bc=ad

(iii)
