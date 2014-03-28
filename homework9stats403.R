#homework 9

#     Members of the Donner party attempted a new route between Fort Bridger, Wyoming, and the Humboldt River, Nevada. This new route 
#     took much longer than expected, and the entire party was trapped in the Sierra Nevada in the winter of 1846-1847. By the time they 
#     were rescued in the spring, many members had died. Table 6.11 on page 198 lists the gender and survival status of all adults who 
#     were trapped during this trip.

#     One way to generate the data follows.

gender <- rep(c("male", "female"), times=c(53, 34))
status <- rep(c("survived", "died", "survived", "died"), times=c(23,30,25,9))
donner <- table(gender, status)

########################################################################################################################################
# 1. Plot the data with a segmented bar graph. Describe the overall patterns seen in the data. (You do not need to include the actual plot.)
#    Hint: There is a file, usefulRfunctions.R, found in the Resources tab, which has a function called stackedbarchart. 
#    This function uses the built-in barplot function, with several additional arguments to make the output easier to interpret.
#    You may use this function if you'd like.

#    Hint2: To swap the variables in the bar chart, an easy way is to change the order of arguments in table, e.g. 
#    from table(a, b) to table(b, a), then re-plot the new table.
########################################################################################################################################


########################################################################################################################################
# 2. Is this an example of an experiment or an observational study?
########################################################################################################################################

A.   Experiment.
[B. 	Observational study.]

########################################################################################################################################
# 3. Is this a cross-classification, cohort, or case-control study?
########################################################################################################################################

A. 	Cross-classification study.
B. 	Cohort study.
C. 	Case-control study.

########################################################################################################################################
# 4. Perform either a simulation study or a Fisher's exact test to test the one-sided hypothesis that females were more likely to survive. 
#    The p-value from your test was . Below, clearly indicate which test your performed, and state your conclusion.

#    You need not justify your choice of procedures. Optionally, include your R code below.

#    Hint: If you've set up your data as described above, prop would function similarly to mean if these were numerical instead of words.
########################################################################################################################################



########################################################################################################################################
# 5. D. K. Grayson stated, "The differential fate of the members of the Donner Party lends strong support to the argument that females 
#   are better able than males to withstand conditions marked by famine and extreme cold." Does the small p-value lead us to 
#   conclude that females have stronger survival abilities than males? Can you suggest any other reasonable explanation?
########################################################################################################################################






########################################################################################################################################
# Answer the following questions for the data displayed in Table 6.5 on page 190.

# One way to generate the data follows.

smoker <- rep(c("smoker", "nonsmoker"), times=c(69, 51))
lungcancer <- rep(c("yes", "no", "yes", "no"), times=c(41,28,19,32))
smoking <- table(smoker, lungcancer)
########################################################################################################################################



########################################################################################################################################
#  6. Which of the following was fixed before the study was conducted?

A.   Explanatory (row) variable.
B.   Response (column) variable.

########################################################################################################################################



########################################################################################################################################
#  7. Is this an example of an experiment or an observational study?

A. 	Experiment.
B. 	Observational study.

########################################################################################################################################




########################################################################################################################################
# 8. Is this a cross-classification, cohort or case-control study?

A. 	Cross-classification study.
B. 	Cohort study.
C. 	Case-control study.

########################################################################################################################################


########################################################################################################################################
# 9. Create a segmented bar chart for the data. Describe the overall patterns seen in the data. (You do not need to include the actual plot.)
#    Hint: There is a file, usefulRfunctions.R, found in the Resources tab, which has a function called stackedbarchart. 
#    This function uses the built-in barplot function, with several additional arguments to make the output easier to interpret. 
#    You may use this function if you'd like.

#    Hint2: To swap the variables in the bar chart, the easiest way is to likely change the order of arguments in table, e.g. from table(a, b) to table(b, a).
########################################################################################################################################




########################################################################################################################################
# 10. Perform either a simulation study or a Fisher's exact test to test the one-sided hypothesis that smokers are more likely to 
#     have lung cancer. The p-value from your test was . Below, clearly indicate which test your performed, and state your conclusion.

#     You need not justify your choice of procedures. Optionally, include your R code below.
#     Hint: For categorical instead of numeric data, prop functions similarly to mean.
########################################################################################################################################




########################################################################################################################################
# 11. [Classwork from 3/26, Lecture 19. As the paper handout could not be distributed on that day, please type in your answers to (i)--(iii) below.]

#     (i) Translate P(S|C) = P(S|!C) into a equation involving a,b,c, and/or d. The algebraic expressions should involve no division. (slide 3/16)

#     (ii) Translate P(C|S) = P(C|!S) into an equation involving a,b,c, and/or d. The algebraic expressions should involve no division. (slide 3/16)

#     (iii) In table 3b of the CDC report on testing trends in the US (Resources/lectures/lec19-handout-b.pdf), 
#     footnote (c) serves to dispel an apparent contradiction between columns labeled "sample size", "No. persons tested", and "% tested".
#     Identify a specific (sample size, No. persons tested, % tested) triple in the table that exemplifies the contradiction,  
#     and briefly explain the apparent contradiction and its resolution.
########################################################################################################################################


