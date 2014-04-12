#1. Assess the assumptions for a one-way anova, using either but not both of the bldg and loc factors. 
#   (Pick the one whose relationship to bacteria counts you personally are more interested in knowing about.) 
#   Include an informal check of equality of variances, and whatever other diagnostics you deem relevant.

attach(bacteria)
#Compute descriptive statistics by group:
# Note: using the tapply() function since both loc bldg are categorical variables. 

#group by locations [door, faucet etc.]
tapply(count, loc:bldg, mean)
tapply(count, loc:bldg, median)
tapply(count, loc:bldg, sd)
tapply(count, loc:bldg, IQR)
tapply(count, loc:bldg, range)

#group by building [ARH, Dibble etc.]
tapply(count, bldg:loc, mean)
tapply(count, bldg:loc, median)
tapply(count, bldg:loc, sd)
tapply(count, bldg:loc, IQR)
tapply(count, bldg:loc, range)

mean(count[bldg=="ARH"])
mean(count[bldg=="Dibble"])
mean(count[bldg=="Noyce"])
mean(count[bldg=="Cowles"])
mean(count[bldg=="Norris"])
mean(count[bldg=="Yellow House"])

#looking at the resutls from the above computations I am really more intrested to see if any of the different 
# locations () hade significanly more bacteria than the others etc.

# respons variable = bacteria count [count]
# factor i want to look close at = location [loc]

#INFORMAL CHECKS
# Find the average count for the locations:
mean(count[loc=="Faucet"])
mean(count[loc=="Door"])
mean(count[loc=="Desk"])
mean(count)
summary(count)
plot(count~bldg)
plot(count~loc)

# informal check of equality of variance
# largest S.d - smallesr S.d. < 2 
tapply(count, loc, sd)
#     Desk     Door   Faucet 
# 500.6360 354.2147 488.6509 
500.6360/354.2147
# 1.413369 < 2 which means it passes the informal test of variance

:::::::::MODEL ANSWER:::::::
  
  We can look at the standard deviations in each of the groups defined by building by using the following:
  
  > with(bacteria,sd(count~bldg))

ARH       Cowles       Dibble       Norris        Noyce Yellow House
509.7478     302.6071     169.8439     116.2957     403.0095     262.1160

The maximum standard deviation is about five times the minimum standard deviation here, so the informal check of variance fails. The ratio is approximately 510 / 116, which is much larger than 2.

If by contrast we picked the location group, we get the following:
  
  > with(bacteria,sd(count~loc))
Desk     Door   Faucet
500.6360 354.2147 488.6509

Here the informal equality of variances check is OK. The maximum over the minimum of the standard deviations is approximately 500 / 354 < 2.

:::::END OF MODEL ANSWER:::::
# 2. Perform a one-way ANOVA with the factor you chose at step 1, as well as a two way ANOVA using both factors 
#    (but no interactions). (Code hint: for the two-way ANOVA you can do anova(lm(count~bldg+loc, data=bacteria)).) 
#    Include the resulting anova tables in your answer, and interpret how they bear on the question of whether your 
#    chosen factor associates with bacteria.

#perform a one-way ANOVA with the factor loc
count_1W_ANOVA <- anova(lm(count~loc, data=bacteria))
count_1W_ANOVA
#Analysis of Variance Table
#Response: count
#Df  Sum Sq Mean Sq F value Pr(>F)
#loc        2  206372  103186  0.5034  0.609
#Residuals 33 6763725  204961   
# P-value for Loc = 0.609 => NOT VERY SIGNIFICANT!

#perform a two-way ANOVA with the factor loc and bldg 
count_2W_ANOVA <- anova(lm(count~bldg+loc, data=bacteria))
summary(count_2W_ANOVA)
#          Analysis of Variance Table
#Response: count
#Df  Sum Sq Mean Sq F value    Pr(>F)    
# bldg       5 3845562  769112  7.3797 0.0001602 ***
# loc        2  206372  103186  0.9901 0.3841829    
#Residuals 28 2918163  104220  
# P-value for Loc = 0.3841829 => better than 0.6 but NOT close to being SIGNIFICANT!
# P-value for bldg = 0.0001602 => much better than Loc and signifcant down to 0 level! = Statistcally significant!
# we can therefore say with confidence that depedning on witch building they sampled 
# we will se sigificant different levels of bacteria count

# 3. Looking at the estimated regression coefficients in the output from lm(count~bldg+loc, data=bacteria), 
#    what is the reference category for the bldg factor? ARH
lm(count~bldg+loc, data=bacteria)

# 4. The numerical value that the fitted two-way ANOVA model predicts for observations of doors in the 
#    Cowles building is . (Hint: refer to output from lm(count~bldg+loc, data=bacteria), and to lecture 14.)
lm(count~bldg+loc, data=bacteria)
intercept + bldgCowles + locDoor
CowlesDoor = (878.06 + (-517.67) + (-142.58))
CowlesDoor
# Prediction forCowlesDoor = 217.81

# 5. The mean of all observations collected from doors is .
lm(count~bldg+loc, data=bacteria)
# intercept + locDoor
justDoor = (878.06 + (-142.58))
justDoor
# Prediction for justDoor = 735.48

# 6. The mean of all observations collected from the Cowles building is:
lm(count~bldg+loc, data=bacteria)
# intercept + Cowles
justCowles = (878.06 + (-517.67))
justCowles
# Prediction for justCowles = 360.39

# 7. In the two-way ANOVA model, what main effect is estimated for the Doors level of the loc factor? 
#    (Hint: Calculation of main effects is discussed in section 4.6 of the textbook.)

# How do I calculate effect size?
# assuming that this is the grand mean = mean(count)
# mean of grand mean (count) - mean of Door factor
mean(count[loc=="Door"])-mean(count)
#  main effect of doors = -105.5278
# or I can do this using the code given in class it will give me the same answer:
mean(count~loc) - mean(count)
#Desk       Door     Faucet 
#37.05556 -105.52778   68.47222 

# 8.

shuffle(bldg, groups=loc)
# shuffled is used to reshuffle the count variables within groups, in this case due to groups=loc, the groups is loc
# this means that within each group (i.e. desk) we have 6 different blgd and each blgd is associated with a count. when we shuffle around the data
# all count that was associated with desk is still associted with desk, they are just paired with a different building. this means that we are no 
# shuffling around where each surface each sample was taken form but from which building.

#this idea is clear if we compute these two plots!
plot(count~bldg)
plot(count~loc)
plot(count~shuffle(bldg, groups=loc)+loc)
# count~loc is same after the shuffeling! but not bldg!

# I have decided to peform a permuntaiton with 1000 reshuffles

obs.f <- anova(lm(count~bldg+loc, data=bacteria))[1, "F value"]
# 7.379694
fs <- do(10000)*anova(lm(count~shuffle(bldg, groups=loc)+loc, data=bacteria))[1, "F value"]

with(fs, table(result >= obs.f))
# FALSE  TRUE 
# 999     1 
p_value <-1/1000
# p_value = 0.001

# this means that even if we dont think we get a P-value of 0.001 which means that out 1000 
# permutaitons only one F stats is greater or equal to our test statiscs
# clearly it is therefore statiscally significant and aliges with our previouse analysis when we assumed the fisher
# assumptions holds true.