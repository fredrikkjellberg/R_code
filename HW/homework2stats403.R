#HOMEWORK #2

########################################################################################################################################
# 1. True or false, and justify briefly: in the schistosomiasis study, all 20 mice have to come from the same population 
#   of mice in order for the randomization test to be valid.
########################################################################################################################################

false. This is not a necessary assumption for a randomization test.
what is necessary however, is that they are ass close to each other as possible. 

########################################################################################################################################
# 2. What is the difference between a random sample and a randomized experiment? Explain briefly, in your own words.
########################################################################################################################################

A random sample is a sample taken from the randomly from some population so that every member of the population has an equal 
chance of being chosen in some sense. This should produce a result whose distribution is roughly the same as the original population distribution.

A randomized experiment is an experiment in which some number of participants are taken and randomly allocated between 
treatment and control groups.

########################################################################################################################################
# 3. Why should boxplots or other graphical techniques be used to visualize data before a parametric test is conducted? Answer in a sentence or two, 
#    identifying a specific problem that graphing your data can help you avoid.
########################################################################################################################################

Visualizing your data in this way can be a good way to check whether the assumptions of the parametric test seem reasonable. For example, a boxplot, 
histogram, or QQ plot can be used to assess whether its reasonable to assume that your data is normally distributed, which can help you avoid using 
tests that might produce misleading results.

assumptions of the parametric test:  that the groups that are being test are somewhat similar; equal variance is the best way to test this!

Plots of your data can also help you identify outliers which might be due to some sort of human error, e.g. entering "100" for a data point instead 
of "10".

########################################################################################################################################
# 4. Concerned about toxins in flea collars, a neighborhood group decides to test the effectiveness of herbal anti-flea treatments by conducting an RCT. 
#    Each cat in the neighborhood is assigned to flea collars (n=43) or to herbal treatment (n=41); one month later, the cats are rounded up and examined 
#    by a veternarian to determine whether they carry fleas (0 or 1). The vet is blinded to which experimental condition each pet belonged to. 

#    The results: 11 flea-collar cats had fleas, as did 19 of the herbal treatment cats.

#    Not having taken Stats 403, the neighborhood statistician uses a two-sample z-test to appraise effectiveness of the intervention. 
#    Based on this information, which of the three assumptions is most strained, and explain.
########################################################################################################################################

The 3 assumptions for a parmetric test is:
  random sample from the target population
  independent observations
  sample size/Normality condition

random sample from the target population:
  there seams to be r
  
  
independent observations:
  There is no effort made to isolate the cats, as cats can interact in ways which might affect their flea status. in this case this means 
  that the each cat flea count might depends onther cats flea count.
  

sample size/Normality condition:
  both of the sample groups sample sizes are over 30 which is the normal 
  treshhold for assuming normality.Flea collars (n=43) or to herbal treatment (n=41)
  
########################################################################################################################################
# 5. A public health worker observes that having a matchbook in one’s pocket is highly correlated with being a smoker. 
#    True or false, and briefly explain: This means the variable “matches in one’s pocket” is a confounding variable 
#    for studies of smoking and heart disease.
########################################################################################################################################

FALSE: To be confounding, a variable needs to interact with both treatment and response. While "matches in one's pocket" 
likely correlates to being a smoker, theres no reason to suspect a direct interaction with heart disease (the only connection between
"matches" and heart disease is through smoking.)

########################################################################################################################################
# 6. Yesterday was payday, so you picked up the tab for all of your friends. You didn’t check your bank account beforehand, 
#    but estimate that you had $550, plus or minus $40. You didn’t look at the check carefully before signing it, either, 
#    but you estimate it at $100, plus or minus $30. Which of the following best characterizes how much you’ve got in the bank now?

#    Optionally, explain your answer. (The main reason to exercise this option is if you want to justify an answer that you expect to differ from our answer.)

#    Hint: There's a formula that you can use to get the answer, but you should also be able to get to it using common-sense considerations 
#    and a process of elimination, as demonstrated in one of the lectures.  We recommend the common sense approach.
########################################################################################################################################

When you add (or subtract) two quantities which have error (variability), the result will have more error than either combined. 
So without doing any calculation, we know that plus or minus $50 is the only option.

Alternatively, we have the formula that, when A and B are independent,

var(A +/- B) = var(A) + var(B)

Taking the square root of each sides, we have that the standard deviation of the sum/difference of two quantities is the square root of the sum of squares.

> sqrt(30^2 + 40^2)
[1] 50

########################################################################################################################################
# 7. Suppose that in the study of schistosomiasis in female mice (Ch.1 of Kuiper and Sklar) the p-value was 0.95. Would you be able to 
#    conclude that there was no difference between the treatment and control means? (1-2 sentence explanation.)
######################################################################################################################################## 

No we would not be able to conclude that there was no difference between the treatment and control group. You would reject the null Hypothesis
(the null being that the difference is equal to 0, there is no difference) if the P - value is smaller than or equal to alpha. We Fail to reject
the null, since P is larger than a. There is not enough evidence to say that there is not difference. 

########################################################################################################################################
#    Assume we are obtaining sample data from a heavily skewed population. For each the following, is it TRUE that we should we expect
#    a histogram of the sample to have a roughly normal shape?
######################################################################################################################################## 

# 8 
A small sample from the population. 
FALSE
A sample from a non-normal population will itself look non-normal, regardless of sample size.

# 9
A large sample from the population.
FALSE
A sample from a non-normal population will itself look non-normal, regardless of sample size.
A large sample from the population.

# 10
A small sample of means of samples drawn from the population.
FALSE
When talking about the distributions of sample means, if the sample size is large enough OR the parent population is normal, 
then we can expect the distribution of sample means to look roughly normal. Neither of those conditions are met here.

# 11
A large sample of means of samples drawn from the population.
TRUE
The sample size is large enough for the Central Limit Theorem to apply.

######################################################################################################################################## 
# 12. Consider question 20 on page 12. Using a t-test , compute the one-sided p-value. Enter the p-value you obtained:

# Create a simulation to test the Music data. Use the technology instructions provided to randomly multiplyalora-
# Jbyeachobserveddifference.Thisrandomlyassignsanorder(Fastdiff- SlowdifforSlowdiff- Fastdiff).Then,foreachiteration,calculatethemeandifference.
# The p-value is the proportion oftimes yom simulation fmmd a mean difference greater than or equal to 1.857.
#  a. Create a histogram of the mean differences. Mark the area on the histogram that represents your p-value.
#  b. Use the p-value to state your conclusions in the context of the problem. Address random allocation and random sampling (or lack of either) 
#     when stating your conclusions.
######################################################################################################################################## 

# t test via mosaic
with(music, t.test(Fastdiff.Slowdiff, alternative="greater"))

# t test the normal 
musicmean <- with(music, mean(Fastdiff.Slowdiff))
musicsd <- with(music, sd(Fastdiff.Slowdiff))
n <- nrow(music)
# n = 28
df <- n - 1
t <- musicmean/(musicsd/sqrt(n))
# pulls data from T-Distribution
# 1 - T, b/c right side test
1 - pt(t, df)
# P-value of 0.01535094

#construct and observe the historgram of the sample distribution of x-bar.
reps <- 100
#makes an array of n numers in means so that we can use this vecotor in our function
means <- numeric(reps)

# for loop that will randomly reshuffle between groups ~shuffle(bldg, groups=loc)
for (i in 1:reps){
  means[i] <- mean(sample(music$Fastdiff.Slowdiff,28,replace=TRUE))
}

# music$Fastdiff.Slowdiff is the same as music[,7]
mean(sample(music$Fastdiff.Slowdiff,28,replace=TRUE))

hist(means, main="Sampling Distribution of X-bar",xlab="Sample Mean")

######################################################################################################################################## 
# 13. Data set: Mice - Using the data for the male mice, run a simulation to decide whether K11777 inhibits schistosome viability (i.e., 
#     reduces worm count) in male mice. (For each part, use at least 1000 simulations, preferably more.)

#     Describe the results, including a histogram of the simulation results (you do not need to include the histogram, a written 
#     description is sufficient), the p-value, and a summary statement indicating your conclusion about the research question of 
#     schistosome viability.
######################################################################################################################################## 


mice <- read.delim("~/Desktop/mice.txt")
                      
# Restructing the data to an easier format
treatment <- rep(c("trt", "ctl"), c(5,5))
count <- with(mice, c(Male.Trt, Male.Ctl))
                      
# The observed test statistic
obs <- diff(mean(count ~ treatment))
                      
# Test statistics from the null distribution
stats <- do(1000)*diff(mean(count ~ shuffle(treatment)))
                      
# Use this to find out the column name, and to make sure the results don't look too wrong
head(stats)
                      
with(stats, table(trt <= obs))
# 0.006
with(stats, hist(trt))

The conclusion is that the results we see are not likely due to chance, therefore we have evidence that the treatment is effective 
in reducing worm count.

######################################################################################################################################## 
# 14. Modify the code you created for the previous part to measure a difference in group medians instead of a difference in means for 
#     the male mice. 
######################################################################################################################################## 


obs <- diff(median(count ~ treatment))
stats <- do(1000)*diff(median(count ~ shuffle(treatment)))
with(stats, table(trt <= obs))[2]
6/1000

The p-value you obtained is:0.006

######################################################################################################################################## 
# 15. You might also wonder if there is a difference in the variability in the groups. Modify the code to test if the variances of each 
#     male group are equal. The p-value you obtained is
######################################################################################################################################## 

obs <- diff(var(count ~ treatment))

stats <- do(1000)*diff(var(count ~ shuffle(treatment)))

with(stats, table(trt <= obs))



# 6. 