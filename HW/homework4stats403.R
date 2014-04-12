#HOMEWORK #4

########################################################################################################################################
# 1. Explain the difference between a population model and a randomization model.
########################################################################################################################################

A population model assumes that there is a population distribution which we are taking a random sample from, 
and we wish to generalize from our sample to the population as a whole.

A randomization model assumes that we have randomly assigned participants in the study into different groups 
(e.g. treatment and control) which will experience different conditions in the study, and tests whether there 
is any difference between the groups that cannot be easily explained by chance differences in allocation between the groups.

########################################################################################################################################
# 2. Give an example of a study involving drop out but not non-compliance, or a study involving non-compliance but not drop out, 
#    explaining briefly why your study involves one but not the other.

#    Give a different example than the Adams-Smith voter turnout study (discussed in Lecture 6).  Your example can be hypothetical.
########################################################################################################################################

"You can't really have an observational study with random assignment into treatment and control groups" - comment

An example of a study involving drop out but not non-compliance would be an experiment where people suffering from cancer are randomly 
allocated in between a placebo group and a group which recieves a new cancer treatment. Every patient in the study complies fully 
with all the instructions, but some of them die from their illness during the course of the study. Here there is full compliance, but drop out.

An example of a study involving non-compliance but not drop out would be an experiment to see if a new exercise is effective for 
losing weight. Participants in the study are randomly allocated between an exercise group and a non-exercise group,and weighed 
each week. However some of the exercise group participants are exhausted by the exercise, and stop completing it each week. 
They still continue to show up for the weigh-ins however. Here all study participants are still reporting data, so nobody has 
dropped out. However, there are non-compliers in the experiment.

########################################################################################################################################
Recall Adams and Smith's (1983) voter turnout study (Lecture 6), the results of which were as follows:

Experimental      condition         Turned out     Didn't                                                                                                      
assignment         received            to vote           vote                                                                                                  
-----------------    -----------------        ----------          ------                                                                                                          
  solicit vote        vote solicited        310                640

solicit vote          no contact            82                 293

no solicitation    vote solicited        0                    0

no solicitation    no contact           315               1010

(For both tests, test against the alternative hypothesis that solicitations to vote increase voting.  
 For Fishers exact test, you can either use fisher.test in R or an online Fisher test calculator (search "Fisher exact test").  
 Either way pay careful attention to whether your p-value is two-sided or one-sided, and if the latter, which one: its easy 
 for the computer to misunderstand you about this.)
########################################################################################################################################
# 3. Test the hypothesis of no effect using a "per protocol," or "as treated," form of comparison. The p-value from Fisher's exact test is:
########################################################################################################################################
library(mosaic)

# I'm ignoring the no solicitation/vote solicited combination as it was empty
assignment <- rep(c("solicit", "none"), c(310 + 640 + 82 + 293, 315 + 1010))
received <- rep(c("solicited", "none"), c(310 + 640, 82 + 293 + 315 + 1010))
voted <- rep(c("voted", "none", "voted", "none", "voted", "none"), c(310, 640, 82, 293, 315, 1010))

# Check everything worked out
table(voted, received, assignment)
# Note that the order you enter those three arguments can greatly change
# how the table looks.

# For "as treated", we ignore `assignment` as we only care about treatment
table(voted, received)
fisher.test(voted, received, alternative="greater")

########################################################################################################################################
# 4. Test the hypothesis of no effect using an "intention to treat," or "as assigned," form of comparison. The p-value from Fisher's 
#    exact test is Incorrect.
########################################################################################################################################

library(mosaic)

# I'm ignoring the no solicitation/vote solicited combination as it was empty
assignment <- rep(c("solicit", "none"),c(310 + 640 + 82 + 293, 315 + 1010))
received <- rep(c("solicited", "none"),c(310 + 640, 82 + 293 + 315 + 1010))
voted <- rep(c("voted", "none", "voted", "none", "voted", "none"),c(310, 640, 82, 293, 315, 1010))

# Check everything worked out
table(voted, received, assignment)
# Note that the order you enter those three arguments can greatly change
# how the table looks.

# For "as assigned", we ignore `received`
table(voted, assignment)
fisher.test(voted, assignment, alternative="greater")

########################################################################################################################################
# 5. Which of the two tests gives evidence of the effect of GOTV solicitations what will be more persuasive to political scientists who 
#    conduct experiments? Comment briefly.
########################################################################################################################################

The intention to treat analysis should be more persuasive.

This is because even though the as treated analysis has a smaller p-value than the intention to treat, it potentially breaks the random 
allocation between the treatment and control groups. This is for a number of reasons.

First, the decision about who is a non-complier is vague, and often researchers can find excuses to make results look better for 
themselves and their hypothesis by finding excuses to make people whose results deviate from their hypothesis "non-compliers" so 
that they don't count against it.

Second, who becomes a non-complier may itself be non-random, e.g. in this case maybe people who don't comply with the terms of the 
voting study are also less likely to vote for some reason, e.g. they have something else to do that is keeping them busy.

In this case you can no longer be sure that randomization is working to "even out" the differences in the groups.

On the other hand, the intention to treat study doesn't suffer from these weaknesses, and if non-compliers are included in the 
treatment group, this should only result in a potential reduction of the power of the test, not the introduction of additional bias. 
It is a more conservative test.

Since the ITT analysis also results in a small p-value, we can reject the null even without worrying about the additional potential for 
bias from the as treated analysis, so this should be more persuasive to most political scientists.


########################################################################################################################################
# 6. Which of these two tests gives evidence of the effect of GOTV solicitations that's more persuasive to you? Comment briefly.
########################################################################################################################################

the second will still reject the null (with margin) but is correctly performed and we are using all the data collected and not just 
droping out data bc we hade a non-complinace problem....

########################################################################################################################################
# 7. Which of these two tests gives evidence of the effect of GOTV solicitations that's more persuasive to you? Comment briefly.
########################################################################################################################################

Power Simulation for the Cheating Spouse Experiment


Recall the cheating spouse  experiment described in Lecture 4:
  
  Question form          Told     Didn't Tell       Total
--------------------------        --------  -----------------  ----------
cheating husband          7              3                 10
cheating wife                4              6                  10
Total                            11            9                  20

Conduct a simulation study of the power to reject the null hypothesis that it makes no difference whether the husband or the wife was 
described as cheating.

Be sure to read the remainder of the question before attempting. Report your sample size, the alternative distribution (the proportions 
in each group) and your calculated power.

You may give your R code to receive partial credit if your answer is incorrect. (Optional)

Instructions:

Interpret "reject" as p < .05, using the same one-sided alternative hypothesis as was discussed in class.  In your simulation, there 
should be two treatment conditions of equal size, as in the actual experiment, but in contrast to the actual experiment the size of 
each of these two groups should be equal to the first two digits of your student ID number. For example, if your student ID number 
is 12345678, then your simulated experiment should have treatment and control groups of size 12.

Simulate experimental results in these two groups from probability distributions as follows:

in the group that is told that a wife has cheated, the probability of reporting the cheating is determined by taking the last two 
digits in your student ID number, dividing by two, rounding down to the nearest integer if this did not result in an integer, and 
then dividing by 100. (For example, with student ID number 1234-5678, you get a probability of .39, 39%. With a student ID number 
of the 8765-4321, you get a probability of .10.)
In the group that is told that a husband is cheated, you choose the probability of reporting the cheating – any probability greater than 1/2.

If your student ID would yield awkward values (e.g. a sample size in the single digits or a proportion near 0%), use other values in your 
ID to come up with a more reasonable setup. Be sure to comment if you have to do this.


Notes: Lab 3 covered some relevant computing techniques.  Referring to code discussed in that lab and available in the lab folders 
on Ctools, you can:

mimic simulating from the required outcome distributions, by modifying the parts of the code that involved resample and husband10 
or wife10;
simulate the outcome distributions and conduct a Fisher test all on one line, by adapting the simulated.cheat.expt function below 
(a similar function was probably demonstrated in your lab) and then cutting and pasting it into R;
after repeating the simulation at least 1000 times, estimate the probability of rejecting the null hypothesis that your 
assumptions correspond to.



Here's the simulated.cheat.expt function. It will require a bit of adaptation to get it to embody your assumptions. 
We recommend that you start with the function as written, cutting and pasting it into R to confirm that you can get it to work, 
and then adapting it to your needs.  (Don't forget to paste the revised function into R once you're done with it!)


simulated.cheat.expt <- function(n) {
  stopifnot(require("mosaic"))
  husband <- rep(c("Tell", "Don't"),   times = c( 7,  3))
  wife    <- rep(c("Tell", "Don't"),   times = c( 4,  6))
  ft <- fisher.test(rep(c("Husband", "Wife"), each = n),
                    c(resample(husband, size=n),
                      resample(wife,    size=n))
  )
  return(ft$p.value)
}




Let's assume my sample size comes out to 27, and my alternative is .68 in Husbands and .31 in Wives.

The updated function would be (with changes hightlighted in green):

simulated.cheat.expt <- function(n) {
stopifnot(require("mosaic"))
husband <- rep(c("Tell", "Don't"),   times = c( 68, 100-68))
wife    <- rep(c("Tell", "Don't"),   times = c( 31, 100-31))
ft <- fisher.test(rep(c("Husband", "Wife"), each = n),
c(resample(husband, size=n),
resample(wife,    size=n))
)
return(ft$p.value)
}

Note that when I create husband and wife, I don't care about the size of each vecotr.
I simply need the proportion in each to be the alternative - I could make those as large or as small as I like. 
I used a length of 100 each for simplicity.

Then, the following would give me my results:

library(mosaic)
pvals <- do(10000)*simulated.cheat.expt(27)
table(pvals < .05)

For my values, the power would be around 68.3%.

Your power will fluctuate greatly. A smaller sample size leads to a smaller power and vice-versa for a 
large sample size. Also, how far away your two proportions are will affect it as well 
(e.g. with proportions .48 and .52, you should expect a small power, even for decently large sample size. 
 On the other hand, with proportions .12 and .94, you should expect a large power, even for small sample sizes.)


size <- c(20, 24, 27, 30, 35, 40)
power <- c(.022, .023, .032, .026, .041, .028)

plot(x    = size,
     y    = power,
     type = 'b',
     lwd  = 2,
     lty  = 4,
     pch  = 16,
     cex  = 2,
     col  = "red",
     main = "Power Curve",
     xlab = "Sample size (n)",
     ylab = "Power of one-sided alternative",
     ylim = c(0,0.06),
     xlim = c(0, 50)
)
