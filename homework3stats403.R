#HOMEWORK #3

########################################################################################################################################
# 1. Conducting a randomization test, a student produces R code including the following:

library(mosaic)
signs = c(-1, +1)
data1 = with(data0,do(500)*mean(sample(signs, 32, replace=TRUE)*Y))

#  Match the R objects to the corresponding statistical concepts.
########################################################################################################################################

signs is just a vector with elements -1 and 1, and holds no meaning.

The original which is apparently worked with is data0, so thats the data distribution.

data1 is the test statistics (sample means) from the simulation study, so it is a sampling distribution.

########################################################################################################################################
# 2. Which of the following R commands might reasonably arise in the process of conducting a randomization-based test for a study using 
#    *paired* random assignment?  And how do you know? (Assume 19 is the sample size.)
########################################################################################################################################

#A.	A and B would be methods of doing simulations of treatment vs control groups.
results = do(500) * diff(mean(outcome ~ shuffle(group)))
#B.	
results = do(500) * diff(mean(outcome ~ resample(group, 19)))
#C.	C has no randomization and thus would not be useful.
results = do(500) * diff(mean(outcome ~ group))
#D.	[correct!!!] D would sometimes change the order of pairing, and thus would be most correct.
results = do(500) * mean(resample(c(-1, +1), 19) * outcome_differences)
#E.	E would likely not do anything useful (group times difference?).
results = do(500) * mean(shuffle(group) * outcome_differences)

########################################################################################################################################
#    Data set: Twins - In a 1990 study by Suddath et al., reported in Ramsey and Schafer, researchers used magnetic resonance imaging to 
#    measure the volume of various regions of the brain for a sample of 15 monozygotic twins, where one twin was affected with 
#    schizophrenia and the other was unaffected. The twins were from North America and comprised eight male pairs, and seven female pairs 
#    ranging in age from 25 to 44 at the time of the study. The sizes in volume (cm3) of the hippocampus as in the file called Twins.
########################################################################################################################################

# 6. Should the data be analyzed as match pairs or be treated as if there were two independent samples?

Matched pairs: We are comparing twins to hold all other differences (e.g. age, height, etc) constant. Therefore, we are pairing the twin measurements.

# 7. Use either appropriate graphics or summary statistics to describe the difference in brain volume for the affected and unaffected twins.

summary(with(Twins, Difference))
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-0.1900  0.0550  0.1100  0.1987  0.3150  0.6700 

summary(Twins)
Pair..       Unaffected       Affected      Difference     
Min.   : 1.0   Min.   :1.250   Min.   :1.02   Min.   :-0.1900  
1st Qu.: 4.5   1st Qu.:1.600   1st Qu.:1.31   1st Qu.: 0.0550  
Median : 8.0   Median :1.770   Median :1.59   Median : 0.1100  
Mean   : 8.0   Mean   :1.759   Mean   :1.56   Mean   : 0.1987  
3rd Qu.:11.5   3rd Qu.:1.935   3rd Qu.:1.78   3rd Qu.: 0.3150  
Max.   :15.0   Max.   :2.080   Max.   :2.02   Max.   : 0.6700 

The mean brain volume appears to be higher in unaffected twins than in affected twins.

You could also attach a histogram of the differences, which you could generate using the command:
  
with(Twins,hist(Difference))

or

hist(Twins$Difference)

# 8. Use the appropriate permutation test to ascertain if the difference in brain volume described in Part B is the result of 
#    schizophrenia or if it could be explained as a chance difference. The p-value you obtained was Correct, and give your conclusion below.

library(mosaic)

# This line needs to refer to your location for Twins, alternatively just load
# it in Rstudio from Tools -> Import Dataset -> From Text File
twins <- read.delim("~/Desktop/Twins.txt")

# I need to use `with` a lot here because I'm doing everything inside the `twins`
# data.
obs <- with(twins, mean(Difference))

# How many observations does twins have?
dim(twins)

# The first entry is the number of rows, the second the number of columns [15  4]
[15  4]

# still kinda unsure how this works, wish there was somthing that would be able to exlapin this for me.
reps <- do(1000)*with(twins, mean(resample(c(-1,1), 15) * Difference))

# Look at reps to find out the name of the variable in it
head(reps)

# We want any difference, so we need both tails
# obs was positive, ~0.19866667
with(reps, table(result <= -obs | result >= obs))
# I observe 2 True, so
3/1000

